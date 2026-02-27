#' Database Connection and Query Functions
#'
#' Functions for connecting to and querying the CauloBrowser DuckDB database.
#'
#' @name fct_database
#' @noRd

#' Get a DuckDB connection
#'
#' Returns a DBI connection to the CauloBrowser DuckDB database.
#' Uses the path from golem-config.yml.
#'
#' @param db_path Optional path override; defaults to config value.
#' @return A DBI connection object.
#' @noRd
get_db_connection <- function() {
  db_path <- get_golem_config("db_path")

  # system.file() silently returns "" when the package is not found
  if (!nzchar(db_path)) {
    stop(
      "Database path could not be resolved.\n",
      "  Set the CAULOBROWSER_DB_PATH environment variable to an existing .duckdb file, or\n",
      "  run `caulobrowser::generate_example_database()` to create a demo database.",
      call. = FALSE
    )
  }

  if (!file.exists(db_path)) {
    stop(
      "Database file not found: ",
      db_path,
      "\n",
      "  Set CAULOBROWSER_DB_PATH to an existing .duckdb file, or\n",
      "  run `caulobrowser::generate_example_database()` to create a demo database.",
      call. = FALSE
    )
  }

  message("Connecting to DuckDB: ", db_path)

  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = db_path,
    read_only = TRUE
  )

  # Verify the database has the expected tables
  tables <- DBI::dbListTables(con)
  required <- c(
    "genes",
    "experiments",
    "experiment_conditions",
    "de_results",
    "timecourse_expression"
  )
  missing <- setdiff(required, tables)

  if (length(missing) > 0) {
    close_db_connection(con)
    stop(
      "Database is missing required tables: ",
      paste(missing, collapse = ", "),
      "\n",
      "  Database path: ",
      db_path,
      "\n",
      "  Run `caulobrowser::generate_example_database()` to create a demo database.",
      call. = FALSE
    )
  }

  return(con)
}

#' Disconnect from DuckDB
#' @param con A DBI connection object.
#' @noRd
close_db_connection <- function(con) {
  DBI::dbDisconnect(con, shutdown = TRUE)
}


#' Search genes by name, CC tag, or gene ID
#'
#' @param con DBI connection
#' @param query Character string: gene name, CC tag (e.g. CCNA_00090), gene ID,
#'   or a comma/semicolon-separated list of any of the above.
#' @return A data.frame of matching gene records.
#' @noRd
search_genes <- function(con, query) {
  terms <- trimws(unlist(strsplit(query, "[,;\\s]+")))
  terms <- terms[nchar(terms) > 0]
  terms <- tolower(terms)

  if (length(terms) == 0) {
    return(data.frame())
  }

  placeholders <- paste(rep("?", length(terms)), collapse = ", ")

  sql <- glue::glue(
    "SELECT *
   FROM genes
   WHERE LOWER(gene_name) IN ({placeholders})
      OR LOWER(cc_tag)    IN ({placeholders})
      OR LOWER(gene_id)   IN ({placeholders})
   ORDER BY gene_name"
  )

  # Same terms bound three times (one per IN clause)
  DBI::dbGetQuery(con, sql, params = rep(terms, 3))
}


#' Get timecourse expression data for given gene IDs
#'
#' Joins `timecourse_expression` with `experiment_conditions`, `experiments`,
#' and `genes`.  Returns column names that match the plotting layer:
#'   - `experiment_type`   (aliased from `experiments.data_type`)
#'   - `timepoint_minutes` (aliased from `experiment_conditions.condition_value`)
#'   - `experiment_id`, `display_label`, `doi` for per-experiment plot cards
#'
#' @param con DBI connection
#' @param gene_ids Character vector of gene IDs.
#' @param genetic_background Optional filter on `experiments.genetic_background`
#'   (e.g. `"wildtype"`).
#' @return A data.frame with columns `gene_id`, `experiment_id`, `experiment_type`,
#'   `display_label`, `doi`, `timepoint_minutes`, `expression_value`,
#'   `condition_label`, `gene_name`, `cc_tag`.
#' @noRd
get_expression_data <- function(con, gene_ids, genetic_background = NULL) {
  placeholders <- paste(rep("?", length(gene_ids)), collapse = ", ")

  base_sql <- "
    SELECT
      tc.gene_id,
      exp.experiment_id,
      exp.data_type        AS experiment_type,
      exp.display_label,
      exp.doi,
      ec.condition_value   AS timepoint_minutes,
      tc.expression_value,
      tc.condition_label,
      g.gene_name,
      g.cc_tag
    FROM timecourse_expression tc
    JOIN genes g
      ON tc.gene_id        = g.gene_id
    JOIN experiments exp
      ON tc.experiment_id  = exp.experiment_id
    JOIN experiment_conditions ec
      ON tc.experiment_id  = ec.experiment_id
     AND tc.condition_label = ec.condition_label
    WHERE tc.gene_id IN ({placeholders})
      AND exp.experiment_class = 'timecourse'"

  if (!is.null(genetic_background)) {
    sql <- glue::glue(paste0(
      base_sql,
      "
      AND exp.genetic_background = ?
    ORDER BY exp.data_type, ec.condition_value"
    ))
    params <- c(gene_ids, genetic_background)
  } else {
    sql <- glue::glue(paste0(
      base_sql,
      "
    ORDER BY exp.data_type, ec.condition_value"
    ))
    params <- gene_ids
  }

  DBI::dbGetQuery(con, sql, params = params)
}


#' Get distinct genetic backgrounds for timecourse experiments
#'
#' @param con DBI connection
#' @return Character vector of distinct `genetic_background` values.
#' @noRd
get_timecourse_backgrounds <- function(con) {
  DBI::dbGetQuery(
    con,
    "SELECT DISTINCT genetic_background
     FROM experiments
     WHERE experiment_class = 'timecourse'
       AND genetic_background IS NOT NULL
     ORDER BY genetic_background"
  )$genetic_background
}


#' Get DE results for given gene IDs
#'
#' @param con DBI connection
#' @param gene_ids Character vector of gene IDs.
#' @return A data.frame with columns `gene_id`, `experiment_id`, `log2fc`,
#'   `padj`, `gene_name`, `cc_tag`, `display_label`.
#' @noRd
get_de_results <- function(con, gene_ids) {
  placeholders <- paste(rep("?", length(gene_ids)), collapse = ", ")

  sql <- glue::glue(
    "SELECT
       dr.gene_id,
       dr.experiment_id,
       dr.log2fc,
       dr.padj,
       g.gene_name,
       g.cc_tag,
       exp.display_label
     FROM de_results dr
     JOIN genes g         ON dr.gene_id       = g.gene_id
     JOIN experiments exp ON dr.experiment_id = exp.experiment_id
     WHERE dr.gene_id IN ({placeholders})
     ORDER BY exp.experiment_id, dr.gene_id"
  )

  DBI::dbGetQuery(con, sql, params = gene_ids)
}


# ── Stubs future additions ──────────────────────────────────────────────────
# fitness and protein_localization are not in the current schema.
# These stubs keep existing UI code from crashing; callers already handle
# empty results gracefully (showing "—").

#' @noRd
get_fitness_data <- function(con, gene_ids) {
  data.frame(
    gene_id = character(),
    essentiality_class = character(),
    stringsAsFactors = FALSE
  )
}

#' @noRd
get_localization_data <- function(con, gene_ids) {
  data.frame(
    gene_id = character(),
    cell_cycle_stage = character(),
    localization_zone = character(),
    stringsAsFactors = FALSE
  )
}
