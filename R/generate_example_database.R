#' Generate an example CauloBrowser database
#'
#' Creates a DuckDB database file pre-populated with demo data for five
#' *Caulobacter crescentus* NA1000 genes across two experiments: a
#' cell-cycle RNA-seq timecourse and a CtrA-depletion DE comparison.
#'
#' @param path Path where the `.duckdb` file should be written.
#'   Defaults to `"caulobrowser_example.duckdb"` in the current working
#'   directory.
#' @param overwrite Logical. If `TRUE` (default) an existing file at
#'   `path` is removed before creating the new database.
#'
#' @return The resolved `path`, invisibly.
#' @export
generate_example_database <- function(
  path = "caulobrowser_example.duckdb",
  overwrite = TRUE
) {
  path <- normalizePath(path, mustWork = FALSE)

  if (file.exists(path) && !overwrite) {
    stop(
      "File already exists: ",
      path,
      "\n  Use overwrite = TRUE to replace it.",
      call. = FALSE
    )
    file.remove(path)
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # ── Schema ──────────────────────────────────────────────────────────────────
  DBI::dbExecute(
    con,
    "
CREATE TABLE genes (
    gene_id         VARCHAR PRIMARY KEY,
    cc_tag          VARCHAR,
    gene_name       VARCHAR,
    ncbi_protein_id VARCHAR,
    gene_biotype    VARCHAR,
    description     VARCHAR
);"
  )

  DBI::dbExecute(
    con,
    "
CREATE TABLE experiments (
    experiment_id       VARCHAR PRIMARY KEY,
    display_label       VARCHAR NOT NULL,
    experiment_class    VARCHAR NOT NULL
        CHECK (experiment_class IN (
            'de_comparison', 'timecourse', 'chipseq', 'proteomics', 'tnseq', 'other'
        )),
    data_type           VARCHAR NOT NULL,
    strain              VARCHAR,
    genetic_background  VARCHAR,
    treatment           VARCHAR,
    treatment_level     VARCHAR,
    growth_phase        VARCHAR,
    media               VARCHAR,
    ref_strain          VARCHAR,
    ref_treatment       VARCHAR,
    ref_treatment_level VARCHAR,
    lab_group           VARCHAR,
    doi                 VARCHAR,
    geo_id              VARCHAR,
    date_added          DATE,
    x_axis_label        VARCHAR,
    y_axis_label        VARCHAR,
    value_label         VARCHAR
);"
  )

  DBI::dbExecute(
    con,
    "
CREATE TABLE experiment_conditions (
    experiment_id   VARCHAR NOT NULL REFERENCES experiments(experiment_id),
    condition_label VARCHAR NOT NULL,
    condition_order INTEGER NOT NULL,
    condition_value DOUBLE,
    condition_units VARCHAR,
    display_label   VARCHAR,
    PRIMARY KEY (experiment_id, condition_label)
);"
  )

  DBI::dbExecute(
    con,
    "
CREATE TABLE de_results (
    gene_id       VARCHAR NOT NULL REFERENCES genes(gene_id),
    experiment_id VARCHAR NOT NULL REFERENCES experiments(experiment_id),
    log2fc        DOUBLE NOT NULL,
    padj          DOUBLE,
    PRIMARY KEY (gene_id, experiment_id)
);"
  )

  DBI::dbExecute(
    con,
    "
CREATE TABLE timecourse_expression (
    gene_id          VARCHAR NOT NULL REFERENCES genes(gene_id),
    experiment_id    VARCHAR NOT NULL REFERENCES experiments(experiment_id),
    condition_label  VARCHAR NOT NULL,
    expression_value DOUBLE NOT NULL,
    PRIMARY KEY (gene_id, experiment_id, condition_label),
    FOREIGN KEY (experiment_id, condition_label)
        REFERENCES experiment_conditions(experiment_id, condition_label)
);"
  )

  DBI::dbExecute(
    con,
    "CREATE INDEX idx_de_exp      ON de_results(experiment_id);"
  )
  DBI::dbExecute(con, "CREATE INDEX idx_de_gene     ON de_results(gene_id);")
  DBI::dbExecute(
    con,
    "CREATE INDEX idx_tc_gene     ON timecourse_expression(gene_id);"
  )
  DBI::dbExecute(
    con,
    "CREATE INDEX idx_tc_exp      ON timecourse_expression(experiment_id);"
  )
  DBI::dbExecute(
    con,
    "CREATE INDEX idx_tc_gene_exp ON timecourse_expression(gene_id, experiment_id);"
  )
  DBI::dbExecute(
    con,
    "CREATE INDEX idx_exp_class   ON experiments(experiment_class);"
  )
  DBI::dbExecute(con, "CREATE INDEX idx_exp_type    ON experiments(data_type);")
  DBI::dbExecute(con, "CREATE INDEX idx_exp_lab     ON experiments(lab_group);")

  # ── Genes ───────────────────────────────────────────────────────────────────
  DBI::dbAppendTable(
    con,
    "genes",
    data.frame(
      gene_id. = c(
        "CCNA_00090",
        "CCNA_00446",
        "CCNA_02647",
        "CCNA_02761",
        "CCNA_01248"
      ),
      cc_tag = c("CC_0001", "CC_0002", "CC_0003", "CC_0004", "CC_0005"),
      gene_name = c("ctrA", "dnaA", "fliF", "pilA", "rpoD"),
      ncbi_protein_id = c(
        "WP_010919523.1",
        "WP_010919456.1",
        "WP_010921928.1",
        "WP_010922042.1",
        "WP_010920629.1"
      ),
      gene_biotype = rep("protein_coding", 5),
      description = c(
        "cell cycle master regulator CtrA",
        "chromosomal replication initiator protein DnaA",
        "flagellar M-ring protein FliF",
        "type IV pilin PilA",
        "RNA polymerase sigma factor RpoD"
      ),
      stringsAsFactors = FALSE
    )
  )

  # ── Experiments ─────────────────────────────────────────────────────────────
  DBI::dbAppendTable(
    con,
    "experiments",
    data.frame(
      experiment_id = c("TC_RNAseq_WT_CC", "DE_ctrA_depl"),
      display_label = c(
        "WT cell cycle timecourse (RNA-seq)",
        "ctrA depletion vs WT (RNA-seq)"
      ),
      experiment_class = c("timecourse", "de_comparison"),
      data_type = c("rnaseq", "rnaseq"),
      strain = c("NA1000", "NA1000"),
      genetic_background = c("wildtype", "ctrA::pMT335"),
      treatment = c(NA_character_, "vanillate depletion"),
      treatment_level = c(NA_character_, "0 h"),
      growth_phase = c("synchronised swarmer", "exponential"),
      media = c("M2G", "M2G"),
      ref_strain = c(NA_character_, "NA1000"),
      ref_treatment = c(NA_character_, NA_character_),
      ref_treatment_level = c(NA_character_, NA_character_),
      lab_group = c("Laub lab", "Laub lab"),
      doi = c("10.1016/j.cell.2009.01.001", "10.1073/pnas.0407828102"),
      geo_id = c("GSE12345", "GSE67890"),
      date_added = as.Date(c("2024-01-15", "2024-03-20")),
      x_axis_label = c("Time (min)", NA_character_),
      y_axis_label = c("Expression (log2 TPM)", "log2 fold change"),
      value_label = c("log2 TPM", "log2FC"),
      stringsAsFactors = FALSE
    )
  )

  # ── Experiment conditions (timecourse time points) ───────────────────────────
  DBI::dbAppendTable(
    con,
    "experiment_conditions",
    data.frame(
      experiment_id = rep("TC_RNAseq_WT_CC", 5),
      condition_label = c("t0", "t30", "t60", "t90", "t120"),
      condition_order = 1:5,
      condition_value = c(0, 30, 60, 90, 120),
      condition_units = rep("min", 5),
      display_label = c("0 min", "30 min", "60 min", "90 min", "120 min"),
      stringsAsFactors = FALSE
    )
  )

  # ── DE results ──────────────────────────────────────────────────────────────
  DBI::dbAppendTable(
    con,
    "de_results",
    data.frame(
      gene_id = c(
        "CCNA_00090",
        "CCNA_00446",
        "CCNA_02647",
        "CCNA_02761",
        "CCNA_01248"
      ),
      experiment_id = rep("DE_ctrA_depl", 5),
      log2fc = c(-2.1, -0.4, 1.8, 0.9, -0.2),
      padj = c(1e-4, 0.32, 2.3e-3, 0.015, 0.78),
      stringsAsFactors = FALSE
    )
  )

  # ── Timecourse expression ────────────────────────────────────────────────────
  # Biologically plausible cell-cycle profiles (log2 TPM)
  expr_patterns <- list(
    CCNA_00090 = c(5.2, 6.8, 7.1, 5.9, 4.8), # ctrA  – peaks in early S-phase
    CCNA_00446 = c(6.1, 5.8, 5.5, 6.2, 6.4), # dnaA  – relatively constant
    CCNA_02647 = c(4.5, 5.1, 6.3, 7.2, 6.8), # fliF  – late-cell-cycle induction
    CCNA_02761 = c(3.8, 4.2, 5.6, 6.9, 7.4), # pilA  – monotonically increasing
    CCNA_01248 = c(7.0, 6.9, 7.1, 6.8, 7.0) # rpoD  – stable housekeeping
  )
  cond_labels <- c("t0", "t30", "t60", "t90", "t120")

  tc_expr <- do.call(
    rbind,
    lapply(names(expr_patterns), function(g) {
      data.frame(
        gene_id = g,
        experiment_id = "TC_RNAseq_WT_CC",
        condition_label = cond_labels,
        expression_value = expr_patterns[[g]],
        stringsAsFactors = FALSE
      )
    })
  )
  DBI::dbAppendTable(con, "timecourse_expression", tc_expr)

  message("Example database written to: ", path)
  invisible(path)
}
