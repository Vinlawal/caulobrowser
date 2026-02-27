test_that("search_genes returns results for known genes", {

  # Create an in-memory test DB
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbExecute(con, "
    CREATE TABLE genes (
      gene_id VARCHAR PRIMARY KEY, gene_name VARCHAR, locus_tag VARCHAR,
      locus_tag_legacy VARCHAR, product VARCHAR, start_pos INTEGER,
      end_pos INTEGER, strand VARCHAR(1), genome_position INTEGER,
      kegg_id VARCHAR, uniprot_id VARCHAR, ncbi_gene_id VARCHAR,
      cog_category VARCHAR, operon_id VARCHAR
    )
  ")

  DBI::dbExecute(con, "
    INSERT INTO genes VALUES
      ('g1', 'ctrA', 'CCNA_03130', 'CC_3035', 'CtrA', 100, 200, '+', 1,
       'ccr:CC_3035', 'P0CAW5', '7334978', 'T', NULL)
  ")

  result <- search_genes(con, "ctrA")
  expect_equal(nrow(result), 1)
  expect_equal(result$gene_name, "ctrA")

  # Search by locus tag
  result2 <- search_genes(con, "CCNA_03130")
  expect_equal(nrow(result2), 1)

  # Empty query
  result3 <- search_genes(con, "")
  expect_equal(nrow(result3), 0)
})


test_that("get_expression_data returns time-course data", {

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbExecute(con, "
    CREATE TABLE genes (
      gene_id VARCHAR PRIMARY KEY, gene_name VARCHAR, locus_tag VARCHAR,
      locus_tag_legacy VARCHAR, product VARCHAR, start_pos INTEGER,
      end_pos INTEGER, strand VARCHAR(1), genome_position INTEGER,
      kegg_id VARCHAR, uniprot_id VARCHAR, ncbi_gene_id VARCHAR,
      cog_category VARCHAR, operon_id VARCHAR
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE expression_timecourse (
      expression_id INTEGER, gene_id VARCHAR, experiment_type VARCHAR,
      condition_label VARCHAR, timepoint_minutes INTEGER,
      expression_value DOUBLE, experiment_label VARCHAR,
      source_pubmed_id VARCHAR
    )
  ")

  DBI::dbExecute(con, "
    INSERT INTO genes VALUES
      ('g1', 'ctrA', 'CCNA_03130', 'CC_3035', 'CtrA', 100, 200, '+', 1,
       NULL, NULL, NULL, NULL, NULL)
  ")

  DBI::dbExecute(con, "
    INSERT INTO expression_timecourse VALUES
      (1, 'g1', 'rnaseq', 'wild_type', 0, 50.0, 'test', NULL),
      (2, 'g1', 'rnaseq', 'wild_type', 20, 80.0, 'test', NULL)
  ")

  result <- get_expression_data(con, "g1")
  expect_equal(nrow(result), 2)
  expect_true("gene_name" %in% colnames(result))

  # Filter by experiment type
  result2 <- get_expression_data(con, "g1", experiment_type = "microarray")
  expect_equal(nrow(result2), 0)
})
