test_that("mod_expression_ui works", {
  ui <- mod_expression_ui(id = "test")
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_expression_ui)
  expect_true("id" %in% names(fmls))
})

test_that("mod_expression_server has correct formals", {
  fmls <- formals(mod_expression_server)
  for (i in c("id", "gene_results", "db_con")) {
    expect_true(i %in% names(fmls))
  }
})

# color_palette is a named character vector with the gene name as its name
testServer(
  mod_expression_server,
  args = list(
    gene_results = shiny::reactive(
      search_genes(generate_example_database(":memory:"), "ctrA")
    ),
    db_con = shiny::reactiveVal(generate_example_database(":memory:"))
  ),
  {
    pal <- color_palette()
    expect_type(pal, "character")
    expect_equal(length(pal), 1)
    expect_true("ctrA" %in% names(pal))
  }
)

# color_palette has one entry per gene when multiple genes are searched
testServer(
  mod_expression_server,
  args = list(
    gene_results = shiny::reactive(
      search_genes(generate_example_database(":memory:"), "ctrA,dnaA")
    ),
    db_con = shiny::reactiveVal(generate_example_database(":memory:"))
  ),
  {
    pal <- color_palette()
    expect_gte(length(pal), 2)
    expect_true("ctrA" %in% names(pal))
    expect_true("dnaA" %in% names(pal))
  }
)

# expression_data returns expected columns when genetic background is set
testServer(
  mod_expression_server,
  args = list(
    gene_results = shiny::reactive(
      search_genes(generate_example_database(":memory:"), "ctrA")
    ),
    db_con = shiny::reactiveVal(generate_example_database(":memory:"))
  ),
  {
    session$setInputs(genetic_background = "wildtype")
    data <- expression_data()
    expect_true(nrow(data) > 0)
    for (col in c("gene_id", "experiment_id", "expression_value")) {
      expect_true(col %in% colnames(data))
    }
  }
)

# expression_data returns 0 rows for a background not in the demo DB
testServer(
  mod_expression_server,
  args = list(
    gene_results = shiny::reactive(
      search_genes(generate_example_database(":memory:"), "ctrA")
    ),
    db_con = shiny::reactiveVal(generate_example_database(":memory:"))
  ),
  {
    session$setInputs(genetic_background = "not_a_background")
    data <- expression_data()
    expect_equal(nrow(data), 0)
  }
)

# experiment_meta returns one row per unique experiment
testServer(
  mod_expression_server,
  args = list(
    gene_results = shiny::reactive(
      search_genes(generate_example_database(":memory:"), "ctrA,dnaA")
    ),
    db_con = shiny::reactiveVal(generate_example_database(":memory:"))
  ),
  {
    session$setInputs(genetic_background = "wildtype")
    meta <- experiment_meta()
    expect_equal(nrow(meta), length(unique(meta$experiment_id)))
  }
)
