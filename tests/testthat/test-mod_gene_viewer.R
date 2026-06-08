test_that("mod_gene_viewer_ui works", {
  ui <- mod_gene_viewer_ui(id = "test")
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_gene_viewer_ui)
  expect_true("id" %in% names(fmls))
})

test_that("mod_gene_viewer_server has correct formals", {
  fmls <- formals(mod_gene_viewer_server)
  for (i in c("id", "location", "db_con")) {
    expect_true(i %in% names(fmls))
  }
})

# Namespace wiring
testServer(
  mod_gene_viewer_server,
  args = list(
    location = shiny::reactive(NULL),
    db_con = shiny::reactiveVal(NULL)
  ),
  {
    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl("test", ns("test")))
  }
)

# effective_location defaults to the full genome span when location is NULL
testServer(
  mod_gene_viewer_server,
  args = list(
    location = shiny::reactive(NULL),
    db_con = shiny::reactiveVal(NULL)
  ),
  {
    loc <- effective_location()
    expect_type(loc, "character")
    expect_match(loc, "NC_011916.1", fixed = TRUE)
    expect_match(loc, "1..4016942", fixed = TRUE)
  }
)

# effective_location defaults to the full genome span when location is empty
testServer(
  mod_gene_viewer_server,
  args = list(
    location = shiny::reactive(""),
    db_con = shiny::reactiveVal(NULL)
  ),
  {
    expect_match(effective_location(), "1..4016942", fixed = TRUE)
  }
)

# effective_location returns the passed location string when it is set
testServer(
  mod_gene_viewer_server,
  args = list(
    location = shiny::reactive("gi|221232939|ref|NC_011916.1|:101960..102943"),
    db_con = shiny::reactiveVal(NULL)
  ),
  {
    expect_equal(
      effective_location(),
      "gi|221232939|ref|NC_011916.1|:101960..102943"
    )
  }
)
