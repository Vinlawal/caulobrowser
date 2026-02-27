# Launch the ShinyApp
# This file is used for deployment (e.g. shinyapps.io, Posit Connect).
# Do not modify unless you know what you are doing.

pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE
)

options("golem.app.prod" = TRUE)

caulobrowser::run_app()
