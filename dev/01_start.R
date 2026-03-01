# Building a Productive Development Environment
# ==================================================
# Run these lines once when you first set up the project.

# Fill the DESCRIPTION file
golem::fill_desc(
  pkg_name = "caulobrowser",
  pkg_title = "CauloBrowser: A Systems Biology Resource for Caulobacter Crescentus",
  pkg_description = "An interactive Shiny application for browsing and visualizing
    curated high-throughput experimental data for Caulobacter crescentus.
    Built with the golem framework.",
  authors = person(
    given = "Berent",
    family = "Aldikacti",
    email = "aldikactiberent@gmail.com",
    role = c("aut", "cre")
  ),
  repo_url = NULL,
  pkg_version = "0.1.0"
)

# Set golem options
golem::set_golem_options()

# Common files
usethis::use_mit_license("Berent Aldikacti")
usethis::use_readme_rmd(open = FALSE)
usethis::use_news_md(open = FALSE)
usethis::use_code_of_conduct(contact = "aldikactiberent@gmail.com")
usethis::use_lifecycle_badge("Experimental")

# Initialize git (if not already a git repo)
# usethis::use_git()

# Init testing infrastructure
golem::use_recommended_tests()

# Favicon
# golem::use_favicon(path = "path/to/favicon.ico")

# Add utility functions
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)
