# Deployment
# ==================================================
# Run these lines when you are ready to deploy.

# ── Check package ─────────────────────────────────
devtools::check()

# ── Deploy to shinyapps.io ────────────────────────
# golem::add_shinyappsio_file()
# rsconnect::deployApp()

# ── Deploy to Posit Connect ──────────────────────
# golem::add_rstudioconnect_file()
# rsconnect::deployApp()

# ── Docker deployment ─────────────────────────────
# golem::add_dockerfile()
# golem::add_dockerfile_with_renv()
golem::add_dockerfile_shinyproxy()
