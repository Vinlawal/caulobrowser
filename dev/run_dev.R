# Run the development version of the app
# =========================================
# Set options here before running the app.

# Detach all loaded packages and clean environment
golem::detach_all_attached()

# Document and reload the package
golem::document_and_reload()

# Run the app
run_app()
