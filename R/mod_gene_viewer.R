#' gene_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gene_viewer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header("Gene Viewer"),
      bslib::card_body(JBrowseR::JBrowseROutput(ns("browserOutput")))
    )
  )
}

#' gene_viewer Server Functions
#'
#' @noRd
mod_gene_viewer_server <- function(
  id,
  location = shiny::reactive(NULL),
  db_con
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    theme <- JBrowseR::theme("#5da8a3", "#333")

    jb_config <- shiny::reactive({
      con <- db_con()
      shiny::req(con)

      jbrowser_meta <- get_gene_viewer_metadata(con)
      tracks_df <- jbrowser_meta$tracks

      assembly <- JBrowseR::assembly(jbrowser_meta$assembly, bgzip = TRUE)
      gff_index <- do.call(
        JBrowseR::text_index,
        as.list(jbrowser_meta$text_index)
      )

      annotations_tracks <- lapply(
        subset(tracks_df, track_type == "feature")$https_paths,
        \(path) JBrowseR::track_feature(path, assembly)
      )
      wiggle_tracks <- lapply(
        subset(tracks_df, track_type == "wiggle")$https_paths,
        \(path) JBrowseR::track_wiggle(path, assembly)
      )

      list(
        assembly = assembly,
        tracks = do.call(
          JBrowseR::tracks,
          c(annotations_tracks, wiggle_tracks)
        ),
        gff_index = gff_index,
        default_session = JBrowseR::default_session(
          assembly,
          unlist(annotations_tracks),
          display_assembly = FALSE
        )
      )
    })

    effective_location <- shiny::reactive({
      loc <- location()
      if (is.null(loc) || !nzchar(trimws(loc))) {
        "gi|221232939|ref|NC_011916.1|:1..4016942"
      } else {
        loc
      }
    })

    output$browserOutput <- JBrowseR::renderJBrowseR({
      cfg <- jb_config()
      JBrowseR::JBrowseR(
        "View",
        assembly = cfg$assembly,
        tracks = cfg$tracks,
        text_index = cfg$gff_index,
        location = effective_location(),
        defaultSession = cfg$default_session,
        theme = theme
      )
    })
  })
}
