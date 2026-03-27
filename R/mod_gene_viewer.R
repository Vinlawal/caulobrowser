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
      bslib::card_body(
        JBrowseR::JBrowseROutput(ns("browserOutput"))
      )
    )
  )
}

#' gene_viewer Server Functions
#'
#' @noRd
mod_gene_viewer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # create the necessary JB2 assembly configuration
    assembly <- JBrowseR::assembly(
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.us-east-1.amazonaws.com/NC_011916.fasta.gz",
      bgzip = TRUE
    )
    # create configuration for a JB2 GFF FeatureTrack
    annotations_track <- JBrowseR::track_feature(
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.us-east-1.amazonaws.com/NC_011916.sorted.gff.gz",
      assembly
    )

    wiggle_track <- JBrowseR::track_wiggle(
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.us-east-1.amazonaws.com/doxyr_15min_1.bw",
      assembly
    )

    # create the tracks array to pass to browser
    tracks <- JBrowseR::tracks(annotations_track, wiggle_track)

    # set up the default session for the browser
    default_session <- JBrowseR::default_session(
      assembly,
      c(annotations_track, wiggle_track)
    )

    output$browserOutput <- JBrowseR::renderJBrowseR(
      JBrowseR::JBrowseR(
        "View",
        assembly = assembly,
        # pass our tracks here
        tracks = tracks,
        default_session = default_session
      )
    )
  })
}
