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

    wiggle_s3_urls <- c(
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.amazonaws.com/Laublab_NA1000_ML2296_Pxyl-gcrA_PYEX_AntiFlag_Control_ChIPSeq.bigWig",
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.amazonaws.com/Laublab_NA1000_ML2297_Pxyl-gcrA-3xFLAG_PYEX_AntiFlag_ChIPSeq.bigWig",
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.amazonaws.com/Laublab_NA1000_ML2297_Pxyl-gcrA-3xFLAG_PYEX_Rifampicin_AntiFlag_ChIPSeq.bigWig",
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.amazonaws.com/Laublab_NA1000_ML2298_Pgcra-gcrA-3xFLAG_PYE_AntiFlag_ChIPSeq.bigWig",
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.amazonaws.com/Laublab_NA1000_ML2299_rpoC-3xFLAG_PYE_AntiFlag_ChIPSeq.bigWig",
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.amazonaws.com/Laublab_NA1000_ML2299_rpoC-3xFLAG_PYE_Rifampicin_AntiFlag_ChIPSeq.bigWig",
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.amazonaws.com/Laublab_NA1000_ML2300_sigma32-3xFLAG_PYE_Rifampicin_AntiFlag_ChIPSeq.bigWig",
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.amazonaws.com/Laublab_NA1000_ML2301_sigma54-3xFLAG_PYE_Rifampicin_AntiFlag_ChIPSeq.bigWig",
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.amazonaws.com/Laublab_NA1000_WT_PYE_AntiRpoD_ChIPSeq.bigWig",
      "https://aws-s3-caulobrowser-data-056153745207-us-east-1-an.s3.amazonaws.com/Laublab_NA1000_WT_PYE_Rifampicin_AntiRpoD_ChIPSeq.bigWig"
    )

    wiggle_tracks <- lapply(wiggle_s3_urls, \(x) {
      JBrowseR::track_wiggle(
        x,
        assembly
      )
    })

    # create the tracks array to pass to browser
    tracks <- do.call(JBrowseR::tracks, c(list(annotations_track), wiggle_tracks))

    # set up the default session for the browser
    default_session <- JBrowseR::default_session(
      assembly,
      c(annotations_track)
    )

    theme <- JBrowseR::theme("#5da8a3", "#333")

    output$browserOutput <- JBrowseR::renderJBrowseR(
      JBrowseR::JBrowseR(
        "View",
        assembly = assembly,
        # pass our tracks here
        tracks = tracks,
        default_session = default_session,
        theme = theme
      )
    )
  })
}
