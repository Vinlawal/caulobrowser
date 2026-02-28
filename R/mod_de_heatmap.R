#' Differential Expression Heatmap Module (Section 3)
#'
#' @description A shiny module for displaying log2 fold-change values as an
#'   interactive heatmap across DE comparison experiments.
#'   X-axis = experiments (display_label), Y-axis = selected genes.
#'   Color: diverging blue-white-red scale centered at log2FC = 0.
#'   A dropdown filters experiments by data_type.
#'
#' @param id Internal parameter for {shiny} module namespacing.
#' @param gene_results Reactive data frame of selected genes from
#'   `mod_gene_search_server()`.
#' @param db_con Reactive DBI connection from `app_server`.
#'
#' @name mod_de_heatmap
#' @noRd

mod_de_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::p(
      class = "text-muted",
      "Log\u00b2 fold-change values from differential expression comparisons.",
      "Blue\u00a0=\u00a0down-regulated, red\u00a0=\u00a0up-regulated relative to the reference.",
      "Use the dropdown to filter by data type."
    ),
    shiny::fluidRow(
      shiny::column(
        width = 5,
        shiny::selectInput(
          ns("data_type"),
          label = "Data type",
          choices = NULL,
          multiple = TRUE,
          width = "100%"
        )
      )
    ),
    ggiraph::girafeOutput(ns("de_heatmap"), height = "auto")
  )
}


mod_de_heatmap_server <- function(id, gene_results, db_con) {
  shiny::moduleServer(id, function(input, output, session) {
    # ── Populate data_type dropdown from DB ────────────────────────────────
    shiny::observe({
      tryCatch(
        {
          types <- get_de_data_types(db_con())
          if (length(types) > 0) {
            choices <- c("All types" = "", stats::setNames(types, types))
            shiny::updateSelectInput(session, "data_type", choices = choices)
          } else {
            shiny::updateSelectInput(
              session,
              "data_type",
              choices = c("No DE experiments available" = "")
            )
          }
        },
        error = function(e) NULL
      )
    })

    # ── Fetch DE data, reacts to gene selection + data_type filter ─────────
    de_data <- shiny::reactive({
      genes <- gene_results()
      shiny::req(nrow(genes) > 0)

      selected <- input$data_type[nzchar(input$data_type)]
      dtype_filter <- if (length(selected) > 0) selected else NULL

      get_de_results_for_heatmap(
        db_con(),
        gene_ids = genes$gene_id,
        data_type = dtype_filter
      )
    })

    # ── Dynamic height: 40 px per experiment row in the tallest facet ────────
    plot_height_px <- shiny::reactive({
      df <- de_data()
      if (is.null(df) || nrow(df) == 0) {
        return(200L)
      }
      max_experiments <- max(
        tapply(df$display_label, df$data_type, function(x) length(unique(x)))
      )
      max(200L, max_experiments * 40L + 150L)
    })

    # ── Render heatmap ─────────────────────────────────────────────────────
    output$de_heatmap <- ggiraph::renderGirafe({
      df <- de_data()

      if (is.null(df) || nrow(df) == 0) {
        empty_plot <- ggplot2::ggplot() +
          ggplot2::annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = paste0(
              "No differential expression data available\n",
              "for the selected genes and data type."
            ),
            hjust = 0.5,
            vjust = 0.5,
            size = 4,
            color = "#888888"
          ) +
          ggplot2::theme_void()
        return(ggiraph::girafe(ggobj = empty_plot))
      }

      plot_de_heatmap(df, height_px = plot_height_px())
    })
  })
}
