#' Gene Search Module
#'
#' @description A shiny module for the gene search input interface.
#'   This is the main entry point: users type gene names or locus tags
#'   and the module returns matched gene records.
#'
#' @param id Internal parameter for {shiny} module namespacing.
#'
#' @name mod_gene_search
#' @noRd

mod_gene_search_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 8,
        shiny::textInput(
          ns("gene_query"),
          label = NULL,
          placeholder = "Enter gene names or locus tags (e.g. ctrA, ftsZ, popZ or CCNA_03130)",
          width = "100%"
        )
      ),
      shiny::column(
        width = 2,
        shiny::actionButton(
          ns("btn_search"),
          "Search",
          icon = shiny::icon("magnifying-glass"),
          class = "btn-primary",
          width = "100%"
        )
      ),
      shiny::column(
        width = 2,
        shiny::actionButton(
          ns("btn_clear"),
          "Clear",
          icon = shiny::icon("xmark"),
          class = "btn-outline-secondary",
          width = "100%"
        )
      )
    ),
    shiny::helpText(
      "Search by gene name (e.g. ctrA), locus tag (e.g. CCNA_03130),",
      "or legacy locus tag (e.g. CC_3035).",
      "Separate multiple genes with commas."
    )
  )
}


mod_gene_search_server <- function(id, db_con) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive: matched gene records
    gene_results <- shiny::eventReactive(input$btn_search, {
      shiny::req(nchar(trimws(input$gene_query)) > 0)

      result <- search_genes(db_con(), input$gene_query)

      if (nrow(result) == 0) {
        shiny::showNotification(
          "No genes found matching your query.",
          type = "warning",
          duration = 4
        )
      }

      result
    })

    # Clear button resets
    shiny::observeEvent(input$btn_clear, {
      shiny::updateTextInput(session, "gene_query", value = "")
    })

    return(gene_results)
  })
}
