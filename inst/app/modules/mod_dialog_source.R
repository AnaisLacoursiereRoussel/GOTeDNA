mod_dialog_source_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    query_modal <- modalDialog(
      title = "Data source",
      tagList(
        DT::dataTableOutput(ns("source"))
      ),
      easyClose = TRUE,
      size = "xl",
      footer = tagList(
        actionButton(ns("dismiss"), "Dismiss")
      )
    )

    observeEvent(r$show_source, {
      if (r$show_source) {
        showModal(query_modal)
        r$show_source <- FALSE
      }
    })

    output$source <- DT::renderDT(
      r$data_filtered |>
      dplyr::ungroup() |>
        dplyr::select(
          c("GOTeDNA_ID", "GOTeDNA_version", "target_gene", "ecodistrict", 
          "station")
        ) |>
        dplyr::distinct()
    )

    observeEvent(input$dismiss, {
      removeModal()
    })
  })
}