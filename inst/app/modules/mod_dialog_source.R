mod_dialog_source_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    query_modal <- modalDialog(
      title = "Reference data authorship",
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
        dplyr::group_by(
          GOTeDNA_ID, GOTeDNA_version, target_subfragment, ecodistrict
        ) |> 
        summarise(
          Samples = n(),
          Stations = length(unique(station))
        ) |>
        mutate(Publication = "to be added") |> 
        dplyr::select(
          	GOTeDNA_ID,	GOTeDNA_version, Publication,	target_subfragment, 
            ecodistrict, Samples,	Stations	
        )
        
    )

    observeEvent(input$dismiss, {
      removeModal()
    })
  })
}