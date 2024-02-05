mod_dialog_definitions_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    query_modal <- modalDialog(
      title = "Glossary",
      tagList(
        DT::dataTableOutput(ns("glossary"))
      ),
      easyClose = TRUE,
      size = "xl",
      footer = tagList(
        actionButton(ns("dismiss"), "OK")
      )
    )

    observeEvent(r$show_help, {
      if (r$show_help) {
        showModal(query_modal)
        r$show_help <- FALSE
      }
    })

    output$glossary <- DT::renderDT(
      gloss |> DT::datatable(escape = FALSE)
    )

    observeEvent(input$dismiss, {
      removeModal()
    })
  })
}