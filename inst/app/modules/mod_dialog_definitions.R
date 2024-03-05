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
        div(
        p("eDNA general wording have been defined and described within:"),
        p("
          Abbott, C.*, Coulson, M.*, Gagné, N.*, Lacoursière‐Roussel, A.*, 
          Parent, G.J.*, Bajno, R., Dietrich, C., May-McNally, S. 2021. 
          Guidance on the Use of Targeted Environmental DNA (eDNA) Analysis for 
          the Management of Aquatic Invasive Species and Species at Risk. DFO 
          Can. Sci. Advis. Sec. Res. Doc. 2021/019. iv + 42 p.
        "),
        class = "text-align-left"
        ),
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
      gloss |> DT::datatable(escape = FALSE, 
      options = list(
        lengthMenu = list(c(5, 10, 25, 50), c('5', '10', '25', '50')),
        pageLength = 5
        )
      )
    )

    observeEvent(input$dismiss, {
      removeModal()
    })
  })
}