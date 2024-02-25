mod_dialog_disclaimers_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    query_modal <- modalDialog(
      title = "Welcome to GOTeDNA!",
      tagList(
        p(em("Guidance on Optimal Timing for eDNA"), "is an interactive tool displaying coastal environmental DNA (eDNA) observations."),
        p("GOTeDNA intends to guide the development of eDNA research and/or monitoring programs based on the observed and predicted optimal detection period(s) for the taxonomic group(s) of interest."),
        h5("Disclaimers:"),
        p("This tool intends to infer optimal eDNA detection periods only, not species spatial distribution."),
        p("Website managers are not responsible for possible errors in the data (e.g., taxonomic misidentification, non-specific amplification, false negative results, etc.). Contact information for data owners is provided for any questions pertaining to the data. "),
        p("GOTeDNA aims to provide guidance on data trends, gaps and facilitate interpretation to help communication, but we recommend interpreting results with eDNA experts. "),
        p("This tool does not aim to quantify species abundance from these data, and we strongly caution against doing so."),
        p("All statistical analyses are provided in a R package at <placeholder>"),
        br(),
        p("This tool should be cited as the reference publication: Lacoursière-Roussel, A., Morrison, M., […] (In preparation) Guidance on Optimal Timing for eDNA […]."),
        p("Data used should cite", em("Data owners"), "Reference is provided.")
      ),
      easyClose = FALSE,
      size = "xl",
      footer = tagList(
        checkboxInput(ns("agreed"), "I have read and understood the above disclaimers.", value = FALSE, width = "90%"),
        actionButton(ns("dismiss"), "OK")
      )
    )

    # Show the model on start up ...
    showModal(query_modal)

    observeEvent(r$show_dialog, {
      if (r$show_dialog) {
        showModal(query_modal)
      }
    })

    # ... or when user wants to change query
    observeEvent(input$dismiss, {
      if (input$agreed) {
        removeModal()
        r$show_dialog <- FALSE
      } else {
        showNotification("You must check the box!", type = "warning")
      }
    })
  })
}