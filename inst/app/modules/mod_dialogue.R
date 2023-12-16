mod_dialogue_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        query_modal <- modalDialog(
            title = "Welcome to GOTeDNA!",
            tagList(
                p(em("Guidance on Optimal Timing for eDNA"), "is an interactive tool displaying coastal environmental DNA (eDNA) observations.."),
                p("GOTeDNA intends to guide the development of eDNA research and/or monitoring programs based on the observed and predicted optimal detection period(s) for the taxonomic group(s) of interest and biodiversity"),
                h5("Disclaimers:"),
                p("This tool intends to infer optimal eDNA detection periods only, not species spatial distribution."),
                p("Caution should occur regarding potential errors in taxonomic assignment and/or non-specific amplification. Website
                managers are not responsible for any lab and bioinformatic misidentification. Contact information for data owners are
                provided for any questions relative to the data."),
                p("We provide guidance on data limitation and interpretation to help communication between data owners and website users, but we recommend interpreting results with eDNA experts."),
                p("This tool does not aim to quantify population. Caution should be exercised in interpreting species abundance from these data.")
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

        observeEvent(r$show_dialogue, {
            if (r$show_dialogue) {
                showModal(query_modal)
            }
        })

        # ... or when user wants to change query
        observeEvent(input$dismiss, {
            if (input$agreed) {
                removeModal()
                r$show_dialogue  <- FALSE
            } else {
                showNotification("You must check the box!", type = "warning")
            }
        })
    })
}