mod_dialog_map_info_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        query_modal <- modalDialog(
            title = "How to select stations using the interactive map",
            tags$ol(
                tags$li("Draw one or several polygons (or rectangles) that encapsulate the stations of interest. 
                Note that points and lines will be ignored."),
                tags$li("Click on 'Confirm' to conform the selection."),
                tags$li("If you want to remove the current selection, click on 'clear'.")
            ),
            easyClose = TRUE,
            size = "l",
            footer = tagList(
                actionButton(ns("dismiss"), "OK")
            )
        )

        observe({
            if (r$show_map_info) {
                showModal(query_modal)
                r$show_map_info <- FALSE
            }
        })

        observeEvent(input$dismiss, 
            removeModal()
        )
    })
}