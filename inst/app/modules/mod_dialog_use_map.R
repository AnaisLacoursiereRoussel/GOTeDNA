mod_dialog_map_info_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        query_modal <- modalDialog(
            title = NULL,
            footer = NULL,
            includeHTML(file.path("www", "doc", "help_map.html")),
            easyClose = TRUE,
            size = "l"
        )
        observe({
            if (r$show_map_info) {
                showModal(query_modal)
                r$show_map_info <- FALSE
            }
        })
    })
}