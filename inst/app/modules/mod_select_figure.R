# Select data and show them on the map
mod_select_figure_ui <- function(id) {
    ns <- NS(id)
    tagList(
        div(
            id = "data_input",
            fluidRow(
                column(
                    6,
                    selectInput("period", "Period", choices = 1:10),
                    selectInput("figtest", "Figure", choices = 1:6)
                ),
                column(
                    6,
                    radioButtons("filterby", "Filter by",
                        choices = list(
                            "Sample size required" = 1,
                            "Proportion of positive samples" = 2
                        )
                    ),
                    selectInput(
                        "threshold",
                        "Normalized detection threshold",
                        choices = seq(75, 95, 5)
                    ),
                )
            )
        ),
        div(
            id = "figure_output",
            plotOutput(ns("figure"))
        ),
        div(
            id = "table_output",
            tableOutput(ns("table"))
        )
    )
}

mod_select_figure_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$figure <- renderPlot({
            plot(1, 1, asp = 1)
        })

        output$table <- renderTable(
            {
                dfs[1:3, ] |>
                    dplyr::select(c(GOTeDNA_ID, ecodistrict, station, year, detected)) |>
                    sf::st_drop_geometry() |>
                    as.data.frame()
            },
            width = "100%"
        )
    })
}