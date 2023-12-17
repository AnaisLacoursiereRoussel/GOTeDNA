# Select data and show them on the map
mod_select_figure_ui <- function(id) {
    ns <- NS(id)
    tagList(
        div(
            id = "data_input",
            fluidRow(
                column(
                    6,
                    selectInput(ns("period"), "Period", choices = 1:7),
                    selectInput(ns("figtest"), "Figure", choices = 1:5)
                ),
                column(
                    6,
                    radioButtons(ns("filterby"), "Filter by",
                        choices = list(
                            "Sample size required" = 1,
                            "Proportion of positive samples" = 2
                        )
                    ),
                    selectInput(
                        ns("threshold"),
                        "Normalized detection threshold",
                        choices = seq(75, 95, 5)
                    ),
                )
            )
        ),
        div(
            id = "figure_output",
            plotOutput(ns("figure"), height = "50vh")
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

        output$figure <- renderPlot(
            {
            if (input$figtest == 1) {
                hm_fig(
                    taxon.level = "class", taxon.name = "Copepoda",
                    ecodistrict.select = "Scotian Shelf",
                    Pscaled_month
                )
            } else if (input$figtest == 2) {
                effort_needed_fig(
                    species.name = "Acartia hudsonica", primer.select = "COI1",
                    ecodistrict.select = "Scotian Shelf", Pscaled_month
                )
            } else if (input$figtest == 3) {
                higher_tax_fig(
                    data = D_mb_ex,
                    higher.taxon.select = "phylum",
                    taxon.name = "Bryozoa",
                    view.by.level = "genus",
                    ecodistrict.select = "Scotian Shelf",
                    primer.select = "COI1"
                )
            } else if (input$figtest == 4) {
                sample_size_fig(
                    data = D_mb_ex, species.name = "Acartia hudsonica",
                    ecodistrict.select = "Scotian Shelf"
                )
            } else if (input$figtest == 5) {
                p1 <- smooth_fig(
                    data = D_mb_ex, species.name = "Acartia longiremis",
                    primer.select = "COI1", ecodistrict.select = "Scotian Shelf"
                )
                p2  <- thresh_fig(
                    taxon.level = "species", taxon.name = "Acartia hudsonica",
                    threshold = "90", ecodistrict.select = "Scotian Shelf", Pscaled_month
                )
                p1 + p2
            }
        }, 
        res = 108)

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