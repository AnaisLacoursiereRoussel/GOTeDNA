# Select data and show them on the map
mod_select_data_ui <- function(id) {
    ns <- NS(id)
    tagList(
        div(
            id = "data_request",
            h2("Data request window", class = "col_1"),
            radioButtons("datatype",
                label = "Data Type", choices = c("qPCR", "Metabarcoding"),
                inline = TRUE
            ),
            fluidRow(
                column(
                    6,
                    selectInput(ns("slc_phy"), "Phylum", choices = tx_phy)
                ),
                column(
                    6,
                    selectInput(ns("slc_cla"), "Class", choices = tx_cla)
                )
            ),
            fluidRow(
                column(
                    6,
                    selectInput(ns("slc_gen"), "Genus", choices = tx_gen)
                ),
                column(
                    6,
                    selectInput(ns("slc_spe"), "Species", choices = tx_spe)
                )
            ),
            leafletOutput(outputId = ns("map"), height = 500)
        )
    )
}

mod_select_data_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$map <- renderLeaflet({
            tmp <- filter_spatial_data(
                dfs, input$slc_phy, input$slc_cla,
                input$slc_gen, input$slc_spe
            )
            leaflet(tmp) |>
                # setView(lng = -63, lat = 48, zoom = 5) |>
                addProviderTiles("Esri.OceanBasemap", group = "Ocean") |>
                addMarkers(
                    data = tmp,
                    clusterOptions = markerClusterOptions()
                ) |>
                htmlwidgets::onRender(
                    "function(el, x) {
                        L.control.zoom({
                        position:'bottomright'
                      }).addTo(this);
                    }"
                )
        })
    })
}