# Select data and show them on the map
mod_select_data_ui <- function(id) {
    ns <- NS(id)
    tagList(
        div(
            id = "data_request",
            h2("Data request window", class = "col_1"),
            fluidRow(
                column(
                    6,
                    radioButtons(ns("datatype"),
                        label = "Data Type", 
                        choices = list(
                           "Species specific (qPCR)" = "qPCR", 
                           "Multi-species (metabarcoding)" = "metabarcoding"
                        ),
                        selected = "qPCR",
                        inline = TRUE
                    )
                ),
                column(
                    6,
                    actionButton(ns("calc_window"), "New search",
                        icon = icon("gear")
                    )
                )
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
            leafletOutput(outputId = ns("map"), height = "50vh")
        )
    )
}

mod_select_data_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observeEvent(input$datatype, {
            r$data_filtered <- gotedna_data[[input$datatype]]
            updateSelectInput(session, "slc_phy", selected = "All")
        })

        observeEvent(input$slc_phy, {
            r$data_filtered <- filter_spatial_data(
                gotedna_data[[input$datatype]], input$slc_phy, input$slc_cla,
                input$slc_gen, input$slc_spe
            )
            if (input$slc_phy == "All") {
                hide(id = "slc_cla")
                hide(id = "slc_gen")
                hide(id = "slc_spe")
            } else {
                show(id = "slc_cla")
                updateSelectInput(session, "slc_cla",
                    choices = c("All", unique(r$data_filtered$class))
                )
            }
        })
        observeEvent(input$slc_cla, {
            r$data_filtered <- filter_spatial_data(
                gotedna_data[[input$datatype]], input$slc_phy, input$slc_cla,
                input$slc_gen, input$slc_spe
            )
            if (input$slc_cla == "All") {
                hide(id = "slc_gen")
                hide(id = "slc_spe")
            } else {
                show(id = "slc_gen")
                updateSelectInput(session, "slc_gen",
                    choices = c("All", unique(r$data_filtered$genus))
                )
            }
        })
        observeEvent(input$slc_gen, {
            r$data_filtered <- filter_spatial_data(
                gotedna_data[[input$datatype]], input$slc_phy, input$slc_cla,
                input$slc_gen, input$slc_spe
            )
            if (input$slc_gen == "All") {
                hide(id = "slc_spe")
            } else {
                show(id = "slc_spe")
                updateSelectInput(session, "slc_spe",
                    choices = c("All", unique(r$data_filtered$scientificName))
                )
            }
        })

        observeEvent(input$calc_window, {
            r$calc_window <- TRUE
        })

        output$map <- renderLeaflet({
            leaflet(r$data_filtered) |>
                # setView(lng = -63, lat = 48, zoom = 5) |>
                addProviderTiles("Esri.OceanBasemap", group = "Ocean") |>
                addMarkers(
                    data = r$data_filtered,
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