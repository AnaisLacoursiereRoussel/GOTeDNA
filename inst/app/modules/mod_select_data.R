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
          selectInput(ns("slc_phy"), "Phylum", choices = "All")
        ),
        column(
          6,
          selectInput(ns("slc_cla"), "Class", choices = "All")
        )
      ),
      fluidRow(
        column(
          6,
          selectInput(ns("slc_gen"), "Genus", choices = "All")
        ),
        column(
          6,
          selectInput(ns("slc_spe"), "Species", choices = "All")
        )
      ),
      uiOutput(outputId = ns("n_smpl")),
      leafletOutput(outputId = ns("map"), height = "50vh")
    )
  )
}

mod_select_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$datatype, {
      r$data_filtered <- gotedna_data[[input$datatype]]
      r$data_station <- gotedna_station[[input$datatype]]
      updateSelectInput(session, "slc_phy",
        selected = "All",
        choices = c("All", unique(r$data_filtered$phylum))
      )
    })

    observeEvent(input$slc_phy, {
      r$data_filtered <- filter_taxa_data(
        gotedna_data[[input$datatype]], input$slc_phy, "All", "All", "All"
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
      r$data_filtered <- filter_taxa_data(
        gotedna_data[[input$datatype]], input$slc_phy, input$slc_cla,
        "All", "All"
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
      r$data_filtered <- filter_taxa_data(
        gotedna_data[[input$datatype]], input$slc_phy, input$slc_cla,
        input$slc_gen, "All"
      )
      if (input$slc_gen == "All") {
        hide(id = "slc_spe")
        updateSelectInput(session, "slc_spe", select = "All")
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

    output$n_smpl <- renderUI({
      tagList(
        p(
          "Total number of samples selected: ",
          strong(format(r$n_sample, big.mark = ","))
        )
      )
    })

    output$map <- renderLeaflet({
      sta <- r$data_station |>
        dplyr::filter(
          ecodistrict %in% r$data_filtered$ecodistrict,
          station %in% r$data_filtered$station
        )
      # count data
      tmp <- r$data_filtered |>
        dplyr::group_by(ecodistrict, station) |>
        dplyr::summarise(count = n())
      sta <- r$data_station |>
        dplyr::inner_join(
          tmp,
          join_by(ecodistrict, station)
        )
      r$n_sample <- sum(tmp$count)
      leaflet(sta) |>
        # setView(lng = -63, lat = 48, zoom = 5) |>
        addProviderTiles("Esri.OceanBasemap", group = "Ocean") |>
        addMarkers(
          data = sta,
          clusterOptions = markerClusterOptions(),
          label = ~ paste(count, "samples")
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