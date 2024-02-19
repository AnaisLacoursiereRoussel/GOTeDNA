# Select data and show them on the map
mod_select_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "data_request",
      h2("Data request", class = "col_1"),
      fluidRow(
        column(
          8,
          radioButtons(ns("datatype"),
            label = "Type of data",
            choices = list(
              "Species specific (qPCR)" = "qPCR",
              "Multi-species (metabarcoding)" = "metabarcoding"
            ),
            selected = "qPCR",
            inline = TRUE
          ),
        ),
        column(
          4,
          selectInput(ns("primer"), "Primer", choices = "unkown")
        ),
        column(
          6,
          selectInput(ns("slc_phy"), "Phylum", choices = "All"),
          selectInput(ns("slc_gen"), "Genus", choices = "All")
        ),
        column(
          6,
          selectInput(ns("slc_cla"), "Class", choices = "All"),
          selectInput(ns("slc_spe"), "Species", choices = "All")
        )
      ),
    fluidRow(
      column(
        6,
        uiOutput(outputId = ns("n_smpl"))
      ),
      column(
        6,
        div(
          id = "button_map",
          actionButton(ns("show_map_info"), "Map info",
            icon = icon("info-circle"),
            title = "Display information about how to use the map below"
          ),
          actionButton(ns("confirm"), "Confirm",
            icon = icon("check"),
            title = "confirm spatial selection"
          ),
          actionButton(ns("refresh"), "Refresh",
            icon = icon("refresh"),
            title = "refresh spatial selection"
          ),
        )
      )
    ),
    mapedit::editModUI(ns("map-select"), height = "50vh"),
    div(
      id = "button_source",
      actionButton("show_source", "Sources",
        icon = icon("eye"),
        title = "access data sources"
      )
    )
  )
  )
}

mod_select_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      if (input$datatype == "qPCR") {
        updateSelectInput(
          session,
          "primer",
          choices = "not available"
        )
      } else {
        tg <- table(r$data_filtered$target_subfragment) |>
          sort() |>
          rev()
        updateSelectInput(
          session,
          "primer",
          choices = names(tg),
          selected = names(tg)[1]
        )
      }
    })

    observe(
      r$primer <- input$primer
    )

    observeEvent(input$datatype, {
      r$data_type <- input$datatype
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
        updateSelectInput(session, "slc_cla", select = "All")
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
        updateSelectInput(session, "slc_gen", select = "All")
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
    observeEvent(input$slc_spe, {
      r$data_filtered <- filter_taxa_data(
        gotedna_data[[input$datatype]], input$slc_phy, input$slc_cla,
        input$slc_gen, input$slc_spe
      )
    })

    output$n_smpl <- renderUI({
      tagList(
        p(
          "Total number of samples selected: ",
          strong(format(r$n_sample, big.mark = ","))
        )
      )
    })

    listenMapData <- reactive({
      list(
        input$slc_phy,
        input$slc_cla,
        input$slc_gen,
        input$slc_spe,
        input$datatype
      )
    })
    observeEvent(listenMapData(), {
      r$taxon_slc <- c(
        input$slc_phy, input$slc_cla, input$slc_gen,
        input$slc_spe
      )
      r$fig_ready <- FALSE
      # count data
      r$geom <- r$data_station |>
        dplyr::inner_join(
          r$data_filtered |>
            dplyr::group_by(ecodistrict, station) |>
            dplyr::summarise(count = n()),
          join_by(ecodistrict, station)
        )
      # reset figures
      r$reload_map <- r$reload_map + 1
    })

    observeEvent(input$confirm, {
      if (!is.null(sf_edits()$all)) {
        cli::cli_alert_info("Using geom(s) drawn to select region")
        if (!is.null(r$geom)) {
          id_slc <- st_contains(sf_edits()$all, r$geom, sparse = FALSE) |>
            apply(2, any)
          if (sum(id_slc)) {
            r$geom <- r$geom[id_slc, ]
            r$data_filtered <- r$data_filtered |>
              dplyr::filter(station %in% r$geom$station)
          } else {
            showNotification("No station selected", type = "warning")
          }
        } else {
          cli::cli_alert_info("`r$geom` is null")
        }
      } else {
        cli::cli_alert_info("`sf_edits()$all` is null")
        showNotification("Empty spatial selection", type = "warning")
      }
      r$reload_map <- r$reload_map + 1
    })

    observeEvent(input$show_map_info, r$show_map_info <- TRUE)


    observeEvent(input$refresh, {
      r$data_filtered <- filter_taxa_data(
        gotedna_data[[input$datatype]], input$slc_phy, input$slc_cla,
        input$slc_gen, "All"
      )
      r$geom <- r$data_station |>
        dplyr::inner_join(
          r$data_filtered |>
            dplyr::group_by(ecodistrict, station) |>
            dplyr::summarise(count = n()),
          join_by(ecodistrict, station)
        )
      r$reload_map <- r$reload_map + 1
    })

    observeEvent(r$geom, {
      r$n_sample <- sum(r$geom$count)
    })

    observeEvent(r$reload_map, {
      sf_edits <<- callModule(
        mapedit::editMod,
        leafmap = leaflet(isolate(r$geom)) |>
          addProviderTiles("Esri.OceanBasemap", group = "Ocean") |>
          addMarkers(
            data = isolate(r$geom),
            clusterOptions = markerClusterOptions(),
            label = ~ paste(count, "samples")
          ),
        id = "map-select"
      )
    })
  })
}