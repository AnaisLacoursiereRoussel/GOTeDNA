# Select data and show them on the map
mod_select_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "data_request",
      div(
        class = "section_header",
        div(
          # title and top buttons
          class = "title-container",
          h1("Data request"),
          div(
            class = "buttons-container",
            actionButton(ns("reset"), "Reset",
              title = "Reset selection to default values"
            ),
            actionButton(ns("hide_fields"), "Show/Hide fields",
              title = "Hide or show fields"
            )
          )
        ),
        # fields
        div(
          id = ns("data_request_fields"),
          class = "inputs-container",
          ## top fields
          div(
            id = "data_request_top_fields",
            fluidRow(
              column(
                3,
                selectInput(ns("datasource"),
                  label = "Data source",
                  choices = list(
                    "GOTeDNA" = "gotedna",
                    "Your own data" = "external_data"
                  ),
                  selected = "gotedna"
                )
              ),
              column(
                3,
                div(
                  id = ns("external_files"),
                  class = "file_input-container",
                  fileInput(
                    ns("external_file"),
                    "Upload your files",
                    multiple = TRUE,
                    accept = NULL,
                    width = NULL,
                    buttonLabel = "Browse...",
                    placeholder = "No file selected",
                    capture = NULL
                  )
                )
              ),
              column(
                3,
                selectInput(ns("data_type"),
                  label = "Type of data",
                  choices = list(
                    "Species specific (qPCR)" = "qPCR",
                    "Multi-species (metabarcoding)" = "metabarcoding"
                  ),
                  selected = "qPCR"
                )
              ),
              column(
                3,
                selectInput(ns("primer"), "Primer", choices = "All")
              )
            )
          ),
          ## bottom fields
          div(
            id = "data_request_bottom_fields",
            fluidRow(
              column(
                3,
                selectInput(
                  ns("taxo_lvl"), "Taxonomic rank",
                  choices = taxonomic_ranks
                )
              ),
              column(
                3,
                selectInput(
                  ns("taxo_id"), "Taxonomic group",
                  choices = "All"
                )
              ),
              column(
                3,
                selectizeInput(ns("slc_spe"), "Species", choices = "All")
              ),
              column(
                3,
                div(
                  id = ns("msg_spatial_area"),
                  p(icon("warning"), "Selection restricted to spatial area")
                )
              )
            )
          )
        )
      ),
      div(
        class = "section_footer",
        id = ns("section_footer_data_request"),
        column(12, uiOutput(outputId = ns("n_smpl_data")))
      )
    ),
    div(
      id = "area_selection",
      div(
        class = "section_header",
        div(
          class = "title-container",
          h1(
            "Area Selection",
            span(
              id = ns("lock"),
              class = "lock", icon("lock")
            )
          ),
          div(
            class = "buttons-container",
            id = "button_map",
            actionButton(ns("confirm"), "Confirm",
              title = "Confirm spatial selection"
            ),
            actionButton(ns("lock"), "Lock view",
              title = "Set and lock map bounds"
            ),
            actionButton(ns("clear_area"), "Clear area",
              title = "Clear current spatial selection"
            ),
            actionButton(ns("hide_map"), "Show/Hide map",
              title = "Hide or show map"
            )
          )
        )
      ),
      div(
        id = ns("map_container"),
        mapedit::editModUI(ns("map-select"), height = "75vh")
      ),
      div(
        class = "section_footer",
        id = ns("section_footer_area_selection"),
        div(
          actionButton(ns("show_map_info"), "How to select",
            title = "Display information about how to use the map below"
          )
        ),
        div(
          uiOutput(outputId = ns("n_smpl_map"))
        )
      )
    )
  )
}


mod_select_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Generate map
    sf_edits <<- callModule(
      mapedit::editMod,
      leafmap = leaflet() |>
        leafem::addMouseCoordinates() |>
        leaflet::addProviderTiles("Esri.OceanBasemap", group = "OceaBasemap") |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") |>
        leaflet::addLayersControl(
          baseGroups = c("OpenStreetMap", "Ocean Basemap"),
          position = "bottomleft"
        ) |>
        leaflet::addScaleBar(
          position = c("bottomright"),
          options = leaflet::scaleBarOptions(maxWidth = 200)
        ),
      id = "map-select"
    )

    ## hide and show
    observeEvent(input$hide_fields, {
      shinyjs::toggle("data_request_fields")
      shinyjs::toggle("section_footer_data_request")
    })

    observeEvent(input$hide_map, {
      shinyjs::toggle("map_container")
      shinyjs::toggle("section_footer_area_selection")
    })

    observe({
      if (is.null(r$geom_slc)) {
        shinyjs::hide("msg_spatial_area")
      } else {
        shinyjs::show("msg_spatial_area")
      }
    })

    shinyjs::hide("lock")
    observeEvent(input$lock, {
      shinyjs::toggle("lock")
      r$lock_view <- !(r$lock_view)
    })


    ## load data
    observe({
      # 2B changed when reading of external data is implemented
      if (input$datasource == "gotedna") {
        gotedna_data <- gotedna_data0
        gotedna_station <- gotedna_station0
      } else {
        gotedna_data <- gotedna_data0
        gotedna_station <- gotedna_station0
      }
    })

    observeEvent(input$data_type, {
      r$data_type <- input$data_type
      r$cur_data <- gotedna_data[[input$data_type]]
      r$data_station <- gotedna_station[[input$data_type]]
      updateSelectInput(session, "slc_phy",
        selected = "All",
        choices = c("All", unique(r$r$cur_data$phylum) |> sort())
      )
    })

    observe(
      if (!is.null(r$station_slc)) {
        r$cur_data_sta_slc <- r$cur_data |>
          dplyr::filter(station %in% r$station_slc)
      } else {
        r$cur_data_sta_slc <- r$cur_data
      }
    )


    ## update Taxo data
    observe({
      updateSelectInput(
        session,
        "taxo_id",
        choices = c(
          "All",
          r$cur_data[[input$taxo_lvl]] |>
            # r$cur_data_sta_slc[[input$taxo_lvl]] |>
            unique() |>
            sort()
        ),
        selected = "All"
      )
    })

    observe({
      if (input$taxo_id != "All") {
        updateSelectizeInput(
          session,
          "slc_spe",
          choices = c(
            "All",
            # r$cur_data_sta_slc[
            #   r$cur_data_sta_slc[[input$taxo_lvl]] == input$taxo_id,
            # ]$scientificName |>
            r$cur_data[
              r$cur_data[[input$taxo_lvl]] == input$taxo_id,
            ]$scientificName |>
              unique() |>
              sort()
          ),
          selected = "All"
        )
      } else {
        updateSelectizeInput(
          session,
          "slc_spe",
          choices = c(
            "All",
            r$cur_data_sta_slc$scientificName |>
              unique() |>
              sort()
          ),
          selected = "All",
        )
      }
    })

    observeEvent(input$reset, {
      updateSelectInput(
        session,
        "taxo_lvl",
        selected = "kingdom"
      )
      r$geom_slc <- r$station_slc <- NULL
      r$geom <- filter_station(r)
      r$reload_map <- r$reload_map + 1
    })


    # primer
    observe({
      if (input$data_type == "qPCR") {
        updateSelectInput(
          session,
          "primer",
          choices = "not available"
        )
      } else {
        updateSelectInput(
          session,
          "primer",
          choices = get_primer_selection(
            r$taxon_lvl_slc, filter_taxon(
              r$cur_data_sta_slc, r$taxon_lvl_slc, r$taxon_id_slc, r$species
            )
          )
        )
      }
    })

    observe(r$primer <- input$primer)


    # sample number
    output$n_smpl_data <- renderUI({
      tagList(
        div(
          class = "sample_selected",
          p(
            "Sample selected: ",
            span(
              class = "sample_selected_data",
              format(r$n_sample, big.mark = ",")
            )
          )
        )
      )
    })
    output$n_smpl_map <- renderUI({
      tagList(
        div(
          class = "sample_selected",
          p(
            "Sample selected: ",
            span(
              class = "sample_selected_map",
              format(r$n_sample, big.mark = ",")
            )
          )
        )
      )
    })


    listenMapData <- reactive({
      list(
        input$taxo_id,
        input$slc_spe,
        input$data_type
      )
    })
    observeEvent(listenMapData() |> debounce(100), {
      r$species <- input$slc_spe
      r$taxon_id_slc <- input$taxo_id
      if (!is.null(input$slc_spe) && input$slc_spe != "All") {
        r$taxon_lvl_slc <- "species"
      } else {
        r$taxon_lvl_slc <- input$taxo_lvl
      }
      r$fig_ready <- FALSE
      # count data
      r$geom <- filter_station(r)
      r$reload_map <- r$reload_map + 1
    })

    observeEvent(input$confirm, {
      if (!is.null(sf_edits()$all)) {
        cli::cli_alert_info("Using geom(s) drawn to select region")
        if (!is.null(r$geom)) {
          id_slc <- sf::st_contains(sf_edits()$all, r$geom, sparse = FALSE) |>
            apply(2, any)
          if (sum(id_slc)) {
            r$geom <- r$geom[id_slc, ]
            r$geom_slc <- sf_edits()$all
            r$station_slc <- r$geom$station
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

    observeEvent(r$geom, {
      r$n_sample <- sum(r$geom$count)
    })

    observeEvent(input$clear_area, {
      r$geom_slc <- r$station_slc <- NULL
      r$geom <- filter_station(r)
      r$reload_map <- r$reload_map + 1
    })

    observeEvent(r$reload_map, {
      prx <- leaflet::leafletProxy("map-select")
      prx$id <- "slc_data-map-select-map"
      update_map(prx, isolate(r$geom), isolate(r$geom_slc), lock_view = r$lock_view)
    })
  })
}


# filter via inner join and used to count samples
filter_station <- function(r) {
  if (length(r$station_slc)) {
    sta <- r$data_station |>
      dplyr::filter(station %in% r$station_slc)
  } else {
    sta <- r$data_station
  }
  dff <- filter_taxon(r$cur_data, r$taxon_lvl_slc, r$taxon_id_slc, r$species)
  sta |>
    dplyr::inner_join(
      dff |>
        dplyr::group_by(ecodistrict, station) |>
        dplyr::summarise(count = n()),
      join_by(station)
    )
}


filter_taxon <- function(data, taxon_lvl, taxon_id, species) {
  out <- data
  if (!is.null(taxon_lvl)) {
    if (taxon_lvl == "species") {
      out <- out |>
        dplyr::filter(scientificName == species)
    } else {
      if (taxon_id != "All") {
        out <- out[
          out[[taxon_lvl]] == taxon_id,
        ]
      }
    }
  }
  out
}

update_map <- function(proxy, geom, geom_slc, lock_view = FALSE) {
  proxy |>
    clearGroup("station") |>
    clearGroup("select_polygon") |>
    addMarkers(
      data = geom,
      clusterOptions = markerClusterOptions(),
      label = ~ paste(count, "samples"),
      group = "station"
    )
  if (!is.null(geom_slc)) {
    geom_type <- geom_slc |>
      sf::st_geometry_type() |>
      as.character()
    ind <- geom_type == "POLYGON"
    if (length(ind)) {
      proxy |>
        addPolygons(
          data = geom_slc[ind, ],
          color = "#75f9c6",
          fillOpacity = 0.1,
          group = "select_polygon"
        )
    } else {
      showNotification(
        "Only rectangles and polygons can be used",
        type = "warning"
      )
    }
  }
  if (!lock_view) {
    bb <- geom |>
      sf::st_bbox() |>
      as.vector()
    proxy |>
      fitBounds(bb[1], bb[2], bb[3], bb[4])
  }
}
