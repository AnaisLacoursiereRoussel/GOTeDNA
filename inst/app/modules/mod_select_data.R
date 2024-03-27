# Select data and show them on the map
mod_select_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "data_request",
      div(
        class = "section_header",
        fluidRow(
          # title and top buttons
          column(8, h1("Data request")),
          column(
            4,
            div(
              class = "top_right_button",
              actionButton(ns("reset"), "Reset",
                title = "Reset selection to default values"
              ),
              actionButton(ns("hide_fields"), "Show/Hide fields",
                title = "Hide or show fields"
              )
            )
          )
        ),
        # fields
        div(
          id = ns("data_request_fields"),
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
                  fileInput(
                    ns("external_file"),
                    "upload your files",
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
                selectInput(ns("datatype"),
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
            ),
            div(
              class = "section_footer",
              id = ns("section_footer_data_request"),
              column(12, uiOutput(outputId = ns("n_smpl_data")))
            )
          )
        )
      )
    ),
    div(
      id = "area_selection",
      div(
        class = "section_header",
        fluidRow(
          column(
            8,
            h1("Area Selection", span(id = ns("lock"), class = "lock", icon("lock")))
          ),
          column(
            4,
            div(
              class = "top_right_button",
              div(
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
          )
        )
      ),
      div(
        id = ns("map_container"),
        mapedit::editModUI(ns("map-select"), height = "60vh")
      ),
      div(
        class = "section_footer",
        id = ns("section_footer_area_selection"),
        fluidRow(
          column(
            6,
            actionButton(ns("show_map_info"), "How to select",
              icon = icon("info-circle"),
              title = "Display information about how to use the map below"
            )
          ),
          column(
            6,
            uiOutput(outputId = ns("n_smpl_map"))
          )
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

    observeEvent(input$datatype, {
      r$data_type <- input$datatype
      r$cur_data <- r$data_filtered <- gotedna_data[[input$datatype]]
      r$data_station <- gotedna_station[[input$datatype]]
      updateSelectInput(session, "slc_phy",
        selected = "All",
        choices = c("All", unique(r$data_filtered$phylum) |> sort())
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
          r$cur_data_sta_slc[[input$taxo_lvl]] |>
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
            r$cur_data_sta_slc[
              r$cur_data_sta_slc[[input$taxo_lvl]] == input$taxo_id,
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
            r$data_filtered$scientificName |> unique() |> sort()
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
      if (input$datatype == "qPCR") {
        updateSelectInput(
          session,
          "primer",
          choices = "not available"
        )
      } else {
        updateSelectInput(
          session,
          "primer",
          choices = get_primer_selection(r$taxon_lvl_slc, r$data_filtered)
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
        input$datatype
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
          id_slc <- st_contains(sf_edits()$all, r$geom, sparse = FALSE) |>
            apply(2, any)
          if (sum(id_slc)) {
            r$geom <- r$geom[id_slc, ]
            # r$data_filtered <- r$data_filtered |>
            #   dplyr::filter(station %in% r$geom$station)
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
      # r$data_filtered <- gotedna_data[[input$datatype]]
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
  dff <- r$cur_data
  if (!is.null(r$taxon_lvl_slc)) {
    if (r$taxon_lvl_slc == "species") {
      dff <- dff |>
        dplyr::filter(scientificName == r$species)
    } else {
      if (r$taxon_id_slc != "All") {
        dff <- dff[
          dff[[r$taxon_lvl_slc]] == r$taxon_id_slc,
        ]
      }
    }
  }
  sta |>
    dplyr::inner_join(
      dff |>
        dplyr::group_by(ecodistrict, station) |>
        dplyr::summarise(count = n()),
      join_by(station)
    )
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
          color = "#75f9c6;",
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
