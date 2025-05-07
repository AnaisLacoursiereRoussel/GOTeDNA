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
          h1("Data request",
               icon("info-circle", class = "definition",
                    title = "Processing speed is impacted if data request is broad. Please select specific taxonomic level or species for increased speed."),
             ),
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
                    "Upload data" = "external_data"
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
                    "Multi-species (metabarcoding)" = "metabarcoding",
                    "Species specific (qPCR)" = "qPCR"
                  ),
                  selected = "metabarcoding"
                )
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
                  ns("taxo_lvl"),
                  "Taxonomy selection",
                  choices = taxonomic_ranks
                )
              ),
              column(
                3, selectInput(ns("taxo_id"),
                  "Taxa",
                  choices = "All"
                )
              ),
              column(
                3,
                selectizeInput(ns("slc_spe"),
                  "Species",
                  choices = "All"
                )
              ),
              column(
                3,
                # https://stackoverflow.com/questions/50218614/shiny-selectinput-to-select-all-from-dropdown
                htmltools::tagQuery(shinyWidgets::pickerInput(ns("primer"),
                  div(
                    "Primer set",
                    icon("info-circle", class = "definition", onclick = "fakeClick('primer-info')",
                         title = "Primer details"),
                  ),
                  choices = "All",
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE
                ))$find(".btn")$removeAttrs("data-toggle")$addAttrs(`data-bs-toggle` = "dropdown")$allTags()
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
            "Area selection",
            span(
              id = ns("lock"),
              class = "lock", icon("lock"),
              title = "Map view locked"
            ),
            span(
              id = ns("restrict"),
              class = "restrict", icon("warning"),
              title = "Selection restricted to spatial area"
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
        class = "leaflet_container",
        style = "display: grid; justify-content: center",
        id = ns("map_container"),
        mapedit::editModUI(ns("map-select"), height = "75vh", width = "65vw")
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
      leafmap = basemap(),
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
        shinyjs::hide("restrict")
      } else {
        shinyjs::show("restrict")
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
      r$protocol_ID <- paste0(
        r$cur_data$protocol_ID
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

    ## update Taxon data
    observe({
      updateSelectInput(
        session,
        "taxo_id",
        choices = c(
          "All",
          r$cur_data[[input$taxo_lvl]] |>
            unique() |>
            sort()
        ),
        selected = "All"
      )
    })

    observeEvent(input$taxo_id, {
      if (input$taxo_id != "All") {
        updateSelectizeInput(
          session,
          "slc_spe",
          choices = c(
            "All",
            r$cur_data[
              r$cur_data[[input$taxo_lvl]] == input$taxo_id,
            ]$species |>
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
            r$cur_data_sta_slc$species |>
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
        selected = "domain"
      )
      updateSelectizeInput(
        session,
        "slc_spe",
        selected = "All"
      )
      r$reset <- r$reset + 1
      r$data_ready <- NULL
    })


    observe({
      r$primer_choices_all <- get_primer_selection(
        r$taxon_lvl_slc, filter_taxon(
          isolate(r$cur_data_sta_slc),
          r$taxon_lvl_slc,
          r$taxon_id_slc,
          r$species
        )
      )
      shinyWidgets::updatePickerInput(
        session,
        "primer",
        choices = r$primer_choices_all,
        selected = r$primer_choices_all
      )
    })

    observe(r$primer <- input$primer)

    # sample number
    output$n_smpl_map <- renderUI({
      tagList(
        div(
          class = "sample_selected",
          p(
            "Total number of samples: ",
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
        input$data_type,
        input$primer
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

            geom_coords <- sf::st_bbox(r$geom_slc)
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
      # Reload module
      sf_edits <<- callModule(
        mapedit::editMod,
        leafmap = basemap(),
        id = "map-select"
      )
      # shinyWidgets::updatePickerInput(
      #   session,
      #   "primer",
      #   selected = r$primer
      # )
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
  dff <- filter_taxon(
    r$cur_data, r$taxon_lvl_slc, r$taxon_id_slc, r$species,
    r$primer
  ) |>
    dplyr::filter(primer %in% r$primer)
  sta |>
    dplyr::inner_join(
      dff |>
        dplyr::group_by(station, materialSampleID) |>
        dplyr::summarise(
          count = dplyr::n_distinct(materialSampleID),
          success = sum(detected)
        ),
      # dplyr::group_by(station, primer, species) |>
      # dplyr::summarise(
      #   success = sum(detected),
      #   count = dplyr::n_distinct(materialSampleID)),
      join_by(station)
    )
}


filter_taxon <- function(data, taxon_lvl, taxon_id, species, primer) {
  out <- data
  if (!is.null(taxon_lvl)) {
    if (taxon_lvl == "species") {
      out <- out |>
        dplyr::filter(species == {{ species }}, primer == primer)
    } else {
      if (taxon_id != "All") {
        out <- out |>
          dplyr::filter(!!dplyr::ensym(taxon_lvl) == taxon_id, primer == primer)
      }
    }
  }
  out
}

update_map <- function(proxy, geom, geom_slc, lock_view = FALSE) {
  proxy |>
    leaflet::clearGroup("station") |>
    leaflet::clearGroup("select_polygon") |>
    leaflet::addMarkers(
      data = geom,
      clusterOptions = leaflet::markerClusterOptions(),
      label = ~ paste(success, "observations"),
      group = "station"
    )
  if (!is.null(geom_slc)) {
    geom_type <- geom_slc |>
      sf::st_geometry_type() |>
      as.character()
    ind <- geom_type == "POLYGON"
    if (length(ind)) {
      proxy |>
        leaflet::addPolygons(
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
      leaflet::fitBounds(bb[1], bb[2], bb[3], bb[4])
  }
}
