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
                selectInput(ns("slc_dom"), "Domain", choices = "All")
              ),
              column(
                3,
                selectInput(ns("slc_kin"), "Kingdom", choices = "All")
              ),
              column(
                3,
                selectInput(ns("slc_phy"), "Phylum", choices = "All")
              ),
              column(
                3,
                selectInput(ns("slc_cla"), "Class", choices = "All")
              ),
              column(
                3,
                selectInput(ns("slc_ord"), "Order", choices = "All")
              ),
              column(
                3,
                selectInput(ns("slc_fam"), "Family", choices = "All")
              ),
              column(
                3,
                selectInput(ns("slc_gen"), "Genus", choices = "All")
              ),
              column(
                3,
                selectInput(ns("slc_spe"), "Species", choices = "All")
              )
            ),
            div(
              class = "section_footer",
              id = ns("section_footer_data_request"),
              column(12, uiOutput(outputId = ns("n_smpl")))
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
          column(8, h1("Area Selection")),
          column(
            4,
            div(
              class = "top_right_button",
              div(
                id = "button_map",
                actionButton(ns("confirm"), "Confirm",
                  icon = icon("check"),
                  title = "Confirm spatial selection"
                ),
                actionButton(ns("refresh"), "Clear area",
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
        actionButton(ns("show_map_info"), "How to select",
          icon = icon("info-circle"),
          title = "Display information about how to use the map below"
        )
      )
    )
  )
}


mod_select_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      # change when reading of external data is implemented
      if (input$datasource == "gotedna") {
        gotedna_data <- gotedna_data0
        gotedna_station <- gotedna_station0
      } else {
        gotedna_data <- gotedna_data0
        gotedna_station <- gotedna_station0
      }
    })

    observeEvent(input$hide_fields, {
      shinyjs::toggle("data_request_fields")
      shinyjs::toggle("section_footer_data_request")
    })

    observeEvent(input$hide_map, {
      shinyjs::toggle("map_container")
      shinyjs::toggle("section_footer_area_selection")
    })


    observeEvent(input$datatype, {
      r$data_type <- input$datatype
      r$data_filtered <- gotedna_data[[input$datatype]]
      r$data_station <- gotedna_station[[input$datatype]]
      updateSelectInput(session, "slc_phy",
        selected = "All",
        choices = c("All", unique(r$data_filtered$phylum) |> sort())
      )
    })

    observeEvent(input$slc_phy, {
      r$data_filtered <- filter_taxa_data(
        gotedna_data[[input$datatype]], input$slc_phy, "All", "All", "All"
      )
      if (input$slc_phy == "All") {
        updateSelectInput(session, "slc_cla", select = "All")
        r$slc_taxon_lvl <- "all"
      } else {
        updateSelectInput(session, "slc_cla",
          choices = c("All", unique(r$data_filtered$class) |> sort())
        )
        r$slc_taxon_lvl <- "phylum"
      }
    })
    observeEvent(input$slc_cla, {
      r$data_filtered <- filter_taxa_data(
        gotedna_data[[input$datatype]], input$slc_phy, input$slc_cla,
        "All", "All"
      )
      if (input$slc_cla == "All") {
        updateSelectInput(session, "slc_gen", select = "All")
      } else {
        updateSelectInput(session, "slc_gen",
          choices = c("All", unique(r$data_filtered$genus) |> sort())
        )
        r$slc_taxon_lvl <- "class"
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
        updateSelectInput(session, "slc_spe",
          choices = c("All", unique(r$data_filtered$scientificName) |> sort())
        )
        r$slc_taxon_lvl <- "genus"
      }
    })
    observeEvent(input$slc_spe, {
      r$data_filtered <- filter_taxa_data(
        gotedna_data[[input$datatype]], input$slc_phy, input$slc_cla,
        input$slc_gen, input$slc_spe
      )
      if (input$slc_spe != "All") r$slc_taxon_lvl <- "species"
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
          choices = get_primer_selection(r$slc_taxon_lvl, r$data_filtered)
        )
      }
    })

    observe(
      r$primer <- input$primer
    )

    output$n_smpl <- renderUI({
      tagList(
        div(
          id = "sample_selected",
          p(
            "Sample selected: ",
            strong(format(r$n_sample, big.mark = ","))
          )
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
      r$geom <- filter_station(r)
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

    observeEvent(input$reset, {
      r$data_filtered <- filter_taxa_data(
        gotedna_data[[input$datatype]], "All", "All", "All", "All"
      )
      updateSelectInput(session, "slc_phy",
        selected = "All",
        choices = c("All", unique(r$data_filtered$phylum))
      )
      r$geom_slc <- r$station_slc <- NULL
      r$geom <- filter_station(r)
      r$reload_map <- r$reload_map + 1
    })


    observeEvent(input$refresh, {
      r$data_filtered <- filter_taxa_data(
        gotedna_data[[input$datatype]], input$slc_phy, input$slc_cla,
        input$slc_gen, "All"
      )
      r$geom_slc <- r$station_slc <- NULL
      r$geom <- filter_station(r)
      r$reload_map <- r$reload_map + 1
    })

    observeEvent(r$geom, {
      r$n_sample <- sum(r$geom$count)
    })

    observeEvent(r$reload_map, {
      sf_edits <<- callModule(
        mapedit::editMod,
        leafmap = make_map(r),
        id = "map-select"
      )
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
  sta |>
    dplyr::inner_join(
      r$data_filtered |>
        dplyr::group_by(ecodistrict, station) |>
        dplyr::summarise(count = n()),
      join_by(station)
    )
}

make_map <- function(r) {
  out <- leaflet(isolate(r$geom)) |>
    addProviderTiles("Esri.OceanBasemap", group = "Ocean") |>
    addMarkers(
      data = isolate(r$geom),
      clusterOptions = markerClusterOptions(),
      label = ~ paste(count, "samples")
    )
  if (!is.null(isolate(r$geom_slc))) {
    geom_type <- isolate(r$geom_slc) |>
      st_geometry_type() |>
      as.character()
    ind <- geom_type == "POLYGON"
    if (length(ind)) {
      out <- out |>
        addPolygons(data = isolate(r$geom_slc)[ind, ], color = "#53b2ad", fillOpacity = 0.1)
    } else {
      showNotification(
        "Only rectangles and polygons can be used",
        type = "warning"
      )
    }
  }
  out
}
