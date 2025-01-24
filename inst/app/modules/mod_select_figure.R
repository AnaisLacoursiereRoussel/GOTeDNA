# Select data and show them on the map
mod_select_figure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "figure_selection",
      div(
        class = "section_header",
        div(
          class = "title-container",
          h1("Figure selection"),
          div(
            class = "buttons-container",
            actionButton(ns("hide_figs"), "Hide/Show figures",
              title = "Hide or show fields"
            )
          )
        )
      ),
      div(
        id = ns("figure_selection_main"),
        div(
          class = "d-flex justify-content-center buttons-container",
          id = "select_all_figures",
          actionButton(
            ns("select_all"),
            "Select all figures",
            title = "Select all figures"
          ),
          actionButton(
            ns("deselect_all"),
            "Deselect all figures",
            title = "Deselect all figures"
          )
        ),
        div(
          id = "thumbnail_container",
          fluidRow(
            class = "justify-content-center",
            add_figure_selection(
              ns("fig_detect"),
              "Monthly eDNA detection probability",
              "img/thumbnails/tn_thresh.svg"
            ),
            add_figure_selection(
              ns("fig_effort"),
              "Guidance on sampling effort",
              "img/thumbnails/tn_effort.svg"
            ),
            add_figure_selection(
              ns("fig_heatmap"),
              "Species detection heatmap",
              "img/thumbnails/tn_heatmap.svg"
            ),
            add_figure_selection(
              ns("fig_samples"),
              "Data variation",
              "img/thumbnails/tn_samples.svg"
            )
          )
        ),
        div(
          class = "d-flex justify-content-center",
          id = "confirm_figures_selection",
          actionButton(
            ns("calc_window"),
            label = "Compute & visualize",
            title = "Compute optimal detection window",
            class = "primary-button"
          )
        ),
      )
    ),
    div(
      id = "observation",
      fluidRow(
        class = "panels-container",
        column(
          3,
          class = "control-panel",
          div(
            id = "obs_panel",
            class = "section_header",
            h1("Observation")
          ),
          div(
            id = "fig_left_panel",
            selectInput(
              ns("threshold"),
              "Threshold",
              choices = ls_threshold,
              selected = 75
            ),
            selectInput(
              ns("proj_id"),
              "GOTeDNA Project",
              choices = "Not available",
              selected = NULL
            ),
            div(
              id = "fig_sampling_info",
              h4("Guidance"),
              div(
                class = "sampling_info-item",
                h6("Optimal timing: "),
                uiOutput(ns("opt_sampl"), class = "fig_text_output")
              ),
              div(
                class = "sampling_info-item",
                h6("Confidence: "),
                uiOutput(ns("conf"), class = "fig_text_output")
              ),
              div(
                class = "sampling_info-item",
                h6("Consistency among years: "),
                uiOutput(ns("var_year"), class = "fig_text_output"),
              )
            ),
            downloadButton(
              ns("export_pdf"),
              "Export to PDF",
              title = "Export figures to PDF",
               class = "primary-button"
            )
          )
        ),
        column(
          9,
          class = "show-panels",
          div(
            class = "fig_main_container",
            div(
              class = "fig_main_container-fig",
              ui_fig_detect("fig_detect", "Monthly eDNA detection probability", "detection.html", ns),
              ui_fig_effort("fig_effort", "Guidance on sampling effort", "sample_size.html", ns),
              ui_fig_hm("fig_heatmap", "Species detection heatmap", "heatmap.html", ns),
              ui_fig_samples("fig_samples", "Data variation", "field_sample.html", ns)
            )
          )
        )
        )
      ),
        div(
          id = "reference_data_authorship",
          div(
            class = "table_title-container",
            h1("Reference data authorship")
          ),
          DT::DTOutput(ns("data_authorship"))
        ),
      #)
    #)
  )
}


mod_select_figure_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$hide_figs, {
      shinyjs::toggle("figure_selection_main")
    })

    observeEvent(input$select_all, {
      for (i in c("fig_detect", "fig_effort", "fig_heatmap", "fig_samples")) {
        show_fig(i)
        r$fig_slc[[i]] <- TRUE
      }
    })

    observeEvent(input$deselect_all, {
      for (i in c("fig_detect", "fig_effort", "fig_heatmap", "fig_samples")) {
        hide_fig(i)
        r$fig_slc[[i]] <- FALSE
      }
    })

    observeEvent(r$reset,
      {
        for (i in c("fig_detect", "fig_effort", "fig_heatmap", "fig_samples")) {
          hide_fig(i)
          r$fig_slc[[i]] <- FALSE
        }
      },
      ignoreInit = TRUE
    )

    hide_fig("fig_detect")
    observeEvent(input$fig_detect, {
      toggle_legends("thresh_legend")
      toggle_fig("fig_detect")
      r$fig_slc$fig_detect <- !r$fig_slc$fig_detect
    })

    hide_fig("fig_effort")
    observeEvent(input$fig_effort, {
      toggle_fig("fig_effort")
      r$fig_slc$fig_effort <- !r$fig_slc$fig_effort
    })

    hide_fig("fig_heatmap")
    observeEvent(input$fig_heatmap, {
      toggle_fig("fig_heatmap")
      r$fig_slc$fig_heatmap <- !r$fig_slc$fig_heatmap
    })

    hide_fig("fig_samples")
    observeEvent(input$fig_samples, {
      toggle_fig("fig_samples")
      r$fig_slc$fig_samples <- !r$fig_slc$fig_samples
    })

    # CALC WINDOW
    observeEvent(
      ignoreInit = TRUE,
      list(input$calc_window, input$threshold, input$proj_id),
      {
        req(input$calc_window)

            r$data_ready <- prepare_data(r) %>%
              filter(GOTeDNA_ID == input$proj_id)

            if (nrow(r$data_ready)) {
              showNotification(
                paste0(
                  "Computing detection window with threshold set to ",
                  input$threshold, "%",
                  ifelse(
                    nrow(r$data_ready) > 1e4,
                    paste0(" (", nrow(r$data_ready), " observations, this may take some time)"),
                    ""
                  )
                ),
                type = "message",
                duration = NULL,
                id = "notif_calc_win"
              )
              newprob <- calc_det_prob(r$data_ready)
              r$scaledprobs <- scale_newprob(r$data_ready, newprob)

              cli::cli_alert_info("Computing optimal detection window")

              thresh_slc <- input$threshold

              win <- calc_window(
                threshold = input$threshold,
                scaledprobs = r$scaledprobs
              )

              j.sim <- jaccard_test(
                r$scaledprobs,
                input$threshold)

              removeNotification(id = "notif_calc_win")

              if (is.null(win)) {
               #  showNotification("No optimal detection window", type = "warning")
                output$opt_sampl <- renderUI("No single detection window")
                output$conf <- renderUI("NA")
                output$var_year <- renderUI(
                  paste(j.sim))
              } else {
                output$opt_sampl <- renderUI(
                  paste(win$opt_sampling$period)
                )
                output$conf <- renderUI(paste(win$fshTest$confidence))
                output$var_year <- renderUI(
                    paste(j.sim)
                  )
              }



              r$fig_ready <- TRUE

              # ca a l'air good a recheck demainNB from KC: if r$scaledprobs includes the same projects as
              # the input project, the following lines should be removed!
              # create project vector
              v_proj <- r$scaledprobs$GOTeDNA_ID |> unique()

            } else {
              showNotification("Data selection is empty", type = "warning")
            }

            shinyscreenshot::screenshot(
                selector = "#data_request_top_fields",
                filename = "data_top",
                download = FALSE, server_dir = tempdir()
              )

            shinyscreenshot::screenshot(
              selector = "#data_request_bottom_fields",
              filename = "data_btm",
              download = FALSE, server_dir = tempdir()
            )

              shinyscreenshot::screenshot(
                selector = "#reference_data_authorship",
                filename = "dat_auth",
                download = FALSE, server_dir = tempdir()
              )

      }
    )

    # UPDATE DATA
    observe({
      out <- r$cur_data_sta_slc
      if (!is.null(r$taxon_lvl_slc)) {
        if (r$taxon_lvl_slc == "species") {
          out <- out |>
            dplyr::filter(species == r$species)

        } else {
          if (r$taxon_id_slc != "All") {
            out <- out[
              out[[r$taxon_lvl_slc]] == r$taxon_id_slc,
            ]

         }
        }
      }

      # primer-based subset
      r$data_active <- out |>
        dplyr::filter(primer %in% r$primer)
      # prevent computation when new data are selected
      r$fig_ready <- FALSE
    })

    observe({
      if (nrow(r$data_active)) {
        v_proj <- r$data_active$GOTeDNA_ID |> table()
        l_proj <- names(v_proj) |> as.list()
        names(l_proj) <- paste0(
          "GOTeDNA ID: ",
          v_proj |> names(),
          " (", v_proj, " samples)"
        )
        updateSelectInput(
          session,
          "proj_id",
          choices = l_proj[v_proj |>
            order() |>
            rev()]
        )
      }
    })



    # FIGURES
    ## DETECTION part 1
    output$fig_smooth_plot_output <- renderPlot({
      if (req(r$fig_ready, cancelOutput = TRUE)) {
        draw_fig_smooth(isolate(r), r$fig_ready && r$fig_slc$fig_detect,
          id = input$proj_id # using proj_id as string
        )
      }
    })
    ## DETECTION part 2
    output$fig_detect_plot_output <- renderPlot({
      if (req(r$fig_ready, cancelOutput = TRUE)) {
        draw_fig_detect(r, r$fig_ready && r$fig_slc$fig_detect, input$threshold
        )
      }
    })

    ## EFFORT
    output$fig_effort_plot_output <- plotly::renderPlotly({
      if (req(r$fig_ready, cancelOutput = TRUE)) {
          ggp <- draw_fig_effort(r, r$fig_ready && r$fig_slc$fig_effort)
        # multiply height by number of species
        nys <- r$data_ready$species |>
          unique() |>
          length()
        plt <- plotly::ggplotly(
          ggp,
          height = 320 * nys
        ) |>
          default_layout() |>
          facet_strip_format() |>
          layout(
            xaxis = list(title = list(
              text = "Number of samples",
              font = list(
                size = 30,
                color = "#5A5A5A"
              ),
              x = 0
            )),
            yaxis = list(title = list(
              text = "Detection probability",
              font = list(
                size = 30,
                color = "#5A5A5A"
              ),
              y = 0
            ))
          )
      }
    })

    ## HEATMAP
    output$fig_heatmap_plot_output <- plotly::renderPlotly({
      if (req(r$fig_ready, cancelOutput = TRUE)) {
        plt_ready <- r$fig_ready && r$fig_slc$fig_heatmap
        ggp <- draw_fig_heatmap(r, plt_ready)
        # multiply height by number of species
        nys <- r$data_ready$species |>
          unique() |>
          length()
        plt <- plotly::ggplotly(
          ggp,
          height = 275 * nys,
          tooltip = c("x", "fill")
        ) |>
          default_layout() |>
          facet_strip_format()
      }
    })

    ## DATA VARIATION
    output$fig_samples_plot_output <- plotly::renderPlotly({
      plt_ready <- r$fig_ready && r$fig_slc$fig_samples
      ggp <- draw_fig_samples(r, plt_ready, id = input$proj_id)
      if (plt_ready) {
        # multiply height per years #
        nys <- r$data_ready$year |>
          unique() |>
          length()
        plotly::ggplotly(
          ggp,
          height = 320 * nys
        ) |>
          default_layout() |>
          facet_strip_format() |>
          layout(
            xaxis = list(title = list(
              text = "Month",
              font = list(
                size = 30,
                color = "#5A5A5A"
              ),
              x = 0
            )),
            yaxis = list(title = list(
              text = "Detection rate",
              font = list(
                size = 30,
                color = "#5A5A5A"
              ),
              y = 0
            ))
          ) # change the style but
        # better than it was!!!!!!
      }
    })



    # DATA AUTHORSHIP TABLE
    output$data_authorship <- DT::renderDT({
      r$data_active |>
        dplyr::ungroup() |>
        dplyr::group_by(
          GOTeDNA_ID,
          GOTeDNA_version,
          LClabel,
          bibliographicCitation
        ) |>
        summarise(
          `Sample #` = dplyr::n_distinct(materialSampleID),
          `Station #` = dplyr::n_distinct(station),
          Contact = unique(ownerContact)
        ) |>
        dplyr::ungroup() |>
        mutate(
          `Indigenous contribution` = ifelse(
            !is.na(LClabel),
            "<button type='submit' style='border: 0; background: transparent'
            onclick='fakeClick(\"fn-conts\")'><img src='img/fn_logo.png' height='25'/>
            </button>",
            NA
          ),
          LClabel = NULL
        ) |>
        dplyr::rename(
          "GOTeDNA ID" = "GOTeDNA_ID",
          "Subproject" = "GOTeDNA_version",
          "Publication" = "bibliographicCitation"
        ) |>
        dplyr::relocate(
          `GOTeDNA ID`, Subproject, # Contact,
          `Sample #`, `Station #`, `Indigenous contribution`, Publication
        ) |>
        DT::datatable(
          escape = FALSE, rownames = FALSE,
          options = list(
            columnDefs = list(list(className = "dt-center", targets = "_all"))
          )
        )
    })

    # EXPORT PDF
    output$export_pdf <- downloadHandler(
     filename = function() {
        paste0("GOTeDNA_report_",Sys.Date(),".pdf")
        },
      contentType = "application/pdf",
      content = function(file) {

        # Data Request
     #   datSrc <- input$datasource
      #  datTyp <- input$data_type
       # primers <- r$primer

        # Area Selection
        if (!is.null(r$geom_slc)){
        geom_coords <- st_bbox(r$geom_slc) %>%
          as.matrix() %>%
          t() %>%
          as.data.frame()
        } else {
          geom_coords <- c("Area selection not confirmed")
        }

        mapDL <- addMarkers(basemap(),
            data = r$geom,
            clusterOptions = markerClusterOptions(),
            label = ~ paste(success, "observations"),
            group = "station"
          ) |>
          addScaleBar()

        mapview::mapviewOptions(fgb = FALSE)

        mapview::mapshot(
          mapDL,
          file = file.path(tempdir(), "mapDL.png")
        )

        # Observation
        thresh_slc <- input$threshold
        projID <- input$proj_id
        sampWin <- calc_window(
          threshold = thresh_slc,
          scaledprobs = r$scaledprobs
        )

        cons <- jaccard_test(
          r$scaledprobs,
          thresh_slc)


        # Reference Data Authorship
        FNdata <- r$data_ready$LClabel

        tempReport <- file.path(tempdir(), "report.Rmd")
        tempDFOlogo <- file.path(tempdir(), "DFOlogo.png")
        tempGOTlogo <- file.path(tempdir(), "GOTeDNAlogo.png")
        temphmLeg <- file.path(tempdir(), "hmLegend.png")
        tempthreshAx <- file.path(tempdir(), "threshAxis.png")
        tempthreshLeg <- file.path(tempdir(), "threshLegend.png")

        file.copy("Report.rmd", tempReport, overwrite = TRUE)
        file.copy("DFOlogo.png", tempDFOlogo, overwrite = TRUE)
        file.copy("GOTeDNAlogo.png", tempGOTlogo, overwrite = TRUE)
        file.copy("hm_legend.png", temphmLeg, overwrite = TRUE)
        file.copy("thresh_axis.png", tempthreshAx, overwrite = TRUE)
        file.copy("thresh_legend.png", tempthreshLeg, overwrite = TRUE)

       # params <- list(win, j.sim)

        out <- rmarkdown::render(tempReport)#,
                                # params = params)
        file.rename(out, file)
        }
        )
  })
}




#------- INTERNALS

prepare_data <- function(r) {
  out <- r$cur_data
  if (length(r$station_slc)) {
    out <- out |>
      dplyr::filter(station %in% r$station_slc)
  }
  if (!is.null(r$taxon_lvl_slc)) {
    if (r$taxon_lvl_slc == "species") {
      out <- out |>
        dplyr::filter(species == r$species)
    } else {
      if (r$taxon_id_slc != "All") {
        out <- out[
          out[[r$taxon_lvl_slc]] == r$taxon_id_slc,
        ]
      }
    }
  }
  # primer-based subset
  out |>
    dplyr::filter(primer %in% r$primer)
}

n_projs <- function(r) {
  proj_ids <- r$data_ready |>
    dplyr::summarise(
      n = sum(detect, nondetect, na.rm = TRUE),
      .by = GOTeDNA_ID
    ) |>
    sort(n, decreasing = TRUE) |>
    select(GOTeDNA_ID)
}



## FIG HELPERS

plotText <- function(txt, size = 6) {
  data.frame(x = 0.5, y = 0.5, txt = txt) |>
    ggplot2::ggplot(ggplot2::aes(x, y, label = txt)) +
    ggplot2::geom_text(size = size) +
    ggplot2::theme_void()
}

plotNotAvailableTaxoLevel <- function() {
  plotText("Plot not available at the species level.")
}

plotNotAvailableSpeciesLevel <- function() {
  plotText("Plot only available at the species level.")
}

plotNotAvailable <- function() {
  plotText(stringr::str_wrap("Plot not available. Click 'Compute & visualize'"))
}

plotNotAvailableForqPCR <- function() {
  plotText("Plot not available for qPCR data.")
}

plotNotAvailableYear <- function() {
  plotText("Plot not available; small sample size.")
}

plotNotAvailableError <- function() {
  plotText(stringr::str_wrap(
    "An error occured while rendering the plot (sample size may be too small).",
    width = 60
  ))
}

add_fixed_legend <- function(file) {
  if (is.null(file)) {
    tagList()
  } else {
    img(
      src = file.path("img", "fixed-legends", file),
      alt = "Legend of the figure"
    )
  }
}


## Detection part 1
draw_fig_smooth <- function(r, ready, id) {
  if (ready) {
    plt <- try(
      smooth_fig(
        r$data_ready |>
          filter(GOTeDNA_ID == id)
      )
    )
    if (inherits(plt, "try-error")) {
      plotNotAvailableError()
      # there are lots of erros due to predLoess, this is better than a misleading
      # message on the plot
    } else {
      plt
    }
  } else {
    plotNotAvailable()
  }
}

## Detection part 2
draw_fig_detect <- function(r, ready, threshold){
  if (ready) {
    plt <- thresh_fig(
      threshold,
      r$scaledprobs
    )
    plt
  } else {
    plotNotAvailable()
  }
}

## Sampling effort
draw_fig_effort <- function(r, ready){
  if (ready) {
    p <- effort_needed_fig(
      r$scaledprobs
    )
    p

  } else {
    plotNotAvailable()
  }
}

## Heatmap
draw_fig_heatmap <- function(r, ready){
  if (ready) {
    p <- try(hm_fig(
      r$scaledprobs))

    if (inherits(p, "try-error")) {
      plotNotAvailableError()
      # there are lots of erros due xmin not found, this is better than a misleading
      # message on the plot
    } else {
      p
    }

    } else {
    plotNotAvailable()
  }
}

## Data variation
draw_fig_samples <- function(r, ready, id) {
  if (ready) {
    p <- field_sample_fig(
      r$data_ready |>
        filter(GOTeDNA_ID == id)
    )
    p
  } else {
    plotNotAvailable()
  }
}



ui_fig_detect <- function(fig_id, title, caption_file, ns) {
  div(
    id = paste0(ns(fig_id), "_fig_container"),
    class = "fig_container",
    h4(title),
    div(
      class = "fig_caption-container",
      div(
        class = "fig_caption",
        includeHTML(file.path("www", "doc", "caption", caption_file))
      )
    ),
    div(
      class = "fig_panel_container",
      div(
        class = "fig_panel",
        bslib::layout_columns(
          bslib::card_image(
            file = "www/img/fixed-legends/thresh_axis.png",
            fill = FALSE,
            width = "80px"
          ),
          bslib::card_body(
            plotOutput(ns("fig_smooth_plot_output")),
            plotOutput(ns("fig_detect_plot_output")),
          ),
          bslib::card_body(
            bslib::card_body(height = "250px"),
            bslib::card_image(
              file = "www/img/fixed-legends/thresh_legend.png",
              fill = FALSE,
              width = "200px"
            )
          ),
          col_widths = c(2, 6, 4)
        )
      )
    )
  )
}

ui_fig_hm <- function(fig_id, title, caption_file, ns) {
  div(
    id = paste0(ns(fig_id), "_fig_container"),
    class = "fig_container",
    h4(title),
    div(
      class = "fig_caption-container",
      div(
        class = "fig_caption",
        includeHTML(file.path("www", "doc", "caption", caption_file))
      )
    ),
    div(
      class = "fig_panel_container",
      div(
        class = "fig_panel",
        bslib::layout_columns(
          bslib::card_body(
            plotly::plotlyOutput(paste0(ns(fig_id), "_plot_output"),
              height = "auto"
            ),
            fillable = TRUE,
          ),
          bslib::card_image(
            file = "www/img/fixed-legends/hm_legend.png",
            fill = FALSE
          ),
          col_widths = breakpoints(
            sm = c(9, 3),
            md = c(10, 2)
          )
        )
      )
    )
  )
}

ui_fig_effort <- function(fig_id, title, caption_file, ns) {
  div(
    id = paste0(ns(fig_id), "_fig_container"),
    class = "fig_container",
    h4(title),
    div(
      class = "fig_caption-container",
      div(
        class = "fig_caption",
        includeHTML(file.path("www", "doc", "caption", caption_file))
      )
    ),
    div(
      class = "fig_panel_container",
      div(
        class = "fig_panel",
        plotly::plotlyOutput(
          paste0(ns(fig_id), "_plot_output"),
          height = "auto"
        )
      ),
    )
  )
}

ui_fig_samples <- function(fig_id, title, caption_file, ns) {
  div(
    id = paste0(ns(fig_id), "_fig_container"),
    class = "fig_container",
    h4(title),
    div(
      class = "fig_caption-container",
      div(
        class = "fig_caption",
        includeHTML(file.path("www", "doc", "caption", caption_file))
      )
    ),
    div(
      class = "fig_panel_container",
      div(
        class = "fig_panel",
        plotly::plotlyOutput(
          paste0(ns(fig_id), "_plot_output"),
          height = "auto"
        )
      ),
    )
  )
}



## Plotly Helpers
facet_strip_format <- function(gp) {
  # get info about facets
  facets <- seq(length(
    stringr::str_which(
      names(gp$x$layout),
      "yaxis"
    )
  ))

  #print(facets)
  n_facets <- length(facets)

  # split x ranges from 0 to 1 into
  # intervals corresponding to number of facets
  # xHi = highest x for shape
  xHi <- seq(0, 1, len = n_facets + 1)
  xHi <- xHi[2:length(xHi)]

  # specify an offset from highest to lowest x for shapes
  xOs <- 0.15

  # annotation manipulations, identified by label name
  # structure: p$x$layout$annotations[[2]]
  ann <- gp$x$layout$annotations
  j <- 1


  # but each annotation between high and low x,
  # a set adjustment to left
  for (i in seq_along(ann)) {
    if (n_facets >= 2) {
      gp$x$layout$annotations[[i]]$y <- ((((xHi[j] - xOs) + xHi[j]) / 2) + 0.11)
      gp$x$layout$annotations[[i]]$x <- 0
      gp$x$layout$annotations[[i]]$xanchor <- "left"
      gp$x$layout$annotations[[i]]$yanchor <- "top"

      j <- j + 1
    } else {
      gp$x$layout$annotations[[i]]$x <- 0
      gp$x$layout$annotations[[i]]$xanchor <- "left"
    }
  }



  # Shape manipulations
  # structure: p$x$layout$shapes[[2]]$
  shp <- gp$x$layout$shapes
  j <- 1
  for (i in seq_along(shp)) {
    if (shp[[i]]$fillcolor == "transparent" & (!is.na(shp[[i]]$fillcolor)) & n_facets >= 2) {
      gp$x$layout$shapes[[i]]$y1 <- xHi[j]
      gp$x$layout$shapes[[i]]$y0 <- (xHi[j] - xOs)
      j <- j + 1
    }
  }

  # x-axis line manipulations, identified by xaxis[i]
  # structure: p$x$layout$xaxis[i]
  # gp[["x"]][["layout"]][["height"]] <- 300 * n_facets
  xax <- facets[facets != 1]
  j <- 1

  for (i in xax) {
    gp$x$layout[[paste0("xaxis", i)]] <- list(
      showline = TRUE,
      linecolor = "rgba(147,149,152,1)",
      ticks = "outside",
      anchor = paste0("y", i),
      textangle = 0,
      tickfont = list(
        size = 20,
        color = "#939888"
      ),
      range = gp$x$layout$xaxis$range,
      ticktext = gp$x$layout$xaxis$ticktext,
      tickvals = gp$x$layout$xaxis$tickvals
    )

    gp$x$layout[[paste0("yaxis", i)]]$tickfont <- list(
      size = 20,
      color = "#939888"
    )
    j <- j + 1
  }

  # domain manipulations
  # set high and low x for each facet domain
  #  lot <- names(gp$x$layout)
  lot <- names(gp$x$layout)
  j <- 1
  for (i in seq_along(lot)) {
    if (!is.na(pmatch("yaxis", lot[i])) & n_facets >= 2) {
      # print(p[['x']][['layout']][[lot[i]]][['domain']][2])
      gp[["x"]][["layout"]][[lot[i]]][["domain"]][2] <- xHi[j]
      gp[["x"]][["layout"]][[lot[i]]][["domain"]][1] <- xHi[j] - xOs
      j <- j + 1
    }
  }

  # gp$x$layout$yaxis$domain[2] <- xHi[j]
  # gp$x$layout$yaxis$domain[1] <- xHi[j] - xOs
  # j<-j+1
  #   gp$x$layout[[paste0("yaxis",i)]]$domain[2] <- xHi[j]
  #  gp$x$layout[[paste0("yaxis",i)]]$domain[1] <- xHi[j] - xOs
  # }
  #  for (i in n_facets) {
  #   gp[["x"]][["layout"]][["annotations"]][[i]][["font"]] <- list(
  #    size = 25,
  #   color = "#5A5A5A"
  #  )
  # }

  return(gp)
}

default_layout <- function(x) {
  x |>
    plotly::layout(
      font = list(family = "Arial"),
      xaxis = list(
        anchor = "y",
        zeroline = TRUE,
        tickfont = list(
          color = "#939888",
          size = 20
        )
      ),
      yaxis = list(
        tickfont = list(
          color = "#939888",
          size = 20
        )
      ),
      legend = list(
        title = list(
          font = list(
            size = 20,
            color = "#5A5A5A"
          )
        ),
        font = list(
          size = 20,
          color = "#939888"
        )
      ),
      margin = list(pad = 5)
    )
}


## Additional helpers

add_thumbnail_button <- function(id, src, title, alt = "Figure thumbnail") {
  # https://stackoverflow.com/questions/44841346/adding-an-image-to-shiny-action-button
  tagList(
    div(
      class = "thumnail_button_container",
      tags$button(
        id = id,
        class = "btn action-button",
        tags$img(
          src = src,
          alt = alt,
          id = "fig-thumbnail"
        ),
        title = title
      ),
      div(
        id = paste0(id, "_thumbnail_selected"),
        class = "thumbnail_selected",
        p("selected")
      )
    ),
  )
}

add_figure_selection <- function(id, title, scr = NULL, info = title) {
  column(
    2,
    div(
      class = "thumbnail",
      add_thumbnail_button(id, scr, info),
      h5(title)
    )
  )
}

show_fig <- function(fig_id) {
  shinyjs::show(paste0(fig_id, "_thumbnail_selected"))
  shinyjs::show(paste0(fig_id, "_fig_container"))
}

hide_fig <- function(fig_id) {
  shinyjs::hide(paste0(fig_id, "_thumbnail_selected"))
  shinyjs::hide(paste0(fig_id, "_fig_container"))
}

toggle_legends <- function(legend_id) {
  shinyjs::toggle(legend_id)
}

toggle_fig <- function(fig_id) {
  shinyjs::toggle(paste0(fig_id, "_thumbnail_selected"))
  shinyjs::toggle(paste0(fig_id, "_fig_container"))
}
