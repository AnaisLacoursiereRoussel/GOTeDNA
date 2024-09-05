# Select data and show them on the map
mod_select_figure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(),
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
            selectInput(ns("proj_id"),
                        "GOTeDNA Project",
                        choices = "not available"),
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
                h6("Consistency: "),
                uiOutput(ns("var_year"), class = "fig_text_output"),
              )
            ),
            actionButton(
              ns("export_pdf"),
              "Export to PDF",
              title = "Export figures to PDF",
              # class = "primary-button"
            )
          )
        ),
        column(
          9,
          class = "show-panels",
          div(
            class = "fig_main_container",
            # div(
            #    class = "fig_main_container-header",
            #   h2("Figures")
            #  ),
            div(
              class = "fig_main_container-fig",
              ui_fig_detect("fig_detect", "Monthly eDNA detection probability", "detection.html", ns),
              ui_fig_effort("fig_effort", "Guidance on sampling effort", "sample_size.html", ns),
              ui_fig_hm("fig_heatmap", "Species detection heatmap", "heatmap.html", ns),
              ui_fig_samples("fig_samples", "Data variation", "field_sample.html", ns)
            )
          )
        ),
        div(
          id = "reference_data_authorship",
          div(
            class = "table_title-container",
            h2("Reference data authorship")
          ),
          DT::DTOutput(ns("data_authorship"))
        ),
      )
    )
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

    observeEvent(
      ignoreInit = TRUE,
      list(input$calc_window, input$threshold),
      {
        #
        if (r$species == "All" && r$data_type == "qPCR") {
          showNotification(
            "For qPCR data, one species should be selected.",
            type = "warning"
          )
        } else {
          if (r$species == "All" && r$taxon_id_slc == "All") {
            showNotification(
              "The current selection is too broad, restrict your selection to
              one specific taxonomic level or to one species.",
              type = "warning",
              duration = 10
            )
          } else {
            r$data_ready <- prepare_data(r)
            if (nrow(r$data_ready)) {
              showNotification(
                paste0(
                  "Computing time window with threshold set to ",
                  input$threshold, "%",
                  ifelse(
                    nrow(r$data_ready) > 1e4,
                    paste0(" (", nrow(r$data_ready), " Observations, this make takes some time)"),
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

              win <- calc_window(
                data = r$data_ready,
                threshold = input$threshold,
                taxon.level = r$taxon_lvl_slc,
                taxon.name = ifelse(r$taxon_lvl_slc == "species",
                                     r$species,
                                     r$taxon_id_slc),
                scaledprobs = r$scaledprobs
              )

              removeNotification(id = "notif_calc_win")

              if (is.null(win)) {
                # showNotification("No optimal detection window", type = "warning")
                output$opt_sampl <- renderUI("No optimal window")
                output$conf <- renderUI("NA")
                output$var_year <- renderUI("NA")
              } else {
                output$opt_sampl <- renderUI(
                  paste(win$period)
                ) # , collapse = ", "))
                output$conf <- renderUI(paste(win$confidence)) # , collapse = ", "))
                output$var_year <- renderUI("Medium")
              }

              r$fig_ready <- TRUE

              # create project vector
              v_proj <- r$scaledprobs$GOTeDNA_ID |> unique()
              l_proj <- seq(v_proj) |> as.list()
              names(l_proj) <- paste0("GOTeDNA ID: ", v_proj)
              updateSelectInput(session, "proj_id", choices = l_proj)
            } else {
              showNotification("Data selection is empty", type = "warning")
            }
          }
        }
      }
    )

    output$plots <- renderUI({
      req(input$sel)
      plot_output_list <- lapply(input$sel, function(par) {
        plotname <- paste0("fig_effort_plot_output", par)
        plotly::plotlyOutput(plotname, inline = TRUE, height = "auto")
      })

      do.call(tagList, plot_output_list)
    })
    # figure must be selected and ready to be drawn

    output$fig_smooth_plot_output <- renderPlot({
      draw_fig_smooth(r, r$fig_ready && r$fig_slc$fig_detect,
        id = input$proj_id |> as.numeric()
      )
    })

    output$fig_detect_plot_output <- renderPlot({
      draw_fig_detect(r, r$fig_ready && r$fig_slc$fig_detect, input$threshold,
        id = input$proj_id |> as.numeric()
      )
    })

    output$fig_effort_plot_output <- plotly::renderPlotly({
      plt_ready <- r$fig_ready && r$fig_slc$fig_effort
      ggp <- draw_fig_effort(r, plt_ready)
      if (plt_ready) {
        # multiply height by number of species
        nys <- r$data_ready$species |>
          unique() |>
          length()
        plt <- plotly::ggplotly(
          ggp,
          height = 320 * nys
          ) |>
          default_layout() |>
          facet_strip_format()|>
          layout(xaxis = list(title = list(text = "Number of samples",
                                           font = list(
                                             size = 30,
                                             color = "#5A5A5A"
                                           ),
                                           x=0)),
                 yaxis = list(title = list(text = "Detection probability",
                                           font = list(
                                             size = 30,
                                             color = "#5A5A5A"
                                           ),
                                           y=0)))
      }

    })



    output$fig_heatmap_plot_output <- plotly::renderPlotly({
      plt_ready <- r$fig_ready && r$fig_slc$fig_heatmap
      ggp <- draw_fig_heatmap(r, plt_ready)
      if (plt_ready) {
        # multiply height by number of species
        nys <- r$data_ready$species |>
          unique() |>
          length()
        plt <- plotly::ggplotly(
        ggp,
        height = 275 * nys,
        tooltip = c("x", "text", "fill")
      ) |>
        default_layout() |>
          facet_strip_format()
      }
    })

    output$fig_samples_plot_output <- plotly::renderPlotly({
      plt_ready <- r$fig_ready && r$fig_slc$fig_samples
      ggp <- draw_fig_samples(r, plt_ready)
      if (plt_ready) {
        # multiply height per years #
        nys <- r$data_ready$year |>
          unique() |>
          length()
        plt <- plotly::ggplotly(
          ggp,
          height = 320 * nys
        ) |>
          default_layout() |>
          facet_strip_format() |>
          layout(xaxis = list(title = list(text = "Month",
                                           font = list(
                                             size = 30,
                                             color = "#5A5A5A"
                                           ),
                                           x = 0)),
                 yaxis = list(title = list(text = "Detection rate",
                                           font = list(
                                             size = 30,
                                             color = "#5A5A5A"
                                           ),
                                           y = 0))) #change the style but
        #better than it was!!!!!!
      }

    })

    output$data_authorship <- DT::renderDT({
      if (!is.null(r$data_ready)) {
        tmp <- r$data_ready
      } else {
        tmp <- r$cur_data_sta_slc
      }
      tmp |>
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
          `GOTeDNA ID`, Subproject, #Contact,
          `Sample #`, `Station #`, `Indigenous contribution`, Publication
        ) |>

        DT::datatable(
          escape = FALSE, rownames = FALSE,
          options = list(
            columnDefs = list(list(className = "dt-center", targets = "_all"))
          )
        )
    })

    output$export_pdf <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(figs = input$fig_main_container)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
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


draw_fig_smooth <- function(r, ready, id) {
  if (ready) {
    p <- try(
      smooth_fig(
        r$data_ready,
        r$taxon_lvl_slc,
        ifelse(r$taxon_lvl_slc == "species", r$species, r$taxon_id_slc)
      )
    )
 #   if (inherits(p, "try-error")) plotNotAvailableYear()  << not sure what this is, but the figure wasn't showing up
    p[[id]]
  } else {
    plotNotAvailable()
  }
}

draw_fig_detect <- function(r, ready, threshold, id) {
  if (ready) {
    p <- thresh_fig(
      r$taxon_lvl_slc,
      ifelse(r$taxon_lvl_slc == "species", r$species, r$taxon_id_slc),
      threshold,
      r$scaledprobs
    )
    p[[id]]
  } else {
    plotNotAvailable()
  }
}

draw_fig_effort <- function(r, ready) {
  if (ready) {
    p <- effort_needed_fig(
      r$taxon_lvl_slc,
      ifelse(r$taxon_lvl_slc == "species", r$species, r$taxon_id_slc),
      r$scaledprobs
    )

    p
  } else {
    plotNotAvailable()
  }
}

draw_fig_heatmap <- function(r, ready) {
  if (ready) {
    p <- hm_fig(
      r$taxon_lvl_slc,
      ifelse(r$taxon_lvl_slc == "species", r$species, r$taxon_id_slc),
      r$scaledprobs
    )
    p
  } else {
    plotNotAvailable()
  }
}

draw_fig_samples <- function(r, ready) {
  if (ready) {
    p <- field_sample_fig(
      r$data_ready,
      r$taxon_lvl_slc,
      ifelse(r$taxon_lvl_slc == "species", r$species, r$taxon_id_slc)
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
          #  selectInput(ns("proj_id"), "Project", choices = "not available")
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
        # uiOutput("plots")
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
    )
    )

  print(facets)
  n_facets <- length(facets)

  # split x ranges from 0 to 1 into
  # intervals corresponding to number of facets
  # xHi = highest x for shape
  xHi <- seq(0, 1, len = n_facets+1)
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
      if (n_facets >= 2){
        gp$x$layout$annotations[[i]]$y <- ((((xHi[j]-xOs)+xHi[j])/2)+0.11)
        gp$x$layout$annotations[[i]]$x <- 0
        gp$x$layout$annotations[[i]]$xanchor <- 'left'
        gp$x$layout$annotations[[i]]$yanchor <- 'top'

        j<-j+1
      } else {
        gp$x$layout$annotations[[i]]$x <- 0
        gp$x$layout$annotations[[i]]$xanchor <- 'left'
        }
}



  # Shape manipulations
  # structure: p$x$layout$shapes[[2]]$
  shp <- gp$x$layout$shapes
  j <- 1
  for (i in seq_along(shp)){
    if (shp[[i]]$fillcolor=="transparent" & (!is.na(shp[[i]]$fillcolor)) & n_facets >= 2){
      gp$x$layout$shapes[[i]]$y1 <- xHi[j]
      gp$x$layout$shapes[[i]]$y0 <- (xHi[j] - xOs)
      j<-j+1
    }
  }

  # x-axis line manipulations, identified by xaxis[i]
  # structure: p$x$layout$xaxis[i]
    #gp[["x"]][["layout"]][["height"]] <- 300 * n_facets
  xax <- facets[facets != 1]
  j <- 1

  for (i in xax){
    gp$x$layout[[paste0("xaxis", i)]] <- list(
      showline = TRUE,
      linecolor = "rgba(147,149,152,1)",
      ticks = "outside",
      anchor = paste0("y",i),
      textangle = 0,
      tickfont = list(
        size = 20,
        color = "#939888"
      ),
      range = gp$x$layout$xaxis$range,
      ticktext = gp$x$layout$xaxis$ticktext,
      tickvals = gp$x$layout$xaxis$tickvals)

    gp$x$layout[[paste0("yaxis", i)]]$tickfont <- list(
      size = 20,
      color = "#939888"
    )
    j<-j+1
  }

    # domain manipulations
  # set high and low x for each facet domain
  #  lot <- names(gp$x$layout)
  lot <- names(gp$x$layout)
  j <- 1
  for (i in seq_along(lot)){
    if (!is.na(pmatch('yaxis', lot[i])) & n_facets >= 2){
      #print(p[['x']][['layout']][[lot[i]]][['domain']][2])
      gp[['x']][['layout']][[lot[i]]][['domain']][2] <- xHi[j]
      gp[['x']][['layout']][[lot[i]]][['domain']][1] <- xHi[j] - xOs
      j<-j+1
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
  #}

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

# toggle_selected <- function(class_id, input_button, fig_id) {
#   cls_id <- paste0(class_id, "_thumbnail_selected")
#   shinyjs::hide(cls_id)
#   observeEvent(input_button, {
#     shinyjs::toggle(cls_id)
#     r$fig_slc[[fig_id]] <- !r$fig_slc[[fig_id]]
#   })
# }
