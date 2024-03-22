# Select data and show them on the map
mod_select_figure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        3,
        h2("Observations"),
        selectInput(ns("threshold"), "Threshold", choices = seq(50, 95, 5), selected = 75),
        actionButton(
          ns("calc_window"),
          label = "Compute & visualize",
          title = "Compute optimal detection window",
          icon = icon("gear"),
          style = "border-color: #53b2ad; border-width: 3px; font-size: 1.25rem; border-radius: 0.8rem"
        ),
        h3("Sampling info"),
        h6("Optimal sampling period: "),
        uiOutput(ns("opt_sampl")),
        h6("Confidence: "),
        uiOutput(ns("conf"), fill = TRUE),
        h6("Variation among year: "),
        uiOutput(ns("var_year")),
        # h5("Variation among primers: ")
        # uiOutput(ns("var_primer"))
        # h5("Variation among datasets: ")
        # (uiOutput(ns("var_dat"))
        h3("Figures"),
        taglist_fig_info(
          ns("fig_heatmap"),
          "Species detection heatmap",
          "",
          "img/tn_heatmap.png"
        ),
        taglist_fig_info(
          ns("fig_effort"),
          "Sample size to achieve detection",
          "",
          "img/tn_effort.png"
        ),
        taglist_fig_info(
          ns("fig_higher"),
          "Field sample size for datasets",
          "",
          "img/tn_higher.png"
        ),
        taglist_fig_info(
          ns("fig_detect"),
          "Monthly eDNA detection probability",
          "",
          "img/tn_smooth.png"
        )
      ),
      column(
        9,
        div(
          id = "fig_caption",
          uiOutput(ns("current_cap"), height = "15vh")
        ),
        div(
          id = "fig_panel",
          plotOutput(ns("current_fig"), height = "65vh")
        )
      ),
    )
  )
}

mod_select_figure_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    observeEvent(input$calc_window, {
      if (r$taxon_slc[4] == "All" && r$data_type == "qPCR") {
        showNotification(
          "For qPCR data, one species should be selected.",
          type = "warning"
        )
      } else {
        if (r$taxon_slc[1] == "All") {
          showNotification("Select at least one phylum", type = "warning")
        } else {
          r$data_filtered2 <- prepare_data(r)
          if (nrow(r$data_filtered2)) {
            cli::cli_alert_info("Computing probabilities")
            showNotification(
              "Computing time window",
              type = "message",
              duration = NULL,
              id = "notif_calc_win"
            )

            newprob <- calc_det_prob(r$data_filtered2)
            r$scaledprobs <- scale_newprob(r$data_filtered2, newprob)
            cli::cli_alert_info("Computing optimal detection window")
            win <- calc_window(
              data = r$data_filtered2, threshold = input$threshold,
              species.name = unique(r$data_filtered2$scientificName),
              scaledprobs = r$scaledprobs
            )
            removeNotification(id = "notif_calc_win")

            if (is.null(win)) {
              # showNotification("No optimal detection window", type = "warning")
              output$opt_sampl <- renderUI("NA")
              output$conf <- renderUI("NA")
              output$var_year <- renderUI("NA")
            } else {
              output$opt_sampl <- renderUI(paste(win$fshDF_month$period, collapse = ", "))
              output$conf <- renderUI(paste(win$fshDF_month$confidence, collapse = ", "))
              output$var_year <- renderUI("todo")
            }
            # output$var_primer <- renderUI("TODO")
            # output$var_dat <- renderUI("TODO")

            r$fig_ready <- TRUE

            # freeze taxon level selected
            r$taxon_slc_compute <- r$taxon_slc
            r$taxon_lvl_compute <- do.call(get_taxon_level, as.list(r$taxon_slc))
            # taxon level selected
            r$taxon.name <- r$taxon_slc_compute[r$taxon_lvl_compute]
            r$taxon.level <- taxon_levels[r$taxon_lvl_compute]
          } else {
            showNotification("Data selection is empty", type = "warning")
          }
        }
      }
    })

    observeEvent(input$fig_heatmap, r$current_fig <- "fig1")
    observeEvent(input$fig_effort, r$current_fig <- "fig2")
    observeEvent(input$fig_higher, r$current_fig <- "fig3")
    observeEvent(input$fig_detect, r$current_fig <- "fig4")

    output$current_fig <- renderPlot(
      {
        switch(r$current_fig,
          fig1 = draw_fig1(r),
          fig2 = draw_fig2(r),
          fig3 = draw_fig3(r, taxon_levels),
          fig4 = draw_fig4(r, input$threshold)
        )
      },
      res = 150
    )

    output$current_cap <- renderUI({
      file <- switch(r$current_fig,
        fig1 = "heatmap.html",
        fig2 = "sample_size.html",
        fig3 = "field_sample.html",
        fig4 = "detection.html"
      )
      #browser()
      includeHTML(file.path("www", "doc", "caption", file))
    })
  })
}


prepare_data <- function(r) {
  if (length(r$station_slc)) {
    out <- r$data_filtered |>
      dplyr::filter(station %in% r$station_slc)
  } else {
    out <- r$data_filtered
  }

  if (r$primer != "not available") {
    r$data_filtered2 <- out |>
      dplyr::filter(target_subfragment == r$primer)
  }
  out
}


draw_fig1 <- function(r) {
  if (r$fig_ready) {
    hm_fig(r$taxon.level, r$taxon.name, r$scaledprobs)
  } else {
    plotNotAvailable()
  }
}

draw_fig2 <- function(r) {
  if (r$fig_ready) {
    if (r$taxon.level != "species") {
      cli::cli_alert_danger("cannot render figure 2")
      plotNotAvailableSpeciesLevel()
    } else {
      data_slc <- r$scaledprobs$Pscaled_month |>
        dplyr::filter(species == r$taxon.name)
      if (r$primer != "not available") {
        data_slc <- data_slc |>
          dplyr::filter(primer == r$primer)
      }
      effort_needed_fig(data_slc)
    }
  } else {
    plotNotAvailable()
  }
}

draw_fig3 <- function(r, taxon_levels) {
  if (r$fig_ready) {
    id_lvl <- which(r$taxon_slc != "All") |> which.max()
    higher_tax_fig(
      data = r$data_filtered,
      higher.taxon.select = taxon_levels[min(id_lvl, 2)],
      taxon.name = r$taxon_slc[min(id_lvl + 1, 2)]
    )
  } else {
    plotNotAvailable()
  }
}


draw_fig4 <- function(r, threshold) {
  if (r$fig_ready) {
    data_slc <- r$data_filtered
    if (r$primer != "not available") {
      data_slc <- data_slc |>
        dplyr::filter(target_subfragment == r$primer)
    }
    p1 <- smooth_fig(data = data_slc, species.name = r$taxon.name)
    if (r$primer != "not available") {
      data_slc <- r$scaledprobs$Pscaled_month |>
        dplyr::filter(primer == r$primer)
    }
    p2 <- thresh_fig(
      r$taxon.level, r$taxon.name,
      threshold = threshold, scaledprobs = data_slc
    )
    p1 + p2
  } else {
    plotNotAvailable()
  }
}
