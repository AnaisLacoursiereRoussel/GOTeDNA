# Select data and show them on the map
mod_select_figure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "figure_selection",
      div(
        class = "section_header",
        fluidRow(
          column(8, h1("Figure selection")),
          column(
            4,
            div(
              class = "top_right_button",
              actionButton(ns("hide_figs"), "Hide/Show figures",
                title = "Hide or show fields"
              )
            )
          )
        )
      ),
      div(
        id = ns("figure_selection_main"),
        fluidRow(
          column(
            12,
            div(
              id = "select_all_figures",
              actionButton(
                ns("select_all"),
                "Select all figures",
                title = "Select all figures"
              )
            )
          )
        ),
        div(
          id = "thumbnail_container",
          fluidRow(
            add_figure_selection(
              ns("fig_heatmap"),
              "Species detection heatmap",
              "img/thumbnails/tn_heatmap.png"
            ),
            add_figure_selection(
              ns("fig_effort"),
              "Sample size to achieve detection",
              "img/thumbnails/tn_effort.png"
            ),
            add_figure_selection(
              ns("fig_higher"),
              "Field sample size for datasets",
              "img/thumbnails/tn_higher.png"
            ),
            add_figure_selection(
              ns("fig_detect"),
              "Monthly eDNA detection probability",
              "img/thumbnails/tn_thresh.png"
            )
          )
        ),
        fluidRow(
          column(
            12,
            div(
              id = "confirm_figures_selection",
              actionButton(
                ns("confirm"),
                "Confirm",
                title = "Confirm selection",
                class = "btn-blue"
              )
            )
          )
        ),
      )
    ),
    div(
      id = "observation",
      div(
        class = "section_header",
        h1("Observation")
      ),
      fluidRow(
        column(
          3,
          div(
            id = "fig_left_panel",
            selectInput(ns("threshold"), "Threshold", choices = seq(50, 95, 5), selected = 75),
            actionButton(
              ns("calc_window"),
              label = "Compute & visualize",
              title = "Compute optimal detection window",
              icon = icon("gear"),
              class = "btn-blue"
            ),
            h4("Sampling info"),
            h6("Optimal sampling period: "),
            uiOutput(ns("opt_sampl"), class = "fig_text_output"),
            h6("Confidence: "),
            uiOutput(ns("conf"), class = "fig_text_output"),
            h6("Variation among year: "),
            uiOutput(ns("var_year"), class = "fig_text_output"),
            h6("Variation among primers: "),
            uiOutput(ns("var_primer"), class = "fig_text_output"),
            actionButton(
              ns("export_pdf"),
              "Export to PDF",
              title = "Export figures to PDF",
              class = "btn-blue"
            )
          )
        ),
        column(
          9,
          div(
            id = "fig_main_container",
            ui_figure(1, "Species detection heatmap", "heatmap.html", ns),
            ui_figure(2, "Sample size to achieve detection", "sample_size.html", ns),
            ui_figure(3, "Field sample size for ", "field_sample.html", ns),
            ui_figure(4, "Monthly eDNA detection probability", "detection.html", ns)
          ),
          div(
            id = "reference_data_authorship",
            DT::DTOutput(ns("data_authorship"))
          ),
        ),
        column(3),
        column(
          9,
          div(
            class = "section_footer",
            actionButton(
              ns("export_biblio"),
              "Export references",
              title = "Export references",
              class = "btn-blue"
            )
          )
        )
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
      for (i in c("fig_heatmap", "fig_effort", "fig_higher", "fig_detect")) {
        shinyjs::show(paste0(i, "_thumbnail_selected"))
        r$fig_slc[[i]] <- TRUE
      }
    })

    shinyjs::hide(paste0("fig_heatmap", "_thumbnail_selected"))
    observeEvent(input$fig_heatmap, {
      shinyjs::toggle(paste0("fig_heatmap", "_thumbnail_selected"))
      r$fig_slc$fig_heatmap <- !r$fig_slc$fig_heatmap
    })

    shinyjs::hide(paste0("fig_effort", "_thumbnail_selected"))
    observeEvent(input$fig_effort, {
      shinyjs::toggle(paste0("fig_effort", "_thumbnail_selected"))
      r$fig_slc$fig_effort <- !r$fig_slc$fig_effort
    })

    shinyjs::hide(paste0("fig_higher", "_thumbnail_selected"))
    observeEvent(input$fig_higher, {
      shinyjs::toggle(paste0("fig_higher", "_thumbnail_selected"))
      r$fig_slc$fig_higher <- !r$fig_slc$fig_higher
    })

    shinyjs::hide(paste0("fig_detect", "_thumbnail_selected"))
    observeEvent(input$fig_detect, {
      shinyjs::toggle(paste0("fig_detect", "_thumbnail_selected"))
      r$fig_slc$fig_detect <- !r$fig_slc$fig_detect
    })

    # toggle_selected("fig_heatmap", input$fig_heatmap, "fig_heatmap")
    # shinyjs::hide("fig_heatmap_thumbnail_selected")

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
              output$opt_sampl <- renderUI(win$period)
              output$conf <- renderUI(win$confidence)
              output$var_year <- renderUI(2)
            }
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
      includeHTML(file.path("www", "doc", "caption", file))
    })

    output$data_authorship <- DT::renderDT({
      r$data_filtered |>
        dplyr::ungroup() |>
        dplyr::group_by(
          GOTeDNA_ID, GOTeDNA_version, materialSampleID
        ) |>
        summarise(
          `Total number \nof samples` = n(),
          `Total number \n of stations` = length(unique(station))
        ) |>
        mutate(
          `Data owner contact` = "To be added",
          `Indigenous data labelling` = "To be added",
          Publication = "DOI; To be added",
          Reference = "To be added"
        ) |>
        dplyr::ungroup() |>
        dplyr::select(
          Publication, `Data owner contact`, `Total number \nof samples`,
          `Total number \n of stations`, Reference
        )
    })
  })
}



ui_figure <- function(fig_id, title, caption_file, ns) {
  div(
    id = ns(paste0("fig_container_", fig_id)),
    class = "fig_container",
    h4(title),
    div(
      class = "fig_caption",
      includeHTML(file.path("www", "doc", "caption", caption_file))
    ),
    div(
      class = "fig_panel",
      plotOutput(ns(paste0("fig_plot_area", fig_id)), height = "65vh")
    )
  )
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

plotText <- function(txt, ...) {
  plot(c(-1, 1), c(-1, 1),
    ann = FALSE, bty = "n", type = "n", xaxt = "n",
    yaxt = "n"
  )
  text(0, 0, txt, ...)
}

plotNotAvailableSpeciesLevel <- function() {
  plotText("Plot only available at the species level.")
}

plotNotAvailable <- function() {
  plotText("Plot not available yet.", cex = 1.5)
}

plotNotAvailableForqPCR <- function() {
  plotText("Plot not available for qPCR data.")
}


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

# toggle_selected <- function(class_id, input_button, fig_id) {
#   cls_id <- paste0(class_id, "_thumbnail_selected")
#  # browser()
#   shinyjs::hide(cls_id)
#   observeEvent(input_button, {
#     browser()
#     shinyjs::toggle(cls_id)
#     r$fig_slc[[fig_id]] <- !r$fig_slc[[fig_id]]
#   })
# }