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
          class = "d-flex justify-content-center",
          id = "select_all_figures",
          actionButton(
            ns("select_all"),
            "Select all figures",
            title = "Select all figures"
          )
        ),
        div(
          id = "thumbnail_container",
          fluidRow(
            class = "justify-content-center",
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
        div(
          class = "d-flex justify-content-center",
          id = "confirm_figures_selection",
          actionButton(
            ns("confirm"),
            "Confirm",
            title = "Confirm selection",
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
            selectInput(ns("threshold"), "Threshold", choices = seq(50, 95, 5), selected = 75),
            actionButton(
              ns("calc_window"),
              label = "Compute & visualize",
              title = "Compute optimal detection window",
              icon = icon("gear"),
              class = "primary-button"
            ),
            div(
              id = "fig_sampling_info",
              h4("Sampling info"),
              div(
                class = "sampling_info-item",
                h6("Optimal sampling period: "),
                uiOutput(ns("opt_sampl"), class = "fig_text_output")
              ),
              div(
                class = "sampling_info-item",
                h6("Confidence: "),
                uiOutput(ns("conf"), class = "fig_text_output")
              ),
              div(
                class = "sampling_info-item",
                h6("Variation among year: "),
                uiOutput(ns("var_year"), class = "fig_text_output"),
              ),
              div(
                class = "sampling_info-item",
                h6("Variation among primers: "),
                uiOutput(ns("var_primer"), class = "fig_text_output")
              )
            ),
            actionButton(
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
            id = "fig_main_container",
            ui_figure("fig_heatmap", "Species detection heatmap", "heatmap.html", ns),
            ui_figure("fig_effort", "Sample size to achieve detection", "sample_size.html", ns),
            ui_figure("fig_higher", "Field sample size for ", "field_sample.html", ns),
            ui_figure("fig_detect", "Monthly eDNA detection probability", "detection.html", ns)
          ),
          div(
            id = "reference_data_authorship",
            div(
              class = "table_title-container",
              h2("Lorem Title")
            ),
            DT::DTOutput(ns("data_authorship"))
          ),
        )
      ),
      # div(
      #   class = "section_footer",
      #   actionButton(
      #     ns("export_biblio"),
      #     "Export references",
      #     title = "Export references",
      #     class = "primary-button"
      #   )
      # )
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
        show_fig(i)
        r$fig_slc[[i]] <- TRUE
      }
    })

    hide_fig("fig_heatmap")
    observeEvent(input$fig_heatmap, {
      toggle_fig("fig_heatmap")
      r$fig_slc$fig_heatmap <- !r$fig_slc$fig_heatmap
    })

    hide_fig("fig_effort")
    observeEvent(input$fig_effort, {
      toggle_fig("fig_effort")
      r$fig_slc$fig_effort <- !r$fig_slc$fig_effort
    })

    hide_fig("fig_higher")
    observeEvent(input$fig_higher, {
      toggle_fig("fig_higher")
      r$fig_slc$fig_higher <- !r$fig_slc$fig_higher
    })

    hide_fig("fig_detect")
    observeEvent(input$fig_detect, {
      toggle_fig("fig_detect")
      r$fig_slc$fig_detect <- !r$fig_slc$fig_detect
    })


    observeEvent(input$calc_window, {
      #
      if (r$species == "All" && r$data_type == "qPCR") {
        showNotification(
          "For qPCR data, one species should be selected.",
          type = "warning"
        )
      } else {
        if (r$species == "All" && r$taxon_id_slc == "All") {
          showNotification(
            "The current selection is too broad, restrict your selection to one
            specific taxonomic level or to one species.",
            type = "warning",
            duration = 10
          )
        } else {
          r$data_ready <- prepare_data(r)
          if (nrow(r$data_ready)) {
            showNotification(
              paste0(
                "Computing time window",
                ifelse(
                  nrow(r$data_ready) > 1e4,
                  paste0("(", nrow(r$data_ready), " samples, this make takes some time)"),
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
              data = r$data_ready, threshold = input$threshold,
              species.name = unique(r$data_ready$scientificName),
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
            r$fig_ready <- TRUE
          } else {
            showNotification("Data selection is empty", type = "warning")
          }
        }
      }
    })

    output$fig_heatmap_plot_output <- renderPlot({
      # figure must be selected and ready to be drawn
      draw_fig_heatmap(r, r$fig_ready && r$fig_slc$fig_heatmap)
    })

    output$fig_effort_plot_output <- renderPlot({
      draw_fig_effort(r, r$fig_ready && r$fig_slc$fig_effort)
    })

    output$fig_higher_plot_output <- renderPlot({
      draw_fig_higher(r, r$fig_ready && r$fig_slc$fig_higher)
    })

    output$fig_detect_plot_output <- renderPlot({
      draw_fig_detect(r, r$fig_ready && r$fig_slc$fig_detect, input$threshold)
    })


    output$data_authorship <- DT::renderDT({
      r$cur_data_sta_slc |>
        dplyr::ungroup() |>
        dplyr::group_by(
          GOTeDNA_ID,
          GOTeDNA_version,
          target_subfragment,
          ecodistrict
        ) |>
        summarise(
          `Samples #` = n(),
          `Stations #` = length(unique(station))
        ) |>
        mutate(
          `Data owner contact` = "To be added",
          `Indigenous data labelling` = "To be added",
          Publication = "DOI to be added",
          Reference = "To be added"
        ) |>
        dplyr::ungroup() |>
        dplyr::select(
          GOTeDNA_ID, GOTeDNA_version, Publication,
          ecodistrict,
          `Data owner contact`, `Samples #`,
          `Stations #`
        )
    })
  })
}



ui_figure <- function(fig_id, title, caption_file, ns) {
  div(
    id = paste0(ns(fig_id), "_fig_container"),
    class = "fig_container",
    h4(title),
    div(
      class = "fig_caption",
      includeHTML(file.path("www", "doc", "caption", caption_file))
    ),
    div(
      class = "fig_panel",
      plotOutput(paste0(ns(fig_id), "_plot_output"), height = "65vh")
    )
  )
}


prepare_data <- function(r) {
  out <- r$cur_data
  if (length(r$station_slc)) {
    out <- out |>
      dplyr::filter(station %in% r$station_slc)
  }
  if (!is.null(r$taxon_lvl_slc)) {
    if (r$taxon_lvl_slc == "species") {
      out <- out |>
        dplyr::filter(scientificName == r$species)
    } else {
      if (r$taxon_id_slc != "All") {
        out <- out[
          out[[r$taxon_lvl_slc]] == r$taxon_id_slc,
        ]
      }
    }
  }
  # do we want to subset?
  # if (r$primer != "not available") {
  #   out <- out |>
  #     dplyr::filter(target_subfragment == r$primer)
  # }
  out
}


draw_fig_heatmap <- function(r, ready) {
  if (ready) {
    hm_fig(
      r$taxon_lvl_slc,
      ifelse(r$taxon_lvl_slc == "species", r$species, r$taxon_id_slc),
      r$scaledprobs
    )
  } else {
    plotNotAvailable()
  }
}

draw_fig_effort <- function(r, ready) {
  if (ready) {
    if (r$taxon_lvl_slc != "species") {
      cli::cli_alert_danger("cannot render figure 2")
      plotNotAvailableSpeciesLevel()
    } else {
      data_slc <- r$scaledprobs$Pscaled_month |>
        dplyr::filter(species == r$species)
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

draw_fig_higher <- function(r, ready) {
  if (ready) {
    if (r$taxon_lvl_slc == "species") {
      plotNotAvailableTaxoLevel()
    } else {
      higher_tax_fig(
        data = r$data_ready,
        higher.taxon.select = r$taxon_lvl_slc,
        taxon.name = r$taxon_id_slc
      )
    }
  } else {
    plotNotAvailable()
  }
}

draw_fig_detect <- function(r, ready, threshold) {
  if (ready) {
    taxon_level <- r$taxon_lvl_slc
    taxon_id <- ifelse(taxon_level == "species", r$species, r$taxon_id_slc)
    data_slc <- r$data_ready
    if (r$primer != "not available") {
      data_slc <- data_slc |>
        dplyr::filter(target_subfragment == r$primer)
    }
    p1 <- smooth_fig(data = data_slc, species.name = taxon_id)
    if (r$primer != "not available") {
      data_slc <- r$scaledprobs$Pscaled_month |>
        dplyr::filter(primer == r$primer)
    }
    p2 <- thresh_fig(taxon_level, taxon_id, threshold = threshold, scaledprobs = data_slc)
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
  text(0, 0, txt, cex = 2, ...)
}

plotNotAvailableTaxoLevel <- function() {
  plotText("Plot not available at the species level.")
}

plotNotAvailableSpeciesLevel <- function() {
  plotText("Plot only available at the species level.")
}

plotNotAvailable <- function() {
  plotText("Plot not available. Click on 'Compute & visualize'")
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

show_fig <- function(fig_id) {
  shinyjs::show(paste0(fig_id, "_thumbnail_selected"))
  shinyjs::show(paste0(fig_id, "_fig_container"))
}
hide_fig <- function(fig_id) {
  shinyjs::hide(paste0(fig_id, "_thumbnail_selected"))
  shinyjs::hide(paste0(fig_id, "_fig_container"))
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
