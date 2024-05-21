# Select data and show them on the map
mod_select_figure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style("#slc_fig-fig_effort_plot_output {height:100vh !important;}
                  #slc_fig-fig_heatmap_plot_output {height:100vh !important;}
                  #slc_fig-fig_samples_plot_output {height:100vh !important;}")
    ),
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
              ns("fig_detect"),
              "Monthly eDNA detection probability",
              "img/thumbnails/tn_thresh.svg"
            ),
            add_figure_selection(
              ns("fig_effort"),
              "Sample size to achieve detection",
              "img/thumbnails/tn_effort.svg"
            ),
            add_figure_selection(
              ns("fig_heatmap"),
              "Species detection heatmap",
              "img/thumbnails/tn_heatmap.svg"
            ),
            add_figure_selection(
              ns("fig_samples"),
              "Field sample size for datasets",
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
            selectInput(ns("threshold"), "Threshold", choices = ls_threshold, selected = 75),
            actionButton(
              ns("re_calc_window"),
              label = "Update computation",
              title = "Compute optimal detection window with updated values",
              class = "primary-button"
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
                h6("Variation among years: "),
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
              ui_figure("fig_effort", "Sample size to achieve detection", "sample_size.html", ns),
              ui_fig_hm("fig_heatmap", "Species detection heatmap", "heatmap.html", ns),
              ui_figure("fig_samples", "Field sample size", "field_sample.html", ns)
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
      list(input$re_calc_window, input$calc_window),
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
                data = r$data_ready |> dplyr::filter(primer == r$primer),
                threshold = input$threshold,
                taxon.level = "species",
                taxon.name = unique(r$data_ready$species),
                scaledprobs = r$scaledprobs
              )
              removeNotification(id = "notif_calc_win")

              if (is.null(win)) {
                # showNotification("No optimal detection window", type = "warning")
                output$opt_sampl <- renderUI("NA")
                output$conf <- renderUI("NA")
                output$var_year <- renderUI("NA")
              } else {
                output$opt_sampl <- renderUI(
                  paste(win$fshDF_month$period[1])
                ) # , collapse = ", "))
                output$conf <- renderUI(paste(win$fshDF_month$confidence[1])) # , collapse = ", "))
                output$var_year <- renderUI("TBD")
              }
              r$fig_ready <- TRUE
            } else {
              showNotification("Data selection is empty", type = "warning")
            }
          }
        }
      }
    )


    # figure must be selected and ready to be drawn
    output$fig_detect_plot_output <- renderPlot({
      draw_fig_detect(r, r$fig_ready && r$fig_slc$fig_detect, input$threshold)
    })

    output$fig_effort_plot_output <- renderPlot({
      draw_fig_effort(r, r$fig_ready && r$fig_slc$fig_effort)
    }
    )

    output$fig_heatmap_plot_output <- renderPlot({
      draw_fig_heatmap(r, r$fig_ready && r$fig_slc$fig_heatmap)
    })

    output$fig_samples_plot_output <- renderPlot({
      draw_fig_samples(r, r$fig_ready && r$fig_slc$fig_samples)
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
          GOTeDNA_version
         ) |>
        summarise(
          `Sample #` = dplyr::n_distinct(materialSampleID),
          `Station #` = length(unique(station)),
          LCicon = unique(LClabel)
        ) |>
        mutate(
          `Data owner contact` = paste0("anais.lacoursiere@dfo-mpo.gc.ca"),
          `Indigenous contribution` = ifelse(
            !is.na(LCicon), c('<img src="img/fn_logo.png" height="25"></img>'),
            NA),
          Publication = "DOI:xx.xxxxx",
          Reference = "Lacoursiere, A. (2019) ....",
          LCicon = NULL
        ) |>
        dplyr::ungroup() |>
        dplyr::relocate(
           GOTeDNA_ID, GOTeDNA_version, Publication,`Data owner contact`,
           `Sample #`, `Station #`, `Indigenous contribution`, Reference
        ) |>
        dplyr::rename("GOTeDNA ID" = "GOTeDNA_ID",
                      "Version" = "GOTeDNA_version") |>
        DT::datatable(escape = FALSE, rownames = FALSE,
                      options = list(
                        columnDefs = list(list(className = 'dt-center', targets = "_all"))
                      ))
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
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  })



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
         bslib::card_image(file = "www/img/fixed-legends/thresh_axis.png",
                     fill = FALSE,
                     width = "80px"),
      bslib::card_body(plotOutput(paste0(ns(fig_id), "_plot_output"))),
      bslib::card_body(
            bslib::card_body(height = "250px"),
            bslib::card_image(file = "www/img/fixed-legends/thresh_legend.png",
                     fill = FALSE,
                     width = "200px")),
         col_widths = c(2, 6, 4)
      )
      )
  ))
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
            plotOutput(paste0(ns(fig_id), "_plot_output"), width = "800px"),
            fillable = TRUE
          ),
          bslib::card_image(file = "www/img/fixed-legends/hm_legend.png",
                     fill = FALSE,
                     width = "200px"),
          col_widths = c(9, 3)
        )
      )
    )
  )
}

ui_figure <- function(fig_id, title, caption_file, ns, legend_file = NULL) {
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
        plotOutput(paste0(ns(fig_id), "_plot_output"))
      ),
     # div(
       # class = "fig_legend",
      #  add_fixed_legend(legend_file)
     #)
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
        dplyr::filter(species == r$species)
    } else {
      if (r$taxon_id_slc != "All") {
        out <- out[
          out[[r$taxon_lvl_slc]] == r$taxon_id_slc,
        ]
      }
    }
  }
  # do we want to subset?
 #  if (r$primer != "not available") {
  #   out <- out |>
   #    dplyr::filter(primer == r$primer)
   #}
 # out
}

#num_projects <- function(r) {
#  proj_ids <- r$data_ready |>
#      dplyr::summarise(n = sum(detect, nondetect, na.rm = TRUE),
#                         .by = GOTeDNA_ID.v
#        ) |>
#      sort(n, decreasing = TRUE) |>
#      select(GOTeDNA_ID.v)#

#}

draw_fig_detect <- function(r, ready, threshold) {
  if (ready) {
    taxon_level <- r$taxon_lvl_slc
    taxon_id <- ifelse(taxon_level == "species", r$species, r$taxon_id_slc)
    data_slc <- r$data_ready
    if (r$primer != "not available") {
      data_slc <- data_slc |>
        dplyr::filter(primer == r$primer)
    }

    p1 <-  smooth_fig(data = data_slc, taxon.level = taxon_level, taxon.name = taxon_id)
#    if (r$primer != "not available") {
      data_slc <- r$scaledprobs$Pscaled_month |>
        dplyr::filter(primer == r$primer)
      # to present the figure of the GOTeDNA_ID and version with the highest sample size
  #    proj_ids <- data_slc |>
  #      dplyr::summarise(n = sum(detect, nondetect, na.rm = TRUE),
  #                       .by = GOTeDNA_ID.v
  #      ) |>
  #      dplyr::arrange(dplyr::desc(n)) |>
  #      dplyr::select(GOTeDNA_ID.v)


#    }
    # p2 is now a list of figures, separated by GOTeDNA_ID and version.
    # goal is to display all figures, but in tabs so user can see all projects
    p2 <- thresh_fig(taxon_level, taxon_id, threshold = threshold, scaledprobs = data_slc)

  #  for (i in names(p2)) {
     # print(
    p1[[1]] / p2[[1]]
    #)
      #}


    } else {
    paste("Plot not available. Click 'Compute & visualize'")
  }
}

draw_fig_effort <- function(r, ready) {
  if (ready) {
    p <- effort_needed_fig(
        r$taxon_lvl_slc,
        ifelse(r$taxon_lvl_slc == "species", r$species, r$taxon_id_slc),
        r$scaledprobs)

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
        data = r$data_ready,
        taxon.select = r$taxon_lvl_slc,
        taxon.name =  ifelse(r$taxon_lvl_slc == "species", r$species, r$taxon_id_slc)
      )

    p

  } else {
    plotNotAvailable()
  }
}

add_fixed_legend <- function(file) {
  if (is.null(file)) {
    tagList()
  } else {
    img(
      # id = "fig_legend_img",
      src = file.path("img", "fixed-legends", file),
      alt = "Legend of the figure"
    )
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
  plotText(stringr::str_wrap("Plot not available. Click 'Compute & visualize'"))
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
