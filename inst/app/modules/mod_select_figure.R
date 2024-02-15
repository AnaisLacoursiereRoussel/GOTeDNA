# Select data and show them on the map
mod_select_figure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = "tabset_figs",
      tabPanel(
        title = "Details",
        value = "details",
        fluidRow(
          column(
            12,
            h2("Observations"),
            h3("Compute optimal detection window"),
            fluidRow(
              column(
                4,
                selectInput(ns("primer"), "Primer", choices = "unkown")
              ),
              column(
                4,
                selectInput(ns("threshold"), "Threshold", choices = seq(50, 95, 5), selected = 90)
              )
            ),
            actionButton(
              ns("calc_window"),
              label = "Compute & visualize",
              title = "Trigger computation",
              icon = icon("gear"),
              style = "border-color: #53b2ad; border-width: 3px; font-size: 1.4rem; border-radius: 0.8rem"
            ),
            h3("Data selected details"),
            tags$table(
              tags$tbody(
                tags$tr(
                  tags$td("Optimal sampling period: ", style = "text-align:right;"),
                  tags$td(uiOutput(ns("opt_sampl")))
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("Confidence: ", style = "text-align:right;"),
                  tags$td(uiOutput(ns("conf")))
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("Variation among year: ", style = "text-align:right;"),
                  tags$td(uiOutput(ns("var_year")))
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("Variation among primers: ", style = "text-align:right;"),
                  tags$td(uiOutput(ns("var_primer")))
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("Variation among datasets: ", style = "text-align:right;"),
                  tags$td(uiOutput(ns("var_dat")))
                )
              ),
            ),
            h3(icon("info-circle"), "About the figures"),
            fluidRow(
              taglist_fig_info(
                "Heatmap",
                "Heatmap of scaled monthly detection probabilities for taxon."
              ),
              taglist_fig_info(
                "Sampling effort 1",
                "Sampling effort needed to obtain different detection thresholds."
              ),
              taglist_fig_info(
                "Sampling effort 2",
                "Sampling effort of taxa within specified taxonomic group per year."
              ),
              taglist_fig_info(
                "Sampling effort 3",
                "Sampling effort by species, year, and region."
              ),
              taglist_fig_info(
                "Detection probability 1",
                "Display species monthly detection probability.."
              ),
              taglist_fig_info(
                "Detection probability 2",
                "Display monthly detection probabilities for selected taxon, and detection probability threshold"
              )
            )
          )
        ),
      ),
      # tabPanel("All", plotOutput(ns("fig_all"), height = "85vh")),
      tabPanel("Heatmap", vaule = "hm", plotOutput(ns("fig_1"), height = "85vh")),
      tabPanel("Sampling effort 1", plotOutput(ns("fig_2"), height = "85vh")),
      tabPanel("Sampling effort 2", plotOutput(ns("fig_3"), height = "85vh")),
      tabPanel("Sampling effort 3", plotOutput(ns("fig_4"), height = "85vh")),
      tabPanel("Detection probability 1", plotOutput(ns("fig_5"), height = "85vh")),
      tabPanel("Detection probability 2", plotOutput(ns("fig_6"), height = "85vh"))
    )
  )
}

mod_select_figure_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      if (r$data_type == "qPCR") {
        updateSelectInput(
          session,
          "primer",
          choices = "not available"
        )
      } else {
        tg <- table(r$data_filtered$target_subfragment) |>
          sort() |>
          rev()
        updateSelectInput(
          session,
          "primer",
          choices = names(tg),
          selected = names(tg)[1]
        )
      }
    })

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
          # compute probabilities
          cli::cli_alert_info("Computing probablities")
          showNotification(
            "Computing time window",
            type = "message",
            duration = NULL,
            id = "notif_calc_win"
          )
          newprob <- calc_det_prob(r$data_filtered)
          r$scaledprobs <- scale_newprob(r$data_filtered, newprob)
          cli::cli_alert_info("Computing optimal detection window")
          win <- calc_window(
            data = r$data_filtered, threshold = input$threshold,
            species.name = unique(r$data_filtered$scientificName),
            scaledprobs = r$scaledprobs
          )
          removeNotification(id = "notif_calc_win")

          r$fig_ready <- TRUE

          if (is.null(win)) {
            showNotification("No optimal detection window", type = "warning")
            output$opt_sampl <- renderUI("UNKNOWN")
            output$conf <- renderUI("UNKNOWN")
            output$var_year <- renderUI("UNKNOWN")
          } else {
            output$opt_sampl <- renderUI(win$period)
            output$conf <- renderUI(win$confidence)
            output$var_year <- renderUI(2)
          }
          output$var_primer <- renderUI("TODO")
          output$var_dat <- renderUI("TODO")

          # freeze taxon level selected
          r$taxon_slc_compute <- r$taxon_slc
          r$taxon_lvl_compute <- do.call(get_taxon_level, as.list(r$taxon_slc))
          # taxon level selected
          r$taxon.name <- r$taxon_slc_compute[r$taxon_lvl_compute]
          r$taxon.level <- taxon_levels[r$taxon_lvl_compute]
        }
      }
    })

    output$fig_1 <- renderPlot(
      {
        if (r$fig_ready) {
          hm_fig(r$taxon.level, r$taxon.name, r$scaledprobs)
        } else {
          plotNotAvailable()
        }
      },
      res = 144
    )

    output$fig_2 <- renderPlot(
      {
        if (r$fig_ready) {
          if (r$taxon.level != "species") {
            cli::cli_alert_danger("cannot render figure 2")
            plotNotAvailableSpeciesLevel()
          } else {
            if (r$data_type == "qPCR") {
              plotNotAvailableForqPCR()
            } else {
              data_slc <- r$scaledprobs$Pscaled_month |>
                dplyr::filter(species == r$taxon.name)
              if (input$primer != "not available") {
                data_slc <- data_slc |>
                  dplyr::filter(primer == input$primer)
              }
              effort_needed_fig(data_slc)
            }
          }
        } else {
          plotNotAvailable()
        }
      },
      res = 144
    )

    output$fig_3 <- renderPlot(
      {
        if (r$fig_ready) {
          if (r$data_type == "qPCR") {
            plotNotAvailableForqPCR()
          } else {
            id_lvl <- which(r$taxon_slc != "All") |> which.max()
            higher_tax_fig(
              data = r$data_filtered,
              higher.taxon.select = taxon_levels[min(id_lvl, 2)],
              taxon.name = r$taxon_slc[min(id_lvl + 1, 2)],
              view.by.level = taxon_levels[min(id_lvl + 1, 3)]
            )
          }
        } else {
          plotNotAvailable()
        }
      },
      res = 144
    )

    output$fig_4 <- renderPlot(
      {
        if (r$fig_ready) {
          # not filtered for primer
          sample_size_fig(data = r$data_filtered, species.name = r$taxon.name)
        } else {
          plotNotAvailable()
        }
      },
      res = 144
    )

    output$fig_5 <- renderPlot(
      {
        if (r$fig_ready) {
          if (r$data_type == "qPCR") {
            plotNotAvailableForqPCR()
          } else {
            data_slc <- r$data_filtered
            if (input$primer != "not available") {
              data_slc <- data_slc |>
                dplyr::filter(target_subfragment == input$primer)
            }
            smooth_fig(data = data_slc, species.name = r$taxon.name)
          }
        } else {
          plotNotAvailable()
        }
      },
      res = 144
    )

    output$fig_6 <- renderPlot(
      {
        if (r$fig_ready) {
          data_slc <- r$scaledprobs
          # maybe better to change the input in thresh_fig.
          if (input$primer != "not available") {
            data_slc$Pscaled_month <- data_slc$Pscaled_month |>
              dplyr::filter(primer == input$primer)
          }
          thresh_fig(
            r$taxon.level, r$taxon.name,
            threshold = input$threshold, data_slc
          )
        } else {
          plotNotAvailable()
        }
      },
      res = 144
    )

    # output$fig_all <- renderPlot(
    #   {
    #     if (r$fig_ready) {
    #     } else {
    #       plotNotAvailable()
    #     }
    #   },
    #   res = 144
    # )
  })
}