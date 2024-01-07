# Select data and show them on the map
mod_select_figure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Details",
        fluidRow(
          column(
            12,
            h2("Observation"),
            actionButton(
              ns("calc_window"),
              label = "Compute and visualize",
              icon = icon("gear")
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
            h3("Figure details"),
            tags$ul(
              tags$li(strong("Figure 1:"), "does that"),
              tags$li(strong("Figure 2:"), "does that"),
              tags$li(strong("Figure 3:"), "does that"),
              tags$li(strong("Figure 4:"), "does that"),
              tags$li(strong("Figure 5:"), "does that")
            )
          )
        ),
      ),
      tabPanel("All", plotOutput(ns("fig_all"), height = "85vh")),
      tabPanel("Figure 1", plotOutput(ns("fig_1"), height = "85vh")),
      tabPanel("Figure 2", plotOutput(ns("fig_2"), height = "85vh")),
      tabPanel("Figure 3", plotOutput(ns("fig_3"), height = "85vh")),
      tabPanel("Figure 4", plotOutput(ns("fig_4"), height = "85vh")),
      tabPanel("Figure 5", plotOutput(ns("fig_5"), height = "85vh"))
    )
  )
}

mod_select_figure_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$calc_window, {
      cli::cli_alert_info("Computing")
      newprob <- calc_det_prob(r$data_filtered)
      scaledprobs <- scale_newprob(r$data_filtered, newprob)
      win <- calc_window(
        data = r$data_filtered, threshold = "90",
        species.name = unique(r$data_filtered$scientificName),
        scaledprobs = scaledprobs
      )
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

      # Creates figures
      r$fig_1 <- hm_fig(taxon.level = "class", taxon.name = "Copepoda", scaledprobs)
      r$fig_2 <- effort_needed_fig(
        species.name = "Acartia hudsonica",
        primer.select = "COI1", scaledprobs
      )
      r$fig_3 <- higher_tax_fig(
        data = D_mb_ex,
        higher.taxon.select = "phylum",
        taxon.name = "Bryozoa",
        view.by.level = "genus",
        primer.select = "COI1"
      )
      r$fig_4 <- sample_size_fig(
        data = D_mb_ex,
        species.name = "Acartia hudsonica"
      )
      p1 <- smooth_fig(
        data = D_mb_ex, species.name = "Acartia longiremis",
        primer.select = "COI1"
      )
      p2 <- thresh_fig(
        taxon.level = "species", taxon.name = "Acartia hudsonica",
        threshold = "90", Pscaled
      )
      r$fig_5 <- p1 + p2
    })

    output$fig_1 <- renderPlot(r$fig_1, res = 144)
    output$fig_2 <- renderPlot(r$fig_2, res = 144)
    output$fig_3 <- renderPlot(r$fig_3, res = 144)
    output$fig_4 <- renderPlot(r$fig_4, res = 144)
    output$fig_5 <- renderPlot(r$fig_5, res = 144)
    output$fig_all <- renderPlot((r$fig_1 | r$fig_2 | r$fig_3) / (r$fig_4 | r$fig_5), res = 72)
  })
}