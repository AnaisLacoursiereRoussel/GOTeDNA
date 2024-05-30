server <- function(input, output, session) {
  # INPUTS
  r <- reactiveValues(
    geom = NULL,
    geom_slc = NULL,
    station_slc = NULL,
    project_slc = NULL,
    taxon_lvl_slc = NULL,
    taxon_id_slc = NULL,
    show_map_info = FALSE,
    reload_map = 0,
    fig_ready = FALSE,
    fig_slc = list(
      fig_heatmap = FALSE,
      fig_effort = FALSE,
      fig_samples = FALSE,
      fig_detect = FALSE,
      fig_smooth = FALSE
    ),
    current_fig = "fig1",
    lock_view = FALSE
  )

   observeEvent(input$navbar, {
    if (input$navbar == "partners") {
      browseURL("https://sites.google.com/view/gotedna/partners")
    }

    if (input$navbar == "team") {
      browseURL("https://sites.google.com/view/gotedna/the-team")
    }
  })

  mod_select_data_server("slc_data", r)

  mod_dialog_disclaimers_server("show_dialog", r)
  observeEvent(input$show_dialog, r$show_dialog <- TRUE)
  observeEvent(input$show_help, r$show_help <- TRUE)
  mod_dialog_map_info_server("show_map_info", r)
  mod_glossary_server("glossary")

  mod_primers_server("primer_seq")
  observeEvent(input$show_source, r$show_source <- TRUE)

  observeEvent(input$reset, {
    shinyjs::reset("data_authorship")
  })

  mod_select_figure_server("slc_fig", r)

}

