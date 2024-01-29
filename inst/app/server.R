server <- function(input, output, session) {
  # INPUTS
  r <- reactiveValues(
    geom = NULL,
    geom_slc = NULL,
    reload_map = 0
  )

  mod_select_data_server("slc_data", r)

  mod_dialog_disclaimers_server("show_dialog", r)
  observeEvent(input$show_dialog, r$show_dialog <- TRUE)
  mod_dialog_definitions_server("show_help", r)
  observeEvent(input$show_help, r$show_help <- TRUE)

  mod_dialog_source_server("show_source", r)
  observeEvent(input$show_source, r$show_source <- TRUE)

  mod_select_figure_server("slc_fig", r)
}
