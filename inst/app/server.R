server <- function(input, output, session) {
  # INPUTS
  r <- reactiveValues()

  mod_select_data_server("slc_data", r)

  mod_dialog_disclaimers_server("show_dialog", r)
  observeEvent(input$show_dialog, r$show_dialog <- TRUE)
  mod_dialog_definitions_server("show_help", r)
  observeEvent(input$show_help, r$show_help <- TRUE)

  mod_select_figure_server("slc_fig", r)
}
