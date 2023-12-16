server <- function(input, output, session) {
  # INPUTS
  r <- reactiveValues()

  mod_select_data_server("slc_data", r)

  mod_dialogue_server("show_dialogue", r)
  observeEvent(input$show_dialogue, r$show_dialogue <- TRUE)
  observeEvent(input$show_help, showNotification("TO DO!", type = "warning"))

  mod_select_figure_server("slc_fig", r)
}
