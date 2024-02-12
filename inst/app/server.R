server <- function(input, output, session) {
  # INPUTS
  r <- reactiveValues(
    geom = NULL,
    geom_slc = NULL,
    fig_ready = FALSE,
    show_map_info = FALSE,
    reload_map = 0,
  )

  observeEvent(r$data_filtered, {
    updateTabsetPanel(
      session = session,
      inputId = "tabset_figs",
      selected = "details"
    )
  })

  mod_select_data_server("slc_data", r)

  mod_dialog_disclaimers_server("show_dialog", r)
  observeEvent(input$show_dialog, r$show_dialog <- TRUE)
  mod_dialog_definitions_server("show_help", r)
  observeEvent(input$show_help, r$show_help <- TRUE)

  mod_dialog_map_info_server("show_map_info", r)

  mod_dialog_source_server("show_source", r)
  observeEvent(input$show_source, r$show_source <- TRUE)


  mod_select_figure_server("slc_fig", r)
}
