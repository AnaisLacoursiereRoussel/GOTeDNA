ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css")
  ),
  fluidRow(
    column(
      5,
      fluidRow(
        column(
          9,
          img(
            src = "img/GOTeDNA_logo_new.png",
            alt = "GOTeDNA_logo",
            id = "logo_gotedna"
          )
        ),
        column(
          3,
          div(
            id = "gotedna_info",
            actionButton("show_dialog", "", icon("info-circle", class = "fa-2xl"), title = "disclaimers"),
            actionButton("show_help", "", icon("question-circle", class = "fa-2xl"), title = "glossary"),
          )
        ),
      ),
      mod_select_data_ui("slc_data")
    ),
    column(
      7,
      div(
        id = "observation_request",
        mod_select_figure_ui("slc_fig")
      )
    )
  )
)
