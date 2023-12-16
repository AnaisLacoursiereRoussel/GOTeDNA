ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css"),
    # https://markusdumke.github.io/articles/2017/11/customize-leaflet-map-in-r-with-html-css-and-javascript/
    tags$style(HTML("
      .leaflet-left .leaflet-control{
        visibility: hidden;
      }
    "))
  ),
  fluidRow(
    column(
      4,
      img(
        src = "img/GOTeDNA_logo_white.png",
        alt = "GOTeDNA_logo",
        id = "logo_gotedna"
      ),
      mod_select_data_ui("slc_data")
    ),
    column(
      8,
      div(
        id = "observation_request",
        fluidRow(
          column(10, h2("Observation")),
          column(
            2,
            actionButton("show_dialogue", "", icon("info-circle", class = "fa-2xl")),
            actionButton("show_help", "", icon("question-circle", class = "fa-2xl")),
          ),
        ),
        mod_select_figure_ui("slc_fig")
      )
    )
  )
)
