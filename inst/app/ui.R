ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "fonts.css")
  ),
  navbarPage(
    img(
      src = "img/logo/GOTeDNA_logo_white_got.svg",
      alt = "GOTeDNA_logo",
      id = "logo_gotedna"
    ),
    tabPanel(
      "Home",
      mod_select_data_ui("slc_data"),
      mod_select_figure_ui("slc_fig")
    ),
    tabPanel(
      "Glossary",
      fluidRow(
        column(2),
        column(8, mod_glossary_ui("glossary")),
        column(2)
      )
    ),
    tabPanel(
      "Disclaimer",
      fluidRow(
        column(3),
        column(6, includeHTML(file.path("www", "doc", "disclaimer.html"))),
        column(3)
      )
    ),
    tabPanel("Partners"),
    tabPanel("Team"),
    tabPanel("Contact"),
  ),
  div(
    id = "footer",
    fluidRow(
      column(
        4,
        img(
          src = "img/logo_partners/logo_dfo.svg",
          alt = "Logo DFO",
          id = "logo_dfo"
        )
      ),
      column(
        4,
        img(
          src = "img/logo_partners/logo_mswc.png",
          alt = "Logo Main eDNA",
          id = "logo_mswc"
        )
      ),
      column(
        4,
        img(
          src = "img/logo_partners/logo_undossd.svg",
          alt = "Logo United Nations Decade of Ocean Science for Sustainable Development",
          id = "logo_undossd"
        )
      )
    )
  )
)