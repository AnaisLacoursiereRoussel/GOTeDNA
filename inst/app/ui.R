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
      div(
        class = "standalone_container",
        div(
          class = "standalone_80",
          mod_glossary_ui("glossary")
        )
      )
    ),
    tabPanel(
      "Disclaimer",
      div(
        class = "standalone_container",
        div(
          class = "standalone_60",
          includeHTML(file.path("www", "doc", "disclaimer.html"))
        )
      )
    ),
    tabPanel(
      "Partners",
      div(
        class = "standalone_container",
        div(
          class = "standalone_60",
          h1("Partners")
        )
      )
    ),
    tabPanel(
      "Indigenous Contributions",
      div(
        class = "standalone_container",
        div(
          class = "standalone_60",
          h1("Indigenous Contributions")
        )
      )
    ),
    tabPanel(
      "Team",
      div(
        class = "standalone_container",
        div(
          class = "standalone_60",
          h1("Team")
        )
      )
    ),
    tabPanel(
      "Contact",
      div(
        class = "standalone_container",
        div(
          class = "standalone_60",
          h1("Contact"),
          fluidRow(
            column(
              4,
              div(
                style = "text-align:center",
                img(
                  src = "img/logo/logo_disclaimer.svg",
                  alt = "Logo GOTeDNA",
                  id = "logo_gotedna_contact"
                )
              )
            ),
            column(
              8,
              includeHTML(file.path("www", "doc", "contact.html"))
            )
          )
        )
      )
    ),
  ),
  div(
    id = "footer",
    fluidRow(
      class = "align-items-center",
      column(
        3,
        img(
          src = "img/logo_partners/DFO_logo_sq.svg",
          alt = "Logo DFO",
          id = "logo_dfo"
        )
      ),
      column(
        3,
        img(
          src = "img/logo_partners/logo_Maine_eDNA_nbg_w.png",
          alt = "Logo Main eDNA",
          id = "logo_mswc"
        )
      ),
      column(
        3,
        img(
          src = "img/logo_partners/logo_obon.svg",
          alt = "Logo OBONt",
          id = "logo_obon"
        )
      ),
      column(
        3,
        img(
          src = "img/logo_partners/logo_undossd.svg",
          alt = "Logo United Nations Decade of Ocean Science for Sustainable Development",
          id = "logo_undossd"
        )
      )
    )
  )
)
