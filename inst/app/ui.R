ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "fonts.css"),
    tags$style(type = "text/css", "body {padding-top: 70px;}")
  ),
  navbarPage(id = "navbar",
             position = "fixed-top",
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
      "Disclaimers",
      div(
        class = "standalone_container",
        div(
          class = "standalone_60",
          h1("Disclaimers"),
          includeHTML(file.path("www", "doc", "disclaimer.html"))
        )
      )
    ),
    tabPanel(
      "Interpretation Guide",
      div(
        class = "standalone_container",
        div(
          class = "standalone_60",
          h1("Interpretation Guide"),
          includeHTML(file.path("www", "doc", "interp_guide.html"))
        )
      )
    ),
    tabPanel(
      title = "Primers",
      value = "primer_info",
      div(
        class = "standalone_container",
        div(
          class = "standalone_60",
          h1("Primers"),
          includeHTML(file.path("www", "doc", "primers.html"))
        )
      )
    ),
    tabPanel(
        title = a("Partners",
        href = "https://sites.google.com/view/gotedna/partners",
        target = "_blank")#,
      #style = "title {text-: center;}",
      #div(
      #  class = "standalone_container",
      #  )#,
        #div(
          #class = "standalone_60",
          #h1(a("Partners")
      #  )
    #  )
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
          h1("Team"),
          wellPanel(
            helpText(a("Meet the GOTeDNA team!",
                       href="https://sites.google.com/view/gotedna/the-team",
                       target = "_blank")
            )
          )
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
                  alt = "GOTeDNA Logo",
                  id = "logo_gotedna_contact",
                  style = "width: 230px"
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
        a(
          img(
            title = "Fisheries and Oceans Canada",
            src = "img/logo_partners/DFO_logo_sq.svg",
            alt = "Fisheries and Oceans Canada Logo",
            id = "logo_dfo"),
          href = "https://www.dfo-mpo.gc.ca/index-eng.html",
          target = "_blank"
        )
      ),
      column(
        3,
        a(
          img(
            title = "Maine-eDNA",
            src = "img/logo_partners/logo_Maine_eDNA_nbg_w.png",
            alt = "Maine-eDNA Logo",
            id = "logo_mswc"),
          href = "https://umaine.edu/edna/",
          target = "_blank"
        )
      ),
      column(
        3,
        a(
          img(
            title = "United Nations Ocean Decade",
            src = "img/logo_partners/logo_undossd.svg",
            alt = "United Nations Decade of Ocean Science for Sustainable Development Logo",
            id = "logo_undossd"),
          href = "https://oceandecade.org/",
          target = "_blank"
        )
      ),
      column(
        3,
        a(
          img(
            title = "Ocean Biomolecular Observing Network",
            src = "img/logo_partners/logo_obon.svg",
            alt = "Ocean Biomolecular Observing Network Logo",
            id = "logo_obon"),
          href = "https://obon-ocean.org/",
          target = "_blank"

        )
      )
    )
  )
)
