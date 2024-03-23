# Glossary module

mod_glossary_ui <- function(id) {
    ns <- NS(id)
    tagList(
        DT::dataTableOutput(ns("glossary")),
        div(
            p("eDNA general wording have been defined and described within:"),
            p("
          Abbott, C.*, Coulson, M.*, Gagné, N.*, Lacoursière‐Roussel, A.*,
          Parent, G.J.*, Bajno, R., Dietrich, C., May-McNally, S. 2021.
          Guidance on the Use of Targeted Environmental DNA (eDNA) Analysis for
          the Management of Aquatic Invasive Species and Species at Risk. DFO
          Can. Sci. Advis. Sec. Res. Doc. 2021/019. iv + 42 p.
        "),
            class = "text-align-left"
        )
    )
}

mod_glossary_server <- function(id, r) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        output$glossary <- DT::renderDT(
            gloss |> DT::datatable(
                escape = FALSE,
                options = list(
                    lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
                    pageLength = 10
                )
            )
        )
    })
}
