# Primer module

mod_primers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("primer_seq"))
    )
}

mod_primers_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$primer_seq <- DT::renderDT(
      DT::datatable(primer_seqs,
        escape = FALSE
        )
      )
  })
}
