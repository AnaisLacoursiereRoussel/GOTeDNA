ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css")
  ),
  fluidRow(
    column(
      4,
      img(
        src = "img/GOTeDNA_logo_white.png",
        alt = "GOTeDNA_logo",
        id = "logo_gotedna"
      ),
      div(
        id = "data_request",
        h2("Data request window", class = "col_1"),
        radioButtons("datatype",
          label = "Data Type", choices = c("qPCR", "Metabarcoding")
        ),
        selectInput("taxo_lvl", "Taxonomic level", choices = taxo_lvl)
      ),
    ),
    column(
      8,
      div(
        id = "observation_request",
        h2("Observation", class = "col_1"),
        div(
          id = "data_input",
          column(
            6,
          selectInput("period", "Period", choices = taxo_lvl)
          ),
          column(
            6,
            selectInput("period2", "Period2", choices = taxo_lvl)
          )
        )
      )
    )
  )
)
