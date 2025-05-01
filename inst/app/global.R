library(GOTeDNA)
library(dplyr)
library(ggplot2)
library(patchwork)
#library(leaflet)
#library(sf)
#library(shiny)
#library(shinyjs)
#library(bslib)
#library(plotly)
cli::cli_alert_info("Packages loaded")

list.files("modules", full.names = TRUE) |>
  lapply(source)
cli::cli_alert_info("Modules loaded")

# Ensure that the user installs these if not already installed
# Necessary to generate PDF report

if(!webshot::is_phantomjs_installed() == TRUE)
  webshot::install_phantomjs()

if(!tinytex::is_tinytex() == TRUE)
  tinytex::install_tinytex()




# Generic helpers
trans_letters <- function(x, pos = 1, fun = toupper) {
  strsplit(x, split = "") |>
    lapply(\(y) {
      y[pos] <- fun(y[pos])
      paste(y, collapse = "")
    }) |>
    unlist()
}

# Data
## import glossary
gloss <- read.csv("data/glossary.csv")
gloss$Term <- paste0('<p align ="right"><b>', trimws(gloss$Term), "</b></p>")
gloss$Definition <- trimws(gloss$Definition)

## import GOTeDNA data
gotedna_data <- gotedna_data0 <- readRDS("data/gotedna_data.rds")
gotedna_station <- gotedna_station0 <- readRDS("data/gotedna_station.rds")
gotedna_primer <- readRDS("data/gotedna_primer.rds")

# taxonomic_ranks <- list("kingdom", "phylum", "family", "order", "class", "genus")
taxonomic_ranks <- list("domain", "kingdom", "phylum", "class", "order", "family", "genus")
names(taxonomic_ranks) <- trans_letters(taxonomic_ranks |> unlist())

ls_threshold <- as.list(seq(50, 95, 5))
names(ls_threshold) <- paste0(seq(50, 95, 5), "%")


# function

basemap <- function() {
  leaflet::leaflet() |>
    leafem::addMouseCoordinates() |>
  #  leaflet::addProviderTiles("Esri.OceanBasemap", group = "OceaBasemap") |>
    leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") |>
  #  leaflet::addProviderTiles("Stadia.StamenTonerLite") |>
   # leaflet::addProviderTiles("Esri.WorldGrayCanvas") |>

   # leaflet::addLayersControl(
    #  baseGroups = c("OpenStreetMap", "Ocean Basemap"),
    #  position = "bottomleft"
  #  ) |>
    leaflet::addScaleBar(
      position = c("bottomright"),
      options = leaflet::scaleBarOptions(maxWidth = 200)
    ) |>
    leaflet.extras::addDrawToolbar(polylineOptions = FALSE,
                                   circleOptions = FALSE,
                                   polygonOptions = TRUE,
                                   rectangleOptions = TRUE,
                                   circleMarkerOptions = FALSE,
                                   markerOptions = FALSE)
}


get_primer_selection <- function(lvl, data) {
  if (is.null(lvl)) {
    return("Not available")
  } else {
    if (lvl == "species") {
      tx_col <- "species"
    } else {
      tx_col <- lvl
    }
    data_available <- data |>
      select({{ tx_col }}, primer) |>
      distinct()
    if (!is.null(data_available) && nrow(data_available)) {
      tmp <- gotedna_primer[[lvl]] |>
        inner_join(
          data_available,
          join_by(primer == primer, {{ lvl }} == {{ tx_col }})
        ) |>
        mutate(
          text = paste0(primer, " (", detects, "/", total, " ; ", perc, "%)")
        )
      out <- as.list(tmp$primer)
      names(out) <- tmp$text
      if (is.null(out)) {
        return("Not available")
      } else {
        return(out)
      }
    } else {
      return("Not available")
    }
  }
}


# Primer information for primer tab
## import glossary
primer_seqs <- read.csv("data/primers.csv") %>%
  dplyr::rename("Primer set" = "PrimerSet",
                "Type of data" = "Data",
                "Original primer name" = "OriginalName",
                "Sequence (5'-3')" = "Seq",
                "Fragment length (bp)" = "bp")


