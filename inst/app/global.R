library(GOTeDNA)
library(dplyr)
library(ggplot2)
library(patchwork)
library(leaflet)
library(sf)
library(shiny)
library(shinyjs)
cli::cli_alert_info("Packages loaded")

list.files("modules", full.names = TRUE) |>
  lapply(source)
cli::cli_alert_info("Modules loaded")

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
## filter data based on user choices of taxa
filter_taxa_data <- function(x, phy, cla, gen, spe) {
  if (phy != "All") {
    x <- x |> dplyr::filter(phylum == phy)
    if (cla != "All") {
      x <- x |> dplyr::filter(class == cla)
      if (gen != "All") {
        x <- x |> dplyr::filter(genus == gen)
        if (spe != "All") {
          x <- x |> dplyr::filter(species == spe)
        }
      }
    }
  }
  x
}

basemap <- function() {
  leaflet() |>
    leafem::addMouseCoordinates() |>
    leaflet::addProviderTiles("Esri.OceanBasemap", group = "OceaBasemap") |>
    leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") |>
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "Ocean Basemap"),
      position = "bottomleft"
    ) |>
    leaflet::addScaleBar(
      position = c("bottomright"),
      options = leaflet::scaleBarOptions(maxWidth = 200)
    )
}


get_primer_selection <- function(lvl, data) {
  if (is.null(lvl)) {
    return("Not available")
  }
  if (lvl == "kingdom") {
    out <- table(data$primer) |>
      sort() |>
      rev()
    names(out)
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
          text = paste0(primer, " (", success, "/", total, " ; ", perc, "%)")
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

get_taxon_level <- function(phy, cla, gen, spe) {
  if (spe != "All") {
    out <- 4
  } else if (gen != "All") {
    out <- 3
  } else if (cla != "All") {
    out <- 2
  } else {
    out <- 1
  }
}

shinyLink <- function(to, label) {
  tags$a(
    class = "shiny__link",
    href = to,
    label
  )
}

#coord_tab <- na.omit(data.frame(longitude = NA_real_, latitude = NA_real_))
