library(cli)
library(dplyr)
library(ggplot2)
library(GOTeDNA)
library(leaflet)
library(mapview)
library(patchwork)
library(sf)
library(shiny)
library(shinyjs)
cli_alert_info("Packages loaded")

list.files("modules", full.names = TRUE) |>
  lapply(source)
cli_alert_info("Modules loaded")

# import glossary
gloss <- read.csv("data/glossary.csv")
gloss$Term <- paste0('<p align ="right"><b>', trimws(gloss$Term), "</b></p>")
gloss$Definition <- trimws(gloss$Definition)

# import all GOTeDNA data
gotedna_data <- readRDS("data/gotedna_data.rds")
gotedna_station <- readRDS("data/gotedna_station.rds")
#
newprob <- readRDS("data/newprob.rds")
Pscaled <- readRDS("data/Pscaled.rds")

taxon_levels <- c("phylum", "class", "genus", "species")

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
          x <- x |> dplyr::filter(scientificName == spe)
        }
      }
    }
  }
  x
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

plotText <- function(txt) {
  plot(c(-1, 1), c(-1, 1),
    ann = FALSE, bty = "n", type = "n", xaxt = "n",
    yaxt = "n"
  )
  text(0, 0, txt)
}

plotNotAvailableSpeciesLevel <- function() {
  plotText("Plot only available at the species level.")
}

plotNotAvailable <- function() {
  plotText("Plot not available yet.")
}

plotNotAvailableForqPCR <- function() {
  plotText("Plot not available for qPCR data.")
}



placeholder_thumbnail <- function() {
  img(
    src = "img/GOTeDNA_wave.png",
    alt = "GOTeDNA_logo",
    id = "place_holder_logo_gotedna",
    style = "height: 4rem"
  )
}


taglist_fig_info <- function(title, info, scr = NULL) {
  column(
    6,
    fluidRow(
      column(
        8,
        h5(title),
        p(info)
      ),
      column(
        4,
        placeholder_thumbnail()
      ),
    )
  )
}