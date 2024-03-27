library(cli)
library(dplyr)
library(ggplot2)
library(GOTeDNA)
library(leaflet)
library(patchwork)
library(sf)
library(shiny)
library(shinyjs)
cli_alert_info("Packages loaded")

list.files("modules", full.names = TRUE) |>
  lapply(source)
cli_alert_info("Modules loaded")

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
taxon_levels <- c("phylum", "class", "genus", "species")

taxonomic_ranks <- list("kingdom", "phylum", "family", "order", "class", "genus")
names(taxonomic_ranks) <- trans_letters(taxonomic_ranks |> unlist())

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


get_primer_selection <- function(lvl, data) {
 if (lvl == "kingdom") {
    out <- table(data$target_subfragment) |>
      sort() |>
      rev()
    names(out)
  } else {
    if (lvl == "species") {
      tx_col <- "scientificName"
    } else {
      tx_col <- lvl
    }
    tmp <- gotedna_primer[[lvl]] |>
      inner_join(
        data |> select({{ tx_col }}, target_subfragment) |> distinct(),
        join_by(primer == target_subfragment,{{ lvl }} == {{ tx_col }})) |>
      mutate(
        text = paste0(primer, " (", success, "/", total, " ; ", perc, "%)")
      )
    out <- as.list(tmp$primer)
    names(out) <- tmp$text
    out
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

plotText <- function(txt, ...) {
  plot(c(-1, 1), c(-1, 1),
    ann = FALSE, bty = "n", type = "n", xaxt = "n",
    yaxt = "n"
  )
  text(0, 0, txt, ...)
}

plotNotAvailableSpeciesLevel <- function() {
  plotText("Plot only available at the species level.")
}

plotNotAvailable <- function() {
  plotText("Plot not available yet.", cex = 1.5)
}

plotNotAvailableForqPCR <- function() {
  plotText("Plot not available for qPCR data.")
}


add_thumbnail_button <- function(id, src, title, alt = "Figure thumbnail") {
  # https://stackoverflow.com/questions/44841346/adding-an-image-to-shiny-action-button
  tags$button(
    id = id,
    class = "btn action-button",
    tags$img(
      src = src,
      alt = alt,
      id = "fig-thumbnail",
      style = "height: 7rem;"
    ),
    title = title
  )
}

add_figure_selection <- function(id, title, scr = NULL, info = title) {
  column(2,
    div(
      class = "thumbnail",
      add_thumbnail_button(id, scr, info),
      h5(title)
    )
  )
}

