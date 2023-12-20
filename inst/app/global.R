library(cli)
library(dplyr)
library(ggplot2)
library(GOTeDNA)
library(kableExtra)
library(leaflet)
library(patchwork)
library(shiny)
library(shinyjs)
cli_alert_info("Packages loaded")

list.files("modules", full.names = TRUE) |>
    lapply(source)
cli_alert_info("Modules loaded")

gloss <- read.csv("data/glossary.csv")
gloss$Term <- paste0('<p align ="right"><b>', trimws(gloss$Term), "</b></p>")
gloss$Definition <- trimws(gloss$Definition)
taxo_lvl <- c("phylum", "class", "order", "family", "genus", "scientificName")
dfs <- D_mb_ex |>
    sf::st_as_sf(
        coords = c("decimalLongitude", "decimalLatitude"),
        crs = sf::st_crs(4326)
    )
newprob <- readRDS("data/newprob.rds")
Pscaled_month <- readRDS("data/Pscaled_month.rds")

tx_phy <- c("All", unique(D_mb_ex$phylum))
tx_cla <- c("All", unique(D_mb_ex$class))
tx_gen <- c("All", unique(D_mb_ex$genus))
tx_spe <- c("All", unique(D_mb_ex$scientificName))


filter_spatial_data <- function(x, phy, cla, gen, spe) {
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
