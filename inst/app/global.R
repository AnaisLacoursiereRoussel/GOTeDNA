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

# import glossary
gloss <- read.csv("data/glossary.csv")
gloss$Term <- paste0('<p align ="right"><b>', trimws(gloss$Term), "</b></p>")
gloss$Definition <- trimws(gloss$Definition)

# import all GOTeDNA data
gotedna_data <- readRDS("data/gotedna_data.rds")
newprob <- readRDS("data/newprob.rds")
Pscaled_month <- readRDS("data/Pscaled_month.rds")

taxo_lvl <- c("phylum", "class", "order", "family", "genus", "scientificName")
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
