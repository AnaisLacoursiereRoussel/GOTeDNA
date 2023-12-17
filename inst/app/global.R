library(cli)
library(dplyr)
library(ggplot2)
library(GOTeDNA)
library(leaflet)
library(patchwork)
library(shiny)
cli_alert_info("Packages loaded")

list.files("modules", full.names = TRUE) |> 
    lapply(source)

cli_alert_info("Modules loaded")

taxo_lvl <- c("phylum", "class", "order", "family", "genus", "scientificName")

dfs <- D_mb_ex |>
    sf::st_as_sf(
        coords = c("decimalLongitude", "decimalLatitude"),
        crs = sf::st_crs(4326)
    )
newprob <- readRDS("data/newprob.rds")
Pscaled_month <- readRDS("data/Pscaled_month.rds")

tx_phy <- c("all", unique(D_mb_ex$phylum))
tx_cla <- c("all", unique(D_mb_ex$class))
tx_gen <- c("all", unique(D_mb_ex$genus))
tx_spe <- c("all", unique(D_mb_ex$scientificName))


filter_spatial_data <- function(x, phy, cla, gen, spe) {
    if (phy != "all") {
        x <- x |> dplyr::filter(phylum == phy)
    }
    if (cla != "all") {
        x <- x |> dplyr::filter(class == cla)
    }
    if (gen != "all") {
        x <- x |> dplyr::filter(genus == gen)
    }
    if (spe != "all") {
        x <- x |> dplyr::filter(scientificName == spe)
    }
    x
}
