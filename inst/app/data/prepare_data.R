# read data
D_mb <- read_data(
  choose.method = "metabarcoding", path.folder = "inst/app/data/raw_xlsx_files"
)
D_qPCR <- read_data(
  choose.method = "qPCR", path.folder = "inst/app/data/raw_xlsx_files"
)

# make a list of two data frames
gotedna_data <- list(
  metabarcoding = D_mb |>
    dplyr::filter(!is.na(decimalLongitude)) |>
    dplyr::mutate(msct = case_when(
      organismQuantity == 0 ~ TRUE,
      organismQuantity > 10 ~ TRUE
    )) |>
    tidyr::drop_na(msct) |>
    dplyr::ungroup() |>
    as.data.frame(),
  qPCR = D_qPCR |>
    dplyr::filter(!is.na(decimalLongitude), !is.na(phylum)) |>
    dplyr::ungroup() |>
    as.data.frame()
)
saveRDS(gotedna_data, "inst/app/data/gotedna_data.rds")


# for performances sake, we use a separate object for station to only display
# a few points on map
get_station <- function(x) {
  x |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(decimalLongitude)) |>
    dplyr::filter(!is.na(phylum)) |>
    dplyr::select(
      c(decimalLongitude, decimalLatitude, station)
    ) |>
    dplyr::distinct() |>
    dplyr::group_by(station) |>
    dplyr::summarise(
      decimalLongitude = mean(as.numeric(decimalLongitude)),
      decimalLatitude = mean(as.numeric(decimalLatitude))
    ) |>
    dplyr::ungroup() |>
    as.data.frame() |>
    sf::st_as_sf(
      coords = c("decimalLongitude", "decimalLatitude"),
      crs = sf::st_crs(4326)
    )
}

gotedna_station <- list(
  metabarcoding = gotedna_data$metabarcoding |> get_station(),
  qPCR = gotedna_data$qPCR |> get_station()
)
saveRDS(gotedna_station, "inst/app/data/gotedna_station.rds")



# Prepare primer data 
gotedna_data <- readRDS("inst/app/data/gotedna_data.rds")
newprob <- calc_det_prob(gotedna_data$metabarcoding)
scaledprobs <- scale_newprob(D_mb_ex, newprob)

scaledprobs$Pscaled_month$phylum |> unique()
 [1] NA                "Chordata"        "Echinodermata"   "Cnidaria"        "Annelida"        "Arthropoda"      "Bryozoa"         "Mollusca"        "Nemertea"        "Platyhelminthes"
[11] "Porifera"        "Xenacoelomorpha" "Rotifera"       

gotedna_data$metabarcoding$phylum |> unique()

[1] "Chordata"        "Echinodermata"   "Cnidaria"        "Annelida"        "Arthropoda"      "Bryozoa"         "Ctenophora"      "Entoprocta"      "Hemichordata"    "Mollusca"       
[11] "Nematoda"        "Nemertea"        "Platyhelminthes" "Porifera"        "Rotifera"        "Xenacoelomorpha" "Brachiopoda"     "Priapulida"      "Gastrotricha"    "Gnathifera"     
[21] "Tardigrada"   


gotedna_primer  <- list()

for (i in c("phylum", "class", "genus", "species")) {
  print(i)
  gotedna_primer[[i]] <- primer_sort(i, scaledprobs$Pscaled_month) |>
    mutate(text = paste0(primer, " (", success, "/", total, " ", perc, "%)")) 
}

saveRDS(gotedna_primer, "inst/app/data/gotedna_primer.rds")
