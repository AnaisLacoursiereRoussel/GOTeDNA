# read data
D_mb <- read_data(
  choose.method = "metabarcoding", path.folder = "inst/app/data/raw_xlsx_files"
)

D_mb_msct <- D_mb %>%
  dplyr::mutate(msct = case_when(
    organismQuantity == 0 ~ TRUE,
    organismQuantity > 10 ~ TRUE
  )) |>
  tidyr::drop_na(msct)

D_mb_nodetect <- D_mb_msct %>%
  dplyr::group_by(
    GOTeDNA_ID, GOTeDNA_version, species, primer, station) %>%
  dplyr::summarise(num_detected = sum(detected)) %>%
  dplyr::filter(num_detected == 0)

D_mb_clean <- dplyr::anti_join(D_mb_msct, D_mb_nodetect,
                               by = c("GOTeDNA_ID","GOTeDNA_version","species","primer", "station"))

D_qPCR <- read_data(
  choose.method = "qPCR", path.folder = "inst/app/data/raw_xlsx_files"
)

# make a list of two data frames
gotedna_data <- list(
  metabarcoding = D_mb_clean |>
    dplyr::filter(!is.na(decimalLongitude),
                  !class %in% c("Aves","Insecta", "Hexapoda"),
                  !order %in% c("Primates","Artiodactyla","Perissodactyla","Rodentia"),
                  !family %in% c("Felidae","Canidae","Procyonidae")) |>
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


#### Need to do for both qPCR and metabarcoding
# Prepare primer data
gotedna_data <- readRDS("inst/app/data/gotedna_data.rds")

newprob_mb <- calc_det_prob(gotedna_data$metabarcoding)
newprob_q <- calc_det_prob(gotedna_data$qPCR)

scaledprobs_mb <- scale_newprob(gotedna_data$metabarcoding, newprob_mb)
scaledprobs_q <- scale_newprob(gotedna_data$qPCR, newprob_q)

gotedna_primer <- list()

# this needs to be based on the area selection
for (i in c("domain", "kingdom", "phylum", "class", "order", "family", "genus", "species")) {
  gotedna_primer[[i]] <- primer_sort(i, dplyr::bind_rows(scaledprobs_mb$Pscaled_month, scaledprobs_q$Pscaled_month)) |>
    mutate(text = paste0(primer, " (", detects, "/", total, " ", perc, "%)"))
}

saveRDS(gotedna_primer, "inst/app/data/gotedna_primer.rds")
