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

# calculate and scale metabarcoding probabilities
newprob_mb <- calc_det_prob(
  data = gotedna_data$metabarcoding[gotedna_data$metabarcoding$GOTeDNA_ID %in% 8,]
)
saveRDS(newprob_mb, "inst/app/data/newprob_mb.rds")

Pscaled_mb <- scale_newprob(
  gotedna_data$metabarcoding[gotedna_data$metabarcoding$GOTeDNA_ID %in% 8,], newprob_mb
)
saveRDS(Pscaled_mb, "inst/app/data/Pscaled_mb.rds")

# calculate and scale qPCR probabilities
newprob_qp <- calc_det_prob(
  data = D_qPCR[D_qPCR$GOTeDNA_ID %in% 11,]
)
saveRDS(newprob_qp, "inst/app/data/newprob_qp.rds")

Pscaled_qp <- scale_newprob(
  D_qPCR[D_qPCR$GOTeDNA_ID %in% 11,], newprob_qp
)
saveRDS(Pscaled_qp, "inst/app/data/Pscaled_qp.rds")
