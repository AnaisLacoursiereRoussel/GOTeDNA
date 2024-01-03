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
    dplyr::filter(!is.na(decimalLongitude)),
  qPCR = D_qPCR |>
    dplyr::filter(!is.na(decimalLongitude)) |>
    dplyr::filter(!is.na((phylum)))
)
saveRDS(gotedna_data, "inst/app/data/gotedna_data.rds")


# for performances sake, we use a seperate object for station to only display
# a few points on map
get_station <- function(x) {
  x |>
    dplyr::filter(!is.na(decimalLongitude)) |>
    dplyr::filter(!is.na(phylum))
    dplyr::select(
      c(decimalLongitude, decimalLatitude, ecodistrict, station)
    ) |>
    dplyr::distinct() |>
    dplyr::group_by(ecodistrict, station) |>
    dplyr::summarise(
      decimalLongitude = mean(as.numeric(decimalLongitude)),
      decimalLatitude = mean(as.numeric(decimalLatitude))
    ) |>
    sf::st_as_sf(
      coords = c("decimalLongitude", "decimalLatitude"),
      crs = sf::st_crs(4326)
    )
}

gotedna_station <- list(
  metabarcoding = D_mb |> get_station(),
  qPCR = D_qPCR |> get_station()
)
saveRDS(gotedna_station, "inst/app/data/gotedna_station.rds")


D_mb <- read_data(
  choose.method = "metabarcoding", path.folder = "inst/app/data/raw_xlsx_files"
)

newprob <- calc_det_prob(
  data = D_mb_ex,
  ecodistrict.select = "Scotian Shelf"
)
saveRDS(newprob, "inst/app/data/newprob.rds")

Pscaled <- scale_newprob(
  D_mb_ex, "Scotian Shelf",
  newprob
)
saveRDS(Pscaled, "inst/app/data/Pscaled.rds")
