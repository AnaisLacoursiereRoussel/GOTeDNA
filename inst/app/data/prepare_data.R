D_mb <- read_data(
    choose.method = "metabarcoding", path.folder = "inst/app/data/raw_xlsx_files"
)
D_qPCR <- read_data(
    choose.method = "qPCR", path.folder = "inst/app/data/raw_xlsx_files"
)
gotedna_data <- list(
    metabarcoding = D_mb |>
        dplyr::filter(!is.na(decimalLongitude)) |>
        sf::st_as_sf(
            coords = c("decimalLongitude", "decimalLatitude"),
            crs = sf::st_crs(4326)
        ),
    qPCR = D_qPCR |>
        dplyr::filter(!is.na(decimalLongitude)) |>
        sf::st_as_sf(
            coords = c("decimalLongitude", "decimalLatitude"),
            crs = sf::st_crs(4326)
        )
)
saveRDS(gotedna_data, "inst/app/data/gotedna_data.rds")

D_mb <- read_data(
    choose.method = "metabarcoding", path.folder = "inst/app/data/raw_xlsx_files"
)

newprob <- calc_det_prob(
    data = D_mb_ex,
    ecodistrict.select = "Scotian Shelf"
)
saveRDS(newprob, "inst/app/data/newprob.rds")

Pscaled_month <- scale_prob_by_month(
    D_mb_ex, "Scotian Shelf",
    newprob$newP_agg
)
saveRDS(Pscaled_month, "inst/app/data/Pscaled_month.rds")
