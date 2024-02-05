# read data
D_mb <- read_data(
    choose.method = "metabarcoding", path.folder = "inst/app/data/raw_xlsx_files"
)
D_qPCR <- read_data(
    choose.method = "qPCR", path.folder = "inst/app/data/raw_xlsx_files"
)


is.missing  <- function(x) {
    is.na(x) | is.null(x) | x == ""
}

D_mb_missing <- D_mb  |> 
    dplyr::filter(
        is.missing(phylum) | is.missing(family) | is.missing(genus)
    )
# nothing

D_qPCR_missing <- D_qPCR |>
    dplyr::filter(
        is.missing(phylum) | is.missing(family) | is.missing(genus)
    )
write.csv(D_qPCR_missing, file = "qPCR_phylo_missing.csv")