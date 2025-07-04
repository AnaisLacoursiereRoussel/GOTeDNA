#' Read and format qPCR and metabarcoding metadata and data from GOTeDNA templates
#'
#' @description Function that reads GOTeDNA qPCR or metabarcoding template MS
#' Excel sheets found in a specified folder. When `choose.methods = "qPCR" or
#' "metabarcoding"` it will compile metadata (sheet 2) and data (sheet 3 or 4,
#' respectively) into a single dataframe. Data frames are then formatted
#' appropriately for use in subsequent analysis and visualizations (e.g., date
#' reformatting, merging metadata and data).
#' * Note that template column names are in the format of Darwin Core Archive
#' (DwC-A) using Darwin Core (DwC) data standards where possible.
#'
#' @param choose.method (required, character) Choices = c("qPCR", "metabarcoding").
#' @param path.folder (optional, character)
#' Default: `path.folder = NULL`. Default will use the current working directory.
#'
#' @return A tibble with 26 columns:
#' * `protocol_ID`
#' * `protocolVersion`
#' * `materialSampleID`
#' * `eventID`
#' * `primer`
#' * `species`
#' * `domain`
#' * `kingdom`
#' * `phylum`
#' * `class`
#' * `order`
#' * `family`
#' * `genus`
#' * `concentration`: provided when choose.method = "qPCR"
#' * `pcr_primer_lod` : provided when choose.method = "qPCR"
#' * `organismQuantity`: provided when choose.method = "metabarcoding"
#' * `date`
#' * ecodistrict`
#' * `LClabel` : Local Contexts label to denote First Nations data sovereignty
#' * `decimalLatitude`
#' * `decimalLongitude`
#' * `station`
#' * `year`
#' * `month`
#' * `detected`
#' * `msct` :logical, where minimum sequence copy threshold = 10
#' * `ownerContact` : email of data owner/steward
#' * `bibliographicCitation` : DOI reference, if applicable
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname read_data
#' @export
#' @examples
#' \dontrun{
#' D_mb <- read_data(
#'  choose.method = "metabarcoding", path.folder = "./inst/testdata"
#' )
#' }

read_data <- function(
  choose.method = c("qPCR", "metabarcoding"), path.folder = NULL
  ) {

  if (is.null(path.folder)) path.folder <- getwd()

  files <- list.files(
    path = path.folder,
    pattern = "^GOTeDNA",
    full.names = TRUE
  )

  choose.method <- match.arg(choose.method)
  slc_sheet  <- switch(choose.method,
    qPCR = "Sample_qPCR data",
    metabarcoding = "Sample_Metabarcoding data"
  )

  samples <- suppressWarnings(
    lapply(files, readxl::read_excel, sheet = slc_sheet)
  )

  names(samples) <- files
  # remove list elements where sample sheet is empty
  samples <- Filter(function(a) any(!is.na(a[["protocol_ID"]])), samples)

  metadata <- lapply(files, readxl::read_excel, sheet = "Sample_Metadata")
  names(metadata) <- files
  # only read in metadata for which there are sample sheets
  metadata <- metadata[names(metadata) %in% names(samples)]

  # Ensures dates are in unified format across dataset
  # In character format to make graphing possible
  suppressWarnings(
    for (i in seq_len(length(metadata))) {
      if (is.double(metadata[[i]]$eventDate)) {
        metadata[[i]]$newDate <- as.character(metadata[[i]]$eventDate)
      } else {
        ifelse(
          nchar(metadata[[i]]$eventDate) == 5,
          (metadata[[i]]$newDate <-
            as.character(
              as.Date(as.numeric(metadata[[i]]$eventDate), origin = "1899-12-30")
            )),
          (metadata[[i]]$newDate2 <-
            as.character(
              as.Date(lubridate::parse_date_time(metadata[[i]]$eventDate, orders = "d m y"))
            ))
        )
      }
      metadata[[i]]$eventDate <- metadata[[i]]$newDate # merge Date columns
      metadata[[i]]$eventDate[!is.na(metadata[[i]]$newDate2)] <- metadata[[i]]$newDate2[!is.na(metadata[[i]]$newDate2)]
      metadata[[i]] <- subset(metadata[[i]], is.na(controlType)) # removes field and extraction blanks
    }
  )

  for (k in seq_len(length(metadata))) {
    metadata[[k]]$year <- lubridate::year(metadata[[k]]$eventDate)
    metadata[[k]]$month <- lubridate::month(metadata[[k]]$eventDate)
  }

  # match event date to samples
  for (j in seq_len(length(samples))) {
    samples[[j]]$date <- metadata[[j]]$eventDate[match(samples[[j]]$materialSampleID, metadata[[j]]$materialSampleID)]
  #  samples[[j]]$ecodistrict <- metadata[[j]]$ecodistrict[match(samples[[j]]$materialSampleID, metadata[[j]]$materialSampleID)] %>%
   #   stringr::str_remove_all( # clean ecodistrict
  #      pattern = "(-?[:digit:])"
  #    )

    samples[[j]]$protocolVersion <- metadata[[j]]$protocolVersion[match(samples[[j]]$materialSampleID, metadata[[j]]$materialSampleID)]
    samples[[j]]$decimalLatitude <- metadata[[j]]$decimalLatitude[match(samples[[j]]$materialSampleID, metadata[[j]]$materialSampleID)]
    samples[[j]]$decimalLongitude <- metadata[[j]]$decimalLongitude[match(samples[[j]]$materialSampleID, metadata[[j]]$materialSampleID)]
    samples[[j]]$station <- metadata[[j]]$samplingStation[match(samples[[j]]$materialSampleID, metadata[[j]]$materialSampleID)]
    samples[[j]]$year <- lubridate::year(samples[[j]]$date)
    samples[[j]]$month <- lubridate::month(samples[[j]]$date)
    samples[[j]]$LClabel <- metadata[[j]]$LClabel[match(samples[[j]]$materialSampleID, metadata[[j]]$materialSampleID)]
    samples[[j]]$ownerContact <- metadata[[j]]$ownerContact[match(samples[[j]]$materialSampleID, metadata[[j]]$materialSampleID)]
    samples[[j]]$bibliographicCitation <- metadata[[j]]$bibliographicCitation[match(samples[[j]]$materialSampleID, metadata[[j]]$materialSampleID)]}

  samples <- lapply(samples, function(x) {
    if (choose.method == "qPCR") {
      x %>%
        tidyr::drop_na(date) %>% # drop lab/field blanks
        subset(!kingdom %in% "NA") %>%
        dplyr::mutate(
          decimalLatitude = suppressWarnings(as.numeric(decimalLatitude)),
          decimalLongitude = suppressWarnings(as.numeric(decimalLongitude)),
          concentration = suppressWarnings(as.numeric(concentration)),
          materialSampleID = suppressWarnings(as.character(materialSampleID)),
          protocolVersion = suppressWarnings(as.numeric(protocolVersion))) %>%
        dplyr::mutate(
          detected = dplyr::case_when(
            is.na(concentration) ~ 0,
            concentration >= pcr_primer_lod ~ 1,
            concentration < pcr_primer_lod ~ 0)) %>%
        dplyr::rename("primer" = "target_gene")

    } else {
      x %>%
        tidyr::drop_na(date) %>%
        dplyr::filter(kingdom != "NA") %>%
        dplyr::mutate(
          detected = dplyr::case_when(
            organismQuantity != 0 ~ 1,
            organismQuantity == 0 ~ 0),
            decimalLatitude = suppressWarnings(as.numeric(decimalLatitude)),
            decimalLongitude = suppressWarnings(as.numeric(decimalLongitude)),
            materialSampleID = suppressWarnings(as.character(materialSampleID)),
            protocolVersion = suppressWarnings(as.numeric(protocolVersion)
        )) %>%
        dplyr::rename("primer" = "target_subfragment")
    }
  })

  GOTeDNA_df <- do.call(dplyr::bind_rows, lapply(samples, function(x) {
    x[, names(x) %in% c(
      "protocol_ID", "protocolVersion", "materialSampleID","eventID", "primer",
      "scientificName", "domain","kingdom", "phylum", "class", "order",
      "family", "genus", "date", #"ecodistrict",
      "LClabel", "decimalLatitude", "decimalLongitude",
      "station", "year", "month", "organismQuantity", "concentration", "pcr_primer_lod", "detected", "ownerContact", "bibliographicCitation"
    )]
  })) |>
    dplyr::rename("species" = "scientificName")
  rownames(GOTeDNA_df) <- NULL

  return(GOTeDNA_df)
}
