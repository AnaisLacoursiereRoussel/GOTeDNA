#' Read and format qPCR and metabarcoding metadata and data from GOTeDNA templates
#' @description Function that reads GOTeDNA qPCR or metabarcoding template MS Excel
#' sheets found in a specified folder. When \code{choose.methods = "qPCR" or "metabarcoding"},
#' it will compile metadata (sheet 2) and data (sheet 3 or 4, respectively) into a single
#' dataframe. Data frames are then formatted appropriately for use in subsequent
#' analysis and visualizations (e.g., date reformatting, merging metadata and data).
#' * Note that template column names are in the format of Darwin Core Archive (DwC-A)
#' using Darwin Core (DwC) data standards where possible.
#'
#' @param choose.method (required, character) Choices = c("qPCR", "metabarcoding").
#' @param path.folder (optional, character)
#' Default: \code{path.folder = NULL}. Default will use the current working directory.
#'
#' @return A tibble with 21 columns:
#' \itemize{
#' \item\code{GOTeDNA_ID},
#' \item\code{GOTeDNA_version},
#' \item\code{eventID}: added internally to keep track of files origins,
#' \item\code{scientificName}: added internally to keep track of files origins,
#' \item\code{target_gene},
#' \item\code{target_subfragment},
#' \item\code{kingdom},
#' \item\code{phylum},
#' \item\code{class},
#' \item\code{order},
#' \item\code{family},
#' \item\code{genus},
#' \item\code{date},
#' \item\code{ecodistrict},
#' \item\code{decimalLatitude},
#' \item\code{decimalLongitude},
#' \item\code{station},
#' \item\code{year},
#' \item\code{month},
#' \item\code{organismQuantity}: provided when choose.method = "metabarcoding",
#' \item\code{concentration}: provided when choose.method = "qPCR",
#' \item\code{detected}
#' }
#'

#' @author Melissa Morrison \email{Melissa.Morrison@@dfo-mpo.gc.ca}
#' @rdname read_data
#' @export
#' @examples
#' \dontrun{
#' D_mb <- read_data(choose.method = "metabarcoding", path.folder="./data/")
#' }
#' @importFrom magrittr `%>%`
read_data <- function(choose.method, path.folder) {

  if (is.null(path.folder)) path.folder <- getwd()

  files <- list.files(path = path.folder,
                      pattern = "GOTeDNA-[0-9]{1,2}_data",
                      full.names = TRUE)

  if (!is.null(choose.method)) {
    choose.method <- match.arg(
      arg = choose.method,
      choices = c("qPCR",
                  "metabarcoding"),
      several.ok = FALSE
    )
  }

  method = data.frame(values=c(3,4),
                      labels = c("qPCR","metabarcoding"))

  choose.method <- switch(choose.method, method$values[method$labels==choose.method])

  samples <- suppressWarnings(
    lapply(files, function(x) readxl::read_excel(x, sheet = choose.method))
  )

  names(samples) <- files
  # remove list elements where sample sheet is empty
  samples <- Filter(function(a) any(!is.na(a[["GOTeDNA_ID"]])), samples)

  metadata <- lapply(files, function(x)  readxl::read_excel(x, sheet = 2))
  names(metadata) <- files
  # only read in metadata for which there are sample sheets
  metadata <- metadata[names(metadata) %in% names(samples)]

  # Ensures dates are in unified format across dataset
  # In character format to make graphing possible
  suppressWarnings(
    for (i in 1:length(metadata)){
      if ((typeof(metadata[[i]]$eventDate) == "double") == TRUE ) {
        metadata[[i]]$newDate <- as.character(metadata[[i]]$eventDate)
      } else {
        ifelse(
          ((nchar(metadata[[i]]$eventDate) == 5) == TRUE),
          (metadata[[i]]$newDate <-
             as.character(
               as.Date(as.numeric(metadata[[i]]$eventDate), origin = "1899-12-30"))),
          (metadata[[i]]$newDate2 <-
             as.character(
               as.Date(lubridate::parse_date_time(metadata[[i]]$eventDate, orders = "d m y")))))
      }

      metadata[[i]]$eventDate <- metadata[[i]]$newDate  # merge Date columns
      metadata[[i]]$eventDate[!is.na(metadata[[i]]$newDate2)] <- metadata[[i]]$newDate2[!is.na(metadata[[i]]$newDate2)]
      metadata[[i]] <- subset(metadata[[i]], is.na(controlType)) # removes field and extraction blanks
    }
  )

  for (k in 1:length(metadata)){
    metadata[[k]]$year <- lubridate::year(metadata[[k]]$eventDate)
    metadata[[k]]$month <- lubridate::month(metadata[[k]]$eventDate)
  }

  # match event date to samples
  for (j in 1:length(samples)) {
    samples[[j]]$date <- metadata[[j]]$eventDate[match(samples[[j]]$eventID, metadata[[j]]$eventID)]
    samples[[j]]$ecodistrict <- metadata[[j]]$ecodistrict[match(samples[[j]]$eventID, metadata[[j]]$eventID)] %>%
      stringr::str_remove_all( # clean ecodistrict
        pattern="(-?[:digit:])")

    samples[[j]]$decimalLatitude <- metadata[[j]]$decimalLatitude[match(samples[[j]]$eventID, metadata[[j]]$eventID)]
    samples[[j]]$decimalLongitude <- metadata[[j]]$decimalLongitude[match(samples[[j]]$eventID, metadata[[j]]$eventID)]
    samples[[j]]$station <- metadata[[j]]$samplingStation[match(samples[[j]]$eventID, metadata[[j]]$eventID)]
    samples[[j]]$year <- lubridate::year(samples[[j]]$date)
    samples[[j]]$month <- lubridate::month(samples[[j]]$date)
  }

  samples <- lapply(samples, function(x) {
    if(any(colnames(x) %in% "concentration")==TRUE) {
      x %>%
        tidyr::drop_na(date) %>% # drop lab/field blanks
        subset(!kingdom %in% "NA") %>%
        dplyr::group_by(eventID) %>%
        dplyr::mutate(detected = dplyr::case_when(
          concentration > 0 ~ 1,
          concentration == 0 ~ 0)) %>% #,
        transform(concentration = suppressWarnings(as.numeric(concentration)))
      # is.na(quantificationCycle) ~ 0)) %>%#,
      #   aboveLOD = dplyr::case_when(
      # all(concentration >= pcr_primer_lod)  ~ 1,
      #all(concentration < pcr_primer_lod) ~ 0))
    } else {
      x %>%
        tidyr::drop_na(date) %>%
        subset(!kingdom %in% "NA") %>%
        dplyr::mutate(detected = dplyr::case_when(
          organismQuantity != 0 ~ 1,
          organismQuantity == 0 ~ 0))

    }
  }
  )

  GOTeDNA_df <- do.call(rbind, lapply(samples, function(x) x[,names(x) %in% c("GOTeDNA_ID", "GOTeDNA_version", "eventID",
                                                                              "target_gene","target_subfragment", "scientificName",
                                                                              "kingdom", "phylum", "class", "order", "family",
                                                                              "genus", "date", "ecodistrict", "decimalLatitude", "decimalLongitude",
                                                                              "station", "year", "month","organismQuantity","concentration", "detected")]))
  rownames(GOTeDNA_df) <- NULL
  return(GOTeDNA_df)
}
