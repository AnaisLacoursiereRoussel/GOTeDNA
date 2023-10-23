# read_edna-------------------------------------------------------------------
#' @title read_edna
#' @description Function that read several MS Excel files found in a specified folder.
#' When \code{qpcr = TRUE}, the function will search for MS Excel files with
#' \code{qPCR_data} in the name. Then it reads the metadata (sheet 2) and the
#' samples (sheet 3). Merge it together based on \code{EVENT_ID}.

#' @param path.folder (optional, character)
#' Default: \code{path.folder = NULL}. Default will use the working directory.

#' @param qpcr (optional, logical) Read qPCR data.
#' Default: \code{qpcr = TRUE}.

#' @return A tidy dataframe (tibble) with 15 columns:
#' \itemize{
#' \item\code{PROJECT_ID},
#' \item\code{SAMPLE_ID},
#' \item\code{DATA_QPCR_FILES}: added internally to keep track of files origins,
#' \item\code{METADATA_QPCR_FILES}: added internally to keep track of files origins,
#' \item\code{ECOREGION},
#' \item\code{KINGDOM},
#' \item\code{PHYLUM},
#' \item\code{CLASS},
#' \item\code{ORDER},
#' \item\code{FAMILY},
#' \item\code{GENUS},
#' \item\code{SCIENTIFIC_NAME}: e.g.: Homarus americanus,
#' \item\code{SPP}: e.g.: H. americanus,
#' \item\code{YEAR},
#' \item\code{MONTH},
#' \item\code{DETECTED}
#' }

#' @details
#' The function looks for MS Excel files with \code{qPCR_data} in the filename.
#' In sheet 2 (the metadata information), the required and read columns are:
#' \code{"projectID", "sampleID", "eventID", "ecoregion", "samplingSite", "eventDate"}.
#' In sheet 2 (the samples and results), the required and read columns are:
#' \code{"eventID", "scientificName", "kingdom", "phylum", "class", "order",
#' "family", "genus", "occurrenceStatus"}.
#'



#' @examples
#' \dontrun{
#' data <- read_edna(path.folder = "optimal_eDNA_data", qpcr = TRUE)
#' }

#' @author Thierry Gosselin \email{thierrygosselin@@icloud.com} and Melissa Morrison \email{Melissa.Morrison@@dfo-mpo.gc.ca}
#' @rdname read_edna
#' @export

read_edna <- function(path.folder = NULL, qpcr = TRUE) {

  if (is.null(path.folder)) path.folder <- getwd()

  # read qpcr files ------------------------------------------------------------
  if (qpcr) {

    # Find the files
    qpcr.files <- list.files(
      path = path.folder,
      all.files = FALSE,
      pattern = "qPCR_data",
      full.names = FALSE)

    n.files <- length(qpcr.files)
    if (n.files < 1) {
      rlang::abort("No qPCR file(s) were found in the path.folder argument")
    } else {
      message("Found ", n.files, "file(s): \n", paste0(qpcr.files, collapse = "\n"))
    }

    # To keep track of the files imported in the tibble
    names(qpcr.files) <- qpcr.files

    # Peek at column names in the first file found
    nms <- names(readxl::read_excel(path = qpcr.files[[1]], sheet = 2, n_max = 0))
    want <- c("projectID", "sampleID", "eventID", "ecoregion", "samplingSite", "eventDate")

    # check that the columns required are found in the file
    check.col <- purrr::keep(.x = nms, .p = nms %in% want)
    if (length(check.col) < 6) {
      em <- c("Required columns in MS Excel files, sheet 2: ", paste0(want, collapse = ", "))
      rlang::abort(em)
    }
    check.col <- em <- NULL

    col.types <- c("numeric", "text", "text", "text", "text", "date")
    discard <- purrr::keep(.x = nms, .p = !nms %in% want)
    replace.discard <- rep_len(x = "skip", length.out = length(discard))
    new.cols.types <-  nms %>%
      stringi::stri_replace_all_fixed(str = ., pattern = want, replacement = col.types, vectorize_all = FALSE) %>%
      stringi::stri_replace_all_fixed(str = ., pattern = discard, replacement = replace.discard, vectorize_all = FALSE)


    # additional mod to column header
    # Personal preferences that mostly avoir confusion and coding error:
    # Columns header in ALL CAPS allows to distinguish them easily in codes.
    # SNAKE_CASE over camelCase just for the sake of making less error while coding.
    # The worst is when camelCase and normal case is mixed in coding...

    # Example: Ct_mean, Ct_SD, CtMean, CtSD, etc.

    want.mod <- c("projectID" =  "PROJECT_ID", "sampleID" = "SAMPLE_ID",
                  "eventID" = "EVENT_ID",
                  "ecoregion" = "ECOREGION", "samplingSite" = "SITE",
                  "eventDate" = "DATE")

    # the tibble is named based on the file
    metadata <- suppressWarnings(
      purrr::map(
        .x = qpcr.files,
        .f = readxl::read_excel,
        sheet = 2,
        col_names = want.mod,
        col_types = new.cols.types,
        na = c(" ", NA),
        skip = 1
      ) %>%
        purrr::list_rbind(x = ., names_to = "METADATA_QPCR_FILES") %>%
        dplyr::filter(!is.na(DATE)) %>%
        dplyr::select(PROJECT_ID, EVENT_ID, SAMPLE_ID, DATE, ECOREGION, SITE, METADATA_QPCR_FILES)
    )

    # no longer needed
    want.mod <- new.cols.types <- replace.discard <- discard <- col.types <- want <- nms <- NULL


    # check
    if (max(unique(lubridate::month(x = metadata$DATE)), na.rm = TRUE) > 12) {
      rlang::abort("Error parsing date entered in eventDate column")
    }


    # Here we are using a different strategy because if you skip the column name to
    # read the data, some columns don'T have data and will generate errors while importing.

    want <- c("eventID", "scientificName", "kingdom", "phylum", "class", "order", "family", "genus", "occurrenceStatus", "DATA_QPCR_FILES")

    samples <-  purrr::map(
      .x = qpcr.files,
      .f = readxl::read_excel,
      sheet = 3,
      col_types = "text",
      na = c(" ", NA)
    ) %>%
      purrr::list_rbind(x = ., names_to = "DATA_QPCR_FILES") %>%
      dplyr::select(dplyr::any_of(want)) %>%
      dplyr::rename_with(.fn = replace_to_snakecase)

    want <- NULL
    message("\n\nFiles read: \n", paste0(qpcr.files, collapse = "\n"))

    # Merge metadata and samples
    # prepare and clean the data

    data <- samples %>%
      dplyr::left_join(metadata, by = "EVENT_ID") %>%
      dplyr::filter(!is.na(DATE)) %>%
      dplyr::mutate(
        DETECTED = dplyr::case_when(
          OCCURRENCE_STATUS != "Not detected" ~ 1,
          OCCURRENCE_STATUS == "Not detected" ~ 0
        ),
        SPECIES = stringr::word(SCIENTIFIC_NAME,-1),
        SPP = paste0(substr(SCIENTIFIC_NAME, 1,1), '.'," ",
                     stringr::word(SCIENTIFIC_NAME,-1)),
        YEAR = lubridate::year(DATE),
        YEAR = factor(x = YEAR, levels = 2017:2022, ordered = TRUE),
        MONTH = lubridate::month(DATE),
        MONTH = factor(
          x = MONTH,
          levels = 1:12,
          labels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
          ordered = TRUE
        ),
        ECOREGION = stringi::stri_replace_all_fixed( # clean ecoregion
          str = ECOREGION,
          pattern = c("-18", "-17", "-16", "-12"),
          replacement = c("", "", "", ""),
          vectorize_all = FALSE),
        ECOREGION = factor(
          ECOREGION,
          levels = c("Grand Banks", "Magdalen Shallows", "Scotian Shelf", "Bay of Fundy"),
          ordered = TRUE),
        PROJECT_ID = factor(PROJECT_ID)
      ) %>%
      dplyr::select(
        PROJECT_ID, SAMPLE_ID, DATA_QPCR_FILES, METADATA_QPCR_FILES, ECOREGION,
        KINGDOM, PHYLUM, CLASS, ORDER, FAMILY, GENUS, SCIENTIFIC_NAME, SPP,
        DATA_QPCR_FILES, YEAR, MONTH, DETECTED)
  }
  return(data)

}#End read_edna
