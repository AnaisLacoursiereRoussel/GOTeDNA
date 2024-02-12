#' Calculate detection probabilities from data that were previously imported
#' with read_data()
#'
#' @description Function that calculates non-parametric probability of detection
#' for each species and primer for the selected ecodistrict. Probabilities are
#' calculated both (1) monthly, across all years; and (2) monthly with each year
#' separate. Outputs are used in subsequent functions.
#'
#' @param data (required, data.frame) Data.frame read in with [read_data()].
#'
#' @return Two lists, each with distinct elements of species; primer ID
#' containing 5-7 columns:
#' * `month`
#' * `n` total number of samples per month
#' * `nd` number of positive detections per month
#' * `p` calculated monthly detection probability
#' * `s` standard deviation
#' * `GOTeDNA_ID`
#' * `year`
#' * `yr.mo` year;month concatenated
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname calc_det_prob
#' @export
#' @examples
#' \dontrun{
#'  D_mb <- read_data(
#'    choose.method = "metabarcoding", path.folder = "./inst/testdata"
#'  )
#'  calc_det_prob(data = D_mb_ex)
#' }
calc_det_prob <- function(data) {
  oop <- options("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  # reset option on exit
  on.exit(options(dplyr.summarise.inform = oop))

  data %<>%
    dplyr::mutate(.,
      id = if (!all(is.na(target_subfragment))) paste0(GOTeDNA_ID, ";", scientificName, ";", target_subfragment) else paste0(GOTeDNA_ID, ";", scientificName, ";", target_gene),
      id.yr = if (!all(is.na(target_subfragment))) paste0(GOTeDNA_ID, ";", scientificName, ";", target_subfragment, ";", year) else paste0(GOTeDNA_ID, ";", scientificName, ";", target_gene, ";", year)
    )
  # Create a variable so detection probability is calculated separately for each GOTeDNA_ID, species, and primer

  # create new list variables to store outputs
  lnd <- length(unique(data$id))
  newP <- vector("list", lnd)
  names(newP) <- unique(data$id)
  SUM <- COM <- comps <- vector("list", length(unique(data$id)))
  names(COM) <- unique(data$id)

  # calculate detection probabilities - year aggregated
  for (species in unique(data$id)) {
    SDF <- data %>%
      dplyr::filter(id == species) %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(
        n = dplyr::n(),
        nd = sum(detected),
        p = nd/n,
        s = sqrt(p * (1 - p)/n)
      ) %>%
      as.data.frame()

    if (any(SDF$n > 1)) {
      newP[[species]] <- SDF
    }
  }
  newP_agg <- newP[lengths(newP) != 0]

  # calculate monthly detection probability for each year
  lny <- length(unique(data$id.yr))
  newP <- vector("list", lny)
  names(newP) <- unique(data$id.yr)
  SUM <- COM <- comps <- vector("list", lny)
  names(COM) <- unique(data$id.yr)

  for (species in unique(data$id.yr)) {
    SDF <- data %>%
      dplyr::filter(id.yr == species) %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(
        n = dplyr::n(),
        nd = sum(detected),
        ecodistrict = unique(ecodistrict),
        p = nd/n,
        s = sqrt(p * (1 - p)/n)
      ) %>%
      as.data.frame()

    if (any(SDF$n > 1)) {
      newP[[species]] <- SDF
    }
  }
  newP <- newP[lengths(newP) != 0]
  newP_yr <- Map(cbind, newP, id.yr = names(newP))

  newP_yr <- lapply(newP_yr, function(x) {
    dplyr::mutate(x,
      GOTeDNA_ID = stringr::word(x$id.yr, 1, sep = stringr::fixed(";")),
      year = stringr::word(x$id.yr, -1, sep = stringr::fixed(";")),
      yr.mo = paste0(year, ";", month),
      id.yr = NULL
    )
  }) # to obtain variation among years

  list(newP_agg = newP_agg, newP_yr = newP_yr)
}
