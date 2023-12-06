#' Normalize detection probabilities with min-max method (range 0-1) by year
#' and month
#'
#' @description This function normalizes (i.e., scales) monthly detection 
#' probabilities for each species and primer that were calculated with the 
#' previous function, `calc_det_prob()` Outputs fed into figure and window 
#' calculation functions.
#' * NOTE: Currently this function only works for metabarcoding data.
#'
#' @param data (required, data.frame) Data.frame imported with [read_data()].
#' Required to join taxonomic information.
#' @param ecodistrict.select (required, character) Ecodistrict present in data.frame.
#' @param newP_agg (required, list) detection probabilities aggregated per month
#' [calc_det_prob()].
#'
#' @return Grouped data.frame with 15 columns:
#' * `id` unique species;primer identifier
#' * `ecodistrict`
#' * `month`
#' * `detect` number of detections
#' * `nondetect` number of non-detections
#' * `scaleP` detection probability scaled to range 0-1
#' * `GOTeDNA_ID`
#' * `species`
#' * `primer`
#' * `phylum`
#' * `class`
#' * `order`
#' * `family`
#' * `genus`
#' * `fill`
#'
#' @author Tim Barrett \email{Tim.Barrett@@dfo-mpo.gc.ca}
#' @author Melissa Morrison \email{Melissa.Morrison@@dfo-mpo.gc.ca}
#' @rdname scale_prob_by_month
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(D_mb_ex, "Scotian Shelf")
#' scale_prob_by_month(
#'   data = D_mb_ex, ecodistrict.select = "Scotian Shelf",
#'   newprob$newP_agg
#' )
#' }
scale_prob_by_month <- function(data, ecodistrict.select, newP_agg) {
  if (!ecodistrict.select %in% data$ecodistrict) {
    stop("Ecodistrict not found in data")
  }
  # Calling the data in is to merge taxonomic info
  data %<>%
    dplyr::filter(., ecodistrict %in% ecodistrict.select)

  # transform CPscaled to data frame
  CPscaled <- lapply(newP_agg, function(x) {
    data.frame(x) %>%
      dplyr::mutate(x, scaleP = dplyr::case_when(
        p == 1 ~ 1,
        p == 0 ~ 0,
        p != 0 | 1 ~ scale_min_max(x$p) # kc: worth checking
      ))
  })
  DF <- do.call(rbind, CPscaled)
  DF <- DF |>
    dplyr::mutate(
      id = rep(names(CPscaled), each = 12),
      detect = nd,
      nondetect = n - nd
    ) |>
    dplyr::select(id, ecodistrict, month, detect, nondetect, scaleP) |>
    dplyr::tibble()
  row.names(DF) <- NULL

  DF[c("GOTeDNA_ID", "species", "primer")] <- stringr::str_split_fixed(DF$id, ";", 3)
  DF <- DF %>%
    dplyr::left_join(unique(data[, c("phylum", "class", "order", "family", "genus", "scientificName")]),
      by = c("species" = "scientificName"),
      multiple = "first"
    )

  # Interpolate missing months
  DF$fill <- NA # add column

  for (species in unique(DF$id)) {
    DF1 <- DF[DF$id == species, ]

    # then add code for interpolation that starts with DF2 = .....
    # add dataframe above and below to help will fills for jan and dec. Needed to have 4 copyies because of max function used below
    DF2 <- rbind(
      cbind(DF1, data.frame(G = 1)),
      cbind(DF1, data.frame(G = 2)),
      cbind(DF1, data.frame(G = 3)),
      cbind(DF1, data.frame(G = 4))
    )

    DF2$fill <- DF2$scaleP

    # which months are NA and define groups with sequential NAs
    month_na_id <- which(is.na(DF2$scaleP))
    nagroups <- cumsum(c(1, abs(month_na_id[-length(month_na_id)] - month_na_id[-1]) > 1))

    # identify which NA groups are in G = 2 or 3 (ignore 1 and 2)
    nagroupsG <- list()
    for (i in unique(nagroups)) {
      nagroupsG[[i]] <- max(DF2$G[month_na_id[which(nagroups == i)]])
    }
    nagroupsGv <- unlist(nagroupsG)
    nagroupsf <- which(nagroupsGv %in% 2:3)

    # loop over final NA groups and fill in using average
    for (i in unique(nagroupsf)) {
      DF2$fill[month_na_id[which(nagroups == i)]] <- (DF2$scaleP[min(month_na_id[which(nagroups == i)]) - 1] + DF2$scaleP[max(month_na_id[which(nagroups == i)]) + 1]) / 2
    }
    # then put values from DF3 back into DF$sp.pr. This assumes that the months are all in the correct order (jan to dec) in DF3 and test_interp
    # DF3 is final DF with fills
    DF3 <- DF2[DF2$G == 2, ]
    DF$fill[DF$id == species] <- DF3$fill
  }

  Pscaled_agg <- DF

  return(Pscaled_agg)
}
