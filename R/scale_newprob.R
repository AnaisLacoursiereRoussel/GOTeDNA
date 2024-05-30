#' Normalize detection probabilities with min-max method (range 011) by month
#' for each year separately.
#'
#' @description This function normalizes (i.e., scales) monthly detection
#' probabilities for each species, primer, and year that were calculated with
#' the previous function, [calc_det_prob()]. Outputs fed into figure and window
#' calculation functions.
#' * NOTE: Currently this function only works for metabarcoding data.
#'
#' @param data (required, data.frame) Data.frame imported with [read_data()]. Required
#' to join taxonomic information.
#' @param newprob (required, list) detection probabilities aggregated per month and year
#' [calc_det_prob()].
#'
#' @return Grouped data.frame with 15 columns:
#' * `id`: unique species;primer;year identifier
#' * `month`:
#' * `detect`: number of detections
#' * `nondetect`: number of non-detections
#' * `scaleP`: detection probability scaled to range 0-1
#' * `GOTeDNA_ID`:
#' * `species`:
#' * `primer`:
#' * `year`:
#' * `phylum`:
#' * `class`:
#' * `order`:
#' * `family`:
#' * `genus`:
#' * `fill`:
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname scale_newprob
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(gotedna_data$metabarcoding, "Scotian Shelf")
#' scale_newprob(data = gotedna_data$metabarcoding, newprob)
#' }
scale_newprob <- function(data, newprob) {

  CPscaled <- lapply(newprob, function(x)
    lapply(x, function(y) {
      data.frame(y) |>
      dplyr::mutate(y, scaleP = dplyr::case_when(
        p == 1 ~ 1,
        p == 0 ~ 0,
        p != 0 | 1 ~ scale_prop(p)
      ))
  })
  )

  DFmo <- lapply(CPscaled$newP_agg, function(x) {
    out <- data.frame(
      month = 1:12,
      detect = NA_integer_,
      nondetect = NA_integer_,
      scaleP = NA_real_
    )
    out$detect[x$month] <- x$nd
    out$nondetect[x$month] <- x$n - x$nd
    out$scaleP[x$month] <- x$scaleP
    out
  }) |>
    do.call(what = rbind) |>
    dplyr::mutate(
      id = rep(names(CPscaled$newP_agg), each = 12)
    ) |>
    dplyr::select(id, month, detect, nondetect, scaleP) |>
    dplyr::tibble()
  row.names(DFmo) <- NULL

  DFmo[c("GOTeDNA_ID.v", "species", "primer")] <- stringr::str_split_fixed(DFmo$id, ";", 3)

  DFmo <- DFmo |>
    dplyr::left_join(
      unique(data[, c("domain", "kingdom", "phylum", "class", "order", "family", "genus", "species")]),
      by = c("species"),
      multiple = "first"
    )
  # Interpolate missing months
  DFmo$fill <- NA # add column

  for (species in unique(DFmo$id)) {
    DF1 <- DFmo[DFmo$id == species, ]

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
    DFmo$fill[DFmo$id == species] <- DF3$fill
  }

  # Pscaled_month <- DFmo %>%
  #  dplyr::ungroup()

  # scale and interpolate each month separately
  DFyr <- lapply(CPscaled$newP_yr, function(x) {
    out <- data.frame(
      month = 1:12,
      detect = NA_integer_,
      nondetect = NA_integer_,
      scaleP = NA_real_
    )
    out$detect[x$month] <- x$nd
    out$nondetect[x$month] <- x$n - x$nd
    out$scaleP[x$month] <- x$scaleP
    out
  }) |>
    do.call(what = rbind) |>
    dplyr::mutate(
      id = rep(names(CPscaled$newP_yr), each = 12)
    ) |>
    dplyr::select(id, month, detect, nondetect, scaleP) |>
    dplyr::tibble()
  row.names(DFyr) <- NULL

  DFyr[c("GOTeDNA_ID.v", "species", "primer", "year")] <- stringr::str_split_fixed(DFyr$id, ";", 4)

  DFyr <- DFyr |>
    dplyr::left_join(
      unique(data[, c("domain", "kingdom", "phylum", "class", "order", "family", "genus", "species")]),
                     by = "species",
                     multiple = "first"
    )

  # Interpolate missing months
  DFyr$fill <- NA # add column

  for (species in unique(DFyr$id)) {
    DF1 <- DFyr[DFyr$id == species, ]

    # then add code for interpolation that starts with DF2 = .....
    # add dataframe above and below to help will fills for jan and dec. Needed to have 4 copyies because of max fucntion used below
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
    # then put values from DF3 back into DF$id.sp.pr.yr. This assumes that the months are all in the correct order (jan to dec) in DF3 and test_interp
    # DF3 is final DF with fills
    DF3 <- DF2[DF2$G == 2, ]
    DFyr$fill[DFyr$id == species] <- DF3$fill
  }

  DFmo$year = NA
  scaledprobs = dplyr::bind_rows(DFmo, DFyr)
#  list(Pscaled_month = DFmo, Pscaled_year = DFyr)
  return(scaledprobs)
}

