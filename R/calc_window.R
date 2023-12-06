#' Calculate species optimal detection window
#'
#' @description This function calculates species optimal eDNA detection period
#' `(i.e., months)` and window `(i.e., length/number of months)` at the
#' desired detection threshold. The window is defined as consecutive months ≥
#' the threshold and executes a Fisher exact test comparing the detection
#' probability within and outside the window. Output provides the odds ratio,
#' p-value, and confidence interval for each primer of the species selected
#' `(i.e., input = Pscaled_agg)` or each primer and year `(i.e., Pscaled_yr)`.
#'
#' @param data (required, data.frame) Data.frame read in with [read_data()]
#' @param ecodistrict.select (required, character) Ecodistrict present in data.frame.
#' @param threshold (required, character): Detection probability threshold for
#' which data are to be displayed to visualize potential optimal detection
#' windows.
#' Choices = one of `c("50","55","60","65","70","75","80","85","90","95")`
#' @param show.variation (required, character) Choices = `c("within_year", "among_years")`
#' When “within_year” is chosen, input will be Pscaled_agg; when “among_years”
#' is chosen, input will be Pscaled_yr.
#' @param species.name (required, character): Full binomial species name.
#'
#' @return A data.frame with 17 columns:
#' * `ecodistrict`
#' * `length` Number of months with optimal detection `(i.e., over
#' the specified threshold)`.
#' * `threshold` Detection probability threshold specified in function call.
#' * `period` Range of months having optimal detection.
#' * `GOTeDNA_ID`
#' * `species`
#' * `primer`
#' * `phylum`
#' * `class`
#' * `order`
#' * `family`
#' * `genus`
#' * `odds ratio`
#' * `p value`
#' * `Lower CI`
#' * `Upper CI`
#' * `confidence` Provided such that p value < 0.001 = Very high;
#' 0.001 < p < 0.01 = High; 0.01 < p < 0.05 = Medium; p >= 0.05 = Low.
#'
#' @author Melissa Morrison \email{Melissa.Morrison@@dfo-mpo.gc.ca}
#' @author Tim Barrett \email{Tim.Barrett@@dfo-mpo.gc.ca}
#' @rdname calc_window
#' @export
#' @examples
#' \dontrun{
#' calc_window(
#'   data = D_mb_ex, ecodistrict.select = "Bay of Fundy", threshold = "90",
#'   show.variation = "within_year", species.name = "Nucula proxima"
#' )
#' }
calc_window <- function(
    data, ecodistrict.select, threshold,
    show.variation = c("within_year", "among_years"), species.name) {
  oop <- options("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  # reset option on exit
  on.exit(options(dplyr.summarise.inform = oop))

  show.variation <- match.arg(show.variation)

  var.lst <- list(
    "within_year" = Pscaled_agg,
    "among_years" = Pscaled_yr
  )

  var.df <- dplyr::filter(var.lst[[show.variation]], species %in% species.name)

  if (!species.name %in% var.df$species) {
    stop("Species not found in data")
  }

  if (!ecodistrict.select %in% var.df$ecodistrict) {
    stop("Ecodistrict not found in data")
  }

  thresh_slc <- seq(50, 95, 5) %>% as.character()
  if (!is.null(threshold)) {
    threshold <- match.arg(
      arg = threshold,
      choices = thresh_slc,
      several.ok = FALSE
    )
  }

  thresh <- data.frame(
    values = c(
      0.49999, 0.54999, 0.59999, 0.64999, 0.69999,
      0.74999, 0.79999, 0.84999, 0.89999, 0.94999
    ),
    labels = thresh_slc
  )

  thresh.value <- switch(threshold,
    thresh$values[thresh$labels == threshold]
  )

  # selects species with only a single month >= threshold
  onemonth <- var.df[var.df$fill >= thresh.value, ] %>%
    dplyr::filter(dplyr::n() == 1)

  # selects species with several months >= threshold
  multmonth <- suppressMessages(
    dplyr::anti_join(var.df[var.df$fill >= thresh.value, ], onemonth)
  ) %>%
    dplyr::mutate(
      prev_ = dplyr::lag(month), # the previous item
      next_ = dplyr::lead(month), # the next item
      diff_y1 = dplyr::case_when( # ifelse
        month == next_ | month == prev_ ~ 0,
        TRUE ~ month - prev_
      )
    )

  multmonth <- multmonth[, !names(multmonth) %in% c("prev_", "next_")]
  multmonth$diff_y1[is.na(multmonth$diff_y1)] <- 0

  # selects species with consecutive months >= threshold
  consecmonth1 <- multmonth %>%
    dplyr::filter(length(id) == sum(diff_y1) + 1) # %>%

  # selects species with consecutive months being December-January >= threshold
  # keep rows with diff_y1 = 11, because that could mean Dec/Jan are consecutive
  # if diff_y1 = 11, keep the row before it.
  # if diff_y1 = 1, keep row before
  consecmonth2 <- multmonth %>%
    dplyr::filter(length(id) == 2 & sum(diff_y1) == 11) # %>%

  # all of the species with consecutive windows AND single month window
  consec.det <- dplyr::bind_rows(onemonth, consecmonth1, consecmonth2)

  # create df where species have no discernible window (for now, this means more than one non-consecutive period)
  optwin <- consec.det[, !names(consec.det) %in% "diff_y1"] %>%
    dplyr::mutate(window = "inwindow")

  inwindow <- optwin %>%
    dplyr::group_by(id, ecodistrict, window) %>%
    dplyr::summarise(
      detect = sum(detect, na.rm = TRUE),
      nondetect = sum(nondetect, na.rm = TRUE)
    )

  # filter out all of the species that have a window from the dataset
  nowin <- var.df %>%
    dplyr::anti_join(optwin, by = c("id", "month")) %>%
    dplyr::mutate(window = "outsidewindow")

  outsidewindow <- nowin %>%
    dplyr::group_by(id, ecodistrict, window) %>%
    dplyr::summarise(
      detect = sum(detect, na.rm = TRUE),
      nondetect = sum(nondetect, na.rm = TRUE)
    )

  window_sum <- dplyr::bind_rows(inwindow, outsidewindow) %>%
    split(.$id)

  both_in_out <- window_sum[sapply(window_sum, function(x) nrow(x) == 2)]

  # Fisher's exact test to compare detection probability within/outside the window for each species and primer
  fshTest <- vector("list", length(names(both_in_out)))
  names(fshTest) <- names(both_in_out)

  if (length(both_in_out) != 0) {
    # gives the period and length of optimal detection window
    opt_sampling <- optwin %>%
      dplyr::summarise(
        id = unique(id),
        ecodistrict = unique(ecodistrict),
        length = length(month),
        threshold = threshold,
        period = paste0(month.abb[min(month)], "-", month.abb[max(month)]),
        mid_month = median(month)
      )

    for (i in names(both_in_out)) {
      f <- fisher.test(matrix(
        c(
          both_in_out[[i]]$detect[both_in_out[[i]]$window == "inwindow"],
          both_in_out[[i]]$detect[both_in_out[[i]]$window == "outsidewindow"],
          both_in_out[[i]]$nondetect[both_in_out[[i]]$window == "inwindow"],
          both_in_out[[i]]$nondetect[both_in_out[[i]]$window == "outsidewindow"]
        ),
        nrow = 2
      ))
      fshTest[[i]] <- data.frame(
        "odds ratio" = f$estimate, "p value" = scales::pvalue(f$p.value),
        "Lower CI" = f$conf.int[1], "Upper CI" = f$conf.int[2],
        check.names = FALSE
      )
    }

    fshDF <- do.call(rbind, fshTest)

    fshDF$id <- rownames(fshDF)
    fshDF <- dplyr::full_join(fshDF, opt_sampling, by = "id")

    if (lengths(strsplit(fshDF$id[1], ";")) == 4) {
      fshDF[c("GOTeDNA_ID", "species", "primer", "year")] <- stringr::str_split_fixed(fshDF$id, ";", 4)
    } else {
      fshDF[c("GOTeDNA_ID", "species", "primer")] <- stringr::str_split_fixed(fshDF$id, ";", 3)
    }

    fshDF <- fshDF %>%
      dplyr::left_join(unique(data[, c("phylum", "class", "order", "family", "genus", "scientificName")]),
        by = c("species" = "scientificName"),
        multiple = "first"
      ) %>%
      dplyr::mutate(confidence = dplyr::case_when(
        `p value` == "<0.001" ~ "Very high",
        `p value` < "0.01" ~ "High",
        `p value` < "0.05" ~ "Medium",
        `p value` >= "0.05" ~ "Low",
        is.na(`p value`) == TRUE ~ "No optimal period"
      ))

    fshDF <- fshDF %>%
      dplyr::select(-"id", -"mid_month") %>%
      dplyr::select(-"odds ratio", -"p value", -"Lower CI", -"Upper CI", -"confidence", dplyr::everything())

    return(fshDF)
  } else {
    return("No optimal detection window")
  }
}
