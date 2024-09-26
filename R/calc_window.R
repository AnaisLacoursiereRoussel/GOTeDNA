#' Calculate species optimal detection window
#'
#' @description This function calculates species optimal eDNA detection period
#' `(i.e., months)` and window `(i.e., length/number of months)` at the
#' desired detection threshold. The window is defined as consecutive months ≥
#' the threshold and executes a Fisher exact test comparing the detection
#' probability within and outside the window. Output provides the odds ratio,
#' p-value, and confidence interval for each primer of the species selected,
#' showing the variation within year and among years, if applicable
#' `(i.e., input = Pscaled_agg and Pscaled_yr)`.
#'
#' @param threshold (required, character): Detection probability threshold for
#' which data are to be displayed to visualize potential optimal detection
#' windows.
#' Choices = one of `c("50","55","60","65","70","75","80","85","90","95")`
#' @param scaledprobs (required, list) Normalized detection probabilities
#'  aggregated per month and year [scale_newprob()].
#'
#' @return A list of two data.frames with 16 to 17 columns:
#' * `length` Number of months with optimal detection `(i.e., over
#' the specified threshold)`.
#' * `threshold` Detection probability threshold specified in function call.
#' * `period` Range of months having optimal detection.
#' * `species`
#' * `odds ratio`
#' * `p value`
#' * `Lower CI`
#' * `Upper CI`
#' * `confidence` Provided such that p value < 0.001 = Very high;
#' 0.001 < p < 0.01 = High; 0.01 < p < 0.05 = Medium; p >= 0.05 = Low.
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname calc_window
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(data = D_mb)
#' scaledprobs <- scale_newprob(D_mb, newprob)
#' calc_window(threshold = "75", scaledprobs, id = "2")
#' }
calc_window <- function(threshold, scaledprobs) {
  oop <- options("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  # reset option on exit
  on.exit(options(dplyr.summarise.inform = oop))

  thresh_slc <- seq(50, 95, 5) %>% as.character()
  threshold <- match.arg(threshold, choices = thresh_slc)
  thresh <- data.frame(
    values = 0.49999 + seq(0, 0.45, 0.05),
    labels = thresh_slc
  )

  thresh.value <- thresh$values[thresh$labels == threshold]

  df <- scaledprobs %>%
    dplyr::filter(is.na(year)) %>%

    dplyr::group_by(month) %>%
    dplyr::summarise(
      nd = sum(det_int, na.rm = TRUE),
      n = sum(det_int, nd_int, na.rm = TRUE),
      fill = mean(fill, na.rm = TRUE),
      scaleP = mean(scaleP, na.rm = TRUE)
    )


  df_thresh <- df %>%
    dplyr::filter(
      fill >= thresh.value
    )

  # selects only a single month >= threshold
  multmonth <- df_thresh %>%
    dplyr::group_by(
      consec = cumsum(c(1, diff(month) != 1)))


  # selects species with consecutive months being December-January >= threshold
  if (length(unique(multmonth$consec)) > 1) {
    if (any(multmonth$month == 12) & any(multmonth$month == 1)) {
      consecmonth <- multmonth[order(multmonth$consec, decreasing = TRUE),]
      # all of the species with consecutive windows AND single month window
    } else {

      consecmonth <- NULL
    }

    consec.det <- consecmonth

  } else {

    # selects species with consecutive months >= threshold
    consecmonth1 <- multmonth %>%
      dplyr::group_by(
        consec) %>%
      dplyr::ungroup()

    consec.det <- consecmonth1
  }

  # create df where species have no discernible window (for now, this means more than one non-consecutive period)
  if(!is.null(consec.det)){
  optwin <- consec.det %>%
    dplyr::mutate(window = "inwindow")

  inwindow <- optwin %>%
    dplyr::group_by(
      window) %>%
    dplyr::summarise(
      detect = sum(nd, na.rm = TRUE),
      nondetect = sum(n, na.rm = TRUE)
    )

  # filter out all of the species that have a window from the dataset
  nowin <- dplyr::anti_join(df, optwin, by = "month") %>%
    dplyr::mutate(window = "outsidewindow")

  outsidewindow <- nowin %>%
    dplyr::group_by(
      window) %>%
    dplyr::summarise(
      detect = sum(nd, na.rm = TRUE),
      nondetect = sum(n, na.rm = TRUE)
    )

  window_sum <- dplyr::bind_rows(inwindow, outsidewindow)

  # Fisher's exact test to compare detection probability within/outside the window for each species and primer
  if (nrow(window_sum) == 2) {

    opt_sampling <- optwin %>%
        dplyr::summarise(
          len = length(month),
          thresh = unique(threshold),
          period = unique(dplyr::case_when(
            len == 1 ~ paste(month.abb[month]),
            len != 1 ~ paste0(month.abb[dplyr::first(month)], "-",
                              month.abb[dplyr::last(month)]))
          ))

      f <- fisher.test(matrix(
        c(
          window_sum$detect[window_sum$window == "inwindow"],
          window_sum$detect[window_sum$window == "outsidewindow"],
          window_sum$nondetect[window_sum$window == "inwindow"],
          window_sum$nondetect[window_sum$window == "outsidewindow"]
        ),
        nrow = 2
      ))

      fshTest <- data.frame(
        "odds ratio" = f$estimate,
        "p value" = scales::pvalue(f$p.value),
        "Lower CI" = f$conf.int[1],
        "Upper CI" = f$conf.int[2],
        check.names = FALSE
      ) %>%
        dplyr::mutate(
          confidence = factor(
            dplyr::case_when(
              `p value` == "<0.001" ~ "Very high",
              `p value` < "0.01" ~ "High",
              `p value` < "0.05" ~ "Medium",
              `p value` >= "0.05" ~ "Low",
              is.na(`p value`) ~ "No optimal period"),
            levels = c("Very high", "High", "Medium", "Low", "No optimal period"),
            ordered = TRUE))

    return(
      list(
      opt_sampling = opt_sampling,
      fshTest = fshTest))

      }


  } else {
    warning("No optimal detection window")
    return(NULL)

  }



}
