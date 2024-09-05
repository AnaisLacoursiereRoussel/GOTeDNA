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
#' @param data (required, data.frame) Data.frame read in with [read_data()].
#' @param threshold (required, character): Detection probability threshold for
#' which data are to be displayed to visualize potential optimal detection
#' windows.
#' Choices = one of `c("50","55","60","65","70","75","80","85","90","95")`
#' @param taxon.level (required, character): Taxonomic selection.
#' @param taxon.name (required, character): Full taxon name.
#' @param scaledprobs (required, list) Normalized detection probabilities
#'  aggregated per month and year [scale_newprob()].
#'
#' @return A list of two data.frames with 16 to 17 columns:
#' * `length` Number of months with optimal detection `(i.e., over
#' the specified threshold)`.
#' * `threshold` Detection probability threshold specified in function call.
#' * `period` Range of months having optimal detection.
#' * `year` Sampling year, only present in calculating variation in separate years.
#' * `GOTeDNA_ID.v`
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
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname calc_window
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(data = D_mb)
#' scaledprobs <- scale_newprob(D_mb, newprob)
#' calc_window(data = D_mb, threshold = "75",
#'  taxon.name = "Acartia longiremis", scaledprobs)
#' }
calc_window <- function(data, threshold, taxon.level, taxon.name, scaledprobs) {
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
    dplyr::filter({{ taxon.level }} %in% taxon.name,
                  is.na(year)) %>%

    dplyr::group_by(GOTeDNA_ID,
                    {{ taxon.level }},
                    month) %>%
    dplyr::summarise(
      nd = sum(detect, na.rm = TRUE),
      n = sum(detect, nondetect, na.rm = TRUE),
      fill = mean(fill, na.rm = TRUE),
      scaleP = mean(scaleP, na.rm = TRUE)
    )


  df_thresh <- df %>%
    dplyr::filter(
      # as.name(taxon.level) %in% taxon.name,
      fill >= thresh.value
    )

  # selects only a single month >= threshold
  onemonth <- df_thresh %>%
    dplyr::filter(
      dplyr::n() == 1
    ) %>%
    dplyr::ungroup()


  # selects several months >= threshold
  multmonth <- dplyr::anti_join(df_thresh, onemonth) %>%
    dplyr::group_by(
      GOTeDNA_ID, {{ taxon.level }},
      consec = cumsum(c(1, diff(month) != 1)))

  # selects species with consecutive months >= threshold
  consecmonth1 <- multmonth %>%
    dplyr::group_by(GOTeDNA_ID,
                    consec = cumsum(c(1, diff(month) != 1))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()


  # selects species with consecutive months being December-January >= threshold
  consecmonth2 <- multmonth %>%
    dplyr::group_by(GOTeDNA_ID) %>%
    dplyr::filter(month %in% 1, month %in% 12) %>%
    dplyr::ungroup()


  # all of the species with consecutive windows AND single month window
  consec.det <- dplyr::bind_rows(onemonth, consecmonth1, consecmonth2) %>%
    unique()

  # create df where species have no discernible window (for now, this means more than one non-consecutive period)
  optwin <- consec.det %>%
    dplyr::mutate(window = "inwindow")

  inwindow <- optwin %>%
    dplyr::group_by(GOTeDNA_ID, window) %>%
    dplyr::summarise(
      detect = sum(nd, na.rm = TRUE),
      nondetect = sum(n, na.rm = TRUE)
    )

  # filter out all of the species that have a window from the dataset
  nowin <- dplyr::anti_join(df, optwin, by = c("GOTeDNA_ID", "month")) %>%
    dplyr::mutate(window = "outsidewindow")

  outsidewindow <- nowin %>%
    dplyr::group_by(GOTeDNA_ID, window) %>%
    dplyr::summarise(
      detect = sum(nd, na.rm = TRUE),
      nondetect = sum(n, na.rm = TRUE)
    )

  window_sum <- dplyr::bind_rows(inwindow, outsidewindow)

  # both_in_out <- lapply(window_sum, function(x) {
  #  x[sapply(x, function(y) nrow(y) == 2)]
  #})

  # Fisher's exact test to compare detection probability within/outside the window for each species and primer
  if (nrow(window_sum) == 2) {

    window_ls <- split(window_sum, ~GOTeDNA_ID)

    fshTest <- vector("list", length(window_ls))

    names(fshTest) <- names(window_ls)
    opt_sampling <- split(optwin, ~GOTeDNA_ID)

    for (id in names(window_ls)) {

      # gives the period and length of optimal detection window

      opt_sampling[[id]] <- opt_sampling[[id]] %>%
        dplyr::group_by({{ taxon.level }}) %>%
        dplyr::summarise(
          len = length(month),
          thresh = unique(threshold),
          period = unique(dplyr::case_when(
            len == 1 ~ paste(month.abb[month]),
            len != 1 ~ paste0(month.abb[min(month)], "-",
                              month.abb[max(month)]),
            month %in% c(1,12) & len != 12 ~ paste0(month.abb[max(month)],"-",
                                                    month.abb[min(month)]))
            #ifelse(
            #  !is.na(!month %in% 11),
            # paste0(month.abb[min(month)], "-", month.abb[max(month)]),
            #paste0(month.abb[max(month)], "-", month.abb[max(month[month!=max(month)] )])
          ))

      f <- fisher.test(matrix(
        c(
          window_ls[[id]]$detect[window_ls[[id]]$window == "inwindow"],
          window_ls[[id]]$detect[window_ls[[id]]$window == "outsidewindow"],
          window_ls[[id]]$nondetect[window_ls[[id]]$window == "inwindow"],
          window_ls[[id]]$nondetect[window_ls[[id]]$window == "outsidewindow"]
        ),
        nrow = 2
      ))



      fshTest[[id]] <- data.frame(
        "odds ratio" = f$estimate,
        "p value" = scales::pvalue(f$p.value),
        "Lower CI" = f$conf.int[1],
        "Upper CI" = f$conf.int[2],
        check.names = FALSE
      )
    }
    fshDF <- dplyr::bind_rows(fshTest[[id]])

    fshDF$GOTeDNA_ID <- names(fshTest)


    fshDF_final <- fshDF %>%
      dplyr::full_join(
        dplyr::bind_rows(
          opt_sampling,
          .id = "GOTeDNA_ID"),
        by = c("GOTeDNA_ID"), multiple = "first") %>%

      dplyr::left_join(
        data[, c("domain", "kingdom", "phylum", "class", "order",
                 "family", "genus", "species")],
        by = taxon.level,
        multiple = "first") %>%
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

    return(fshDF_final)

  } else {
    warning("No optimal detection window")
    return(NULL)

  }



}
