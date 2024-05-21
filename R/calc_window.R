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

  df <- lapply(scaledprobs, function(x) {
    dplyr::filter(x, !!dplyr::ensym(taxon.level) %in% taxon.name)
  })

  df_thresh <- lapply(scaledprobs, function(x) {
    x |> dplyr::filter(
      !!dplyr::ensym(taxon.level) %in% taxon.name,
      fill >= thresh.value
    )
  })

  # selects only a single month >= threshold
  onemonth <- lapply(df_thresh, function(x) {
    x %>%
      dplyr::group_by(id) %>%
      dplyr::filter(
        dplyr::n() == 1
      ) %>%
      dplyr::ungroup()
  })

  # selects several months >= threshold
  multmonth <- mapply(
    function(x, y) {
      suppressMessages(
        dplyr::anti_join(x, y)
      ) %>%
        dplyr::group_by(id, consec = cumsum(c(1, diff(month) != 1))) #%>%
        #dplyr::mutate(
         # prev_ = dplyr::lag(month), # the previous item
          #next_ = dplyr::lead(month), # the next item
          #diff_y1 = dplyr::case_when( # ifelse
          #  month == next_ | month == prev_ ~ 0,
          #  TRUE ~ month - prev_
          #)
      #  )
    },
    df_thresh,
    onemonth,
    SIMPLIFY = FALSE
  )


  #multmonth <- lapply(multmonth, function(x) {
  #  dplyr::select(x, c(-prev_, -next_))
  #})

  #multmonth <- lapply(multmonth, function(x) {
   # x$diff_y1[is.na(x$diff_y1)] <- 0
  #  x
  #})

  # selects species with consecutive months >= threshold
  consecmonth1 <- lapply(multmonth, function(x) {
    x %>%
      dplyr::group_by(id, consec = cumsum(c(1, diff(month) != 1))) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::ungroup()
      #dplyr::filter(diff_y1 == 1, diff_y1 )#length(id) == sum(diff_y1) + 1)
  })

  # selects species with consecutive months being December-January >= threshold
  consecmonth2 <- lapply(multmonth, function(x) {
    x %>%
      dplyr::group_by(id) %>%
      dplyr::filter(month %in% 1, month %in% 12) %>%
      dplyr::ungroup()
  })

  # all of the species with consecutive windows AND single month window
  consec.det <- mapply(dplyr::bind_rows, onemonth, consecmonth1, consecmonth2,
                       SIMPLIFY = FALSE) %>%
    lapply(unique)

  # create df where species have no discernible window (for now, this means more than one non-consecutive period)
  optwin <- lapply(consec.det, function(x) {
   x %>%
      dplyr::mutate(window = "inwindow")
  })

  inwindow <- lapply(optwin, function(x) {
    x %>%
      dplyr::group_by(id, window) %>%
      dplyr::summarise(
        detect = sum(detect, na.rm = TRUE),
        nondetect = sum(nondetect, na.rm = TRUE)
      )
  })

  # filter out all of the species that have a window from the dataset
  nowin <- mapply(
    function(x, y) {
      dplyr::anti_join(x, y, by = c("id", "month")) %>%
        dplyr::mutate(window = "outsidewindow")
    },
    df,
    optwin,
    SIMPLIFY = FALSE
  )

  outsidewindow <- lapply(nowin, function(x) {
    x %>%
      dplyr::group_by(id, window) %>%
      dplyr::summarise(
        detect = sum(detect, na.rm = TRUE),
        nondetect = sum(nondetect, na.rm = TRUE)
      )
  })

  window_sum <- mapply(
    function(x, y) {
      dplyr::bind_rows(x, y) %>% split(.$id)
    },
    inwindow,
    outsidewindow,
    SIMPLIFY = FALSE
  )

  both_in_out <- lapply(window_sum, function(x) {
    x[sapply(x, function(y) nrow(y) == 2)]
  })

  fshTest <- vector("list", 2L)
  names(fshTest) <- names(both_in_out)
  opt_sampling <- vector("list", 2L)
  names(opt_sampling) <- names(both_in_out)
  fshDF <- vector("list")

  # Fisher's exact test to compare detection probability within/outside the window for each species and primer
  for (var in names(both_in_out)) {
    if (length(both_in_out[[var]]) == 0) {
      warning("No optimal detection window")
      return(NULL)
    } else {
      # gives the period and length of optimal detection window

      opt_sampling[[var]] <- optwin[[var]] %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(
          len = length(month),
          thresh = threshold,
          period = ifelse(
            len == 1,
            paste(month.abb[month]),
                  ifelse(
            !is.na(!month %in% 11),
            paste0(month.abb[min(month)], "-", month.abb[max(month)]),
            paste0(month.abb[max(month)], "-", month.abb[max(month[month!=max(month)] )])
         )))

      for (i in names(both_in_out[[var]])) {
        f <- fisher.test(matrix(
          c(
            both_in_out[[var]][[i]]$detect[both_in_out[[var]][[i]]$window == "inwindow"],
            both_in_out[[var]][[i]]$detect[both_in_out[[var]][[i]]$window == "outsidewindow"],
            both_in_out[[var]][[i]]$nondetect[both_in_out[[var]][[i]]$window == "inwindow"],
            both_in_out[[var]][[i]]$nondetect[both_in_out[[var]][[i]]$window == "outsidewindow"]
          ),
          nrow = 2
        ))

        fshTest[[var]][[i]] <- data.frame(
          "odds ratio" = f$estimate,
          "p value" = scales::pvalue(f$p.value),
          "Lower CI" = f$conf.int[1],
          "Upper CI" = f$conf.int[2],
          check.names = FALSE
        )

        fshDF[[var]] <- do.call(
          dplyr::bind_rows, fshTest[[var]]
        )
        fshDF[[var]]$id <- names(fshTest[[var]]) }}}

  #fshDF2 <- lapply(fshDF, function(x)
  #  dplyr::bind_rows(x))

  fshDF_year <- dplyr::full_join(fshDF$Pscaled_year, opt_sampling$Pscaled_year, by = "id")
  fshDF_year[c("GOTeDNA_ID.v", "species", "primer", "year")] <- stringr::str_split_fixed(fshDF_year$id, ";", 4)
  fshDF_year <- dplyr::left_join(fshDF_year, unique(
    data[, c("domain", "kingdom", "phylum", "class", "order", "family", "genus", "species")]),
    by = "species",
    multiple = "first"
  ) %>%
    dplyr::mutate(confidence = dplyr::case_when(
      `p value` == "<0.001" ~ "Very high",
      `p value` < "0.01" ~ "High",
      `p value` < "0.05" ~ "Medium",
      `p value` >= "0.05" ~ "Low",
      is.na(`p value`) ~ "No optimal period"
    ))

  fshDF_month <- dplyr::full_join(fshDF$Pscaled_month, opt_sampling$Pscaled_month, by = "id")
  fshDF_month[c("GOTeDNA_ID.v", "species", "primer")] <- stringr::str_split_fixed(fshDF_month$id, ";", 3)
  fshDF_month <- dplyr::left_join(fshDF_month, unique(
    data[, c("domain", "kingdom", "phylum", "class", "order", "family", "genus", "species")]),
    by = "species",
    multiple = "first"
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

    #dplyr::select(-"id", -"mid_month") %>%
    #dplyr::select(-"odds ratio", -"p value", -"Lower CI", -"Upper CI", -"confidence", dplyr::everything())

  #fshDF_month$confidence <- factor(fshDF_month$confidence,
    #                               levels = c("Very high", "High", "Medium", "Low", "No optimal period"),
    #                               ordered = TRUE)

  list(fshDF_month = fshDF_month, fshDF_year = fshDF_year) %>%
    return()

}
