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
#' @param species.name (required, character): Full binomial species name.
#' @param scaledprobs (required, list) Normalized detection probabilities
#'  aggregated per month and year [scale_newprob()].
#'
#' @return A data.frame with 16 columns:
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
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname calc_window
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(data = D_mb_ex)
#' scaledprobs <- scale_newprob(D_mb_ex, newprob)
#' calc_window(data = D_mb_ex, threshold = "90",
#'  species.name = "Acartia longiremis", scaledprobs)
#' }
calc_window <- function(data, threshold, species.name, scaledprobs) {
  oop <- options("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  # reset option on exit
  on.exit(options(dplyr.summarise.inform = oop))

  # if (! all(species.name %in% scaledprobs[[1]]$species)) {
  #   stop("Species not found in data")
  # }

  thresh_slc <- seq(50, 95, 5) %>% as.character()
  threshold <- match.arg(threshold, choices = thresh_slc)
  thresh <- data.frame(
    values = 0.49999 + seq(0, 0.45, 0.05),
    labels = thresh_slc
  )
  thresh.value <- thresh$values[thresh$labels == threshold]

  df <- lapply(scaledprobs, function(x) {
    dplyr::filter(x, species %in% species.name)
  })

  df_thresh <- lapply(scaledprobs, function(x) {
    x |> dplyr::filter(
      species %in% species.name,
        fill >= thresh.value
    )
  })

  # selects only a single month >= threshold
  onemonth <- lapply(df_thresh, function(x) {
    x %>%
      dplyr::filter(
        dplyr::n() == 1
      )
  })

  # selects several months >= threshold
  multmonth <- mapply(
    function(x, y) {
      suppressMessages(
        dplyr::anti_join(x, y)
      ) %>%
        dplyr::mutate(
          prev_ = dplyr::lag(month), # the previous item
          next_ = dplyr::lead(month), # the next item
          diff_y1 = dplyr::case_when( # ifelse
            month == next_ | month == prev_ ~ 0,
            TRUE ~ month - prev_
          )
        )
    },
    df_thresh,
    onemonth,
    SIMPLIFY = FALSE
  )

  multmonth <- lapply(multmonth, function(x) {
    dplyr::select(x, c(-prev_, -next_))
  })

  multmonth <- lapply(multmonth, function(x) {
    x$diff_y1[is.na(x$diff_y1)] <- 0
    x
  })

  # selects species with consecutive months >= threshold
  consecmonth1 <- multmonth %>%
    lapply(function(x) {
      dplyr::filter(x, length(x$id) == sum(x$diff_y1) + 1)
    })

  # selects species with consecutive months being December-January >= threshold
  # keep rows with diff_y1 = 11, because that could mean Dec/Jan are consecutive
  # if diff_y1 = 11, keep the row before it.
  # if diff_y1 = 1, keep row before
  consecmonth2 <- multmonth %>%
    lapply(function(x) {
      dplyr::filter(x, length(id) == 2 & sum(diff_y1) == 11)
    })

  # all of the species with consecutive windows AND single month window
  consec.det <- mapply(dplyr::bind_rows, onemonth, consecmonth1, consecmonth2,
    SIMPLIFY = FALSE)

  # create df where species have no discernible window (for now, this means more than one non-consecutive period)
  optwin <- lapply(consec.det, function(x) {
    dplyr::select(x, -diff_y1) %>%
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


  # Fisher's exact test to compare detection probability within/outside the window for each species and primer
  for (var in names(both_in_out)) {
    if (length(both_in_out[[var]]) != 0) {
      # gives the period and length of optimal detection window
      fshTest <- vector("list", 2L)
      names(fshTest) <- names(both_in_out)
      opt_sampling <- vector("list")

      opt_sampling[[var]] <- optwin[[var]] %>%
        dplyr::summarise(
          id = unique(id),
          length = length(month),
          threshold = threshold,
          period = paste0(month.abb[min(month)], "-", month.abb[max(month)]),
          mid_month = median(month)
        )

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

        fshDF <- vector("list")

        fshDF[[var]] <- do.call(
          dplyr::bind_rows, fshTest[[var]][[i]]
        )
        fshDF[[var]]$id <- names(fshTest[[var]])
        fshDF2 <- do.call(dplyr::bind_rows, fshDF[[var]])

        if (lengths(strsplit(fshDF2$id[1], ";")) == 4) {
          fshDF3 <- dplyr::full_join(fshDF2, opt_sampling$Pscaled_year, by = "id")

          fshDF3[c("GOTeDNA_ID", "species", "primer", "year")] <- stringr::str_split_fixed(fshDF3$id, ";", 4)
        } else {
          fshDF3 <- dplyr::full_join(fshDF2, opt_sampling$Pscaled_month, by = "id")

          fshDF3[c("GOTeDNA_ID", "species", "primer")] <- stringr::str_split_fixed(fshDF3$id, ";", 3)
        }
      }

      fshDF4 <- fshDF3 %>%
        dplyr::left_join(unique(data[, c("phylum", "class", "order", "family", "genus", "scientificName")]),
          by = c("species" = "scientificName"),
          multiple = "first"
        ) %>%
        dplyr::mutate(confidence = dplyr::case_when(
          `p value` == "<0.001" ~ "Very high",
          `p value` < "0.01" ~ "High",
          `p value` < "0.05" ~ "Medium",
          `p value` >= "0.05" ~ "Low",
          is.na(`p value`) ~ "No optimal period"
        ))

      fshDF4 <- fshDF4 %>%
        dplyr::select(-"id", -"mid_month") %>%
        dplyr::select(-"odds ratio", -"p value", -"Lower CI", -"Upper CI", -"confidence", dplyr::everything())

      return(fshDF4)
    } else {
      warning("No optimal detection window")
      return(NULL)
    }
  }
}
