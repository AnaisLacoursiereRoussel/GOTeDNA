#' Calculate variation in optimal window among years
#'
#' @description This function calculates the Jaccard Similarity Index to provide
#' a measure of annual variation in species optimal eDNA detection window at the
#' desired detection threshold. The window is defined as consecutive months ≥
#' the threshold and executes a Fisher exact test comparing the detection
#' probability within and outside the window.
#'
#' @param scaledprobs (required, list) Normalized detection probabilities
#'  aggregated per month and year [scale_newprob()].
#' @param threshold (required, character): Detection probability threshold for
#' which data are to be displayed to visualize potential optimal detection
#' windows.
#' Choices = one of `c("50","55","60","65","70","75","80","85","90","95")`
#'
#' @return A data.frame with 10 columns:
#' * `species`
#' * `wt_mean` Weighted mean Jaccard Similarity Index.
#' * `primer`
#' * `kingdom`
#' * `phylum`
#' * `class`
#' * `order`
#' * `family`
#' * `genus`
#' * `GOTeDNA_ID`

#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname jaccard_test
#' @export
#' @examples
#' \dontrun{
#' jaccard.test(scaledprobs, threshold = "75")
#' }
jaccard_test <- function(scaledprobs, threshold) {

  thresh_slc <- seq(50, 95, 5) %>% as.character()
  threshold <- match.arg(threshold, choices = thresh_slc)
  thresh <- data.frame(values = 0.49999 + seq(0, 0.45, 0.05),
                       labels = thresh_slc)

  thresh.value <- thresh$values[thresh$labels == threshold]

  data <- scaledprobs %>%
    dplyr::filter(!is.na(year)) %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise(
      detect = sum(detect, na.rm = TRUE),
      n = sum(detect, nondetect, na.rm = TRUE),
      fill = mean(fill, na.rm = TRUE)
    )

  df <- data %>%
      dplyr::mutate(
        fill_bin = case_when(
          fill < thresh.value ~ NA,
          fill >= thresh.value ~ 1
      )) %>%
      tidyr::pivot_wider(id_cols = year,
                         names_from = month,
                         values_from = fill_bin) %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames(var="year") %>%
      as.matrix()

    n_year = data %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(n = sum(n, na.rm=TRUE))


 # df = df[purrr::map(df, nrow) > 1]

  n_mths <- df %>%
      as.data.frame() %>%
  #    mutate(year = rownames(.)) %>%
      mutate(
        n_mths = select(., 1:12) %>%
               rowSums(na.rm = TRUE)) %>%
      select(#year,
             n_mths)

  if (nrow(df) > 1) {

  compare = t(combn(nrow(df), 2, FUN=function(x) df[x[1],] == df[x[2],]))

  } else {
    compare = NA
  }

  if (all(is.na(compare)) == FALSE ){

  n_union = data.frame(
      n_union = combn(nrow(n_mths), 2, FUN=function(x)
        max(sum(df[x[1],], na.rm = TRUE),
            sum(df[x[2],], na.rm = TRUE))),
      n_wt = combn(nrow(n_year), 2, FUN=function(x)
        sum(n_year[x[1],"n"], n_year[x[2],"n"]))
    )

    rownames(compare) = combn(nrow(df), 2, FUN=function(x) paste0("year",x[1],"_year",x[2]))

    jacc_df <- compare %>%
      as.data.frame() %>%
      mutate(group = rownames(.)) %>%
      rowwise() %>%
      mutate(
        n_int = #select(., V1:V12) %>%
          rowSums(pick(V1:V12), na.rm = TRUE)) %>%
      select(group, n_int) %>%
      cbind(n_union) %>%
      mutate(
        J_index = n_int/n_union,
        J_wt = J_index*n_wt
      )

  jacc_wt_mean <- jacc_df %>%
    dplyr::summarise(
      wt_mean = sum(J_wt)/sum(n_wt) *100
    ) %>%
    dplyr::mutate(
      wt_text = dplyr::case_when(
        between(wt_mean, 0, 9.99) == TRUE ~ "Very low",
        between(wt_mean, 10, 29.99) == TRUE ~ "Low",
        between(wt_mean, 30, 69.99) == TRUE ~ "Medium",
        between(wt_mean, 70, 89.99) == TRUE ~ "High",
        between(wt_mean, 90, 100) == TRUE ~ "Very high"
      )
    )

  return(jacc_wt_mean$wt_text)
  } else {
    return(paste("Multi-year data not available"))
  }


}

