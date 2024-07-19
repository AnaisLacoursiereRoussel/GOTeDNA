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
#' * `GOTeDNA_ID.v`

#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname jaccard_test
#' @export
#' @examples
#' \dontrun{
#' jaccard.test(data = D_mb, threshold = "75",
#'  taxon.name = "Acartia longiremis", scaledprobs)
#' }
jaccard_test <- function(scaledprobs, threshold) {

  thresh_slc <- seq(50, 95, 5) %>% as.character()
  threshold <- match.arg(threshold, choices = thresh_slc)
  thresh <- data.frame(values = 0.49999 + seq(0, 0.45, 0.05),
                       labels = thresh_slc)

  thresh.value <- thresh$values[thresh$labels == threshold]

  df = vector("list")
  n_year = vector("list")

  data <- scaledprobs %>%
    dplyr::filter(!is.na(year))

  for (i in unique(data$species)) {
    df[[i]] <- data[data$species %in% i,] %>%
      mutate(fill_bin = case_when(
        fill < thresh.value ~ NA,
        fill >= thresh.value ~ 1
      )) %>%
      tidyr::pivot_wider(id_cols = year,
                         names_from = month,
                         values_from = fill_bin) %>%

      remove_rownames %>% column_to_rownames(var="year") %>%
      as.matrix()

    n_year[[i]] = data[data$species %in% i,] %>%
      group_by(id) %>%
      dplyr::summarise(n = sum(detect+nondetect, na.rm=TRUE))
  }

  df = df[purrr::map(df, nrow) > 1]

  n_mths <- lapply(df, function(x) {
    x %>%
      as.data.frame() %>%
      mutate(year = rownames(.)) %>%
      rowwise() %>%
      mutate(
        n_mths = sum(c_across(1:12), na.rm = TRUE)) %>%
      select(year, n_mths)
  })

  compare = lapply(df, function(y) {
    t(combn(nrow(y), 2, FUN=function(x) y[x[1],] == y[x[2],]))
  })

  n_union = vector("list")
  jacc_lst = vector("list")

  for (i in names(df)) {
    n_union[[i]] = data.frame(
      n_union = combn(nrow(n_mths[[i]]), 2, FUN=function(x)
        max(sum(df[[i]][x[1],], na.rm = TRUE),
            sum(df[[i]][x[2],], na.rm = TRUE))),
      n_wt = combn(nrow(n_year[[i]]), 2, FUN=function(x)
        sum(n_year[[i]][x[1],"n"], n_year[[i]][x[2],"n"]))
    )

    rownames(compare[[i]]) = combn(nrow(df[[i]]), 2, FUN=function(x) paste0("year",x[1],"_year",x[2]))

    jacc_lst[[i]] <- compare[[i]] %>%
      as.data.frame() %>%
      mutate(group = rownames(.)) %>%
      rowwise() %>%
      mutate(
        n_int = sum(c_across(V1:V12), na.rm = TRUE)) %>%
      select(group, n_int) %>%
      cbind(n_union[[i]]) %>%
      mutate(
        J_index = n_int/n_union,
        J_wt = J_index*n_wt
      )

  }

  jacc_wt_mean <- jacc_lst %>%
    bind_rows(.id = "species") %>%
    dplyr::summarise(
      wt_mean = sum(J_wt)/sum(n_wt),
      .by = "species"
    ) %>%
    left_join(scaledprobs[c("species", "primer", "kingdom","phylum","class","order","family","genus","GOTeDNA_ID.v")], by = "species",
              multiple = "first")

  return(jacc_wt_mean)

}
