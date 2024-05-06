#' Sort primer choices by percent rank detection in data request window.
#'
#' @description This function sorts the available primers by percent success
#' in detecting the selected taxa to allow selection of the optimal primer.
#' Ingests data produced with `[scale_newprob()]`.
#'
#' @param scaledprobs_month (required, data.frame): Normalized detection.
#' @param taxon.level taxonomic level.
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname primer_sort
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(gotedna_data$metabarcoding)
#' scaledprobs <- scale_newprob(gotedna_data$metabarcoding, newprob)
#' primer_sort("phylum", scaledprobs$Pscaled_month)
#' }
primer_sort <- function(
    taxon.level = c("domain", "kingdom", "phylum", "class", "order", "family", "genus", "species"),
    scaledprobs_month) {
  # However the filtering happens (choosing the taxon level/name, region, and
  # threshold), I'm not sure how to code that. The following code calculates
  # the percent rank of each primer for the taxon level selected, user could
  # then have the option of selecting all primers or a specific primer.
  out <- scaledprobs_month |>
    dplyr::group_by(dplyr::across(dplyr::all_of(taxon.level)), primer) |>
    dplyr::summarise(
      success = sum(scaleP > 0.74449, na.rm = TRUE),
      total = sum(!is.na(nondetect)),
      perc = round(success / total * 100)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(taxon.level)), -total, -perc) # sort by total number of samples first,
  # and then by percent success

  out
}
