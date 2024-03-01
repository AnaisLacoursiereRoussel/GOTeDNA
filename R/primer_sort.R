#' Sort primer choices by percent rank detection in data request window.
#'
#' @description This function sorts the available primers by percent success
#' in detecting the selected taxa to allow selection of the optimal primer.
#' Ingests data produced with `[scale_newprob()]`.
#'
#' @param scaledprobs_month (required, data.frame): Normalized detection.
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname primer_sort
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(D_mb_ex)
#' scaledprobs <- scale_newprob(D_mb_ex, newprob)
#' primer_sort(taxon.level, scaledprobs$Pscaled_month)
#' }
primer_sort <- function(
    taxon.level = c("phylum", "class", "genus", "species"),
    scaledprobs_month) {

  taxon.level <- dplyr::ensym(taxon.level)

  # However the filtering happens (choosing the taxon level/name, region, and
  # threshold), I'm not sure how to code that. The following code calculates
  # the percent rank of each primer for the taxon level selected, user could
  # then have the option of selecting all primers or a specific primer.
  data <- scaledprobs_month

  data %<>%
    dplyr::group_by(GOTeDNA_ID, !!taxon.level, primer) %>%
    dplyr::summarise(success = sum(scaleP > 0.74449, na.rm = TRUE),
                     total = sum(!is.na(nondetect)),
                     perc = round(success/total*100)) %>%
    dplyr::ungroup()

  data %>%
    dplyr::arrange(!!taxon.level, -perc)
}
