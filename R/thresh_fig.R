#' Display monthly detection probabilities for selected taxon, and
#' detection probability threshold
#'
#' @description This function displays detection probabilities in bins, with
#' probabilities over a specified threshold displayed in purple and under the
#' threshold in grey.
#'
#' @param taxon.level (required, data.frame): Select taxonomic level to view.
#' Choices = one of `c("phylum", "class", "order", "family", "genus", "species")`
#' @param taxon.name (required, character): Select taxon name that matches the level
#' provided in `taxon.level`. E.g., if `taxon.level = "genus"` enter genus name, etc.
#' @param threshold (required, character): Detection probability threshold for
#' which data are to be displayed to visualize potential optimal detection windows.
#' Choices = one of `"50","55","60","65","70","75","80","85","90","95")`
#' @param scaledprobs_month  (required, data.frame) Normalized detection
#' probabilities as returned by the element `month` of the list returned by
#' [scale_newprob()].
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname thresh_fig
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(gotedna_data$metabarcoding)
#' scaledprobs <- scale_newprob(gotedna_data$metabarcoding, newprob)
#' thresh_fig(
#'   taxon.level = "species", taxon.name = "Acartia hudsonica",
#'   threshold = "90", scaledprobs$Pscaled_month
#' )
#' }
thresh_fig <- function(
    taxon.level = c("domain", "kingdom", "phylum", "class", "order", "family", "genus", "species"),
    taxon.name,
    threshold,
    scaledprobs) {

  taxon.level <- match.arg(taxon.level)

  thresh_slc <- as.character(seq(50, 95, 5))
  threshold <- match.arg(arg = threshold, choices = thresh_slc)
  thresh <- data.frame(
    values = paste0("thresh", thresh_slc),
    labels = thresh_slc
  )

  thresh.value <- switch(threshold,
                         thresh$values[thresh$labels == threshold]
  )

  scaledprobs %<>%
    dplyr::filter(!!dplyr::ensym(taxon.level) %in% taxon.name) %>%
    dplyr::group_by(GOTeDNA_ID.v, !!dplyr::ensym(taxon.level), month) %>%
    dplyr::summarise(
      nd = sum(detect, na.rm = TRUE),
      n = sum(detect, nondetect, na.rm = TRUE),
      fill = mean(fill, na.rm = TRUE),
      scaleP = mean(scaleP, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      thresh95 = (fill >= 0.94999) * 1,
      thresh90 = (fill >= 0.89999) * 1,
      thresh85 = (fill >= 0.84999) * 1,
      thresh80 = (fill >= 0.79999) * 1,
      thresh75 = (fill >= 0.74999) * 1,
      thresh70 = (fill >= 0.69999) * 1,
      thresh65 = (fill >= 0.64999) * 1,
      thresh60 = (fill >= 0.59999) * 1,
      thresh55 = (fill >= 0.54999) * 1,
      thresh50 = (fill >= 0.49999) * 1
    )

  plots = vector("list")

  for (proj in unique(scaledprobs$GOTeDNA_ID.v)) {
    plots[[proj]] <- with(scaledprobs[scaledprobs$GOTeDNA_ID.v %in% proj, ],

  ggplot2::ggplot() +
    ggplot2::geom_hline(
      mapping = ggplot2::aes(yintercept = y),
      data.frame(y = c(0:4) / 4),
      color = "darkgrey"
    ) +
    ggplot2::geom_vline(
      mapping = ggplot2::aes(xintercept = x),
      data.frame(x = 1:12),
      color = "lightgrey"
    ) +
    ggplot2::geom_col(
      dplyr::filter(scaledprobs, GOTeDNA_ID.v %in% proj & !!dplyr::ensym(thresh.value) %in% "1"),
      mapping = ggplot2::aes(x = month, y = fill), fill = viridis::viridis(1), position = "dodge2",
      width = 0.9, show.legend = FALSE, alpha = .9#, fill = viridis::viridis(1)
    ) +
    ggplot2::geom_col(
      dplyr::filter(scaledprobs, GOTeDNA_ID.v %in% proj & !!dplyr::ensym(thresh.value) %in% "0"),
      mapping = ggplot2::aes(x = month, y = fill), fill = "darkgrey", position = "dodge2",
      width = 0.9, show.legend = FALSE, alpha = .9#, fill = "darkgrey"
    ) +
    # To make the interpolated data stand out
    ggpattern::geom_col_pattern(
      dplyr::filter(scaledprobs, GOTeDNA_ID.v %in% proj & is.nan(scaleP)),
      mapping = ggplot2::aes(x = month, y = fill),
      position = "dodge2",
      width = 0.9, show.legend = FALSE, fill = NA,
      pattern_color = "white", pattern_density = 0.05, pattern_spacing = 0.015, pattern_key_scale_factor = 0.6
    ) +
    ggplot2::coord_polar() +
    ggplot2::scale_x_continuous(
      limits = c(0.5, 12.5),
      breaks = 1:12,
      labels = month.abb
    ) +
    ggplot2::labs(
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal() +
    theme_circle
    )
  #  ggplot2::theme(
     # panel.grid = ggplot2::element_blank(),
    #  axis.title.y = ggplot2::element_text(hjust = 1, vjust = 2),
     # plot.background = ggplot2::element_rect(fill = "white", colour = NA)
   # )
  }
  return(plots)
}
