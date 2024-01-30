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
#' @param scaledprobs (required, data.frame) Normalized detection
#' probabilities as returned by [scale_newprob()].
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname thresh_fig
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(D_mb_ex)
#' scaledprobs <- scale_newprob(D_mb_ex, newprob)
#' thresh_fig(
#'   taxon.level = "species", taxon.name = "Acartia hudsonica",
#'   threshold = "90", scaledprobs
#' )
#' }
thresh_fig <- function(taxon.level, taxon.name, threshold, scaledprobs) {
  taxon.level <- match.arg(
    arg = taxon.level,
    choices = c("phylum", "class", "order", "family", "genus", "species")
  )

  thresh_slc <- as.character(seq(50, 95, 5))
  threshold <- match.arg(arg = threshold, choices = thresh_slc)
  thresh <- data.frame(
    values = paste0("thresh", thresh_slc),
    labels = thresh_slc
  )

  Pthresh <- scaledprobs$Pscaled_month |>
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
    ) |>
    tidyr::drop_na(fill)

  Pthresh[Pthresh$phylum == "Chordata", ] %>%
    tidyr::drop_na(class)

  thresh.value <- switch(threshold,
    thresh$values[thresh$labels == threshold]
  )
  data <- Pthresh[Pthresh[[taxon.level]] %in% taxon.name, ]

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
      data[data[[thresh.value]] %in% "1", ],
      mapping = ggplot2::aes(x = month, y = fill), position = "dodge2",
      width = 0.9, show.legend = TRUE, alpha = .9, fill = viridis::viridis(1)
    ) +
    ggplot2::geom_col(
      data[data[[thresh.value]] %in% "0", ],
      mapping = ggplot2::aes(x = month, y = fill), position = "dodge2",
      width = 0.9, show.legend = TRUE, alpha = .9, fill = "darkgrey"
    ) +
    # To make the interpolated data stand out
    ggpattern::geom_col_pattern(
      dplyr::filter(data, is.na(scaleP)),
      mapping = ggplot2::aes(x = month, y = fill),
      position = "dodge2",
      width = 0.9, show.legend = TRUE, fill = NA,
      pattern_color = "white", pattern_density = 0.05, pattern_spacing = 0.015, pattern_key_scale_factor = 0.6
    ) +
    ggplot2::coord_polar() +
    ggplot2::facet_wrap(~ GOTeDNA_ID + primer + species,
      ncol = 2,
      labeller = function(x) {
        x[2]
      }
    ) + # will need to italicize the species name, but it would also be good if the name didn't show up if there is only one species to facet (if executing at higher taxonomy level [i.e., anything except species])
    ggplot2::scale_x_continuous(
      limits = c(0.5, 12.5),
      breaks = 1:12,
      labels = month.abb
    ) +
    ggplot2::labs(
      x = NULL, y = "Normalized detection probability",
      title = scientific_name_formatter(taxon.name),
      subtitle = paste0("Detection threshold: ", threshold, "%")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(hjust = 1, vjust = 2),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}
