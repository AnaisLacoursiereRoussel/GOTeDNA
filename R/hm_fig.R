#' Displays heatmap of scaled monthly detection probabilities for taxon
#'
#' @description This function displays scaled species detection probabilities
#' of the chosen taxonomic group with probabilities produced with
#' [scale_newprob()]. All primers that are available in the data are
#' displayed.
#' NOTE: interpolated `(i.e., missing)` data are not used in this
#' representation.
#'
#' @param taxon.level (required, character): Select taxonomic level to view.
#' @param taxon.name (required, character): Select taxon name that matches the
#' level provided in `taxon.level`. E.g., if `taxon.level = "genus"` enter
#' genus name, etc.
#' @param scaledprobs  (required, data.frame) Normalized detection
#' probabilities as returned by [scale_newprob()].
#'
#' @rdname hm_fig
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(D_mb_ex)
#' scaledprobs <- scale_newprob(D_mb_ex, newprob)
#' hm_fig(taxon.level = "class", taxon.name = "Copepoda", scaledprobs)
#' }
hm_fig <- function(
    taxon.level = c("phylum", "class", "order", "family", "genus", "species"),
    taxon.name, scaledprobs) {
  taxon.level <- match.arg(taxon.level)

  data <- scaledprobs$Pscaled_month[
    scaledprobs$Pscaled_month[[taxon.level]] %in% c(taxon.name),
  ]


  ggplot2::ggplot(
    data,
    ggplot2::aes(x = month, y = reorder(species, dplyr::desc(species)))
  ) +
    ggplot2::geom_rect(
      xmin = 1, xmax = 1.1, ymin = 1, ymax = 1.1,
      linewidth = 0, ggplot2::aes(colour = "No data")
    ) + # this is to create the "No data" legend entry, but if there's another way then perfect
    ggplot2::geom_tile(ggplot2::aes(fill = scaleP)) +
    ggh4x::facet_nested_wrap(~primer, ncol = 1, scales = "free") +
    # ggh4x::force_panelsizes(rows=)+ It would be nice to have all the rows the same size, but I'm not sure if there's a way
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0, 1), na.value = "lightgrey") +
    #   ggplot2::scale_y_discrete(expand=c(0,0))+
    ggplot2::scale_x_continuous(breaks = 1:12, labels = month.abb) +
    ggplot2::labs(
      fill = "Normalized \nDetection \nProbability", x = NULL, y = NULL,
      title = "Species monthly normalized detection probability by primer \n(years combined)",
      subtitle = paste0(stringr::str_to_title(taxon.level), ": ", taxon.name),
      colour = NULL
    ) +
    ggplot2::scale_colour_manual(values = NA) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(order = 1),
      colour = ggplot2::guide_legend(override.aes = list(colour = NULL, fill = "lightgrey"), order = 2)
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face = "italic"),
      legend.spacing = grid::unit(0, "cm")
    )
}
