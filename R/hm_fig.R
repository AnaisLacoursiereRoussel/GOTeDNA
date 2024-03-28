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
#' @param scaledprobs (required, data.frame) Normalized detection
#' probabilities as returned by [scale_newprob()].
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname hm_fig
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(D_mb)
#' scaledprobs <- scale_newprob(D_mb, newprob)
#' hm_fig(taxon.level = "class", taxon.name = "Copepoda", scaledprobs)
#' }
hm_fig <- function(
    taxon.level = c("phylum", "class", "order", "family", "genus", "species"),
    taxon.name, scaledprobs) {
  taxon.level <- match.arg(taxon.level)

  data <- scaledprobs$Pscaled_year[
    scaledprobs$Pscaled_year[[taxon.level]] %in% c(taxon.name),
  ]

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = month, y = reorder(year, dplyr::desc(year)))
  ) +
    ggplot2::geom_rect(
      xmin = 0.1, xmax = 0.2, ymin = 0.1, ymax = 0.2,
      linewidth = 0, ggplot2::aes(colour = "No data")
    ) + # this is to create the "No data" legend entry, but if there's another way then perfect
    ggplot2::geom_tile(dplyr::filter(data, scaleP > 0),
                       mapping = ggplot2::aes(x = month,
                                              y = reorder(year, dplyr::desc(year)),
                                              fill = scaleP)) +
    ggplot2::geom_tile(dplyr::filter(data, scaleP == 0),
                       mapping = ggplot2::aes(x = month,
                                              y = reorder(year, dplyr::desc(year)),
                                              alpha = "Not detected"),
                       fill = "lightgrey")+
    ggh4x::facet_grid2(species ~ primer,
                       scales = "free",
                       space = "free",
                       switch = "y",
                       strip = ggh4x::strip_nested(clip = "off",
                                            size = "variable")) +
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0.00001, 1), na.value = "white") +
    ggplot2::scale_alpha_manual(name = NULL,
                                values = c(1))+
    ggplot2::scale_x_continuous(breaks = 1:12,
                                labels = month.abb,
                                expand = c(0,0)) +
    ggplot2::scale_y_discrete(expand = c(0,0)) +
    ggplot2::labs(
      fill = "Normalized \ndetection \nprobability", x = NULL, y = NULL,
      subtitle = paste0(stringr::str_to_title(taxon.level), ": ", taxon.name),
      colour = NULL, alpha = NULL
    ) +
    ggplot2::scale_colour_manual(values = "white") +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(order = 1),
      alpha = ggplot2::guide_legend(override.aes = list(colour = "lightgrey",
                                                        fill = "lightgrey"),
                                    order = 2),
      colour = ggplot2::guide_legend(override.aes = list(colour = "black",
                                                         fill = "white"),
                                     order = 3)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, colour = "lightgrey"),
      panel.grid = ggplot2::element_blank(),
      legend.spacing = ggplot2::unit(0, "cm"),
      legend.position = "bottom",
      legend.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
      legend.justification = "left",
      strip.placement = "outside",
      strip.background = ggplot2::element_rect(fill = "grey95", colour = "white"),
      strip.text.y.left = ggplot2::element_text(angle = 0),
      axis.text.x = ggplot2::element_text(angle = 30, vjust = 1, hjust = 1, size = 8)
    ) +
   ggh4x::force_panelsizes(rows = ggplot2::unit(2, "cm"))
}
