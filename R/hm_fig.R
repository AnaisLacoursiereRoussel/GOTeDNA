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
    ggplot2::geom_tile(dplyr::filter(data, scaleP > 0),
                       mapping = ggplot2::aes(x = month,
                                              y = reorder(year, dplyr::desc(year)),
                                              fill = scaleP)) +
    ggplot2::geom_tile(dplyr::filter(data, scaleP == 0),
                       mapping = ggplot2::aes(x = month,
                                              y = reorder(year, dplyr::desc(year))),
                       fill = "lightgrey")+
    ggh4x::facet_wrap2(species ~ .,
                       scales = "free",
                       ncol = 1,
                       strip = ggh4x::strip_nested(clip = "off",
                                            size = "variable")) +
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0.00001, 1), na.value = "white",
                                  guide = NULL) +
    ggplot2::scale_x_continuous(breaks = 1:12,
                                labels = month.abb,
                                expand = c(0,0)) +
    ggplot2::scale_y_discrete(expand = c(0,0)) +
    ggplot2::labs(
      fill = NULL, x = NULL, y = NULL#,
     # title = ifelse(
      #  taxon.level != "species",
       # paste0(stringr::str_to_title(taxon.level), ": ", taxon.name),
        #""
      #  )
    ) +
    ggplot2::scale_colour_manual(values = "white") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, colour = "lightgrey"),
      panel.grid = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(25, "pt"),
      plot.title = ggplot2::element_text(face = "bold",
                                         size = 30,
                                         hjust = 0,
                                         colour = "#5A5A5A",
                                         margin = ggplot2::margin(b = 20, unit = "pt")),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 20, colour = "#5A5A5A", hjust = 0,
                                         margin = ggplot2::margin(b = 15, unit = "pt")),
      axis.text.x = ggplot2::element_text(
       # vjust = 1, hjust = 1,
        size = 20, colour = "#939598"),
      axis.text.y = ggplot2::element_text(size = 20, colour = "#939598")
    ) +
   ggh4x::force_panelsizes(rows = ggplot2::unit(100, "pt"),
                           total_width = ggplot2::unit(620, "pt"))


}
