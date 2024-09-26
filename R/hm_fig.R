#' Displays heatmap of scaled monthly detection probabilities for taxon
#'
#' @description This function displays scaled species detection probabilities
#' of the chosen taxonomic group with probabilities produced with
#' [scale_newprob()]. All primers that are available in the data are
#' displayed.
#' NOTE: interpolated `(i.e., missing)` data are not used in this
#' representation.
#'
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
#' hm_fig(scaledprobs)
#' }
hm_fig <- function(
    scaledprobs) {

  df <- scaledprobs %>%
    dplyr::filter(!is.na(year)) %>%
    dplyr::rename("Detection rate" = "scaleP") %>%
    dplyr::mutate(species = reorder(species, dplyr::desc(species)))

  df$Month <- factor(df$month,
                       levels = 1:12,
                       labels = c("Jan","Feb","Mar","Apr","May",
                                  "Jun","Jul","Aug","Sep","Oct",
                                  "Nov","Dec"))
  ggplot2::ggplot(
  ) +
    ggplot2::geom_tile(dplyr::filter(df, `Detection rate` > 0 | is.na(`Detection rate`)),
                       mapping = ggplot2::aes(x = Month,
                                              y = reorder(year, dplyr::desc(year)),
                                              fill = `Detection rate`)) +
    ggplot2::geom_tile(dplyr::filter(df, `Detection rate` == 0),
                       mapping = ggplot2::aes(x = Month,
                                              y = reorder(year, dplyr::desc(year))),
                       fill = "lightgrey", inherit.aes = FALSE)+
    ggplot2::facet_wrap(~species,
                        ncol = 1) +
    ggplot2::scale_fill_viridis_c(direction = -1, limits = c(0.00001, 1), na.value = "white",
                                  guide = NULL) +
    ggplot2::scale_x_discrete(expand = c(0,0)) +
    ggplot2::scale_y_discrete(expand = c(0,0)) +
    ggplot2::labs(
      fill = NULL, x = NULL, y = NULL) +
    ggplot2::scale_colour_manual(values = "white") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, colour = "lightgrey"),
      panel.grid = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(20, "pt"),
      plot.title = ggplot2::element_text(face = "bold",
                                         size = 30,
                                         hjust = 0,
                                         colour = "#5A5A5A",
                                         margin = ggplot2::margin(b = 20,
                                                                  unit = "pt")),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 20,
                                         colour = "#5A5A5A"),
      axis.text.x = ggplot2::element_text(
        size = 20, colour = "#939598"),
      axis.text.y = ggplot2::element_text(size = 20, colour = "#939598")
    )
}
