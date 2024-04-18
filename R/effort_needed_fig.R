#' Calculate sampling effort needed to obtain different detection thresholds.
#'
#' @description This function calculates number of samples needed to obtain
#' species detection at different thresholds by using scaled and interpolated
#' data produced with `[scale_newprob()]`.
#'
#' @param scaledprobs_month  (required, data.frame) Normalized detection
#' probabilities as returned by the element `month` of the list returned by
#' [scale_newprob()] for one species and one primer.
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname effort_needed_fig
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(D_mb_ex)
#' scaledprobs <- scale_newprob(D_mb_ex, newprob)
#' effort_needed_fig(
#'   scaledprobs$Pscaled_month |> dplyr::filter(
#'       species %in% "Acartia hudsonica",
#'       primer == "COI1"
#'   )
#' )
#' }
effort_needed_fig <- function(scaledprobs_month) {

  species.name <- unique(scaledprobs_month$species)
  stopifnot(length(unique(scaledprobs_month$species)) == 1)

  primer.select <- unique(scaledprobs_month$primer)
  stopifnot(length(primer.select) == 1)

  if (!primer.select %in% scaledprobs_month$primer) {
    cli::cli_alert_warning("Primer not found in data -- cannot render figure")
    return(NULL)
  }

  DF2 <- expand.grid(p = scaledprobs_month$fill, n = seq_len(10), P = NA)
  DF2 <- DF2 |>
    merge(
      data.frame(p = scaledprobs_month$fill, month = scaledprobs_month$month)
    )

  for (i in seq_len(nrow(DF2))) {
    DF2$P[i] <- 1 - dbinom(0, size = DF2$n[i], prob = DF2$p[i]) # 1 - probability of zero detects
  }

  ggplot2::ggplot(DF2, ggplot2::aes(y = P, x = n, colour = as.factor(month))) +
    ggplot2::geom_point(size = 5) +
    ggplot2::theme_classic(base_size = 24) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::scale_y_continuous("Detection probability",
      limits = c(0, 1),
      expand = c(0, 0.01)
    ) +
    ggplot2::scale_x_continuous(
      #expand = c(0, 0),
      limits = c(1, 10),
      breaks = 1:10
    ) +
    ggplot2::scale_colour_viridis_d(
      direction = -1,
      breaks = 1:12,
      labels = month.abb
    ) +
    ggplot2::labs(
      colour = "Month",
      title = species.name,
      subtitle = paste("Primer:", primer.select),
      x = "Number of samples"
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(
      label.position = "left",
      label.hjust = 1
    )) +
    ggplot2::theme(
      # plotting components

      ## drop minor gridlines
      panel.grid = ggplot2::element_blank(),
      # change grid lines to gray
      #  panel.grid.major =  element_line(color = "#d0d0d0"),
      # fill the plot and panel spaces with grey and remove border
      #  panel.background = element_blank(),
      # plot.background = element_blank(),
      panel.border = ggplot2::element_blank(),
      # remove strip background
      strip.background = ggplot2::element_blank(),
      # adjust the margins of plots and remove axis ticks
      plot.margin = ggplot2::margin(0.5, 1, 0.5, 1,
                                    unit = "cm"),
      axis.ticks = ggplot2::element_line(
        linewidth = 0.1,
        colour = "#939598"),
      # change text family, size, and adjust position of titles
      text = ggplot2::element_text(
        family = "Arial", size = 24),
      axis.text = ggplot2::element_text(
        colour = "#939598", size = 20),
      axis.title = ggplot2::element_text(colour = "#5A5A5A",
                                         size = 24),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(0.5, 0, 0, 0, unit = "cm"),
        hjust = 0),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(b = 0.66,
                                 unit = "cm"),
        hjust = 0),
      axis.line = ggplot2::element_line(
        linewidth = 0.1,
        colour = "#939598"),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 30,
        hjust = 0,
        colour = "#5A5A5A"),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(size = 24,
                                            margin = ggplot2::margin(b = 0.66, unit = "cm"),
                                            colour = "#5A5A5A",
                                            hjust = 0),
      legend.title.align = 1,
      legend.text = ggplot2::element_text(size = 20,
                                          colour = "#939598"),
      legend.position = "right",
      legend.box.just = "right",
      legend.key.spacing.y = ggplot2::unit(20, "pt"),
      legend.spacing.y = ggplot2::unit(20, "pt"),
      legend.title = ggplot2::element_text(colour = "#5A5A5A",
                                           margin = ggplot2::margin(b = 20))

    )


}
