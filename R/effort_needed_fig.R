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
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_classic() +
    ggplot2::scale_y_continuous("Detection probability",
      limits = c(0, 1),
      expand = c(0, 0.01)
    ) +
    ggplot2::scale_x_continuous(
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
      title = scientific_name_formatter(species.name),
      subtitle = paste("Primer:", primer.select),
      x = "Number of samples"
    )
}
