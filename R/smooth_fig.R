#' Display species monthly detection probability by primer and region.
#'
#' @description This function displays the species optimal detection period as
#' monthly detection probabilities with LOESS smoothing over years.
#'
#' @param data (required, data.frame): Data.frame read in with [read_data()].
#' @param species.name (required, character): Full binomial species name.
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname smooth_fig
#' @export
#' @examples
#' \dontrun{
#' data <- D_mb_ex |> dplyr::filter(
#'      scientificName == "Acartia longiremis",
#'      target_subfragment == "COI1"
#'  )
#' smooth_fig(data = data, species.name = "Acartia longiremis")
#' }
smooth_fig <- function(data, species.name) {
  oop <- options("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  # reset option on exit
  on.exit(options(dplyr.summarise.inform = oop))

  primer.select <- unique(data$target_subfragment)
  if (length(primer.select) > 1) {
    cli::cli_alert_warning("Primer not unique")
    subt <- paste("Primer:", paste0(primer.select, collapse = ", "))
  } else {
    if (is.na(primer.select)) {
      subt <- NULL
    } else {
      subt <- paste("Primer:", primer.select)
    }
  }

  data %<>%
    dplyr::group_by(scientificName, year, month) %>%
    dplyr::summarise(n = dplyr::n(), nd = sum(detected))

  # Do smoothing by making continuous time (rbind time series and focus on middle period)
  data$prob <- data$nd / data$n

  data$month <- as.numeric(data$month)

  data %<>%
    dplyr::group_by(year) %>%
    tidyr::drop_na(prob) %>%
    dplyr::mutate(scaleP = scale_prop(prob)) %>%
    dplyr::mutate(scaleP = dplyr::case_when(
      scaleP == "NaN" ~ prob,
      scaleP != "NaN" ~ scaleP
    ))

  Dsummary24 <- Dsummary12 <- data
  Dsummary12$month <- data$month + 12
  Dsummary24$month <- data$month + 24

  Dsummary_comb <- rbind(data, Dsummary12, Dsummary24)

  # Loess smoother. Span = 3/12
  loessmod <- loess(scaleP ~ month, Dsummary_comb, span = 3 / 12) # 1 month before and after to smooth)

  NEW <- data.frame(month = seq(1, 35, 0.1))
  NEW$PRED <- predict(loessmod, newdata = NEW$month)

  NEW2 <- NEW[NEW$month > 12 & NEW$month <= 24, ]
  NEW2$month <- NEW2$month - 12

  ggplot2::ggplot() +
    ggplot2::geom_hline(ggplot2::aes(yintercept = y), data.frame(y = c(0:4) / 4), color = "lightgrey") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = x), data.frame(x = 0:12), color = "lightgrey") +
    ggplot2::geom_path(data = NEW2, ggplot2::aes(x = month, y = PRED), show.legend = TRUE, colour = "blue") +
    ggplot2::geom_point(data = data, ggplot2::aes(x = month, y = scaleP, col = as.factor(year)), show.legend = TRUE, alpha = .9, size = 2) +
    ggplot2::coord_polar() +
    ggplot2::labs(
      col = "Year", x = NULL, y = "Normalized detection probability",
      title = scientific_name_formatter(species.name),
      subtitle = subt
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_colour_manual(values = palette("Alphabet")) +
    ggplot2::scale_x_continuous(
      limits = c(0, 12),
      breaks = 1:12,
      labels = month.abb
    ) +
    ggplot2::scale_y_continuous(limits = c(-0.1, 1), breaks = c(0, 0.25, 0.50, 0.75, 1)) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(hjust = 1)
    )
}
