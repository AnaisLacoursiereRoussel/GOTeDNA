#' Display species monthly detection probability by primer and region.
#'
#' @description This function displays the species optimal detection period as
#' monthly detection probabilities with LOESS smoothing over years.
#'
#' @param data (required, data.frame): Data.frame read in with [read_data()].
#' @param taxon.level (required, character): Taxonomic level selection.
#' @param taxon.name (required, character): Full binomial species name.
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname smooth_fig
#' @export
#' @examples
#' \dontrun{
#' data <- gotedna_data$metabarcoding |> dplyr::filter(
#'      species == "Acartia longiremis",
#'      primer == "COI1"
#'  )
#' smooth_fig(data = data, species.name = "Acartia longiremis")
#' }
smooth_fig <- function(
    data,
    taxon.level = c("domain", "kingdom", "phylum", "class", "order", "family", "genus", "species"),
    taxon.name) {

 taxon.level <- match.arg(taxon.level)

  data %<>%
    dplyr::filter(!!dplyr::ensym(taxon.level) %in% taxon.name) %>%
    dplyr::mutate(GOTeDNA_ID.v = paste0(GOTeDNA_ID, ".", GOTeDNA_version)) %>%
    dplyr::group_by(GOTeDNA_ID.v, !!dplyr::ensym(taxon.level), year, month) %>%
    dplyr::summarise(n = dplyr::n(),
                     nd = sum(detected))

  # Do smoothing by making continuous time (rbind time series and focus on middle period)
  data$prob <- data$nd / data$n

  data$month <- as.numeric(data$month)

  data %<>%
    dplyr::group_by(GOTeDNA_ID.v, year) %>%
    tidyr::drop_na(prob) %>%
    dplyr::mutate(scaleP = scale_prop(prob)) %>%
    dplyr::mutate(scaleP = dplyr::case_when(
      scaleP == "NaN" ~ prob,
      scaleP != "NaN" ~ scaleP
    ))

  data.split <- split(data, data$GOTeDNA_ID.v)

  NEW_data <- lapply(data.split, function(x) {

  Dsummary24 <- Dsummary12 <- x
  Dsummary12$month <- x$month + 12
  Dsummary24$month <- x$month + 24

  Dsummary_comb <- rbind(x,
                         Dsummary12,
                         Dsummary24)

  # Loess smoother. Span = 3/12
  loessmod <- loess(scaleP ~ month, Dsummary_comb, span = 3 / 12) # 1 month before and after to smooth)

  NEW <- data.frame(month = seq(1, 35, 0.1))
  NEW$PRED <- predict(loessmod, newdata = NEW$month)

  NEW2 <- NEW[NEW$month > 12 & NEW$month <= 24, ]
  NEW2$month <- NEW2$month - 12

  list(NEW2) |> dplyr::bind_rows()
  })

  plots <- list()

  for (proj in names(data.split)) {
    plots[[proj]] <- with(data.split[[proj]],
                          ggplot2::ggplot() +
                            ggplot2::geom_hline(ggplot2::aes(yintercept = y), data.frame(y = c(0:4) / 4), color = "lightgrey") +
                            ggplot2::geom_vline(ggplot2::aes(xintercept = x), data.frame(x = 0:12), color = "lightgrey") +
                            ggplot2::geom_path(data = NEW_data[[proj]],
                                               ggplot2::aes(x = month,
                                                            y = PRED),
                                               show.legend = TRUE, colour = "blue") +
                            ggplot2::geom_point(data = data.split[[proj]],
                                                ggplot2::aes(x = month,
                                                             y = scaleP,
                                                             col = as.factor(year)),
                                                show.legend = TRUE,
                                                alpha = .9, size = 5) +
                            ggplot2::coord_polar(
                              clip = "off"
                            ) +
                            ggplot2::labs(
                              col = "Year", x = NULL, y = NULL#,
                              # title = species.name,
                              # subtitle = subt
                            ) +
                            ggplot2::theme_minimal() +
                            ggplot2::scale_colour_manual(values = palette("Alphabet")) +
                            ggplot2::scale_x_continuous(
                              limits = c(0.5, 12.5),
                              breaks = 1:12,
                              labels = month.abb
                            ) +
                            ggplot2::guides(colour = ggplot2::guide_legend(
                              label.position = "left",
                              label.hjust = 1
                            )) +
                            ggplot2::scale_y_continuous(limits = c(-0.1, 1.01), breaks = c(0, 0.25, 0.50, 0.75, 1)) +
                            theme_circle
    )
  }

  return(plots)

}


