#' Calculate sampling effort needed to obtain different detection thresholds.
#'
#' @description This function calculates number of samples needed to obtain
#' species detection at different thresholds by using scaled and interpolated
#' data produced with `[scale_newprob()]`.
#'
#' @param scaledprobs  (required, data.frame) Normalized detection
#' probabilities as returned by the element `month` of the list returned by
#' [scale_newprob()] for one species and one primer.
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname effort_needed_fig
#' @export
#' @examples
#' \dontrun{
#' newprob <- calc_det_prob(gotedna_data$metabarcoding)
#' scaledprobs <- scale_newprob(gotedna_data$metabarcoding, newprob)
#' effort_needed_fig(
#'   scaledprobs
#'   )
#' )
#' }
effort_needed_fig <- function(
    scaledprobs) {

  df <- scaledprobs %>%
    dplyr::filter(is.na(year))


  df$month <- factor(df$month,
                       levels = 1:12,
                       labels = c("Jan","Feb","Mar","Apr","May",
                                  "Jun","Jul","Aug","Sep","Oct",
                                  "Nov","Dec"))
  DF2 <- vector("list")

  for (sp in unique(df$species)) {

    DF2[[sp]] <- expand.grid(p = df[df$species == sp,]$fill,
                             `Samples needed` = seq_len(25),
                             `Detection rate` = NA)

    DF2[[sp]] <- DF2[[sp]] |>
      merge(
        data.frame(p = df[df$species == sp,]$fill,
                   Month = df[df$species == sp,]$month,
                   Species = df[df$species == sp,]$species
        )
      )

    for (i in seq_len(nrow(DF2[[sp]]))) {
      DF2[[sp]]$`Detection rate`[i] <- 1 - dbinom(0, size = DF2[[sp]]$`Samples needed`[i], prob = DF2[[sp]]$p[i]) # 1 - probability of zero detects
    }
  }

  DF_tot <- dplyr::bind_rows(DF2) |>
    dplyr::mutate(Species = reorder(Species, dplyr::desc(Species)))

  ggplot2::ggplot(DF_tot,
                  ggplot2::aes(y = `Detection rate`,
                               x = `Samples needed`,
                               colour = Month)) +
    ggplot2::geom_point(size = 3, show.legend = FALSE) +
    ggplot2::theme_classic(base_size = 24) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::scale_y_continuous(
                                limits = c(0, 1),
                                expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      limits = c(1, 25),
      breaks = seq(5,25,5)
    ) +
    ggplot2::scale_colour_viridis_d(
      direction = -1,
      breaks = 1:12,
      labels = c("Jan","Feb","Mar","Apr","May",
                 "Jun","Jul","Aug","Sep","Oct",
                 "Nov","Dec")
    ) +
    ggplot2::facet_wrap(~Species,
                       ncol = 1, scales = "free") +
    ggplot2::labs(
      x = NULL, y = NULL
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(20, "pt"),
      strip.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(
        linewidth = 0.1,
        colour = "#939598"),
      text = ggplot2::element_text(
        family = "Arial", size = 24),
      axis.text = ggplot2::element_text(
        colour = "#939598", size = 20),
      axis.title = ggplot2::element_text(colour = "#5A5A5A",
                                         size = 24),
      axis.title.x = ggplot2::element_text(
        hjust = 0),
      axis.title.y = ggplot2::element_text(
        hjust = 0),
      axis.line = ggplot2::element_line(
        linewidth = 0.1,
        colour = "#939598"),
      strip.placement = "outside"

    )

}

