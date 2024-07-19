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
#'   scaledprobs$Pscaled_month |> dplyr::filter(
#'       species %in% "Acartia hudsonica",
#'       primer == "COI1"
#'   )
#' )
#' }
effort_needed_fig <- function(
    taxon.level = c("phylum", "class", "order", "family", "genus", "species"),
    taxon.name,
    scaledprobs) {

  taxon.level <- match.arg(taxon.level)

  scaledprobs %<>%
    dplyr::filter(!!dplyr::ensym(taxon.level) %in% taxon.name,
                  is.na(year)) %>%
    dplyr::group_by(GOTeDNA_ID)


  scaledprobs$month <- factor(scaledprobs$month,
                       levels = 1:12,
                       labels = c("Jan","Feb","Mar","Apr","May",
                                  "Jun","Jul","Aug","Sep","Oct",
                                  "Nov","Dec"))
  DF2 <- vector("list")

  for (sp in unique(scaledprobs$species)) {

    DF2[[sp]] <- expand.grid(p = scaledprobs[scaledprobs$species == sp,]$fill,
                             `Samples needed` = seq_len(10),
                             `Detection rate` = NA)

    DF2[[sp]] <- DF2[[sp]] |>
      merge(
        data.frame(p = scaledprobs[scaledprobs$species == sp,]$fill,
                   Month = scaledprobs[scaledprobs$species == sp,]$month,
                   Species = scaledprobs[scaledprobs$species == sp,]$species,
                   GOTeDNA_ID = scaledprobs[scaledprobs$species == sp,]$GOTeDNA_ID
        )
      )

    for (i in seq_len(nrow(DF2[[sp]]))) {
      DF2[[sp]]$`Detection rate`[i] <- 1 - dbinom(0, size = DF2[[sp]]$`Samples needed`[i], prob = DF2[[sp]]$p[i]) # 1 - probability of zero detects
    }
  }
  #DF2 |> dplyr::bind_rows()
  #})

  DF_tot <- dplyr::bind_rows(DF2)

  ggplot2::ggplot(DF_tot,
                  ggplot2::aes(y = `Detection rate`,
                               x = `Samples needed`,
                               colour = Month)) +
    ggplot2::geom_point(size = 5, show.legend = FALSE) +
    ggplot2::geom_hline(
      mapping = ggplot2::aes(yintercept = 0),
      size = 1)+
    ggplot2::theme_classic(base_size = 24) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::scale_y_continuous("Detection probability",
                                limits = c(0, 1),
                                expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      #expand = c(0, 0),
      limits = c(1, 10),
      breaks = 1:10
    ) +
    ggplot2::scale_colour_viridis_d(
      direction = -1,
      breaks = 1:12,
      labels = c("Jan","Feb","Mar","Apr","May",
                 "Jun","Jul","Aug","Sep","Oct",
                 "Nov","Dec")
    ) +
    ggplot2::facet_wrap(~Species,
                       ncol = 1) +
    ggplot2::labs(
      x = "Number of samples"
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(25, "pt"),
      strip.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(0.5, 1, 0.5, 1,
                                    unit = "cm"),
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
        margin = ggplot2::margin(0.5, 0, 0, 0, unit = "cm"),
        hjust = 0),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 0.66,
                                 unit = "cm"),
        hjust = 0),
      axis.line = ggplot2::element_line(
        linewidth = 0.1,
        colour = "#939598"),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(size = 20, colour = "#5A5A5A", hjust = 0,
                                         margin = ggplot2::margin(b = 15, unit = "pt")),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(size = 24,
                                            margin = ggplot2::margin(b = 0.66, unit = "cm"),
                                            colour = "#5A5A5A",
                                            hjust = 0)

    )

}

