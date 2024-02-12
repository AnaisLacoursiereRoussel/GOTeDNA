#' Display actual sampling effort by species, year, and region.
#'
#' @description This function displays the actual number of eDNA samples taken
#' for the specified species and region.
#'
#' @param data (required, data.frame): Data.frame read in with [read_data()].
#' @param species.name (required, character): Full binomial species name.
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname sample_size_fig
#' @export
#' @examples
#' \dontrun{
#' sample_size_fig(
#'  data = D_mb_ex |> dplyr::filter(scientificName == "Acartia hudsonica"),
#'  species.name = "Acartia hudsonica"
#' )
#' }
sample_size_fig <- function(data, species.name) {
  oop <- options("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  on.exit(options(dplyr.summarise.inform = oop))

  # if (!species.name %in% data$scientificName) {
  #   stop("Species not found in data")
  # }

  data %<>%
    # dplyr::filter(., scientificName == species.name) %>%
    dplyr::group_by(scientificName, eventID, station, year, month) %>%
    # dplyr::group_by(scientificName, year, month) %>% # We could have a separate figure where the sample size is shown in size of bubble.
    dplyr::summarise(
      n = dplyr::n(),
      nd = sum(detected),
      freq_det = nd / n
    )

  ggplot2::ggplot() +
    ggplot2::geom_jitter(data,
      mapping = ggplot2::aes(
        month, freq_det,
        colour = scientificName
      ),
      # size = n), # separate figure where the sample size is shown in size of bubble.
      alpha = 0.8,
      na.rm = TRUE, width = 0.5, height = 0.01
    ) +
    ggplot2::scale_colour_viridis_d(guide = "none") +
    ggh4x::facet_grid2(year ~ .,
      strip = ggh4x::strip_nested(bleed = TRUE)
    ) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-.01, 1.05)) +
    ggplot2::scale_x_continuous(
      limits = c(1, 12),
      breaks = 1:12,
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    ) +
    ggplot2::scale_size_continuous(limits = c(0, NA), breaks = seq(0, 50, 10)) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::labs(
      x = "Month", y = "Proportion of positive samples",
      title = scientific_name_formatter(species.name),
      size = "Sampling effort"
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_line(colour = "lightgrey"),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(),
      legend.position = NULL,
      strip.background = ggplot2::element_rect(fill = "grey95", colour = "black"),
      strip.text.y.left = ggplot2::element_text(angle = 0),
      strip.placement = "outside"
    )
}
