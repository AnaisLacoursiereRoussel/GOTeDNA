#' Display actual sampling effort of taxa within specified taxonomic group.
#'
#' @description This function displays the actual number of eDNA samples taken
#' for all species of a specified taxonomic group per year.
#'
#' @param data (required, data.frame): Data.frame read in with [read_data()]
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname field_sample_fig
#' @export
#' @examples
#' \dontrun{
#' field_sample_fig(
#'   data = gotedna_data$metabarcoding
#' )
#' }
field_sample_fig <- function(
    data
    ) {

  data %<>%
    dplyr::group_by(species, year, month) %>%
    dplyr::summarise(
      n = dplyr::n(),
      nd = sum(detected, na.rm = TRUE),
      freq_det = nd / n
    ) %>%
    dplyr::rename("Month" = "month",
                  "Species" = "species",
                  "Detection rate" = "freq_det",
                  "Sample size" = "n",
                  "Year" = "year")


  data$Year <- reorder(as.numeric(data$Year),
                       dplyr::desc(as.numeric(data$Year)))

  ggplot2::ggplot() +
    ggplot2::geom_jitter(data,
                         mapping = ggplot2::aes(
                           x = Month,
                           y = `Detection rate`,
                           colour = Species,
                           size = `Sample size`
                         ),
                         alpha = 0.9,
                         na.rm = TRUE, width = 0.5, height = 0.01
    ) +
    ggplot2::scale_colour_manual(values = palette("Alphabet"))+
    ggplot2::facet_wrap(~Year,
                       ncol = 1,
                       scales = "free") +
    ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                                limits = c(-.01, 1),
                                expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(limits = c(0.5,12.5),
                                breaks = 1:12,
                                labels = month.abb,
                              expand = c(0, 0)) +
    ggplot2::scale_size_continuous(limits = c(0, NA), breaks = seq(0, 50, 10)) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::guides(size = ggplot2::guide_legend(order = 1,
                                                 label.position = "left",
                                                 label.hjust = 1),
                    colour = ggplot2::guide_legend(order = 2,
                                                   label.position = "left",
                                                   label.hjust = 1,
                                                   override.aes = list(size = 5)
                    )
    ) +
    ggplot2::labs(
      x = NULL, y = NULL,
      colour = NULL,
      size = NULL
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(30, "pt"),
      panel.border = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(angle = 0,
                                         colour = "#939598"),
      axis.ticks = ggplot2::element_line(
        linewidth = 1,
        colour = "#939598"),
      axis.line = ggplot2::element_line(
        linewidth = 1,
        colour = "#939598"),
      text = ggplot2::element_text(
        family = "Arial", size = 24),
      axis.text = ggplot2::element_text(
        colour = "#939598", size = 20),
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
