#' Utility functions
#'
#' @name scale_prop
#' @rdname utils
#' @keywords internal
#' @importFrom grDevices palette
#' @importFrom magrittr %>% %<>%
#' @importFrom stats dbinom fisher.test loess median predict reorder window
#
#' @export


# scaling of detection probabilities by max proportion
scale_prop <- function(x) {
  (x/max(x))
}

# custom theme for all figures
theme_circle <- ggplot2::theme(
  # plotting components

  ## drop minor gridlines
  panel.grid = ggplot2::element_blank(),
  # change grid lines to gray
  #  panel.grid.major =  element_line(color = "#d0d0d0"),
  # fill the plot and panel spaces with grey and remove border
  #  panel.background = element_blank(),
  # plot.background = element_blank(),
  panel.border = ggplot2::element_blank(),
  # adjust the margins of plots and remove axis ticks
 # plot.margin = ggplot2::margin(b = 25, l = 25,
  #                              unit = "pt"),
  axis.ticks = ggplot2::element_blank(),
  # change text family, size, and adjust position of titles
  text = ggplot2::element_text(
    family = "Arial", size = 24),
  axis.text = ggplot2::element_text(
    colour = "#939598", size = 20),
  axis.text.y = ggplot2::element_blank(),
  axis.title = ggplot2::element_text(colour = "#5A5A5A",
                                     size = 24),
  axis.line = ggplot2::element_blank(),
  plot.title = ggplot2::element_text(
    face = "bold",
    size = 30,
    hjust = 0,
    colour = "#5A5A5A",
    margin = ggplot2::margin(b = 5, unit = "pt")),
  plot.title.position = "plot",
  plot.subtitle = ggplot2::element_text(size = 24,
                                        margin = ggplot2::margin(b = 25, unit = "pt"),
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
