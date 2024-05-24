#' Display actual sampling effort of taxa within specified taxonomic group.
#'
#' @description This function displays the actual number of eDNA samples taken
#' for all species of a specified taxonomic group per year.
#'
#' @param data (required, data.frame): Data.frame read in with [read_data()]
#' @param taxon.select (required, character): Taxonomic level of interest
#' Choices = one of `c("domain", "kingdom", "phylum", "class", "order", "family",`
#' `"genus", "species")`.
#' @param taxon.name (required, character): Select taxon name that matches the level
#' provided in `taxon.select`. E.g., if `taxon.select = "class"`
#' enter class name, etc..
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname field_sample_fig
#' @export
#' @examples
#' \dontrun{
#' field_sample_fig(
#'   data = gotedna_data$metabarcoding,
#'   taxon.select = "phylum",
#'   taxon.name = "Bryozoa"
#' )
#' }
field_sample_fig <- function(data, taxon.select, taxon.name) {
  oop <- options("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  on.exit(options(dplyr.summarise.inform = oop))

 # if (!taxon.name %in% data[[taxon.select]]) {
#    stop("Taxon not found in data")
 # }

  if (!is.null(taxon.select)) {
    taxon.select <- match.arg(
      arg = taxon.select,
      choices = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
      several.ok = FALSE
    )
  }


  data %<>%
    dplyr::filter(!!dplyr::ensym(taxon.select) %in% taxon.name) %>%
    dplyr::group_by(kingdom, phylum, class, order, family, genus, species, year, month) %>%
    dplyr::summarise(
      n = dplyr::n(),
      nd = sum(detected),
      freq_det = nd / n
    )

  ggplot2::ggplot() +
    ggplot2::geom_jitter(data,
      mapping = ggplot2::aes(
        x = month,
        y = freq_det,
        colour = species,
        size = n
      ),
      alpha = 0.9,
      na.rm = TRUE, width = 0.5, height = 0.01
    ) +
    ggplot2::scale_colour_manual(values = palette("Alphabet"))+
  #  ggh4x::facet_grid2(year ~ .,
   #   strip = ggh4x::strip_nested(bleed = TRUE)
   # ) +
    ggh4x::facet_wrap2(~year,
                        ncol = 1,
                        axes = "all",
                       remove_labels = TRUE,
                       strip.position = "right") +
   # lemon::coord_capped_cart(bottom = 'both') +
    ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-.01, 1),
                                expand = c(0, 0.03)
                                ) +
    ggplot2::scale_x_continuous(limits = c(1, 12), breaks = 1:12,
      labels = month.abb,
      expand = c(0.04, 0)
      ) +
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
      x = "Month", y = "Proportion of positive samples",
     # title = ifelse(
      #  taxon.select != "species",
       # paste(stringr::str_to_title(taxon.select), taxon.name),
        #paste(taxon.name)),
      colour = "Species",
      size = "Sampling effort"
    ) +
    ggplot2::theme(
      # plotting components

      ## drop minor gridlines
      panel.grid = ggplot2::element_blank(),
      # change grid lines to gray
      panel.grid.minor.x =  ggplot2::element_line(color = "#d0d0d0"),
      panel.spacing = ggplot2::unit(25, "pt"),
      # fill the plot and panel spaces with grey and remove border
      #  panel.background = element_blank(),
      # plot.background = element_blank(),
      panel.border = ggplot2::element_blank(),
      # remove strip background
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(angle = 0,
                                         colour = "#939598"),
      # adjust the margins of plots and remove axis ticks
      plot.margin = ggplot2::margin(0.5, 1, 0.5, 1,
                                    unit = "cm"),
      axis.ticks = ggplot2::element_line(
        linewidth = 1,
        colour = "#939598"),
      axis.line = ggplot2::element_line(
        linewidth = 1,
        colour = "#939598"),
      # change text family, size, and adjust position of titles
      text = ggplot2::element_text(
        family = "Arial", size = 24),
      axis.text = ggplot2::element_text(
        colour = "#939598", size = 20),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 10, unit = "pt")),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 10, unit = "pt")
      ),
      axis.title = ggplot2::element_text(colour = "#5A5A5A",
                                         size = 24),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(0.5, 0, 0, 0, unit = "cm"),
        hjust = 0),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 20,
                                 unit = "pt"),
        hjust = 0),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 30,
        hjust = 0,
        colour = "#5A5A5A",
        margin = ggplot2::margin(b = 20, unit = "pt")),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(size = 24,
                                            margin = ggplot2::margin(b = 20, unit = "pt"),
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
     )+
    ggh4x::force_panelsizes(rows = ggplot2::unit(100, "pt"),
                            total_width = ggplot2::unit(620, "pt"))
}
