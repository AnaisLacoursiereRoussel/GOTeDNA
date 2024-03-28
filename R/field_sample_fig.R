#' Display actual sampling effort of taxa within specified taxonomic group.
#'
#' @description This function displays the actual number of eDNA samples taken
#' for all species of a specified taxonomic group per year.
#'
#' @param data (required, data.frame): Data.frame read in with [read_data()]
#' @param taxon.select (required, character): Taxonomic level of interest
#' Choices = one of `c("kingdom", "phylum", "class", "order", "family", "genus)`
#' @param taxon.name (required, character): Select taxon name that matches the level
#' provided in `higher.taxon.select`. E.g., if `taxon.level = "class"`
#' enter class name, etc..
#'
#' @author Anais Lacoursiere-Roussel \email{Anais.Lacoursiere@@dfo-mpo.gc.ca}
#' @rdname field_sample_fig
#' @export
#' @examples
#' \dontrun{
#' higher_tax_fig(
#'   data = D_mb_ex,
#'   higher.taxon.select = "phylum",
#'   taxon.name = "Bryozoa"
#' )
#' }
field_sample_fig <- function(data, taxon.select, taxon.name) {
  oop <- options("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  on.exit(options(dplyr.summarise.inform = oop))

  if (!taxon.name %in% data[[taxon.select]]) {
    stop("Taxon not found in data")
  }

  if (!is.null(taxon.select)) {
    taxon.select <- match.arg(
      arg = taxon.select,
      choices = c("kingdom", "phylum", "class", "order", "family", "genus"),
      several.ok = FALSE
    )
  }

  data %<>%
    dplyr::filter(!!dplyr::ensym(taxon.select) %in% taxon.name) %>%
    dplyr::group_by(kingdom, phylum, class, order, family, genus, scientificName, year, month) %>%
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
        colour = scientificName,
        size = n
      ),
      alpha = 0.9,
      na.rm = TRUE, width = 0.5, height = 0.01
    ) +
    ggplot2::scale_colour_manual(values = palette("Alphabet"))+
    ggh4x::facet_grid2(year ~ .,
      strip = ggh4x::strip_nested(bleed = TRUE)
    ) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-.01, 1)) +
    ggplot2::scale_x_continuous(limits = c(1, 12), breaks = 1:12,
      labels = month.abb) +
    ggplot2::scale_size_continuous(limits = c(0, NA), breaks = seq(0, 50, 10)) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::guides(size = ggplot2::guide_legend(order = 1),
                    colour = ggplot2::guide_legend(order = 2,
                                          label.theme = ggplot2::element_text(
                                            face = "italic",
                                            size = 8
                                          )
                                          )) +
    ggplot2::labs(
      x = "Month", y = "Proportion of positive samples",
      title = paste(stringr::str_to_title(taxon.select), taxon.name),
      colour = "Species",
      size = "Sampling effort"
    ) +
   theme_gotedna
}
