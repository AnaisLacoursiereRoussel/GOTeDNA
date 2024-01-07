#' Display actual sampling effort of taxa within specified taxonomic group.
#' @description This function displays the actual number of eDNA samples taken
#' for all taxa of a specified higher taxonomic group per year.
#'
#' @param data (required, data.frame): Data.frame read in with [read_data()]
#' @param higher.taxon.select (required, character): Higher taxonomic level of
#' interest. Choices = one of `c("kingdom", "phylum", "class", "order", "family")`
#' @param taxon.name (required, character): Select taxon name that matches the level
#' provided in `higher.taxon.select`. E.g., if `taxon.level = "class"`
#' enter class name, etc..
#' @param view.by.level (required, character): Lower taxonomic level to view
#' detection probability by. Cannot be higher level than higher group specified in
#' `higher.taxon.select` Choices = one of `c("phylum", "class", "order",
#' "family", "genus")`
#' @param primer.select (required, character): Select primer as different primers
#' may provide different detection rates.

#' @author Melissa Morrison \email{Melissa.Morrison@@dfo-mpo.gc.ca}
#' @author Tim Barrett \email{Tim.Barrett@@dfo-mpo.gc.ca}
#' @rdname higher_tax_fig
#' @export
#' @examples
#' \dontrun{
#' higher_tax_fig(
#'   data = D_mb_ex,
#'   higher.taxon.select = "phylum",
#'   taxon.name = "Bryozoa",
#'   view.by.level = "genus",
#'   primer.select = "COI1"
#' )
#' }
higher_tax_fig <- function(data, higher.taxon.select, taxon.name, view.by.level, primer.select) {
  oop <- options("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  on.exit(options(dplyr.summarise.inform = oop))

  if (!taxon.name %in% data[[higher.taxon.select]]) {
    stop("Taxon not found in data")
  }

  if (!is.null(view.by.level)) {
    view.by.level <- match.arg(
      arg = view.by.level,
      choices = c("phylum", "class", "order", "family", "genus"),
      several.ok = FALSE
    )
  }

  if (!is.null(higher.taxon.select)) {
    higher.taxon.select <- match.arg(
      arg = higher.taxon.select,
      choices = c("kingdom", "phylum", "class", "order", "family"),
      several.ok = FALSE
    )
  }

  if (!primer.select %in% data$target_subfragment) {
    stop("Primer not found in data")
  }

  data %<>%
    dplyr::filter(., target_subfragment == primer.select &
      !!dplyr::ensym(higher.taxon.select) %in% taxon.name) %>%
    dplyr::group_by(kingdom, phylum, class, order, family, genus, year, month) %>%
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
        colour = !!dplyr::ensym(view.by.level),
        size = n
      ),
      alpha = 0.8,
      na.rm = TRUE, width = 0.5, height = 0.01
    ) +
    ggplot2::scale_colour_viridis_d() +
    ggh4x::facet_grid2(year ~ .,
      strip = ggh4x::strip_nested(bleed = T)
    ) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-.01, 1)) +
    ggplot2::scale_x_continuous(limits = c(1, 12), breaks = 1:12,
      labels = month.abb) +
    ggplot2::scale_size_continuous(limits = c(0, NA), breaks = seq(0, 50, 10)) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::labs(
      x = "Month", y = "Proportion of positive samples",
      title = paste(stringr::str_to_title(higher.taxon.select), taxon.name),
      colour = stringr::str_to_title(view.by.level),
      size = "Sampling effort"
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA),
      axis.line = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_line(colour = "lightgrey"),
      axis.ticks = ggplot2::element_line(),
      legend.position = NULL,
      strip.background = ggplot2::element_rect(fill = "grey95", colour = "black"),
      strip.text.y.left = ggplot2::element_text(angle = 0),
      strip.placement = "outside"
    )
}
