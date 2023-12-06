#' Display monthly detection probabilities for selected taxon, ecodistrict, and
#' detection probability threshold
#' 
#' @description This function displays detection probabilities in bins, with
#' probabilities over a specified threshold displayed in purple and under the
#' threshold in grey.
#'
#' @param taxon.level (required, data.frame): Select taxonomic level to view.
#' Choices = one of `c("phylum", "class", "order", "family", "genus", "species")`
#' @param taxon.name (required, character): Select taxon name that matches the level
#' provided in `taxon.level`. E.g., if `taxon.level = "genus"` enter genus name, etc.
#' @param threshold (required, character): Detection probability threshold for
#' which data are to be displayed to visualize potential optimal detection windows.
#' Choices = one of `"50","55","60","65","70","75","80","85","90","95")`
#' @param ecodistrict.select (required, character): Ecodistrict present in data.frame.

#' @author Melissa Morrison \email{Melissa.Morrison@@dfo-mpo.gc.ca}
#' @author Tim Barrett \email{Tim.Barrett@@dfo-mpo.gc.ca}
#' @rdname thresh_fig
#' @export
#' @examples
#' \dontrun{
#' thresh_fig(
#'   taxon.level = "species", taxon.name = "Acartia hudsonica", threshold = "90",
#'   ecodistrict.select = "Scotian Shelf"
#' )
#' }
thresh_fig <- function(taxon.level, taxon.name, threshold, ecodistrict.select) {
  if (!ecodistrict.select %in% Pscaled_agg$ecodistrict) {
    stop("Ecodistrict not found in data")
  }

  # Pscaled_agg %>%
  #   dplyr::filter(., ecodistrict == ecodistrict.select)

  Pthresh <- Pscaled_agg %>%
    dplyr::mutate(thresh95 = dplyr::case_when(
      fill < 0.94999 ~ 0,
      fill >= 0.94999 ~ 1
    )) %>%
    dplyr::mutate(thresh90 = dplyr::case_when(
      fill < 0.89999 ~ 0,
      fill >= 0.89999 ~ 1
    )) %>%
    dplyr::mutate(thresh85 = dplyr::case_when(
      fill < 0.849999 ~ 0,
      fill >= 0.849999 ~ 1
    )) %>%
    dplyr::mutate(thresh80 = dplyr::case_when(
      fill < 0.79999 ~ 0,
      fill >= 0.79999 ~ 1
    )) %>%
    dplyr::mutate(thresh75 = dplyr::case_when(
      fill < 0.74999 ~ 0,
      fill >= 0.74999 ~ 1
    )) %>%
    dplyr::mutate(thresh70 = dplyr::case_when(
      fill < 0.69999 ~ 0,
      fill >= 0.69999 ~ 1
    )) %>%
    dplyr::mutate(thresh65 = dplyr::case_when(
      fill < 0.649999 ~ 0,
      fill >= 0.649999 ~ 1
    )) %>%
    dplyr::mutate(thresh60 = dplyr::case_when(
      fill < 0.59999 ~ 0,
      fill >= 0.59999 ~ 1
    )) %>%
    dplyr::mutate(thresh55 = dplyr::case_when(
      fill < 0.549999 ~ 0,
      fill >= 0.549999 ~ 1
    )) %>%
    dplyr::mutate(thresh50 = dplyr::case_when(
      fill < 0.49999 ~ 0,
      fill >= 0.49999 ~ 1
    )) %>%
    tidyr::drop_na(fill)

  Pthresh[Pthresh$phylum == "Chordata", ] %>%
    tidyr::drop_na(class)

  if (!is.null(taxon.level)) {
    taxon.level <- match.arg(
      arg = taxon.level,
      choices = c("phylum", "class", "order", "family", "genus", "species"),
      several.ok = FALSE
    )
  }

  thresh_slc <- as.character(seq(50, 95, 5))
  if (!is.null(threshold)) {
    threshold <- match.arg(
      arg = threshold,
      choices = thresh_slc
       ,
      several.ok = FALSE
    )
  }

  thresh <- data.frame(
    values = paste0("thresh", thresh_slc),
    labels = thresh_slc
  )

  thresh.value <- switch(threshold,
    thresh$values[thresh$labels == threshold]
  )
  data <- Pthresh[Pthresh[[taxon.level]] %in% taxon.name, ]

  print(ggplot2::ggplot() +
    ggplot2::geom_hline(
      mapping = ggplot2::aes(yintercept = y),
      data.frame(y = c(0:4) / 4),
      color = "darkgrey"
    ) +
    ggplot2::geom_vline(
      mapping = ggplot2::aes(xintercept = x),
      data.frame(x = 1:12),
      color = "lightgrey"
    ) +
    ggplot2::geom_col(
      data[data[[thresh.value]] %in% "1", ],
      mapping = ggplot2::aes(x = month, y = fill), position = "dodge2",
      width = 0.9, show.legend = TRUE, alpha = .9, fill = viridis::viridis(1)
    ) +
    ggplot2::geom_col(
      data[data[[thresh.value]] %in% "0", ],
      mapping = ggplot2::aes(x = month, y = fill), position = "dodge2",
      width = 0.9, show.legend = TRUE, alpha = .9, fill = "darkgrey"
    ) +
    # To make the interpolated data stand out
    ggpattern::geom_col_pattern(
      dplyr::filter(data, is.na(scaleP)),
      mapping = ggplot2::aes(x = month, y = fill),
      position = "dodge2",
      width = 0.9, show.legend = TRUE, fill = NA,
      pattern_color = "white", pattern_density = 0.05, pattern_spacing = 0.015, pattern_key_scale_factor = 0.6
    ) +
    ggplot2::coord_polar() +
    ggplot2::facet_wrap(~ primer + species, ncol = 2) + # will need to italicize the species name, but it would also be good if the name didn't show up if there is only one species to facet (if executing at higher taxonomy level [i.e., anything except species])
    ggplot2::scale_x_continuous(
      limits = c(0.5, 12.5),
      breaks = 1:12,
      labels = month.abb
    ) +
    ggplot2::labs(
      x = NULL, y = "Normalized detection probability",
      title = scientific_name_formatter(taxon.name),
      subtitle = paste0("Detection threshold: ", threshold, "%")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(hjust = 1, vjust = 2),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    ))
}
