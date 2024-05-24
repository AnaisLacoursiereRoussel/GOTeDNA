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


  data <- scaledprobs$Pscaled_month[
    scaledprobs$Pscaled_month[[taxon.level]] %in% c(taxon.name),
  ]
  data$month <- factor(data$month,
                          levels = 1:12,
                          labels = c("Jan","Feb","Mar","Apr","May",
                          "Jun","Jul","Aug","Sep","Oct",
                          "Nov","Dec"))

  #data.split <- split(data, data$GOTeDNA_ID.v)

  DF2 <- vector("list")

#DF2 <- lapply(data.split, function(x) {

  for (sp in unique(data$species)) {

  DF2[[sp]] <- expand.grid(p = data[data$species == sp,]$fill,
                           `Samples needed` = seq_len(10),
                           `Detection rate` = NA)

  DF2[[sp]] <- DF2[[sp]] |>
    merge(
      data.frame(p = data[data$species == sp,]$fill,
                 Month = data[data$species == sp,]$month,
                 Species = data[data$species == sp,]$species,
                 GOTeDNA_ID.v = data[data$species == sp,]$GOTeDNA_ID.v
                 )
    )

  for (i in seq_len(nrow(DF2[[sp]]))) {
    DF2[[sp]]$`Detection rate`[i] <- 1 - dbinom(0, size = DF2[[sp]]$`Samples needed`[i], prob = DF2[[sp]]$p[i]) # 1 - probability of zero detects
  }
  }
    #DF2 |> dplyr::bind_rows()
#})

  DF_tot <- dplyr::bind_rows(DF2)
  #plots = vector("list")

 # for (proj in names(DF2)){
 #   for (sp in unique(DF2$Species)){
      #DF2[[proj]]$Species)){

  #    plots[[proj]][[sp]] <- with(DF2[[proj]][DF2[[proj]]$Species == sp,],

  ggplot2::ggplot(DF_tot,
                  ggplot2::aes(y = `Detection rate`, x = `Samples needed`, colour = Month)) +
    ggplot2::geom_point(size = 5, show.legend = FALSE) +
    ggplot2::theme_classic(base_size = 24) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::scale_y_continuous("Detection probability",
      limits = c(0, 1),
      expand = c(0, 0.01)
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
    ggh4x::facet_wrap2(Species ~ .,
                       ncol = 1,
                       strip = ggh4x::strip_nested(clip = "off",
                                                   size = "variable")) +
    ggplot2::labs(
     # colour = "Month",
     # title = species.name,
     # subtitle = scaledprobs_month$species,
      x = "Number of samples"
    ) +
   # ggplot2::guides(colour = ggplot2::guide_legend(
  #    label.position = "left",
   #   label.hjust = 1
  #  )) +
    ggplot2::theme(
      # plotting components

      ## drop minor gridlines
      panel.grid = ggplot2::element_blank(),
      # change grid lines to gray
      #  panel.grid.major =  element_line(color = "#d0d0d0"),
      # fill the plot and panel spaces with grey and remove border
      #  panel.background = element_blank(),
      # plot.background = element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(25, "pt"),
      # remove strip background
      strip.background = ggplot2::element_blank(),
      # adjust the margins of plots and remove axis ticks
      plot.margin = ggplot2::margin(0.5, 1, 0.5, 1,
                                    unit = "cm"),
      axis.ticks = ggplot2::element_line(
        linewidth = 0.1,
        colour = "#939598"),
      # change text family, size, and adjust position of titles
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
   #   plot.title = ggplot2::element_text(
  #      face = "bold",
  #      size = 30,
  #      hjust = 0,
  #      colour = "#5A5A5A"),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(size = 24,
                                            margin = ggplot2::margin(b = 0.66, unit = "cm"),
                                            colour = "#5A5A5A",
                                            hjust = 0)

    ) +
    ggh4x::force_panelsizes(rows = ggplot2::unit(200, "pt"),
                            total_width = ggplot2::unit(620, "pt"))
    #  )
  #  }

#}

 # return(plots)

}
