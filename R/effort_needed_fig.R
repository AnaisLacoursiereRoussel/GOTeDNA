#' Calculate sampling effort needed to obtain different detection thresholds.
#' @description This function calculates number of samples needed to obtain
#' species detection at different thresholds by using scaled and interpolated
#' data produced with \code{scale_prob_by_month()}.
#'
#' @param species.name (required, character): Full binomial species name.
#' @param primer.select (required, character): Select primer as different primers
#' may provide different detection rates.
#' @param ecodistrict.select (required, character): Ecodistrict present in data.frame.

#' @author Melissa Morrison \email{Melissa.Morrison@@dfo-mpo.gc.ca}
#' @author Tim Barrett \email{Tim.Barrett@@dfo-mpo.gc.ca}
#' @rdname effort_needed_fig
#' @export
#' @examples
#' \dontrun{
#' effort_needed_fig(species.name = "Acartia hudsonica", primer.select = "COI1",
#' ecodistrict.select = "Scotian Shelf")
#' }
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
effort_needed_fig <- function(species.name, primer.select, ecodistrict.select) {

if (!is.null(species.name)){
  species.name <- match.arg(
    arg = species.name,
    choices = c(Pscaled_agg$species),
    several.ok = FALSE
  )
}

if(!ecodistrict.select %in% Pscaled_agg$ecodistrict) {
  stop("Ecodistrict not found in data")
}

if(!primer.select %in% Pscaled_agg$primer) {
  stop("Primer not found in data")
}

data <- Pscaled_agg %>%
  dplyr::filter(ecodistrict == ecodistrict.select &
                  species ==species.name &
                  primer==primer.select)

DF2 <- expand.grid(p=data$fill,
                   n=1:10,
                   P=NA)
DF2 = DF2 %>%
  merge(data.frame(p=data$fill, month=data$month))

for(i in 1:nrow(DF2)){
  DF2$P[i] <- 1 - dbinom(0,size=DF2$n[i],prob=DF2$p[i]) # 1 - probability of zero detects
}

ggplot2::ggplot(DF2,ggplot2::aes(y=P,x=n,colour=as.factor(month))) +
  ggplot2::geom_point(size=2) +
  ggplot2::theme_classic() +
  ggplot2::scale_y_continuous("Probability of at least one detection",
                              limits=c(0,1),
                              expand=c(0,0.01)) +
  ggplot2::scale_x_continuous(limits=c(1,10),
                              breaks=1:10) +
  ggplot2::scale_colour_viridis_d(direction=-1,
                                  breaks=1:12,
                                  labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  ggplot2::labs(colour="Month",
                title=scientific_name_formatter(species.name),
                subtitle=paste("Primer:",primer.select),
                x="Sample size required")
}
