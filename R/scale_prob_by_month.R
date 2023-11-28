#' Normalize detection probabilities with min-max method (range 0-1) by year and month
#' @description This function normalizes (i.e., scales) monthly detection probabilities for
#' each species and primer that were calculated with the previous function,
#' \code{calc_det_prob()}. Outputs fed into figure and window calculation functions.
#' * NOTE: Currently this function only works for metabarcoding data.
#'
#' @param data (required, data.frame) Data.frame imported with read_data(). Required
#' to join taxonomic information.
#' @param ecodistrict.select (required, character) Ecodistrict present in data.frame.
#'
#' @return Grouped data.frame with 15 columns:
#' \itemize{
#' \item\code{id}: unique species;primer identifier
#' \item\code{ecodistrict},
#' \item\code{month},
#' \item\code{detect}: number of detections
#' \item\code{nondetect}: number of non-detections
#' \item\code{scaleP}:  detection probability scaled to range 0-1
#' \item\code{GOTeDNA_ID},
#' \item\code{species},
#' \item\code{primer},
#' \item\code{phylum},
#' \item\code{class},
#' \item\code{order},
#' \item\code{family},
#' \item\code{genus},
#' \item\code{fill},
#' }
#'

#' @author Tim Barrett \email{Tim.Barrett@@dfo-mpo.gc.ca}
#' @author Melissa Morrison \email{Melissa.Morrison@@dfo-mpo.gc.ca}
#' @rdname scale_prob_by_month
#' @export
#' @examples
#' \dontrun{
#' scale_prob_by_month(data = D_mb, ecodistrict.select = "Scotian Shelf")
#' }
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
scale_prob_by_month <- function(data, ecodistrict.select) {

  # Implement min max scaling of detection probabilities
  minMax <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }

  if(!ecodistrict.select %in% data$ecodistrict) {
    stop("Ecodistrict not found in data")
  }

  # Calling the data in is to merge taxonomic info
  data %<>%
    dplyr::filter(., ecodistrict %in% ecodistrict.select)

  # transform CPscaled to data frame
  non_det <- as.data.frame(matrix(ncol=14,nrow=length(names(newP_agg))))
  names(non_det) <- c("id",paste0("M",1:12),"ecodistrict")
  non_det$id <- names(newP_agg)

  for(i in 1:length(names(newP_agg))){
    non_det[i,newP_agg[[i]]$month+1] <- newP_agg[[i]]$n-newP_agg[[i]]$nd
    non_det[i, "ecodistrict"] <- newP_agg[[i]][1,"ecodistrict"]
  }

  det = as.data.frame(matrix(ncol=14,nrow=length(names(newP_agg))))
  names(det) <- c("id",paste0("M",1:12),"ecodistrict")
  det$id <- names(newP_agg)

  for(i in 1:length(names(newP_agg))){
    det[i,newP_agg[[i]]$month+1] <- newP_agg[[i]]$nd
    det[i, "ecodistrict"] <- newP_agg[[i]][1,"ecodistrict"]
  }

  non_det <- non_det %>%
    tidyr::pivot_longer(-c(id, ecodistrict), names_to = "month", values_to = "nondetect") %>%
    dplyr::group_by(id, ecodistrict) %>%
    dplyr::mutate(month = dplyr::recode(month,
                                        "M1"=1,
                                        "M2"=2,
                                        "M3"=3,
                                        "M4"=4,
                                        "M5"=5,
                                        "M6"=6,
                                        "M7"=7,
                                        "M8"=8,
                                        "M9"=9,
                                        "M10"=10,
                                        "M11"=11,
                                        "M12"=12))
  det <- det %>%
    tidyr::pivot_longer(-c(id, ecodistrict), names_to = "month", values_to = "detect") %>%
    dplyr::group_by(id, ecodistrict) %>%
    dplyr::mutate(month = dplyr::recode(month,
                                        "M1"=1,
                                        "M2"=2,
                                        "M3"=3,
                                        "M4"=4,
                                        "M5"=5,
                                        "M6"=6,
                                        "M7"=7,
                                        "M8"=8,
                                        "M9"=9,
                                        "M10"=10,
                                        "M11"=11,
                                        "M12"=12))

  DF = dplyr::left_join(det,non_det[,c(1,3,4)], by=c("id","month"))

  CP = as.data.frame(matrix(ncol=14,nrow=length(names(newP_agg))))
  names(CP) <- c("id",paste0("M",1:12),"ecodistrict")
  CP$id <- names(newP_agg)

  CPscaled = lapply(newP_agg, function(x) {
    data.frame(x) %>%
      #  dplyr::mutate(scaleP = minMax(x$p)) %>%
      dplyr::mutate(x, scaleP = dplyr::case_when(
        p == 1 ~ 1,
        p == 0 ~ 0,
        p != 0 | 1 ~ minMax(x$p)
      ))
  })

  for(i in 1:length(names(CPscaled))){
    CP[i,CPscaled[[i]]$month+1] <- CPscaled[[i]]$scaleP
    CP[i, "ecodistrict"] <- CPscaled[[i]][1,"ecodistrict"]
  }

  CP_long = CP %>%
    tidyr::pivot_longer(-c(id, ecodistrict), names_to = "month", values_to = "scaleP") %>%
    dplyr::group_by(id, ecodistrict) %>%
    dplyr::mutate(month = dplyr::recode(month,
                                        "M1"=1,
                                        "M2"=2,
                                        "M3"=3,
                                        "M4"=4,
                                        "M5"=5,
                                        "M6"=6,
                                        "M7"=7,
                                        "M8"=8,
                                        "M9"=9,
                                        "M10"=10,
                                        "M11"=11,
                                        "M12"=12))

  DF <- dplyr::left_join(DF,CP_long, by=c("id","ecodistrict","month"))
  DF[c("GOTeDNA_ID","species","primer")] = stringr::str_split_fixed(DF$id, ";", 3)
  DF <- DF %>%
    dplyr::left_join(unique(data[,c("phylum","class","order","family","genus","scientificName")]),
                     by=c("species"="scientificName"),
                     multiple="first")

  # Interpolate missing months
  DF$fill <- NA #add column

  for(species in unique(DF$id)) {

    DF1 <- DF[DF$id==species,]

    #then add code for interpolation that starts with DF2 = .....
    # add dataframe above and below to help will fills for jan and dec. Needed to have 4 copyies because of max fucntion used below
    DF2 <- rbind(cbind(DF1,data.frame(G=1)),
                 cbind(DF1,data.frame(G=2)),
                 cbind(DF1,data.frame(G=3)),
                 cbind(DF1,data.frame(G=4)))

    DF2$fill <- DF2$scaleP

    # which months are NA and define groups with sequential NAs
    month_na_id <- which(is.na(DF2$scaleP))
    nagroups <- cumsum(c(1, abs(month_na_id[-length(month_na_id)] - month_na_id[-1]) > 1))

    # identify which NA groups are in G = 2 or 3 (ignore 1 and 2)
    nagroupsG <- list()
    for(i in unique(nagroups)) {nagroupsG[[i]] <- max(DF2$G[month_na_id[which(nagroups==i)]])}
    nagroupsGv <- unlist(nagroupsG)
    nagroupsf <- which(nagroupsGv%in%2:3)

    # loop over final NA groups and fill in using average
    for(i in unique(nagroupsf)){
      DF2$fill[month_na_id[which(nagroups==i)]] <- (DF2$scaleP[min(month_na_id[which(nagroups==i)])-1] + DF2$scaleP[max(month_na_id[which(nagroups==i)])+1])/2
    }
    #then put values from DF3 back into DF$sp.pr. This assumes that the months are all in the correct order (jan to dec) in DF3 and test_interp
    # DF3 is final DF with fills
    DF3 <- DF2[DF2$G==2,]
    DF$fill[DF$id==species] <- DF3$fill
  }

  Pscaled_agg <- DF %>%
    dplyr::ungroup()

  return(Pscaled_agg)
}
