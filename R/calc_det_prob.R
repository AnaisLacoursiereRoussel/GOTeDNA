## Calculate detection probabilities ----
calc_det_prob = function(data, ecodistrict.select) {

  options(dplyr.summarise.inform = FALSE)

  if(!ecodistrict.select %in% data$ecodistrict) {
    stop("Ecodistrict not found in data")
  }

  data %<>%
    dplyr::filter(., ecodistrict == ecodistrict.select) %>% # select the ecodistrict to calculate per region
    dplyr::mutate(
      id = if(any(is.na(target_subfragment)==FALSE)) paste0(GOTeDNA_ID,";",scientificName,";",target_subfragment) else paste0(GOTeDNA_ID,";",scientificName,";",target_gene),
      id.yr = if(any(is.na(target_subfragment)==FALSE)) paste0(GOTeDNA_ID,";",scientificName,";",target_subfragment,";",year) else paste0(GOTeDNA_ID,";",scientificName,";",target_gene,";",year))
  # Create a variable so detection probability is calculated separately for each GOTeDNA_ID, species, and primer

  # create new list variables to store outputs
  newP <- vector('list',length(unique(data$id)))
  names(newP) <- unique(data$id)
  comps <- vector('list',length(unique(data$id)))
  SUM <- vector('list',length(unique(data$id)))
  COM <- vector('list',length(unique(data$id)))
  names(COM) <- unique(data$id)

  # calculate detection probabilities - year aggregated
  for (species in unique(data$id)){
    DF <- data[data$id%in%species,]
    uDF <- unique(DF[,c('month','detected')])
    dm <- unique(DF[,c('month')])

    SDF <- DF %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(n=dplyr::n(),
                       nd=sum(detected),
                       ecodistrict=unique(ecodistrict)) %>%
      as.data.frame()

    SDF$p <- SDF$nd/SDF$n
    SDF$s <- sqrt(SDF$p*(1-SDF$p)/SDF$n)

    if(any(SDF$n > 1)){
      newP[[species]] <- SDF
    }
  }
  newP <- newP[lengths(newP)!=0]
  #capture.output(newP, file=paste0("newP_",abbreviate(ecodistrict.select, minlength=1),".csv"))
  .GlobalEnv$newP_agg = newP

  # calculate monthly detection probability for each year

  newP <- vector('list',length(unique(data$id.yr)))
  names(newP) <- unique(data$id.yr)
  comps <- vector('list',length(unique(data$id.yr)))
  SUM <- vector('list',length(unique(data$id.yr)))
  COM <- vector('list',length(unique(data$id.yr)))
  names(COM) <- unique(data$id.yr)

  for (species in unique(data$id.yr)){
    DF <- data[data$id.yr%in%species,]
    uDF <- unique(DF[,c('month','detected')])
    dm <- unique(DF[,c('month')])

    SDF <- DF %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(n=dplyr::n(),
                       nd=sum(detected),
                       ecodistrict=unique(ecodistrict)) %>%
      as.data.frame()

    SDF$p <- SDF$nd/SDF$n
    SDF$s <- sqrt(SDF$p*(1-SDF$p)/SDF$n)

    if(any(SDF$n > 1)){
      newP[[species]] <- SDF
    }
  }
  newP <- newP[lengths(newP)!=0]
  newP <- Map(cbind, newP, id.yr=names(newP))

  .GlobalEnv$newP_yr = lapply(newP, function(x)
    dplyr::mutate(x,
                  GOTeDNA_ID = stringr::word(x$id.yr, 1, sep=stringr::fixed(";")),
                  year = stringr::word(x$id.yr, -1, sep=stringr::fixed(";")),
                  yr.mo = paste0(year,";",month),
                  id.yr=NULL)) # to obtain variation among years
}
