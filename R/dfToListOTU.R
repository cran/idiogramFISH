#' dfToListOTU
#' This is an internal function that makes list of d.fs
#'
#' @keywords internal
#'
#' @param dfLookOTU df with or without otu to list
#'
#' @return list

dfToListOTU<-function(dfLookOTU){

  if("OTU" %in% colnames(dfLookOTU)){
    listOfdfLookOTU <- base::split(dfLookOTU, factor(dfLookOTU$OTU,levels = unique(dfLookOTU$OTU)  ) )
    names(listOfdfLookOTU) <- unique(dfLookOTU$OTU)
  } else {
    listOfdfLookOTU <- list(dfLookOTU)
    names(listOfdfLookOTU)<-1
  }

return(listOfdfLookOTU)

}
