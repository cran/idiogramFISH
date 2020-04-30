#' newOrderColumn
#' This is an internal function that adds neworder column to listOfdfChromSizeCenType
#'
#' It returns rulers (axis)
#'
#' @keywords internal
#'
#' @param listOfdfChromSizeCenType only d.f. of this cen. type
#' @param listOfdfChromSize list of all d.f. of chr.
#'
#' @return data.frame

newOrderColumn<-function(listOfdfChromSize,listOfdfChromSizeCenType){
  for (s in 1:length(listOfdfChromSize)){
    selecteddfChromData<-which(names(listOfdfChromSizeCenType)==names(listOfdfChromSize)[[s]])
    if(length(selecteddfChromData)>0){
      listOfdfChromSizeCenType[[selecteddfChromData]]$neworder <- listOfdfChromSize[[s]]$neworder[match(
        listOfdfChromSizeCenType[[selecteddfChromData]]$chrName,
        listOfdfChromSize[[s]]$chrName)]

    }
  }
  return(listOfdfChromSizeCenType)
}

addNeworderColumn<-function(listOfdfChromSize,orderlist){
  for (s in 1:length(listOfdfChromSize)){
    if(class(listOfdfChromSize[[s]])=="data.frame") {
      listOfdfChromSize[[s]]<-listOfdfChromSize[[s]][orderlist[[s]], ] # important THIS orders
      listOfdfChromSize[[s]]$neworder<-1:nrow(listOfdfChromSize[[s]])
    }
  } # end for
  return(listOfdfChromSize)
}

