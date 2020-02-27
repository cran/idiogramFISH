#' filterExtraOTU
#' This is an internal function that filters list of d.f. of marks
#'
#' It returns rulers (axis)
#'
#' @keywords internal
#'
#' @param listOfdfMarkPosCenType only d.f. of this cen. type
#' @param listOfdfChromSize list of all d.f. of chr.
#'
#' @return data.frame

filterExtraOTU<-function(listOfdfChromSize,listOfdfMarkPosCenType){

  if(length(setdiff(names(listOfdfMarkPosCenType ),
                    names(listOfdfChromSize) ) )>0){
    diff <- setdiff(names(listOfdfMarkPosCenType ),
                    names(listOfdfChromSize) )
    message(
      crayon::red(
        paste(c("\nWarning:",
                diff,
                "OTU(s) of dfMarkPos data.frame NOT in Chr. size (main) data.frame, they will not be plotted"),
              sep=" ", collapse = " "
        ) # pas
      ) ) #message

    #
    #   delete names in diff
    #

    listOfdfMarkPosCenType<-listOfdfMarkPosCenType[which(names(listOfdfMarkPosCenType) %in% diff ) ]

  }
  return(listOfdfMarkPosCenType)
}
