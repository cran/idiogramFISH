#' removeNAFromList
#' This is an internal function that removes NA from list
#'
#' It returns rulers (axis)
#'
#' @keywords internal
#'
#' @param noNA original list
#' @param areNA which are NA
#' @param mycolors vector of colors
#' @importFrom grDevices col2rgb
#'
#' @return list

removeNAFromList<-function(noNA,areNA){
  noNA[areNA]<-NA
  noNA<-noNA[!is.na(noNA)]
  return(noNA)
}

filter_colors<- function(mycolors){
  mycolors <- mycolors[mycolors!=""]
  mycolors <- tryCatch(mycolors[sapply(mycolors, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) {
               message(crayon::red(paste("Color",X,"invalid, removed")
               ) ); return(FALSE)
             })
  } )], error=function(e) {character(0) } )
  return(mycolors)
}
