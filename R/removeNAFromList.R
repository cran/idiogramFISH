#' removeNAFromList
#' This is an internal function that removes NA from list
#'
#' It returns rulers (axis)
#'
#' @keywords internal
#'
#' @param noNA original list
#' @param areNA which are NA
#'
#' @return list

removeNAFromList<-function(noNA,areNA){
  noNA[areNA]<-NA
  noNA<-noNA[!is.na(noNA)]
  return(noNA)
}
