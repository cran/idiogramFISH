#' makeVectorNames
#' This is an internal function that makes a vector of names
#'
#' @keywords internal
#'
#' @param namedList original list
#' @param attr attribute
#' @param selector value of attribute
#'
#' @return character vector

makeVectorNames<-function(namedList,attr,selector){
  Vector<-integer()
  for(i in 1:length(namedList)){
    if(attr(namedList[[i]],attr)==selector){Vector<-c(Vector,i)}
  }
  nameVector<-names(namedList)[Vector]
  return(nameVector)
}
