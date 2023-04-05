#' checkArmHolocenError
#' This is an internal function that makes list of d.fs
#'
#' @keywords internal
#'
#' @param listOfdfArmGISHInternal df list
#' @param CenTypeNames character vector
#'
#' @return list

checkArmHolocenError <- function(listOfdfArmGISHInternal, CenTypeNames) {
  listOfdfArmGISHInternalHolocen <- listOfdfArmGISHInternal[which(names(listOfdfArmGISHInternal) %in% CenTypeNames)]

  if (length(listOfdfArmGISHInternalHolocen) > 0) {
    message(crayon::red("\nERROR: Some mark(s) of an holocentric karyotype as being in a chromosome arm REMOVED"))
  }
}
