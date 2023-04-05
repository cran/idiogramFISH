#' newOrderColumn
#' This is an internal function that adds neworder column to
#' listOfdfChromSizeCenType
#'
#' It returns rulers (axis)
#'
#' @keywords internal
#'
#' @param listOfdfChromSizeCenType only d.f. of this cen. type
#' @param listOfdfChromSize list of all d.f. of chr.
#'
#' @return data.frame

newOrderColumn <- function(listOfdfChromSize, listOfdfChromSizeCenType) {
  for (s in seq_along(listOfdfChromSize)) {
    selecteddfChromData <- which(names(listOfdfChromSizeCenType) == names(listOfdfChromSize)[[s]])
    if (length(selecteddfChromData) > 0) {
      listOfdfChromSizeCenType[[selecteddfChromData]]$neworder <- listOfdfChromSize[[s]]$neworder[match(
        listOfdfChromSizeCenType[[selecteddfChromData]]$chrName,
        listOfdfChromSize[[s]]$chrName
      )]
    }
  }
  return(listOfdfChromSizeCenType)
}

addNeworderColumn <- function(listOfdfChromSize, orderlist) {
  for (s in seq_along(listOfdfChromSize)) {
    if (inherits(listOfdfChromSize[[s]], "data.frame")) {
      listOfdfChromSize[[s]] <- listOfdfChromSize[[s]][orderlist[[s]], ] # important THIS orders
      listOfdfChromSize[[s]]$neworder <- seq_len(nrow(listOfdfChromSize[[s]]))
    }
  }
  return(listOfdfChromSize)
}
