# insertInList
#' This is an internal function that inserts in vector or list
#'
#' It returns list or vector
#'
#' @keywords internal
#'
#' @param list vector or list
#' @param pos index to insert in
#' @param elems elements to be inserted
#' @return list or vector
insertInList <- function(list, pos, elems) {
  len <- length(list)
  j <- 0
  for (i in seq_along(pos)) {
    if (pos[i] == 1) {
      list <- c(elems[j + 1], list)
    } else if (pos[i] == length(list) + 1) {
      list <- c(list, elems[j + 1])
    } else if (pos[i] <= length(list)) {
      list <- c(list[1:(pos[i] - 1 + j)], elems[j + 1], list[(pos[i] + j):(len + j)])
    } else {
      message(crayon::blue("Possible problem parsing joins"))
    }
    j <- j + 1
  }
  return(list)
}
