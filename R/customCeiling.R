#' Function to do ceiling at different levels
#'
#' @description This function reads number to round
#'
#' @param x number to round
#' @param customDecimals desired decimal positions
#' @keywords internal
#' @return numeric

customCeiling <- function(x, customDecimals = 1) {
  x2 <- x * 10^customDecimals
  ceiling(x2) / 10^customDecimals
}
