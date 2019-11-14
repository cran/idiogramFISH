#' Function imported from package badger which is not available in CRAN
#' @description imported from package badger
#'
#' @param x x
#' @param y y
#' @param color color
#' @param url url
#'
#' @keywords internal
#'
#' @return character
#'
#'
badge_custom <- function (x, y, color, url = NULL)
{
  x <- gsub(" ", "%20", x)
  y <- gsub(" ", "%20", y)
  x <- gsub("-", "--", x)
  y <- gsub("-", "--", y)
  badge <- paste0("![](https://img.shields.io/badge/", x, "-",
                  y, "-", color, ".svg)")
  if (is.null(url))
    return(badge)
  paste0("[", badge, "](", url, ")")
}
