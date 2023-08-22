#' textLabelDots
#' This is an internal function that plot labels of cM marks
#'
#' It returns a plot
#'
#' @keywords internal
#'
#' @param listOfdfChromSize radius
#' @param listOfdfMarkPosCr color
#' @param specialChrSpacing vertices number
#' @param chrSpacing aspect
#' @param markLabelSize size of mark label font
#' @param pattern pattern to remove from mark name
#' @param xBoundariesQuar x size
#'
#' @return plotted text
#' @importFrom graphics polygon text
#'

textLabelDots <- function(xMarkCr, yMarkCr, listOfdfChromSize, listOfdfMarkPosCr, specialChrSpacing,
                          chrSpacing, markLabelSize, pattern, bannedMarkName, xBoundariesQuar) {
  for (s in seq_along(xMarkCr)) {
    corr_index <- which(names(listOfdfChromSize) %in% names(listOfdfMarkPosCr)[[s]])
    if (attr(listOfdfChromSize[[corr_index]], "ytitle") == "cM") {
      chrSpacing2 <- specialChrSpacing
    } else {
      chrSpacing2 <- chrSpacing
    }

    for (m in seq_along(xMarkCr[[s]])) {
      if (!listOfdfMarkPosCr[[s]]$markName[m] %in% bannedMarkName) {
        mapply(function(x, y, z) {
          graphics::text(
            x = x,
            y = y,
            label = z,
            cex = markLabelSize,

            adj = 0
          )
        },
        x = xMarkCr[[s]][[m]][[2]] + xBoundariesQuar + chrSpacing2 * .1,
        y = (yMarkCr[[s]][[m]][1]),
        z = sub(pattern, "", listOfdfMarkPosCr[[s]]$markName[m])

        )
      }
    }
  }
}
