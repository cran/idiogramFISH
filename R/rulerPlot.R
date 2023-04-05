#' rulerPlot
#' This is an internal function that plots rulers
#'
#' It returns rulers (axis)
#'
#' @keywords internal
#'
#' @param ycoord y coords
#' @param listOfdfChromSize list of all d.f chr size
#' @param listOfdfChromSizeCenType only d.f. of this cen. type
#' @param fromZerotoMax intervals
#' @param rulerNumberSize font
#' @param rulerPos pos. of ruler
#' @param ruler.tck tick size and orient.
#'
#' @return axis
#' @importFrom graphics axis rug
#'

rulerPlot <- function(ycoord, listOfdfChromSize, listOfdfChromSizeCenType, fromZerotoMax, rulerNumberSize, rulerPos, ruler.tck, lwd.chr,
                      moveKarHor2, mkhValue, useMinorTicks, miniTickFactor, verticalPlot, moveAllKarValueHor) {
  amount <- if (verticalPlot) {
    length(ycoord)
  } else {
    1
  }

  rulerNumberSize <- if (rulerNumberSize <= 0) {
    0.1
  } else {
    rulerNumberSize
  }

  for (i in 1:amount) {
    corr_index <- which(names(listOfdfChromSize) %in% names(listOfdfChromSizeCenType)[[i]])

    divisor2 <- as.numeric(attr(listOfdfChromSize[[corr_index]], "divisor"))

    if (names(listOfdfChromSize)[corr_index] %in% moveKarHor2) {
      rulerPos2 <- rulerPos + mkhValue + moveAllKarValueHor
    } else {
      rulerPos2 <- rulerPos + moveAllKarValueHor
    }

    if (attr(listOfdfChromSizeCenType[[i]], "ytitle") == "cM") {
      labels <- unlist(fromZerotoMax[[i]]) * divisor2
    } else if (attr(listOfdfChromSizeCenType[[i]], "ytitle") == "Mb") {
      labels <- unlist(fromZerotoMax[[i]]) * divisor2 / 1e6
    } else { # ytitle notmb
      labels <- unlist(fromZerotoMax[[i]]) * divisor2
    }

    #
    #     long tick and labels
    #

    locations <- unlist(ycoord[[i]])
    miniInterval <- (locations[2] - locations[1]) / miniTickFactor
    locationsMinor <- seq(locations[1], locations[length(locations)], miniInterval)

    graphics::axis(
      side = 2,
      at = locations,
      labels = labels,
      cex.axis = rulerNumberSize,
      las = 1,
      pos = rulerPos2,
      tck = ruler.tck,
      lwd = lwd.chr,
      lwd.ticks = lwd.chr
    )
    if (useMinorTicks) {
      rug(x = locationsMinor, ticksize = ruler.tck / 2, side = 2, pos = rulerPos2)
    }

    # ) # l
  } # FOR
}
