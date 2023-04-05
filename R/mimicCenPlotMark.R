#' mimicCenPlotMark
#' This is an internal function that plot marks
#'
#' It returns a plot
#'
#' @keywords internal
#'
#' @param squareness squareness of vertices <
#' @param xMark x component of polygon
#' @param yMark yMark component of polygon
#' @param dfMarkColorInt colors for marks
#' @param listOfdfMarkPosCenStyle list of df. of mark pos.
#' @param chrWidth numeric, width of chr.
#' @param specialChrWidth numeric, width of chr.
#' @param yfactor yMark distortion based on canvas proportion
#' @param n numeric, to define vertices of rounded portions
#' @param lwd.chr thick of border line
#' @param listOfdfChromSize chr size df list
#' @param circularPlot boolean TRUE for circ.
#' @param y list, y coords.
#' @param markLabelSize numeric, font size
#' @param separFactor numeric, sep. kars.
#' @param labelSpacing numeric, spacing
#' @param circleCenter numeric
#' @param circleCenterY numeric
#' @param radius numeric
#' @param ylistTransChr list, transf. coords.
#' @param rotation rotate
#' @param labelOutwards srt
#'
#' @return plot
#' @importFrom graphics polygon text
#'

mimicCenPlotMark <- function(squareness, xMark, yMark,
                             defCenStyleCol,
                             listOfdfMarkPosCenStyle,
                             chrWidth, specialChrWidth,
                             yfactor,
                             n,
                             lwd.mimicCen, listOfdfChromSize,
                             circularPlot,
                             y,
                             markLabelSize,
                             separFactor,
                             labelSpacing,
                             circleCenter, circleCenterY, radius,
                             ylistTransChr, rotation, labelOutwards,
                             yMarkLine, xMarkRightLine, xMarkLeftLine,
                             x,
                             cenFormat = "triangle",
                             pts) {
  if (squareness <= 20) {
    roundedX <- roundedY <- list()

    for (s in seq_along(yMark)) {
      corr_index <- which(names(listOfdfChromSize) %in% names(listOfdfMarkPosCenStyle)[[s]])

      if (attr(listOfdfChromSize[[corr_index]], "ytitle") == "cM") {
        chrWidth2 <- specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }

      r2 <- chrWidth2 / (squareness * 2)

      xyCoords <- mapXYCenMimic(
        1, (length(yMark[[s]])),
        yMark[[s]],
        xMark[[s]],
        yfactor, r2,
        pts,
        cenFormat
      )

      roundedX[[s]] <- xyCoords$roundedX
      roundedY[[s]] <- xyCoords$roundedY

      attr(roundedY[[s]], "spname") <- attr(yMark[[s]], "spname")
      attr(roundedX[[s]], "spname") <- attr(xMark[[s]], "spname")
    }

    names(roundedY) <- names(yMark)
    xMark <- roundedX
    yMark <- roundedY
  }

  if (circularPlot == FALSE) {
    lapply(
      seq_along(xMark), function(w) {
        mapply(function(x, y, z) {
          graphics::polygon(
            x = x,
            y = y,
            col = defCenStyleCol,
            lwd = 0.05,
            border = defCenStyleCol
          )
        },
        x = xMark[[w]],
        y = yMark[[w]],
        z = listOfdfMarkPosCenStyle[[w]]$markName
        )
      }
    )

    #
    # left line
    #
    lapply(
      seq_along(xMarkLeftLine), function(w) {
        mapply(function(x, y, z) {
          graphics::lines(
            x = x,
            y = y,
            col = defCenStyleCol,
            lwd = lwd.mimicCen,
          )
        },
        x = xMarkLeftLine[[w]],
        y = yMarkLine[[w]],
        z = listOfdfMarkPosCenStyle[[w]]$markName
        )
      }
    )

    #
    # r. line
    #

    lapply(seq_along(xMarkRightLine), function(w) {
      mapply(function(x, y, z) {
        graphics::lines(
          x = x,
          y = y,
          col = defCenStyleCol
          , lwd = lwd.mimicCen,
        )
      },
      x = xMarkRightLine[[w]],
      y = yMarkLine[[w]],
      z = listOfdfMarkPosCenStyle[[w]]$markName
      )
    })
  } else {

    #
    #   x to vertical - cenFormat where
    #

    xlistNew <- xHortoVer(xMark)

    yMarkPer <- markMapPer(yMark, y)
    ylistTransMark <- transyListMark(yMarkPer, ylistTransChr)


    circleMapsMarks <- applyMapCircle(radius, circleCenter, circleCenterY, separFactor,
      ylistTransMark, xlistNew, n, 0, chrWidth,
      rotation = rotation
    )

    drawCenStyle(circleMapsMarks, defCenStyleCol, lwd.mimicCen)

    ############# left line
    xlistNew <- xHortoVer(xMarkLeftLine)

    yMarkPer <- markMapPer(yMarkLine, y)

    ylistTransMark <- transyListMark(yMarkPer, ylistTransChr)

    circleMapsMarks <- applyMapCircle(radius, circleCenter, circleCenterY, separFactor,
      ylistTransMark, xlistNew, n, 0, chrWidth, rotation = rotation)

    drawPlotMarkLine(circleMapsMarks, defCenStyleCol, lwd.mimicCen)
    ############# right line
    xlistNew <- xHortoVerDots(xMarkRightLine, x)

    yMarkPer <- markMapPer(yMarkLine, y)

    ylistTransMark <- transyListMark(yMarkPer, ylistTransChr)

    circleMapsMarks <- applyMapCircle(radius, circleCenter, circleCenterY, separFactor,
      ylistTransMark, xlistNew, n, 0, chrWidth, rotation = rotation)

    drawPlotMarkLine(circleMapsMarks, defCenStyleCol, lwd.mimicCen)
  }
}

mimicCenPlotMarkInside <- function(pattern, bannedMarkName, squareness, xMarkCenStyleBody, yMarkCenStyleBody, #nolint: object_usage_linter
                                   defCenStyleCol,
                                   dfMarkColorInt,
                                   listOfdfMarkPosCenStyle,
                                   chrWidth, specialChrWidth,
                                   yfactor, n,
                                   lwd.mimicCen, listOfdfChromSize,
                                   circularPlot,
                                   y,
                                   markLabelSize,
                                   separFactor,
                                   labelSpacing,
                                   circleCenter, circleCenterY, radius,
                                   ylistTransChr, rotation, labelOutwards,
                                   yMarkLine, xMarkRightLine, xMarkLeftLine,
                                   x,
                                   lwd.chr,
                                   legend,
                                   cenFormat = "triangle",
                                   pts,
                                   alpha_val = 1) {
  if (squareness <= 20) {

    roundedXInside <- roundedYInside <- list()

    for (s in seq_along(yMarkCenStyleBody)) {
      corr_index <- which(names(listOfdfChromSize) %in% names(listOfdfMarkPosCenStyle)[[s]])

      if (attr(listOfdfChromSize[[corr_index]], "ytitle") == "cM") {
        chrWidth2 <- specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }

      r2 <- chrWidth2 / (squareness * 2)

      #
      # second rounding
      #

      xyCoords <- mapXYCenMimicInside(
        1,
        length(yMarkCenStyleBody[[s]]),
        yMarkCenStyleBody[[s]],
        xMarkCenStyleBody[[s]],
        yfactor,
        r2,
        pts,
        cenFormat
      )

      roundedXInside[[s]] <- xyCoords$roundedX
      roundedYInside[[s]] <- xyCoords$roundedY

      attr(roundedYInside[[s]], "spname") <- attr(yMarkCenStyleBody[[s]], "spname")
      attr(roundedXInside[[s]], "spname") <- attr(xMarkCenStyleBody[[s]], "spname")
    }

    names(roundedYInside) <- names(yMarkCenStyleBody)
    xMarkCenStyleBody <- roundedXInside
    yMarkCenStyleBody <- roundedYInside
  }

  if (circularPlot == FALSE) {
    lapply(
      seq_along(yMarkCenStyleBody), function(m) {
        mapply(function(x, y, z) {
          graphics::polygon(
            x = x,
            y = y,
            col = dfMarkColorInt$markColor[match(z, dfMarkColorInt$markName)],
            lwd = lwd.chr,
            border = dfMarkColorInt$markBorderColor[match(z, dfMarkColorInt$markName)]
          )
        },
        x = xMarkCenStyleBody[[m]],
        y = yMarkCenStyleBody[[m]],
        z = listOfdfMarkPosCenStyle[[m]]$markName # ifelse here gives error
        )
      }
    )
  } else {

    #
    #   x to vertical original as mark
    #

    xlistNew <- xHortoVer(xMarkCenStyleBody)

    yMarkPer <- markMapPer(yMarkCenStyleBody, y)

    ylistTransMark <- transyListMark(yMarkPer, ylistTransChr)

    ###
    # as in cen, does not work, names
    ###

    circleMapsMarks <- applyMapCircle(radius, circleCenter, circleCenterY, separFactor,
      ylistTransMark, xlistNew, n, 0, chrWidth,
      rotation = rotation
    )

    drawPlotMark(circleMapsMarks, dfMarkColorInt, listOfdfMarkPosCenStyle, lwd.chr, alpha_val)

    if (legend == "inline") {
      circleMapsMarksLabelMimicCen <- applyMapCircle(radius, circleCenter, circleCenterY, separFactor,
        ylistTransMark, xlistNew, n,
        labelSpacing, chrWidth,
        rotation = rotation
      )

      circLabelMark(
        bannedMarkName, circleMapsMarksLabelMimicCen, listOfdfMarkPosCenStyle,
        markLabelSize, pattern, labelOutwards, circleCenter, circleCenterY
      )
    }
  }
}
