# FUNCTIONS: cMPlotMark, cMLeftPlotMark
#' This is an internal function that eliminates factors
#'
#' It returns a data.frames
#'
#' @keywords internal
#'
#' @param xMark x component of polygon
#' @param yMark y component of polygon
#' @param y y of chr
#' @param dfMarkColorInt colors for marks
#' @param listOfdfMarkPoscM list of df. of mark pos.
#' @param lwd.cM thick of border line
#' @param circularPlot boolean
#' @param radius numeric
#' @param circleCenter numeric
#' @param circleCenterY numeric
#' @param separFactor numeric
#' @param markLabelSize numeric
#' @param pattern character, regex
#' @param n numeric
#' @param labelSpacing numeric
#' @param chrWidth numeric, chr. width
#' @param ylistTransChr list
#' @param rotation rotation
#' @param labelOutwards srt
#' @param alpha_val numeric
#'
#' @return plot
#' @importFrom graphics polygon text
#' @importFrom scales alpha
#'
cMPlotMark <- function(bannedMarkName, xMark, yMark, y, x, dfMarkColorInt, listOfdfMarkPoscM, lwd.cM, circularPlot, #nolint: object_usage_linter
                       radius, circleCenter, circleCenterY, separFactor, markLabelSize, pattern, n, labelSpacing, chrWidth,
                       ylistTransChr, rotation, labelOutwards, alpha_val) {
  if (circularPlot == FALSE) {
    lapply(
      seq_along(xMark), function(w) {
        mapply(function(x, y, z) {
          graphics::lines(
            x = x,
            y = y,
            col = alpha(dfMarkColorInt$markColor[match(z, dfMarkColorInt$markName)], alpha_val),
            lwd = lwd.cM,
          )
        },
        x = xMark[[w]],
        y = yMark[[w]],
        z = listOfdfMarkPoscM[[w]]$markName
        )
      }
    )
  } else {

    #
    #   x to vertical
    #

    xlistNew <- xMarkMapLeft(xMark, x) # left

    yMarkPer <- markMapPercM(yMark, y)

    ylistTransMark <- transyListMark(yMarkPer, ylistTransChr)

    circleMapsMarks <- applyMapCircle(radius, circleCenter, circleCenterY, separFactor,
      ylistTransMark, xlistNew, n, 0, chrWidth,
      rotation = rotation
    )


    drawPlotMark(circleMapsMarks, dfMarkColorInt, listOfdfMarkPoscM, lwd.cM, alpha_val)

    circleMapsLabels <- applyMapCircle(radius, circleCenter, circleCenterY, separFactor,
      ylistTransMark, xlistNew, n,
      labelSpacing,
      chrWidth,
      rotation = rotation,
      label = FALSE
    )

    circLabelMark(bannedMarkName, circleMapsLabels, listOfdfMarkPoscM, markLabelSize, pattern, labelOutwards,
      circleCenter, circleCenterY,
      iscM = FALSE,
      adj = 0.5
    )
  }
}
