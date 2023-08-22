# plotlabelsright
#' This is an internal function to plot labels to the right, when "aside" for
#' its position
#'
#' It returns a graphic element with the legends
#'
#' @keywords internal
#'
#' @param y The y axis coordinates of chromosomes
#' @param markLabelSpacer distance from right chr to legend
#' @param chrWidth chr widht
#' @param dfMarkColorInt data.frame of mark characteristics
#' @param allMarkMaxSize maximum size of marks
#' @param normalizeToOne transformation value of karyotype height
#' @param markLabelSize font size of legends
#' @param xfactor relative proportion of x vs y axes
#' @param legendWidth factor to increase width of legend squares and dots
#' @param legendHeight factor to increase height of legend squares and dots
#' @param n numeric, vertices for round parts
#' @param pattern, character, regex to remove from markNames
#' @param legendYcoord numeric modify Y position of legend
#' @param useOneDot boolean, when \code{TRUE} plots only one dot

#' @importFrom graphics polygon text
#'
#' @return Returns a graphics element
#'

plotlabelsright <- function(maxx, y, markLabelSpacer, chrWidth, dfMarkColorInt, allMarkMaxSize, normalizeToOne,
                            markLabelSize, xfactor, legendWidth, legendHeight, n, pattern,
                            legendYcoord, useOneDot, dotsAsOval, circularPlot) {
  miny <- (min(unlist(y)))

  chrWidth <- chrWidth * legendWidth

  labelx <- (maxx + markLabelSpacer) + (c(0, chrWidth, chrWidth, 0) + 0)

  labelx <- t(replicate(nrow(dfMarkColorInt), labelx))

  if (is.na(legendHeight)) {
    if (exists("allMarkMaxSize")) {
      legendHeight <- allMarkMaxSize * normalizeToOne
    } else {
      legendHeight <- 1 * normalizeToOne
    }
  } else {
    legendHeight <- legendHeight * normalizeToOne
  }

  labely <- sapply(
    c(0, 0, legendHeight, legendHeight),
    function(x) x + ((0:(nrow(dfMarkColorInt) - 1)) * (legendHeight * 2))
  ) + miny + legendYcoord

  #
  #   labelx and y to matrix
  #

  if (!inherits(labely, "matrix")) {
    labely <- t(as.matrix(labely))
  }

  if (!inherits(labelx, "matrix")) {
    labelx <- t(as.matrix(labelx))
  }

  #
  # remove dots
  #

  labelytoplot <- labely[which(dfMarkColorInt$style != "dots"), ]
  labelxtoplot <- labelx[which(dfMarkColorInt$style != "dots"), ]

  #
  #   labelxplot and y to matrix
  #

  ifelse(inherits(labelytoplot, "matrix"),
    labelytoplot <- base::split(labelytoplot, row(labelytoplot)),
    labelytoplot <- list(t(as.matrix(labelytoplot)))
  )

  ifelse(
    inherits(labelxtoplot, "matrix"),
    labelxtoplot <- base::split(labelxtoplot, row(labelxtoplot)),
    labelxtoplot <- list(t(as.matrix(labelxtoplot)))
  )
  # squares labels

  if (length(dfMarkColorInt$markName[which(dfMarkColorInt$style != "dots")]) > 0) {
    marks <- dfMarkColorInt$markColor[which(dfMarkColorInt$style != "dots")]

    borders <- dfMarkColorInt$markBorderColor[which(dfMarkColorInt$style != "dots")]

    graphics::text(
      x = t(labelx[which(dfMarkColorInt$style != "dots"), 2]),
      y = t(
        (c(labely[which(dfMarkColorInt$style != "dots"), 1] +
          labely[which(dfMarkColorInt$style != "dots"), 3]) / 2) - .01
      ),
      labels = sub(pattern, "", dfMarkColorInt$markName[which(dfMarkColorInt$style != "dots")]),
      cex = markLabelSize,
      col = "black",
      pos = 4
    )

    mapply(function(x, y, z, w) {
      graphics::polygon(
        x = x,
        y = y,
        col = z,
        lwd = rep(.5, length(marks)),
        border = w
      )
    },
    x = labelxtoplot,
    y = labelytoplot,
    z = marks,
    w = borders
    )
  }


  ##########################################
  # circular labels to the right DOTS
  ##########################################

  {
    labelxdiff <- (max(labelx) - min(labelx))


    if (useOneDot == FALSE) {
      diffxQuar <- labelxdiff / 4
      xcenters <- c((min(labelx) + diffxQuar), (min(labelx) + 3 * diffxQuar))
    } else {
      diffxHalf <- labelxdiff / 2
      xcenters <- (min(labelx) + diffxHalf)
    }
    listOfxcenters <- rep(list(xcenters), nrow(dfMarkColorInt[which(dfMarkColorInt$style == "dots"), ]))

    labelydiffs <- labely[which(dfMarkColorInt$style == "dots"), 3] - labely[which(dfMarkColorInt$style == "dots"), 2]

    labelydiffhalf <- labelydiffs[1] / 2

    ycenters <- labely[which(dfMarkColorInt$style == "dots"), 2] + labelydiffhalf

    if (useOneDot == FALSE) {
      listOfycenters <- lapply(ycenters, function(x) rep(x, 2))
    } else {
      listOfycenters <- lapply(ycenters, function(x) rep(x, 1))
    }

    rad <- labelydiffhalf
    radX <- labelxdiff / 2

    if (circularPlot || dotsAsOval == FALSE) {
      radX <- rad
    }

    yfactor <- 1

    if (length(listOfxcenters) > 0) {
      lapply(
        seq_along(listOfxcenters), function(u) {
          mapply(function(x, y, radiusX, radius, z, w) {
            pts2 <- seq(0, 2 * pi, length.out = n)
            xy2 <- cbind(x + (radiusX * sin(pts2) * xfactor), y + (radius * cos(pts2) * yfactor))
            graphics::polygon(xy2[, 1],
              xy2[, 2],
              col = z,
              border = w
            )
          },
          x = listOfxcenters[[u]],
          y = listOfycenters[[u]],
          radiusX = radX,
          radius = rad,
          z = dfMarkColorInt$markColor[which(dfMarkColorInt$style == "dots")][[u]],
          w = dfMarkColorInt$markBorderColor[which(dfMarkColorInt$style == "dots")][[u]]
          )
        }
      )
      graphics::text(
        x = t(labelx[which(dfMarkColorInt$style == "dots"), 2]),
        y = t(
          c(labely[which(dfMarkColorInt$style == "dots"), 1] +
            labely[which(dfMarkColorInt$style == "dots"), 3]) / 2 - .01
        ),
        labels = sub(pattern, "", dfMarkColorInt$markName[which(dfMarkColorInt$style == "dots")]),
        cex = markLabelSize,
        col = "black",
        pos = 4
      )
    }
  }
}
