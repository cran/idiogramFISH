#' FUNCTIONS: yVertoHor, xHortoVer, xHortoVerRoundCen, xHortoVerDots,
#' mapCircle, mapRadius,
#' transYList, mapOTUnames, addOTUnames, applyMapCircle, intercalate,
#' drawPlot, drawPlotMark,
#' drawPlotMarkLine, drawCen, circLabelMark, circPlotDots, plotChrNames,
#' addChrNameAttrMark,
#' addChrNameAttrMarkDots, xMarkMap, xMarkMapLeft, markMapPer, markMapPercM,
#' markMapPerDots,
#' radDotsPer, centerMarkMapPer, transyListMark, transyListCen, transRadDots,
#' transyListMarkDots,
#' oneDot, OTUlabelsright
#'
# grep -oP '^\K\w+(?=<-function)' ../R/orientation.R | sed ':a;N;s/\n/, /;ba'
#'
#' @keywords internal
#'
#' @param y intervals
#' @param monocenNames character, vector with names of OTUs
#' @param y list, chr coords.
#'
#' @return plot
#' @importFrom graphics mtext
#' @importFrom grDevices colorRampPalette
#' @importFrom scales alpha

yVertoHor <- function(y, monocenNames) { #nolint: cyclocomp_linter
  ylistNew <- list()

  for (s in seq_along(y)) {
    if (names(y[s]) %in% monocenNames) { # mono test

      ylistNew[[s]] <- y[[s]]

      if (length(y[[s]]) >= (2 * 2)) {
        diffPrevious <- 0

        for (i in seq(1, (length(y[[s]]) - 1), by = 2)) {
          ylistNew[[s]][[i]] <- ylistNew[[s]][[i]] - min(y[[s]][[i]], na.rm = TRUE)
          ylistNew[[s]][[i + 1]] <- ylistNew[[s]][[i + 1]] - min(y[[s]][[i]], na.rm = TRUE)
        }
        i <- 3
        for (i in seq(3, (length(y[[s]]) - 1), by = 2)) {
          diffPrevious <- diffPrevious + max(ylistNew[[s]][[i - 1]], na.rm = TRUE) - min(ylistNew[[s]][[i - 2]], na.rm = TRUE)

          ylistNew[[s]][[i]] <- ylistNew[[s]][[i]] + diffPrevious
          ylistNew[[s]][[i + 1]] <- ylistNew[[s]][[i + 1]] + diffPrevious
        }
        for (c in seq_along(y[[s]])) {
          attr(ylistNew[[s]][[c]], "chrName1") <- attr(y[[s]][[c]], "chrName1")
        }
      }
    } else { # end mono beg. holo
      ylistNew[[s]] <- y[[s]]
      if (length(y[[s]]) >= 2) {
        diffPrevious <- 0
        for (i in 2:length(y[[s]])) {
          diffPrevious <- diffPrevious + max(ylistNew[[s]][[i - 1]], na.rm = TRUE) - min(ylistNew[[s]][[i - 1]], na.rm = TRUE)
          ylistNew[[s]][[i]] <- ylistNew[[s]][[i]] + diffPrevious
        }
        for (c in seq_along(y[[s]])) {
          attr(ylistNew[[s]][[c]], "chrName1") <- attr(y[[s]][[c]], "chrName1")
        }
      }
    }
  }
  return(ylistNew)
}

#' @param xlist list, x coordinates
#' @keywords internal
xHortoVer <- function(xlist, shrink = 0) {
  xlistNew <- list()
  for (s in seq_along(xlist)) {
    xlistNew[[s]] <- xlist[[s]]
    for (i in seq_along(xlist[[s]])) {
      minChro <- min(xlistNew[[s]][[i]], na.rm = TRUE)
      maxChro <- max(xlistNew[[s]][[i]], na.rm = TRUE)
      xlistNew[[s]][[i]] <- xlistNew[[s]][[i]] - minChro + (shrink * (maxChro - minChro))
      attr(xlistNew[[s]][[i]], "rowIndex") <- attr(xlist[[s]][[i]], "rowIndex") # i
    }
    names(xlistNew)[s] <- names(xlist[s])
    attr(xlistNew[[s]], "spname") <- attr(xlist[[s]], "spname")
  } # for
  return(xlistNew)
} # fun

#' @param x only d.f. of this cen. type
#' @keywords internal
xChrtdMarkMap <- function(xChrtdMark, x, shrink = 0) {
  xChrtdMarkList <- list()

  for (s in seq_along(xChrtdMark)) {
    xChrtdMarkList[[s]] <- xChrtdMark[[s]]

    currName <- attr(xChrtdMark[[s]], "spname")

    corrIndex <- which(names(x) %in% currName)

    for (m in seq_along(xChrtdMark[[s]])) {
      currChrNameB <- attr(xChrtdMark[[s]][[m]], "rowIndex")

      minMark <- min(xChrtdMark[[s]][[m]], na.rm = TRUE)
      maxChrtdMark <- max(xChrtdMark[[s]][[m]], na.rm = TRUE)

      xChrtdMarkList[[s]][[m]] <- xChrtdMarkList[[s]][[m]] - min(unlist(x[[corrIndex]][currChrNameB])) +
        ((shrink / 2) * (maxChrtdMark - minMark))

      attr(xChrtdMarkList[[s]][[m]], "rowIndex") <- currChrNameB
    }
    attr(xChrtdMarkList[[s]], "spname") <- attr(xChrtdMark[[s]], "spname")
  }
  return(xChrtdMarkList)
}

xChrtdMap <- function(xChrtd, x, shrink = 0) {
  xChrtdList <- list()

  for (s in seq_along(xChrtd)) {
    xChrtdList[[s]] <- xChrtd[[s]]

    for (m in seq_along(xChrtd[[s]])) {


      minMark <- min(xChrtd[[s]][[m]], na.rm = TRUE)
      maxChrtd <- max(xChrtd[[s]][[m]], na.rm = TRUE)

      xChrtdList[[s]][[m]] <- xChrtdList[[s]][[m]] - min(unlist(x[[s]][m])) + ((shrink / 2) * (maxChrtd - minMark))

      attr(xChrtdList[[s]][[m]], "rowIndex") <- m
    }
    attr(xChrtdList[[s]], "spname") <- attr(xChrtd[[s]], "spname")
  }
  return(xChrtdList)
}

xHortoVerMid <- function(xlist, shrink = 0) {
  xlistNew <- list()

  for (s in seq_along(xlist)) {
    xlistNew[[s]] <- xlist[[s]]

    for (i in seq_along(xlist[[s]])) {
      minChro <- min(xlistNew[[s]][[i]], na.rm = TRUE)
      maxChro <- max(xlistNew[[s]][[i]], na.rm = TRUE)

      xlistNew[[s]][[i]] <- xlistNew[[s]][[i]] - minChro + (shrink * (maxChro - minChro))

      attr(xlistNew[[s]][[i]], "rowIndex") <- attr(xlist[[s]][[i]], "rowIndex") # i
    }
    names(xlistNew)[s] <- names(xlist[s])
    attr(xlistNew[[s]], "spname") <- attr(xlist[[s]], "spname")
  }
  return(xlistNew)
}

#' @param diffXRounded numeric, difference of chr. width to rounded corner
#' @keywords internal

xHortoVerRoundCen <- function(xlist, diffXRounded) {
  xlistNew <- list()

  for (s in seq_along(xlist)) {
    xlistNew[[s]] <- xlist[[s]]
    for (i in seq_along(xlist[[s]])) {
      minSpecies <- min(xlistNew[[s]][[i]], na.rm = TRUE)
      xlistNew[[s]][[i]] <- xlistNew[[s]][[i]] - minSpecies + diffXRounded
    }
    names(xlistNew)[s] <- names(xlist[s])
  }

  return(xlistNew)
}

xHortoVerRoundCenExt <- function(xlist, diffXRounded) {
  xlistNew <- list()

  for (s in seq_along(xlist)) {
    xlistNew[[s]] <- xlist[[s]]
    for (i in seq_along(xlist[[s]])) {
      minSpecies <- min(xlistNew[[s]][[i]], na.rm = TRUE)
      maxX <- max(xlistNew[[s]][[i]] - minSpecies + diffXRounded, na.rm = TRUE)
      xlistNew[[s]][[i]] <- xlistNew[[s]][[i]] - minSpecies + maxX
    }
    names(xlistNew)[s] <- names(xlist[s])
  }

  return(xlistNew)
}

#' @param xMarkList list, mark coords.
#' @keywords internal

xHortoVerDots <- function(xMarkList, x) {
  xlistNew <- list()

  for (s in seq_along(xMarkList)) {
    xlistNew[[s]] <- list()

    currName <- attr(xMarkList[[s]], "spname")
    corrIndex <- which(names(x) %in% currName) # x5 x
    # i
    for (i in seq_along(xMarkList[[s]])) {
      currChrNameB <- attr(xMarkList[[s]][[i]], "rowIndex")
      corrIndexChrNameB <- which(names(x[[corrIndex]]) %in% currChrNameB) # x5 x

      xlistNew[[s]][[i]] <- xMarkList[[s]][[i]]

      minXChr <- min(x[[corrIndex]][[corrIndexChrNameB]])

      if (length(xMarkList[[s]][[i]]) > 1) {
        for (d in seq_along(xMarkList[[s]][[i]])) {
          xlistNew[[s]][[i]][[d]] <- xlistNew[[s]][[i]][[d]] - minXChr
        }
      } else {
        xlistNew[[s]][[i]] <- xlistNew[[s]][[i]] - minXChr
      }
    }
    attr(xlistNew[[s]], "spname") <- attr(xMarkList[[s]], "spname")
  }

  return(xlistNew)
}

#' @param circleCenter numeric, modify center of circle of chr.
#' @param chrWidth character, chr. width
#' @param labelSpacing numeric, spacing of labels to chr. or mark.
#' @param position numeric, indexer for karyotype position
#' @param n numeric, number of vertices
#' @param separFactor numeric, separation of kar. in circle
#' @param radius numeric, radius
#' @param circleCenterY numeric
#' @keywords internal
mapCircle <- function(x, y, n, radius, circleCenter, circleCenterY, position, separFactor,
                      labelSpacing, chrWidth, rotation) {
  r2 <- radius * position * chrWidth * 2 + position * separFactor + chrWidth * labelSpacing

  yValue2 <- list()
  xValue2 <- list()

  if (length(y) > 1) {
    for (i in 1:(length(y) - 1)) {
      yValue2[[i]] <- seq(y[i], y[i + 1], length.out = n) * 2 # stands for 2pi = 360 degrees
      xValue2[[i]] <- seq(x[i], x[i + 1], length.out = n)
    }
  } else {
    yValue2[[1]] <- y[1] * 2 # stands for 2pi = 360 degrees
    xValue2[[1]] <- x[1]
  }

  x1b <- (r2 + unlist(xValue2)) * sin(unlist(yValue2) * pi - (rotation * pi)) + circleCenter # 0
  y1b <- (r2 + unlist(xValue2)) * cos(unlist(yValue2) * pi - (rotation * pi)) + circleCenterY # 1

  xyCircle <- list()
  xyCircle$x <- x1b
  xyCircle$y <- y1b

  attr(xyCircle, "chrName1") <- attr(y, "chrName1")
  return(xyCircle)
}

mapRadius <- function(x, y, n, radius, circleCenter, circleCenterY, position, separFactor, labelSpacing, chrWidth, rotation) {
  newrad <- radius * position * chrWidth * 2 + position * separFactor + chrWidth * labelSpacing + x
  newrad <- 2 * pi * newrad * y
  return(newrad)
}

#' @param shrinkFactor numeric, percentage in decimal of whole chr.
#' @param ylistNew list, chr coords.
#' @keywords internal
transYList <- function(ylistNew, shrinkFactor, monocenNames) {
  ylistTrans <- list()
  for (s in seq_along(ylistNew)) {
    yMin <- min(unlist(ylistNew[[s]]), na.rm = TRUE)
    ylistTrans[[s]] <- lapply(ylistNew[[s]], function(y) y - yMin)
  }
  for (s in seq_along(ylistNew)) {
    yMax <- max(unlist(ylistTrans[[s]]), na.rm = TRUE)
    ylistTrans[[s]] <- lapply(ylistTrans[[s]], function(y) y / yMax)
  }
  for (s in seq_along(ylistTrans)) {
    ylistTrans[[s]] <- lapply(ylistTrans[[s]], function(y) y * shrinkFactor)

    if (names(ylistNew[s]) %in% monocenNames) { # mono test

      for (c in seq(1, (length(ylistTrans[[s]])), by = 2)) {
        ylistTrans[[s]][[c]] <- (ylistTrans[[s]][[c]]) + ((((1 - shrinkFactor) / 2) * (c * 2 - 1)) / length(ylistTrans[[s]]))
        attr(ylistTrans[[s]][[c]], "chrName1") <- attr(ylistNew[[s]][[c]], "chrName1")
        ylistTrans[[s]][[c + 1]] <- (ylistTrans[[s]][[c + 1]]) + ((((1 - shrinkFactor) / 2) * (c * 2 - 1)) / length(ylistTrans[[s]]))
      }

      ylistTrans[[s]] <- lapply(ylistTrans[[s]], function(x) x + (((1 - shrinkFactor) / 2) / length(ylistTrans[[s]])))
    } else { # monocen                                                - else holo

      for (c in seq_along(ylistTrans[[s]])) {
        ylistTrans[[s]][[c]] <- (ylistTrans[[s]][[c]]) + (((1 - shrinkFactor) / 2) * (c * 2 - 1)) / length(ylistTrans[[s]])
        attr(ylistTrans[[s]][[c]], "chrName1") <- attr(ylistNew[[s]][[c]], "chrName1")
      }
    }
    attr(ylistTrans[[s]], "positionnoNA") <- attr(ylistNew[[s]], "positionnoNA")
  }
  return(ylistTrans)
}

#' @param firstXchrEach list, first element from xlistNewChr
#' @param OTUlabelSpacing numeric, spacing for OTU name
#' @param firstYchrEach list, first element from xlistNewChr
#' @param ylistNewChr list, chr coords.
#' @keywords internal

mapOTUnames <- function(firstYchrEach, firstXchrEach, ylistNewChr, n, radius, circleCenter, circleCenterY,
                        separFactor, OTUlabelSpacing, chrWidth, rotation) {
  circleMapsOTUname <- list()
  for (s in seq_along(firstYchrEach)) {
    position <- as.numeric(attr(ylistNewChr[[s]], "positionnoNA"))
    circleMapsOTUname[[s]] <- mapCircle(
      x = firstXchrEach[[s]],
      y = firstYchrEach[[s]],
      n = n,
      radius = radius,
      circleCenter = circleCenter,
      circleCenterY = circleCenterY,
      position,
      separFactor,
      OTUlabelSpacing,
      chrWidth,
      rotation
    )
    attr(circleMapsOTUname[[s]], "name") <- names(firstYchrEach[s])
    attr(circleMapsOTUname[[s]], "positionnoNA") <- position
  }
  return(circleMapsOTUname)
}

#' @param circleMapsOTUname list, resulting from applyMapCircle
#' @param OTUsrt numeric, angle for OTU name
#' @param OTUlegendHeight numeric
#' @param OTUTextSize numeric, font size of OTU
#' @param OTUplacing boolean use number instead of OTU name
#' @keywords internal
addOTUnames <- function(circleMapsOTUname, OTUTextSize, OTUsrt = 0, OTUplacing = "first", OTUfont2, OTUfamily2,
                        circleCenter, OTULabelSpacerx, circleCenterY, OTULabelSpacery, OTUlegendHeight, radius,
                        chrWidth, normalizeToOne, OTUcentered, OTUjustif, separFactor, labelSpacing) {
  maxPos <- numeric()
  for (s in seq_along(circleMapsOTUname)) {
    maxPos <- max(maxPos, as.numeric(attr(circleMapsOTUname[[s]], "positionnoNA")))

    centerX <- min(unlist(circleMapsOTUname[[s]]$x))
    centerY <- min(unlist(circleMapsOTUname[[s]]$y))

    graphics::text(
      x = centerX,
      y = centerY,
      label = if (OTUplacing == "first") {
        attr(circleMapsOTUname[[s]], "name")
      } else if (OTUplacing == "number") {
        attr(circleMapsOTUname[[s]], "positionnoNA")
      } else {
        ""
      },
      cex = OTUTextSize

      , adj = 0.5,
      srt = OTUsrt,
      font = OTUfont2,
      family = OTUfamily2
    )
  }

  if (OTUplacing == "number" || OTUplacing == "simple") {
    OTUlabelsright(
      circleMapsOTUname, circleCenter, OTULabelSpacerx, circleCenterY, OTULabelSpacery,
      OTUlegendHeight, radius, chrWidth, OTUfont2, OTUfamily2, OTUTextSize, normalizeToOne,
      OTUplacing, OTUcentered, OTUjustif, maxPos, separFactor, labelSpacing
    )
  }
}

#' @param cfunction character, name of function to apply
#' @param ylistTrans list, trans .chr coords.
#' @param unlist boolean, for unlisting x or y coordinates
#' @param xlistNew list, transformed coordinates
#' @keywords internal
applyMapCircle <- function(radius, circleCenter, circleCenterY, separFactor, ylistTrans, xlistNew,
                           n = NA,
                           labelSpacing,
                           chrWidth, unlist = FALSE, cfunction = mapCircle,
                           specialOTUNames = NA, chrWFactor = NA, rotation,
                           label = FALSE) {
  circleMaps <- list()

  for (s in seq_along(ylistTrans)) {
    circleMaps[[s]] <- list()
    labelSpacing2 <- labelSpacing

    if (!is.na(specialOTUNames[1]) && specialOTUNames[1] != "") {
      if (names(ylistTrans[s]) %in% specialOTUNames) {
        labelSpacing2 <- labelSpacing * chrWFactor
      }
    }

    for (c in seq_along(ylistTrans[[s]])) {
      position <- as.numeric(attr(ylistTrans[[s]], "positionnoNA"))
      mSign <- 1
      mSign2 <- 1

      if (length(attr(ylistTrans[[s]][[c]], "squareSide"))) {
        if (attr(ylistTrans[[s]][[c]], "squareSide") %in% c("cMLeft", "left")) {
          mSign <- -1
          if (label) {
            mSign2 <- -1
          }
        }
      }

      circleMaps[[s]][[c]] <- cfunction(
        x = if (unlist) {
          unlist(xlistNew[[s]][[c]])
        } else {
          xlistNew[[s]][[c]]
        },
        y = if (unlist) {
          unlist(ylistTrans[[s]][[c]])
        } else {
          ylistTrans[[s]][[c]]
        },
        n = n,
        radius = radius,
        circleCenter = circleCenter,
        circleCenterY = circleCenterY,
        position,
        separFactor,
        labelSpacing2 * mSign,
        chrWidth * mSign2,
        rotation
      )
      attr(circleMaps[[s]][[c]], "squareSide") <- attr(ylistTrans[[s]][[c]], "squareSide")
    }
    attr(circleMaps[[s]], "positionB") <- s
    names(circleMaps)[s] <- names(ylistTrans[s])
  }
  return(circleMaps)
}

intercalate <- function(y, monocenNames, attr = FALSE) {
  newOrder <- list()
  for (s in seq_along(y)) {
    if (attr == FALSE) {
      if (names(y[s]) %in% monocenNames) {
        newOrder[[s]] <- list()

        for (a in 1:(length(y[[s]]) / 2)) {
          b <- a * 2 - 1
          newOrder[[s]][[b]] <- y[[s]][[a]]
          names(newOrder[[s]])[b] <- names(y[[s]][a])
        }

        for (a in ((length(y[[s]]) / 2) + 1):length(y[[s]])) {
          b <- (a - (length(y[[s]]) / 2)) * 2
          newOrder[[s]][[b]] <- y[[s]][[a]]
          names(newOrder[[s]])[b] <- names(y[[s]][a])
        }

      } else {
        newOrder[[s]] <- list()
        newOrder[[s]] <- y[[s]]
        names(newOrder[[s]]) <- names(y[[s]])
      }
    } else {
      if (attr(y[[s]], "spname") %in% monocenNames) {
        newOrder[[s]] <- list()

        for (a in 1:(length(y[[s]]) / 2)) {
          b <- a * 2 - 1
          newOrder[[s]][[b]] <- y[[s]][[a]]
          names(newOrder[[s]])[b] <- names(y[[s]][a])
        }

        for (a in ((length(y[[s]]) / 2) + 1):length(y[[s]])) {
          b <- (a - (length(y[[s]]) / 2)) * 2
          newOrder[[s]][[b]] <- y[[s]][[a]]
          names(newOrder[[s]])[b] <- names(y[[s]][a])
        }

      } else {
        newOrder[[s]] <- list()
        newOrder[[s]] <- y[[s]]
        names(newOrder[[s]]) <- names(y[[s]])
      }
    }

    attr(newOrder[[s]], "positionnoNA") <- attr(y[[s]], "positionnoNA")
  }
  newOrder <- Filter(function(x) {
    length(x) >= 1
  }, newOrder)
  return(newOrder)
}

#' @param circleMaps list, resulting from applyMapCircle
#' @param lwd.chr numeric, width of line
#' @param chrColor character, chr. color
#' @keywords internal

drawPlot <- function(circleMaps, chrColor, lwd.chr, chrBorderColor2) {
  for (s in seq_along(circleMaps)) {
    for (i in seq_along(circleMaps[[s]])) {
      graphics::polygon(
        x = circleMaps[[s]][[i]]$x,
        y = circleMaps[[s]][[i]]$y,
        col = chrColor,
        lwd = lwd.chr,
        border = chrBorderColor2
      )
    }
  }
}

#' @param dfMarkColorInt data.frame of mark charact.
#' @param listOfdfMarkPosSq list, of d.f.s of marks
#' @keywords internal

drawPlotMark <- function(circleMaps, dfMarkColorInt, listOfdfMarkPosSq, lwd.chr, alpha_val = 1) {
  for (s in seq_along(circleMaps)) {
    for (i in seq_along(circleMaps[[s]])) {
      graphics::polygon(
        x = circleMaps[[s]][[i]]$x,
        y = circleMaps[[s]][[i]]$y,
        col = alpha(dfMarkColorInt$markColor[match(
          listOfdfMarkPosSq[[s]]$markName[[i]],
          dfMarkColorInt$markName
        )], alpha_val),
        lwd = lwd.chr,
        border = dfMarkColorInt$markBorderColor[match(
          listOfdfMarkPosSq[[s]]$markName[[i]],
          dfMarkColorInt$markName
        )]

      )
    }
  }
} # fun

drawCenStyle <- function(circleMaps, defCenStyleCol, lwd.chr) {
  for (s in seq_along(circleMaps)) {
    for (i in seq_along(circleMaps[[s]])) {
      graphics::polygon(
        x = circleMaps[[s]][[i]]$x,
        y = circleMaps[[s]][[i]]$y,
        col = defCenStyleCol,
        lwd = lwd.chr,
        border = defCenStyleCol
      )
      graphics::lines(
        x = circleMaps[[s]][[i]]$x,
        y = circleMaps[[s]][[i]]$y,
        col = defCenStyleCol,
        lwd = lwd.chr,
      )
    }
  }
}

drawPlotMarkLine <- function(circleMaps, defCenStyleCol, lwd.chr) {
  for (s in seq_along(circleMaps)) {
    for (i in seq_along(circleMaps[[s]])) {
      graphics::lines(
        x = circleMaps[[s]][[i]]$x,
        y = circleMaps[[s]][[i]]$y,
        col = defCenStyleCol,
        lwd = lwd.chr,
      )
    }
  }
}

#' @param cenColor2 character, cen. color
#' @param cenBorder character, cen border color
#' @keywords internal
drawCen <- function(circleMaps, cenColor2, cenBorder, lwd.chr) {
  for (s in seq_along(circleMaps)) {
    for (i in seq_along(circleMaps[[s]])) {
      graphics::polygon(
        x = circleMaps[[s]][[i]]$x,
        y = circleMaps[[s]][[i]]$y,
        col = cenColor2,
        lwd = lwd.chr,
        border = cenBorder
      )
    }
  }
}

#' @param listOfdfMarkPos list, of d.f.s of marks
#' @param markLabelSize numeric, size of mark label
#' @param labelOutwards see srt of text function
#' @param pattern character, regex pattern to remove from mark names
#' @keywords internal

circLabelMark <- function(bannedMarkName, circleMaps, listOfdfMarkPos, markLabelSize, pattern,
                          labelOutwards, circleCenter, circleCenterY,
                          iscM = FALSE, adj = 0.5, iscMLeft = FALSE) {
  for (s in seq_along(circleMaps)) {
    for (i in seq_along(circleMaps[[s]])) {
      if (!listOfdfMarkPos[[s]]$markName[i] %in% bannedMarkName) {
        centerX <- mean(unlist(circleMaps[[s]][[i]]$x))
        centerY <- mean(unlist(circleMaps[[s]][[i]]$y))

        if (labelOutwards) {
          delta_x <- centerX - circleCenter
          delta_y <- centerY - circleCenterY
          theta_radians <- atan2(delta_y, delta_x)

          srt <- (theta_radians * 180) / pi

          if (c(srt > 90 & srt <= 180) || c(srt < -90 & srt >= -180)) {
            srt <- srt + 180
          }
          srt <- round(srt)
        } else {
          srt <- 0
        }

        w <- tryCatch(listOfdfMarkPos[[s]]$chrRegionOrig[i], error = function(e) {
          NA
        })
        z <- sub(pattern, "", listOfdfMarkPos[[s]]$markName[i])

        graphics::text(
          x = centerX,
          y = centerY,
          label = ifelse(any(is.na(w), is.null(w)), z, ""),
          cex = markLabelSize
          , adj = adj,
          srt = srt
        )
      }
    }
  }
}

#' @param colBorderCr character, border color
#' @param colCr character color of mark
#' @param radiusMap function, map radius in circle
#' @param xfactor numeric, modify y x aspect
#' @param circleMapsMarksCr list, resulting from applyMapCircle
#' @keywords internal
circPlotDots <- function(circleMapsMarksCr, xfactor, radiusMap,
                         colCr, colBorderCr, n, alpha_val = 1) {
  lapply(
    seq_along(circleMapsMarksCr), function(m) {
      lapply(
        seq_along(circleMapsMarksCr[[m]]), function(u) {
          mapply(function(x, y,
                          radius, z, w) {
            pts2 <- seq(0, 2 * pi, length.out = n * 4)
            xy2 <- cbind(x + radius * sin(pts2) * xfactor, y + radius * cos(pts2))

            graphics::polygon(xy2[, 1],
              xy2[, 2],
              col = alpha(z, alpha_val),
              border = w
            )
          },
          x = circleMapsMarksCr[[m]][[u]]$x,
          y = circleMapsMarksCr[[m]][[u]]$y,
          radius = radiusMap[[m]][[u]],
          z = colCr[[m]][[u]],
          w = colBorderCr[[m]][[u]]
          )
        }
      )
    }
  )
}

#' @param chrNames list, resulting from applyMapCircle
#' @param indexIdTextSize numeric, text size
#' @param chrId character, type of chr. name
#' @keywords internal
plotChrNames <- function(chrNames, indexIdTextSize, chrId, monocenNames, chrColor) {
  labels <- list()

  for (s in seq_along(chrNames)) {
    divisorL <- ifelse(names(chrNames[s]) %in% monocenNames, 2, 1)

    labels[[s]] <- list()
    for (i in seq(1, length(chrNames[[s]]), by = divisorL)) {
      if (chrId == "original") {
        labels[[s]][[i]] <- attr(chrNames[[s]][[i]], "chrName")
      } else {
        labels[[s]][[i]] <- (i + ifelse(divisorL == 2, 1, 0)) / divisorL
      }
      centerX <- mean(unlist(chrNames[[s]][[i]]$x))
      centerY <- mean(unlist(chrNames[[s]][[i]]$y))
      graphics::text(
        x = centerX,
        y = centerY,
        label = labels[[s]][[i]],
        cex = indexIdTextSize

        , adj = 0.5,
        col = colorRampPalette(c(chrColor, "black"))(100)[50]
      )
    }
  }
}

#
#   attr chrNameB to yMark and xMark
#
#' @param xMark list, mark coords.
#' @param yMark list, mark coords.
#' @keywords internal

addChrNameAttrMark <- function(xMark, yMark, x) {
  markList <- list()
  for (i in seq_along(xMark)) {
    currName <- attr(xMark[[i]], "spname")
    corrIndex <- which(names(x) %in% currName)
    for (j in seq_along(xMark[[i]])) {
      for (k in seq_along(x[[corrIndex]])) {
        match <- which(xMark[[i]][[j]] %in% x[[corrIndex]][[k]])
        if (length(match) > 0) {
          attr(xMark[[i]][[j]], "chrNameB") <- k
          attr(yMark[[i]][[j]], "chrNameB") <- k
        } else {
          NA
        }
      }
    }
  }
  markList$xMark <- xMark
  markList$yMark <- yMark
  return(markList)
}


addChrNameAttrMarkDots <- function(xMark, yMark, x) {
  markList <- list()
  for (i in seq_along(xMark)) {
    currName <- attr(xMark[[i]], "spname")
    corrIndex <- which(names(x) %in% currName)
    for (j in seq_along(xMark[[i]])) {
      for (k in seq_along(x[[corrIndex]])) {
        distance <- xMark[[i]][[j]][[2]] - xMark[[i]][[j]][[1]]
        match <- which((xMark[[i]][[j]][[1]] - distance / 2) %in% x[[corrIndex]][[k]])

        if (length(match) > 0) {
          attr(xMark[[i]][[j]], "chrNameB") <- k
          attr(yMark[[i]][[j]], "chrNameB") <- k
        } else {
          NA
        }
      }
    }
  }
  markList$xMark <- xMark
  markList$yMark <- yMark
  return(markList)
}

xMarkMap <- function(xMark, x, shrink) {
  xMarkList <- list()

  for (s in seq_along(xMark)) {
    xMarkList[[s]] <- xMark[[s]]

    corrIndex <- which(names(x) %in% attr(xMark[[s]], "spname"))

    for (m in seq_along(xMark[[s]])) {
      name <- attr(xMark[[s]][[m]], "rowIndex")

      minMark <- min(xMark[[s]][[m]], na.rm = TRUE)
      maxMark <- max(xMark[[s]][[m]], na.rm = TRUE)

      xMarkList[[s]][[m]] <- xMarkList[[s]][[m]] - min(unlist(x[[corrIndex]][name])) + ((shrink / 2) * (maxMark - minMark))

      attr(xMarkList[[s]][[m]], "rowIndex") <- name
    }
    attr(xMarkList[[s]], "spname") <- attr(xMark[[s]], "spname")
  }
  return(xMarkList)
}

xMarkMapLeft <- function(xMark, x) {
  xMarkList <- list()

  for (s in seq_along(xMark)) {
    xMarkList[[s]] <- xMark[[s]]

    corrIndex <- which(names(x) %in% attr(xMark[[s]], "spname"))

    for (m in seq_along(xMark[[s]])) {
      name <- attr(xMark[[s]][[m]], "rowIndex")

      xMarkList[[s]][[m]] <- xMarkList[[s]][[m]] - min(unlist(x[[corrIndex]][name]))

      attr(xMarkList[[s]][[m]], "rowIndex") <- name
    }
    attr(xMarkList[[s]], "spname") <- attr(xMark[[s]], "spname")
  }
  return(xMarkList)
}

markMapPer <- function(yMark, y, useNA = FALSE) {
  yMarkPer <- list()

  for (s in seq_along(yMark)) {
    yMarkPer[[s]] <- list()
    corrIndex <- which(names(y) %in% attr(yMark[[s]], "spname"))

    for (m in seq_along(yMark[[s]])) {
      name <- attr(yMark[[s]][[m]], "rowIndex")
      chrSize <- max(unlist(y[[corrIndex]][name])) - min(unlist(y[[corrIndex]][name]))
      distBeg <- min(yMark[[s]][[m]]) - (min(unlist(y[[corrIndex]][name])))
      disBegPer <- distBeg / chrSize

      distEnd <- max(yMark[[s]][[m]]) - (min(unlist(y[[corrIndex]][name])))
      disEndPer <- distEnd / chrSize
      diffPer <- disEndPer - disBegPer

      diffReal <- distEnd - distBeg

      fac <- diffPer / diffReal
      fac <- ifelse(is.na(fac), 0, fac)

      if (all(is.na(yMark[[s]][[m]])) && useNA) {
        yMarkPer[[s]][[m]] <- NA
      } else {
        yMarkPer[[s]][[m]] <- psum(((yMark[[s]][[m]] - min(unlist(y[[corrIndex]][name]))) - distBeg) * fac,
          disBegPer,
          na.rm = TRUE
        )
      }

      attr(yMarkPer[[s]][[m]], "rowIndex") <- name
      attr(yMarkPer[[s]][[m]], "squareSide") <- attr(yMark[[s]][[m]], "squareSide")
    }
    attr(yMarkPer[[s]], "spname") <- attr(yMark[[s]], "spname")
  }
  return(yMarkPer)
}


markMapPerCen <- function(yMark, y) {
  yMarkPer <- list()

  for (s in seq_along(yMark)) {
    yMarkPer[[s]] <- list()
    corrIndex <- which(names(y) %in% names(yMark)[s])
    for (m in seq_along(yMark[[s]])) {

      armSize <- max(unlist(y[[corrIndex]][[m]])) - min(unlist(y[[corrIndex]][[m]]))

      distBeg <- min(yMark[[s]][[m]]) - (min(unlist(y[[corrIndex]][[m]])))
      disBegPer <- distBeg / armSize

      distEnd <- max(yMark[[s]][[m]]) - (min(unlist(y[[corrIndex]][[m]])))
      disEndPer <- distEnd / armSize
      diffPer <- disEndPer - disBegPer

      diffReal <- distEnd - distBeg

      fac <- diffPer / diffReal
      fac <- ifelse(is.na(fac), 0, fac)

      yMarkPer[[s]][[m]] <- psum(((yMark[[s]][[m]] - min(unlist(y[[corrIndex]][[m]]))) - distBeg) * fac,
        disBegPer,
        na.rm = TRUE
      )
      attr(yMarkPer[[s]][[m]], "rowIndex") <- m
    }
    names(yMarkPer[[s]]) <- seq_along(yMarkPer[[s]])
    names(yMarkPer)[s] <- names(y[s])

  }

  return(yMarkPer)
}

markMapPercM <- function(yMark, y) {
  yMarkPer <- list()

  for (s in seq_along(yMark)) {
    yMarkPer[[s]] <- list()
    corrIndex <- which(names(y) %in% attr(yMark[[s]], "spname"))

    for (m in seq_along(yMark[[s]])) {
      name <- attr(yMark[[s]][[m]], "rowIndex")
      chrSize <- max(unlist(y[[corrIndex]][name])) - min(unlist(y[[corrIndex]][name]))
      distBeg <- min(yMark[[s]][[m]]) - (min(unlist(y[[corrIndex]][name])))
      disBegPer <- distBeg / chrSize
      fac <- disBegPer / distBeg
      fac <- ifelse(is.na(fac), 0, fac)
      yMarkPer[[s]][[m]] <- ((yMark[[s]][[m]] - min(unlist(y[[corrIndex]][name]))) - distBeg) * fac + disBegPer
      attr(yMarkPer[[s]][[m]], "rowIndex") <- name
      attr(yMarkPer[[s]][[m]], "squareSide") <- attr(yMark[[s]][[m]], "squareSide")
    }
    attr(yMarkPer[[s]], "spname") <- attr(yMark[[s]], "spname")
  }
  return(yMarkPer)
}

markMapPerDots <- function(yMark, y) {
  yMarkPer <- list()

  for (s in seq_along(yMark)) {
    yMarkPer[[s]] <- list()
    corrIndex <- which(names(y) %in% attr(yMark[[s]], "spname"))

    for (m in seq_along(yMark[[s]])) {
      name <- attr(yMark[[s]][[m]], "rowIndex")
      chrSize <- max(unlist(y[[corrIndex]][name])) - min(unlist(y[[corrIndex]][name]))
      distBeg <- min(yMark[[s]][[m]][[1]]) - (min(unlist(y[[corrIndex]][name])))
      disBegPer <- distBeg / chrSize
      fac <- disBegPer / distBeg
      fac <- ifelse(is.na(fac), 0, fac)
      yMarkPer[[s]][[m]] <- list()
      if (length(yMark[[s]][[m]]) > 1) {
        yMarkPer[[s]][[m]][1:2] <- ((yMark[[s]][[m]][[1]] - min(unlist(y[[corrIndex]][name]))) - distBeg) * fac + disBegPer
      } else {
        yMarkPer[[s]][[m]] <- ((yMark[[s]][[m]][[1]] - min(unlist(y[[corrIndex]][name]))) - distBeg) * fac + disBegPer
      }
      attr(yMarkPer[[s]][[m]], "rowIndex") <- name
    }
    attr(yMarkPer[[s]], "spname") <- attr(yMark[[s]], "spname")
  }
  return(yMarkPer)
}

#' @param rad list, list of radius for dots marks
#' @keywords internal

radDotsPer <- function(rad, y) {
  radPer <- list()

  for (s in seq_along(rad)) {
    radPer[[s]] <- list()
    corrIndex <- which(names(y) %in% attr(rad[[s]], "spname"))

    for (m in seq_along(rad[[s]])) {
      name <- attr(rad[[s]][[m]], "rowIndex")
      chrSize <- max(unlist(y[[corrIndex]][name])) - min(unlist(y[[corrIndex]][name]))
      radSize <- rad[[s]][[m]][[1]]
      radPerC <- radSize / chrSize
      radPer[[s]][[m]] <- list()
      if (length(rad[[s]][[m]]) > 1) {
        radPer[[s]][[m]][1:2] <- radPerC
      } else {
        radPer[[s]][[m]] <- radPerC
      }
    }
  }
  return(radPer)
}

centerMarkMapPer <- function(yMark, y) {
  yMarkPer <- list()

  for (s in seq_along(yMark)) {
    yMarkPer[[s]] <- list()
    corrIndex <- which(names(y) %in% attr(yMark[[s]], "spname"))

    for (m in seq_along(yMark[[s]])) {
      name <- attr(yMark[[s]][[m]], "rowIndex")
      chrSize <- max(unlist(y[[corrIndex]][name])) - min(unlist(y[[corrIndex]][name]))
      distBeg <- min(yMark[[s]][[m]]) - (min(unlist(y[[corrIndex]][name])))
      disBegPer <- distBeg / chrSize
      distEnd <- max(yMark[[s]][[m]]) - (min(unlist(y[[corrIndex]][name])))
      disEndPer <- distEnd / chrSize
      diffPer <- disEndPer - disBegPer
      diffReal <- distEnd - distBeg
      fac <- diffPer / diffReal
      fac <- ifelse(is.na(fac), 0, fac)
      center <- (disBegPer + disEndPer) / 2
      len <- length(yMark[[s]][[m]])

      yMarkPer[[s]][[m]] <- rep(center, len)

      attr(yMarkPer[[s]][[m]], "rowIndex") <- name
      attr(yMarkPer[[s]][[m]], "squareSide") <- attr(yMark[[s]][[m]], "squareSide")
    }
    attr(yMarkPer[[s]], "spname") <- attr(yMark[[s]], "spname")
  }
  return(yMarkPer)
}

#
#   trans Marks
#
#' @param ylistTransChr list, trans .chr coords.
#' @param yMarkPer list, mark coords in percentage.
#' @keywords internal

transyListMark <- function(yMarkPer, ylistTransChr) {
  ylistTransMark <- list()

  for (s in seq_along(yMarkPer)) {
    ylistTransMark[[s]] <- list()
    corrIndex <- which(names(ylistTransChr) %in% attr(yMarkPer[[s]], "spname"))

    for (m in seq_along(yMarkPer[[s]])) {
      name <- NULL
      corrIndexMark <- NULL
      name <- attr(yMarkPer[[s]][[m]], "rowIndex")
      corrIndexMark <- which(names(ylistTransChr[[corrIndex]]) %in% name)

      if (length(corrIndexMark) > 0) {
        chrSize <- max(unlist(ylistTransChr[[corrIndex]][corrIndexMark])) - min(unlist(ylistTransChr[[corrIndex]][corrIndexMark]))

        ylistTransMark[[s]][[m]] <- yMarkPer[[s]][[m]] * chrSize
        ylistTransMark[[s]][[m]] <- ylistTransMark[[s]][[m]] + min(unlist(ylistTransChr[[corrIndex]][corrIndexMark]))
        attr(ylistTransMark[[s]][[m]], "squareSide") <- attr(yMarkPer[[s]][[m]], "squareSide")
      }
    }
    attr(ylistTransMark[[s]], "spname") <- attr(yMarkPer[[s]], "spname")
    attr(ylistTransMark[[s]], "positionnoNA") <- attr(ylistTransChr[[corrIndex]], "positionnoNA")
  }
  return(ylistTransMark)
}

transyListCen <- function(yMarkPer, ylistTransChr) {
  ylistTransMark <- list()

  for (s in seq_along(yMarkPer)) {
    ylistTransMark[[s]] <- list()
    corrIndex <- which(names(ylistTransChr) %in% names(yMarkPer)[s])
    for (m in seq_along(yMarkPer[[s]])) {


      if (length(m) > 0) {
        intercaIndex <- m * 2 - 1
        chrSize <- max(unlist(ylistTransChr[[corrIndex]][[intercaIndex]])) - min(unlist(ylistTransChr[[corrIndex]][[intercaIndex]]))

        ylistTransMark[[s]][[m]] <- yMarkPer[[s]][[m]] * chrSize
        ylistTransMark[[s]][[m]] <- ylistTransMark[[s]][[m]] + min(unlist(ylistTransChr[[corrIndex]][[intercaIndex]]))
      }
    }
    attr(ylistTransMark[[s]], "positionnoNA") <- attr(ylistTransChr[[corrIndex]], "positionnoNA")
    names(ylistTransMark[[s]]) <- seq_along(ylistTransMark[[s]])
  }
  return(ylistTransMark)
}

#' @param radPerCr list, radius percentages
#' @keywords internal
transRadDots <- function(radPerCr, yMarkPerCr, ylistTransChr) {
  radTrans <- list()
  # s<-1
  for (s in seq_along(radPerCr)) {
    radTrans[[s]] <- list()
    corrIndex <- which(names(ylistTransChr) %in% attr(yMarkPerCr[[s]], "spname"))
    for (m in seq_along(radPerCr[[s]])) {
      name <- NULL
      name <- attr(yMarkPerCr[[s]][[m]], "rowIndex")
      corrIndexMark <- NULL
      corrIndexMark <- which(names(ylistTransChr[[corrIndex]]) %in% name)


      chrSize <- max(unlist(ylistTransChr[[corrIndex]][corrIndexMark])) - min(unlist(ylistTransChr[[corrIndex]][corrIndexMark]))

      radTrans[[s]][[m]] <- list()
      if (length(radPerCr[[s]][[m]]) > 1) {
        radTrans[[s]][[m]][1:2] <- radPerCr[[s]][[m]][[1]] * chrSize
      } else {
        radTrans[[s]][[m]] <- radPerCr[[s]][[m]] * chrSize
      }
    }
    attr(radTrans[[s]], "spname") <- attr(yMarkPerCr[[s]], "spname")
    attr(radTrans[[s]], "positionnoNA") <- attr(ylistTransChr[[corrIndex]], "positionnoNA")
  }
  return(radTrans)
}


transyListMarkDots <- function(yMarkPer, ylistTransChr) {
  ylistTransMark <- list()

  for (s in seq_along(yMarkPer)) {
    ylistTransMark[[s]] <- list()
    corrIndex <- which(names(ylistTransChr) %in% attr(yMarkPer[[s]], "spname"))
    for (m in seq_along(yMarkPer[[s]])) {
      name <- NULL
      name <- attr(yMarkPer[[s]][[m]], "rowIndex")
      corrIndexMark <- NULL
      corrIndexMark <- which(names(ylistTransChr[[corrIndex]]) %in% name)


      chrSize <- max(unlist(ylistTransChr[[corrIndex]][corrIndexMark])) - min(unlist(ylistTransChr[[corrIndex]][corrIndexMark]))

      ylistTransMark[[s]][[m]] <- list()

      if (length(yMarkPer[[s]][[m]]) > 1) {
        ylistTransMark[[s]][[m]][1:2] <- yMarkPer[[s]][[m]][[1]] * chrSize
        ylistTransMark[[s]][[m]][1:2] <- ylistTransMark[[s]][[m]][[1]] + min(unlist(ylistTransChr[[corrIndex]][corrIndexMark]))
      } else {
        ylistTransMark[[s]][[m]] <- yMarkPer[[s]][[m]] * chrSize
        ylistTransMark[[s]][[m]] <- ylistTransMark[[s]][[m]] + min(unlist(ylistTransChr[[corrIndex]][corrIndexMark]))
      }
      attr(ylistTransMark[[s]][[m]], "rowIndex") <- attr(yMarkPer[[s]][[m]], "rowIndex")
    }
    attr(ylistTransMark[[s]], "spname") <- attr(yMarkPer[[s]], "spname")
    attr(ylistTransMark[[s]], "positionnoNA") <- attr(ylistTransChr[[corrIndex]], "positionnoNA")
  }
  return(ylistTransMark)
}

mapChrCenter <- function(ylistTransChr) {
  listChrCenter <- list()

  for (s in seq_along(ylistTransChr)) {
    listChrCenter[[s]] <- list()
    for (c in seq_along(ylistTransChr[[s]])) {
      chrSize <- max(unlist(ylistTransChr[[s]][[c]])) - min(unlist(ylistTransChr[[s]][[c]]))

      listChrCenter[[s]][[c]] <- rep((.5 * chrSize) + min(unlist(ylistTransChr[[s]][[c]])), 2)
      attr(listChrCenter[[s]][[c]], "chrName1") <- attr(ylistTransChr[[s]][[c]], "chrName1")
    }
    attr(listChrCenter[[s]], "positionnoNA") <- attr(ylistTransChr[[s]], "positionnoNA")
  }
  return(listChrCenter)
}

#' @param xMarkCr list, mark coords.
#' @keywords internal

oneDot <- function(xMarkCr) {
  oneDotXList <- list()
  for (s in seq_along(xMarkCr)) {
    oneDotXList[[s]] <- list()
    for (m in seq_along(xMarkCr[[s]])) {
      both <- unlist(xMarkCr[[s]][[m]])
      oneDotXList[[s]][[m]] <- sum(both) / 2
      attr(oneDotXList[[s]][[m]], "rowIndex") <- attr(xMarkCr[[s]][[m]], "rowIndex")
    }
    attr(oneDotXList[[s]], "spname") <- attr(xMarkCr[[s]], "spname")
  }
  return(oneDotXList)
}

#' @param fixCenBorder2 boolean, change cen border
#' @param parparlistOfdfMarkPosDataCen list, of d.f.s of marks
#' @keywords internal

drawCenMarks <- function(circleMaps, dfMarkColorInt,
                         parparlistOfdfMarkPosDataCen, lwd.chr, fixCenBorder2, chrColor) {
  for (s in seq_along(circleMaps)) {
    for (m in seq_along(circleMaps[[s]])) {
      graphics::polygon(
        x = circleMaps[[s]][[m]]$x,
        y = circleMaps[[s]][[m]]$y,
        col = dfMarkColorInt$markColor[match(
          parparlistOfdfMarkPosDataCen[[s]]$markName[[m]],
          dfMarkColorInt$markName
        )],
        lwd = lwd.chr,
        border = ifelse(fixCenBorder2,
          chrColor,
          dfMarkColorInt$markBorderColor[match(
            parparlistOfdfMarkPosDataCen[[s]]$markName[[m]],
            dfMarkColorInt$markName
          )]
        )
      )
    }
  }
}

OTUlabelsright <- function(y, circleCenter, OTULabelSpacerx, circleCenterY, OTULabelSpacery, OTUlegendHeight, radius,
                           chrWidth, font, family, OTUTextSize, normalizeToOne, OTUplacing, OTUcentered,
                           OTUjustif, maxPos, separFactor, labelSpacing) {
  if (OTUcentered) {
    labelx1 <- circleCenter + OTULabelSpacerx
  } else {
    labelx1 <- (circleCenter + radius * chrWidth * 2 * maxPos + maxPos * separFactor + chrWidth * labelSpacing + OTULabelSpacerx)
  }
  labelx <- rep(labelx1, length(y))


  center <- (circleCenterY + OTULabelSpacery)

  if (is.na(OTUlegendHeight)) {
    OTUlegendHeight <- normalizeToOne
  } else {
    OTUlegendHeight <- OTUlegendHeight
  }

  lHeights <- sapply(OTUlegendHeight, function(x) x + ((0:(length(y) - 1)) * OTUlegendHeight * 2))

  blabely <- lHeights - min(lHeights)
  halfmaxLH <- max(blabely) / 2
  labely <- blabely + center - halfmaxLH

  OTUNamesVec <- character()
  for (s in seq_along(y)) {
    if (OTUplacing == "number") {
      OTUNamesVec[s] <- paste(attr(y[[s]], "positionnoNA"), attr(y[[s]], "name"))
    } else {
      OTUNamesVec[s] <- paste(attr(y[[s]], "name"))
    }
  }
  graphics::text(
    x = labelx,
    y = labely,
    labels = OTUNamesVec,
    font = font,
    family = family,
    cex = OTUTextSize
    , adj = OTUjustif
  )
}

psum <- function(..., na.rm = FALSE) {
  rowSums(do.call(cbind, list(...)), na.rm = na.rm)
}

`%W>%` <- function(lhs, rhs) {
  w <- options()$warn
  on.exit(options(warn = w))
  options(warn = -1)
  eval.parent(substitute(lhs %>% rhs))
} # https://stackoverflow.com/questions/47475923/custom-pipe-to-silence-warnings
