#' arrowPlotMark
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
#' @param listOfdfMarkPosArrow list of df. of mark pos.
#' @param chrWidth numeric, width of chr.
#' @param n numeric, to define vertices of rounded portions
#' @param lwd.chr thick of border line
#' @param circularPlot boolean TRUE for circ.
#' @param y list, y coords.
#' @param markLabelSize numeric, font size
#' @param separFactor numeric, sep. kars.
#' @param labelSpacing numeric, spacing
#' @param circleCenter numeric
#' @param circleCenterY numeric
#' @param radius numeric
#' @param ylistTransChr list, transf. coords.
#' @param rotation rotate*
#'
#' @return plot
#' @importFrom graphics polygon text
#'

arrowPlotMark<-function(squareness, xMark, yMark,
                        dfMarkColorInt,listOfdfMarkPosArrow,
                        chrWidth,
                        n,
                        lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,
                        circleCenter,circleCenterY,radius,
                        ylistTransChr,rotation,arrowheadWidthShrink
                        ) {

    if(circularPlot==FALSE) {

      for (w in 1:length(xMark)){

        for (n in 1:length(xMark[[w]] ) ){

          # pqcen<-attr(yMark[[w]][[n]],"arm")
          # markT<-attr(yMark[[w]][[n]],"squareSide")
          #
          # if(pqcen == "pcen" & markT == "inProtein"){
          #   m1 <- mean(yMark[[w]][[n]][2:3])
          #   yMark[[w]][[n]][3:4]<-m1
          # } else if (pqcen == "qcen" & markT == "inProtein") {
          #   m1 <- mean(yMark[[w]][[n]][2:3])
          #   yMark[[w]][[n]][c(1,2,5)]<-m1
          # }
          x =xMark[[w]][[n]]
          y =yMark[[w]][[n]]
          z =listOfdfMarkPosArrow[[w]]$markName[[n]]

                graphics::polygon(
                  x=x,
                  y=y,
                  col= dfMarkColorInt$markColor[match( z, dfMarkColorInt$markName)],
                  lwd=lwd.chr,
                  border =
                  dfMarkColorInt$markBorderColor[match(z, dfMarkColorInt$markName)]
                ) # pol
        }
      }
    } else { # circ true

        yMarkPer <- markMapPer(yMark,y)
        xlistNew <- xMarkMap(xMark,x, arrowheadWidthShrink)
        ylistTransMark<-transyListMark(yMarkPer,ylistTransChr)
        circleMapsMarks  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor
                                           ,ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

        drawPlotMark(circleMapsMarks,dfMarkColorInt,listOfdfMarkPosArrow,lwd.chr)

} # circular
} # FUN
