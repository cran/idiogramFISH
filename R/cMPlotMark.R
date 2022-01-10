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
#'
#' @return plot
#' @importFrom graphics polygon text
#'

cMPlotMark<-function(bannedMarkName,xMark, yMark,y, x, dfMarkColorInt,listOfdfMarkPoscM, lwd.cM,circularPlot,
                     radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,n,labelSpacing,chrWidth,
                     ylistTransChr,rotation,labelOutwards) {


if(circularPlot==FALSE) {

    lapply(1:length(xMark), function(w) mapply(function(x,y,z)
      graphics::lines(
        x=x,
        y=y,
        col=dfMarkColorInt$markColor[match(     z   ,dfMarkColorInt$markName)],
        lwd=lwd.cM,
      ), # lines
    x=xMark[[w]],
    y=yMark[[w]]
    ,z=listOfdfMarkPoscM[[w]]$markName
    ) # mapply
    ) # lapp

} else { # else circ true

  #
  #   x to vertical
  #

  xlistNew<-xMarkMapLeft(xMark,x) # left

  yMarkPer<-markMapPercM(yMark,y)

  ylistTransMark<-transyListMark(yMarkPer,ylistTransChr)

  circleMapsMarks  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor
                                     ,ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)


  drawPlotMark(circleMapsMarks,dfMarkColorInt,listOfdfMarkPoscM,lwd.cM)

  circleMapsLabels <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor
                                         ,ylistTransMark,xlistNew,n,
                                         labelSpacing,
                                         chrWidth,
                                         rotation=rotation
                                     ,label=FALSE)

  circLabelMark(bannedMarkName,circleMapsLabels,listOfdfMarkPoscM,markLabelSize,pattern,labelOutwards,
                    circleCenter,circleCenterY
                    ,iscM=FALSE
                    ,adj=0.5)
  }
} # fun

