# roundPlotMark
#' This is an internal function that eliminates factors
#'
#' It returns a data.frames
#'
#' @keywords internal
#'
#' @param xMark x component of polygon
#' @param yMark y component of polygon
#' @param y y of chr
#' @param dfMarkColorInternal colors for marks
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
#' @param legend character
#' @param ylistTransChr list
#' @param rotation rotation
#' @param labelOutwards srt
#'
#' @return plot
#' @importFrom graphics polygon text
#'

cMPlotMark<-function(xMark, yMark,y, dfMarkColorInternal,listOfdfMarkPoscM, lwd.cM,circularPlot,
                     radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,n,labelSpacing,chrWidth,
                     legend,ylistTransChr,rotation,labelOutwards) {
# xMarkcM<<-xMark
# yMarkcM<<-yMark
# ylistTransChrI<<-ylistTransChr
# yInternalI<<-y
# radiusI<<-radius
# circleCenterI<<-circleCenter
# circleCenterYI<<-circleCenterY
# separFactorI<<-separFactor
# nI<<-n
# chrWidthI<<-chrWidth

# listOfdfMarkPoscMInternal<<-listOfdfMarkPoscM

  if(circularPlot==FALSE) {

    lapply(1:length(xMark), function(w) mapply(function(x,y,z)
      graphics::lines(
        x=x,
        y=y,
        col=dfMarkColorInternal$markColor[match(     z   ,dfMarkColorInternal$markName)],
        lwd=lwd.cM,
      ), # lines
    x=xMark[[w]],
    y=yMark[[w]]
    ,z=listOfdfMarkPoscM[[w]]$markName
    ) # mapply
    ) # lapp
} # CIRC
  else { # circ true

  #
  #   x to vertical
  #

  xlistNew<-xHortoVer(xMark)

  yMarkPer<-markMapPercM(yMark,y)

  ylistTransMark<-transyListMark(yMarkPer,ylistTransChr)

  circleMapsMarks  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)


  drawPlotMark(circleMapsMarks,dfMarkColorInternal,listOfdfMarkPoscM,lwd.cM)

    # if(legend=="inline"){
      circleMapsLabels <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMark,xlistNew,n,labelSpacing,
                                         chrWidth,rotation=rotation)

      circLabelMark(circleMapsLabels,listOfdfMarkPoscM,markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY,iscM=TRUE)
    # }
  }
} # fun
