#' roundPlotMark
#' This is an internal function that plot marks
#'
#' It returns a plot
#'
#' @keywords internal
#'
#' @param roundness roundness of vertices <
#' @param xMark x component of polygon
#' @param yMark yMark component of polygon
#' @param dfMarkColorInternal colors for marks
#' @param listOfdfMarkPosSq list of df. of mark pos.
#' @param chrWidth numeric, width of chr.
#' @param specialChrWidth numeric, width of chr.
#' @param yfactor yMark distortion based on canvas proportion
#' @param n numeric, to define vertices of rounded portions
#' @param lwd.chr thick of border line
#' @param listOfdfChromSize chr size df list
#' @param circularPlot boolean TRUE for circ.
#' @param y list, y coords.
#' @param markLabelSize numeric, font size
#' @param pattern character, regex
#' @param separFactor numeric, sep. kars.
#' @param labelSpacing numeric, spacing
#' @param circleCenter numeric
#' @param circleCenterY numeric
#' @param radius numeric
#' @param legend character
#' @param ylistTransChr list, transf. coords.
#' @param rotation rotate
#' @param labelOutwards srt
#'
#' @return plot
#' @importFrom graphics polygon text
#'

roundPlotMark<-function(roundness, xMark, yMark,
                        dfMarkColorInternal,listOfdfMarkPosSq,
                        chrWidth, specialChrWidth,
                        yfactor,n,
                        lwd.chr,listOfdfChromSize,
                        circularPlot,
                        y,
                        markLabelSize,pattern,
                        separFactor,
                        labelSpacing,
                        circleCenter,circleCenterY,radius,
                        legend,ylistTransChr,rotation,labelOutwards) {

#  xMarkSq<<-xMark
#  yMarkSq<<-yMark

#  listOfdfMarkPosSqInternal<<-listOfdfMarkPosSq
#  dfMarkColorInternal2<<-dfMarkColorInternal

  if(roundness>20) {

    if(circularPlot==FALSE) {

            lapply(1:length(xMark), function(w) mapply(function(x,y,z)
                graphics::polygon(
                  x=x,
                  y=y,
                  col= dfMarkColorInternal$markColor[match(     z   , dfMarkColorInternal$markName)],
                  lwd=lwd.chr,
                  border = ifelse(dfMarkColorInternal$markColor[match(z,dfMarkColorInternal$markName)]=="white",
                  "black",
                  dfMarkColorInternal$markColor[match(z,dfMarkColorInternal$markName)]
                          ) # ifelse
                ), # pol
              x=xMark[[w]],
              y=yMark[[w]]
              ,z=listOfdfMarkPosSq[[w]]$markName

        ) # mapply
    ) # lapp
    } # CIRC
    else { # circ true

      #
      #   x to vertical
      #

#      xMarkSq<<-xMark
#      yMarkSq<<-yMark
#      yInternalSq<<-y


        xlistNew<-xHortoVer(xMark)

        yMarkPer<-markMapPer(yMark,y)

        textyMarkPer<-centerMarkMapPer(yMark,y)

        ylistTransMark<-transyListMark(yMarkPer,ylistTransChr)

        textylistTransMark<-transyListMark(textyMarkPer,ylistTransChr)

        circleMapsMarks  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,    ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

        circleMapsLabels <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,textylistTransMark,xlistNew,n,
                                           labelSpacing,chrWidth,rotation=rotation)

        drawPlotMark(circleMapsMarks,dfMarkColorInternal,listOfdfMarkPosSq,lwd.chr)

if(legend=="inline"){
        circLabelMark(circleMapsLabels,listOfdfMarkPosSq,markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY)
}

    } # circular
  } else {                                            # roundness < 20

    pts_1 <- seq(-pi/2, 0, length.out = n)
    pts_2 <- seq( 0, pi/2, length.out = n)
    pts_3 <- seq(pi, pi*1.5, length.out = n)
    pts_4 <- seq(pi/2, pi, length.out = n)

    yModMark<-yMark # yMark

    topline_y<-list()
    bottomline_y<-list()
    topBotline_x<-list()
    x2_1<-list()
    x2_2<-list()
    y2_1<-list()
    y2_2<-list()
    xy_1<-list()
    xy_2<-list()
    xy_3<-list()
    xy_4<-list()
    newLongx<-list()
    newLongy<-list()

    for (s in 1:length(yModMark) ) {

      corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosSq)[[s]] )

      if(attr(listOfdfChromSize[[corr_index]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }

      r2 <- chrWidth2/(roundness*2)

      xyCoords<-mapXY(1 , (length(yModMark[[s]]) ) ,
                      yMark[[s]], yModMark[[s]] ,
                      xMark[[s]],
                      yfactor,r2,
                      pts_1,pts_2,pts_3,pts_4)

      newLongx[[s]]<-xyCoords$newLongx
      newLongy[[s]]<-xyCoords$newLongy

      attr(newLongy[[s]],"sqname") <- attr(yMark[[s]],"sqname")
      attr(newLongx[[s]],"sqname") <- attr(xMark[[s]],"sqname")

    } # for
    names(newLongy)<-names(yMark)

#    newLongyMarks<<-newLongy
#    newLongxMarks<<-newLongx

    if(circularPlot==FALSE) {

      lapply(1:length(xMark), function(w)
        mapply(function(x,y,z)
          graphics::polygon(x=x, y=y,
                            col=dfMarkColorInternal$markColor[match(z   ,dfMarkColorInternal$markName)],
                            lwd=lwd.chr,
                            border=dfMarkColorInternal$markBorderColor[match(z,dfMarkColorInternal$markName)]
                            ), # pol
                x=newLongx[[w]],
                y=newLongy[[w]] #
                ,z=listOfdfMarkPosSq[[w]]$markName
                ) # mapply
      ) # l

    } # cir FALSE

    else { # circ true

      xlistNew<-xHortoVer(newLongx)

      yMarkPer<-markMapPer(newLongy,y)
      ylistTransMark<-transyListMark(yMarkPer,ylistTransChr)

      textyMarkPer<-centerMarkMapPer(newLongy,y)
      textylistTransMark<-transyListMark(textyMarkPer,ylistTransChr)

      circleMapsMarks  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

      circleMapsLabels <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,textylistTransMark,xlistNew,n,
                                         labelSpacing,chrWidth,rotation=rotation)

      drawPlotMark(circleMapsMarks,dfMarkColorInternal,listOfdfMarkPosSq,lwd.chr)
if(legend=="inline"){
      circLabelMark(circleMapsLabels,listOfdfMarkPosSq,markLabelSize,pattern)
}

    } # circular
  } # else ROUNDNESS
} # FUN
