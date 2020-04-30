#' mimicCenPlotMark
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

mimicCenPlotMark<-function(roundness, xMark, yMark,
                        dfMarkColorInternal,listOfdfMarkPosCenStyle,
                        chrWidth, specialChrWidth,
                        yfactor,n,
                        lwd.chr,listOfdfChromSize,
                        circularPlot,
                        y,
                        markLabelSize,
                        separFactor,
                        labelSpacing,
                        circleCenter,circleCenterY,radius,
                        ylistTransChr,rotation,labelOutwards) {

#  xMarkSq<<-xMark
#  yMarkSq<<-yMark

#  listOfdfMarkPosCenStyleInternal<<-listOfdfMarkPosCenStyle
#  dfMarkColorInternal2<<-dfMarkColorInternal

  if(roundness>20) {

    if(circularPlot==FALSE) {

            lapply(1:length(xMark), function(w) mapply(function(x,y,z)
                graphics::polygon(
                  x=x,
                  y=y,
                  col= dfMarkColorInternal$markColor[match(     z   , dfMarkColorInternal$markName)],
                  lwd=lwd.chr*3,
                  border = #ifelse(dfMarkColorInternal$markColor[match(z,dfMarkColorInternal$markName)]=="white", "black",
                  dfMarkColorInternal$markBorderColor[match(z,dfMarkColorInternal$markName)]
                          #) # ifelse
                ), # pol
              x=xMark[[w]],
              y=yMark[[w]]
              ,z=listOfdfMarkPosCenStyle[[w]]$markName

        ) # mapply
      ) # lapp
    } # CIRC FALSE
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

        drawPlotMark(circleMapsMarks,dfMarkColorInternal,listOfdfMarkPosCenStyle,lwd.chr*3)

    } # circular TRUE

  } else {                                            # roundness < 20 ##############################################

    pts<- seq(-pi/2, pi*1.5, length.out = n)

    yModMark<-yMark # yMark

    newLongx<-list()
    newLongy<-list()

    for (s in 1:length(yModMark) ) {

      corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosCenStyle)[[s]] )

      if(attr(listOfdfChromSize[[corr_index]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }

      r2 <- chrWidth2/(roundness*2)

      xyCoords<-mapXYCen(1 , (length(yModMark[[s]]) ) ,
                      yMark[[s]], yModMark[[s]] ,
                      xMark[[s]],
                      yfactor,r2,
                      pts)

      newLongx[[s]]<-xyCoords$newLongx
      newLongy[[s]]<-xyCoords$newLongy

      attr(newLongy[[s]],"spname") <- attr(yMark[[s]],"spname")
      attr(newLongx[[s]],"spname") <- attr(xMark[[s]],"spname")

    } # for
    names(newLongy)<-names(yMark)

#    newLongyMarks<<-newLongy
#    newLongxMarks<<-newLongx

    if(circularPlot==FALSE) {

      lapply(1:length(xMark), function(w)
        mapply(function(x,y,z)
          graphics::polygon(x=x, y=y,
                            col=dfMarkColorInternal$markColor[match(z   ,dfMarkColorInternal$markName)],
                            lwd=lwd.chr*3,
                            border=dfMarkColorInternal$markBorderColor[match(z,dfMarkColorInternal$markName)]
                            ), # pol
                x=newLongx[[w]],
                y=newLongy[[w]] #
                ,z=listOfdfMarkPosCenStyle[[w]]$markName
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

      drawPlotMark(circleMapsMarks,dfMarkColorInternal,listOfdfMarkPosCenStyle,lwd.chr*3)

    } # circular
  } # else ROUNDNESS

} # FUN
