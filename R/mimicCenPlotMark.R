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

mimicCenPlotMark<-function(squareness, xMark, yMark,
                           defCenStyleCol,
                        listOfdfMarkPosCenStyle,
                        chrWidth, specialChrWidth,
                        yfactor
                        ,n,
                        lwd.mimicCen,listOfdfChromSize,
                        circularPlot,
                        y,
                        markLabelSize,
                        separFactor,
                        labelSpacing,
                        circleCenter,circleCenterY,radius,
                        ylistTransChr,rotation,labelOutwards,
                        yMarkLine,xMarkRightLine,xMarkLeftLine,
                        x,
                        cenFormat="triangle"
                        ,pts
                        ) {



if (squareness <= 20) {

  roundedX<-list()
  roundedY<-list()


  for (s in 1:length(yMark) ) {

    corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosCenStyle)[[s]] )

    if(attr(listOfdfChromSize[[corr_index]],"ytitle")=="cM"){
      chrWidth2  <-specialChrWidth
    } else {
      chrWidth2 <- chrWidth
    }

    r2 <- chrWidth2/(squareness*2)

    xyCoords<-mapXYCenMimic(1 , (length(yMark[[s]]) ) ,
                            yMark[[s]],
                            xMark[[s]],
                            yfactor,r2,
                            pts,
                            cenFormat)

    roundedX[[s]]<-xyCoords$roundedX
    roundedY[[s]]<-xyCoords$roundedY

    attr(roundedY[[s]],"spname") <- attr(yMark[[s]],"spname")
    attr(roundedX[[s]],"spname") <- attr(xMark[[s]],"spname")

  } # for s

  names(roundedY)<-names(yMark)
  xMark<-roundedX
  yMark<-roundedY

} # end squareness

    if(circularPlot==FALSE) {

      lapply(1:length(xMark), function(w) mapply(function(x,y,z)
                graphics::polygon(
                  x=x,
                  y=y,
                  col = defCenStyleCol
                  ,lwd = 0.05
                  ,border = defCenStyleCol
                ), # pol
              x=xMark[[w]],
              y=yMark[[w]]
              ,z=listOfdfMarkPosCenStyle[[w]]$markName

        ) # mapply
      ) # lapp

      #
      # left line
      #
      lapply(1:length(xMarkLeftLine), function(w) mapply(function(x,y,z)
        graphics::lines(
          x=x,
          y=y,
          col = defCenStyleCol
          ,lwd = lwd.mimicCen,
        ), # pol
        x=xMarkLeftLine[[w]],
        y=yMarkLine[[w]]
        ,z=listOfdfMarkPosCenStyle[[w]]$markName

      ) # mapply
      ) # lapp

      #
      # r. line
      #

      lapply(1:length(xMarkRightLine), function(w) mapply(function(x,y,z)
        graphics::lines(
          x=x,
          y=y,
          col = defCenStyleCol#dfMarkColorInt$markColor[match(     z   , dfMarkColorInt$markName)],
          ,lwd = lwd.mimicCen,
        ), # pol
        x=xMarkRightLine[[w]],
        y=yMarkLine[[w]]
        ,z=listOfdfMarkPosCenStyle[[w]]$markName
      )
      )#l


    } else { # circ true

      #
      #   x to vertical - cenFormat where
      #

        xlistNew <- xHortoVer(xMark)

        yMarkPer <- markMapPer(yMark,y)
        ylistTransMark <- transyListMark(yMarkPer, ylistTransChr)


        circleMapsMarks  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor
                                           ,ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

        drawCenStyle(circleMapsMarks,defCenStyleCol,lwd.mimicCen)

        ############# left line
        xlistNew<-xHortoVer(xMarkLeftLine)

        yMarkPer <- markMapPer(yMarkLine,y)

        ylistTransMark <- transyListMark(yMarkPer, ylistTransChr)

        circleMapsMarks  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,    ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

        drawPlotMarkLine(circleMapsMarks,defCenStyleCol,lwd.mimicCen)
        ############# right line
        xlistNew<-xHortoVerDots(xMarkRightLine,x)

        yMarkPer <- markMapPer(yMarkLine,y)

        ylistTransMark <- transyListMark(yMarkPer, ylistTransChr)

        circleMapsMarks  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,    ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

        drawPlotMarkLine(circleMapsMarks,defCenStyleCol,lwd.mimicCen)

    } # circular TRUE

} # FUN

mimicCenPlotMarkInside<-function(pattern,bannedMarkName,squareness, xMarkCenStyleBody, yMarkCenStyleBody,
                           defCenStyleCol,
                           dfMarkColorInt,
                           listOfdfMarkPosCenStyle,
                           chrWidth, specialChrWidth,
                           yfactor,n,
                           lwd.mimicCen,listOfdfChromSize,
                           circularPlot,
                           y,
                           markLabelSize,
                           separFactor,
                           labelSpacing,
                           circleCenter,circleCenterY,radius,
                           ylistTransChr,rotation,labelOutwards,
                           yMarkLine,xMarkRightLine,xMarkLeftLine,
                           x,
                           lwd.chr, # new
                           legend,
                           cenFormat="triangle"
                           ,pts) {


  if(squareness <= 20 ) { # squareness


    roundedXInside<-list()
    roundedYInside<-list()


    for (s in 1:length(yMarkCenStyleBody) ) {

      corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosCenStyle)[[s]] )

      if(attr(listOfdfChromSize[[corr_index]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }

      r2 <- chrWidth2/(squareness*2)

      #
      # second rounding ?
      #

      xyCoords<-mapXYCenMimicInside(1,
                                    length(yMarkCenStyleBody[[s]]),
                                    yMarkCenStyleBody[[s]],
                                    xMarkCenStyleBody[[s]],
                                    yfactor,
                                    r2,
                                    pts,
                                    cenFormat)

      roundedXInside[[s]]<-xyCoords$roundedX
      roundedYInside[[s]]<-xyCoords$roundedY

      attr(roundedYInside[[s]],"spname") <- attr(yMarkCenStyleBody[[s]],"spname")
      attr(roundedXInside[[s]],"spname") <- attr(xMarkCenStyleBody[[s]],"spname")

    } # for s

    names(roundedYInside)<-names(yMarkCenStyleBody)
    xMarkCenStyleBody<-roundedXInside
    yMarkCenStyleBody<-roundedYInside


  } # end squareness <=20

  if(circularPlot==FALSE) {

    lapply(1:length(yMarkCenStyleBody), function(m)
      mapply(function(x,y,z)
        graphics::polygon(
          x=x,
          y=y,
          col = dfMarkColorInt$markColor[match(z, dfMarkColorInt$markName)] ,
          lwd= lwd.chr,
          border= dfMarkColorInt$markBorderColor[match(z, dfMarkColorInt$markName)] # z outside
        ), # p
        x=xMarkCenStyleBody[[m]],
        y=yMarkCenStyleBody[[m]],
        z=listOfdfMarkPosCenStyle[[m]]$markName # ifelse here gives error

      ) # mapply
    ) #l


  } else { # circ true

    #
    #   x to vertical original as mark
    #

    xlistNew <- xHortoVer(xMarkCenStyleBody)

    yMarkPer <- markMapPer(yMarkCenStyleBody,y)

    ylistTransMark <- transyListMark(yMarkPer, ylistTransChr)

    ###
    # as in cen, does not work, names
    ###

    circleMapsMarks  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor
                                       ,ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

    drawPlotMark(circleMapsMarks,dfMarkColorInt,listOfdfMarkPosCenStyle,lwd.chr)

    if(legend=="inline"){
      circleMapsMarksLabelMimicCen  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor
                                                      ,ylistTransMark,xlistNew,n,
                                                 labelSpacing ,chrWidth, rotation=rotation)

      circLabelMark(bannedMarkName,circleMapsMarksLabelMimicCen,listOfdfMarkPosCenStyle
                    , markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY)
    }

  } # circular TRUE

} # FUN

