#' roundPlotMark
#' This is an internal function that plot marks
#'
#' It returns a plot
#'
#' @keywords internal
#'
#' @param squareness squareness of vertices <
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

roundPlotMark<-function(bannedMarkName,squareness, xMark, yMark,
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

  if(squareness>20) {

    if(circularPlot==FALSE) {

            lapply(1:length(xMark), function(w) mapply(function(x,y,z)
                graphics::polygon(
                  x=x,
                  y=y,
                  col= dfMarkColorInternal$markColor[match(     z   , dfMarkColorInternal$markName)],
                  lwd=lwd.chr,
                  border =
                    # ifelse(dfMarkColorInternal$markColor[match(z,dfMarkColorInternal$markName)]=="white",
                  # "black",
                  dfMarkColorInternal$markBorderColor[match(z,dfMarkColorInternal$markName)]
                          # ) # ifelse
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

  listOfdfMarkPosSq2 <- listOfdfMarkPosSq

        circLabelMark(bannedMarkName,circleMapsLabels,listOfdfMarkPosSq2,markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY)
}

    } # circular
  } else {                                            # squareness < 20

    pts_1 <- seq(-pi/2, 0, length.out = n)
    pts_2 <- seq( 0, pi/2, length.out = n)
    pts_3 <- seq(pi, pi*1.5, length.out = n)
    pts_4 <- seq(pi/2, pi, length.out = n)

    yModMark<-yMark # yMark

    roundedX<-list()
    roundedY<-list()

    for (s in 1:length(yModMark) ) {

      corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosSq)[[s]] )

      if(attr(listOfdfChromSize[[corr_index]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }

      r2 <- chrWidth2/(squareness*2)

      xyCoords<-mapXY(1 , (length(yModMark[[s]]) ) ,
                      yMark[[s]], yModMark[[s]] ,
                      xMark[[s]],
                      yfactor,r2,
                      pts_1,pts_2,pts_3,pts_4)

      roundedX[[s]]<-xyCoords$roundedX
      roundedY[[s]]<-xyCoords$roundedY

      attr(roundedY[[s]],"spname") <- attr(yMark[[s]],"spname")
      attr(roundedX[[s]],"spname") <- attr(xMark[[s]],"spname")

    } # for
    names(roundedY)<-names(yMark)

#    newLongyMarks<<-roundedY
#    newLongxMarks<<-roundedX

    if(circularPlot==FALSE) {

      lapply(1:length(xMark), function(w)
        mapply(function(x,y,z)
          graphics::polygon(x=x, y=y,
                            col=dfMarkColorInternal$markColor[match(z   ,dfMarkColorInternal$markName)],
                            lwd=lwd.chr,
                            border=dfMarkColorInternal$markBorderColor[match(z,dfMarkColorInternal$markName)]
                            ), # pol
                x=roundedX[[w]],
                y=roundedY[[w]] #
                ,z=listOfdfMarkPosSq[[w]]$markName
                ) # mapply
      ) # l

    } # cir FALSE

    else { # circ true

      xlistNew<-xHortoVer(roundedX)

      yMarkPer<-markMapPer(roundedY,y)
      ylistTransMark<-transyListMark(yMarkPer,ylistTransChr)

      textyMarkPer<-centerMarkMapPer(roundedY,y)
      textylistTransMark<-transyListMark(textyMarkPer,ylistTransChr)

      circleMapsMarks  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

      circleMapsLabels <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,textylistTransMark,xlistNew,n,
                                         labelSpacing,chrWidth,rotation=rotation)

      drawPlotMark(circleMapsMarks,dfMarkColorInternal,listOfdfMarkPosSq,lwd.chr)
if(legend=="inline"){
  listOfdfMarkPosSq2 <- listOfdfMarkPosSq

     # circLabelMark(circleMaps,     listOfdfMarkPos, markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY,iscM=FALSE)
      circLabelMark(bannedMarkName,circleMapsLabels,listOfdfMarkPosSq2,markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY)
}

    } # circular
  } # else ROUNDNESS
} # FUN

chrtSqMark<-function(squareness,yMarkSq,xMarkSq,xModifier,r2,dfMarkColorInternal,lwd.chr,listOfdfMarkPosSq,n){

XmarkChrt1<-XmarkChrt2<-YmarkChrt1<-YmarkChrt2<-list()

if (squareness > 20 ) {

  for (s in 1:length(yMarkSq) ) {
    markXYSq<-mapXYchromatidHolo(1 ,
                                 (length(yMarkSq[[s]]) ) ,
                                 yMarkSq[[s]],
                                 xMarkSq[[s]],
                                 xModifier
    )

    XmarkChrt1[[s]]<-markXYSq$xCT1
    XmarkChrt2[[s]]<-markXYSq$xCT2
    YmarkChrt1[[s]]<-markXYSq$yCT1
    YmarkChrt2[[s]]<-markXYSq$yCT2

  } # for s
} else { # squareness <20
  for (s in 1:length(yMarkSq) ) {

    pts<- seq(-pi/2, pi*1.5, length.out = n*4)

    # markXYSq<-mapXYchromatidHoloRo(1 ,
    # mapXYmarksRo <- function(start,end,y,x,r2, xModifier,pts
    markXYSq<-mapXYmarksRo(1 ,
                          length(yMarkSq[[s]])  ,
                                   yMarkSq[[s]],
                                   xMarkSq[[s]],
                                   r2,
                                   xModifier,
                                   pts
    )

    # right or both:

    XmarkChrt1[[s]]<-markXYSq$markRightx # was holoRightx this has nothing to do with holo
    YmarkChrt1[[s]]<-markXYSq$markRighty

    XmarkChrt2[[s]]<-markXYSq$markLeftx
    YmarkChrt2[[s]]<-markXYSq$markLefty

  } # for
} # squareness


if(length(YmarkChrt1)>0) { # PLOT  SQ CHROMATID

  YmarkChrt1 <- YmarkChrt1[lengths(YmarkChrt1) != 0]
  XmarkChrt1 <- XmarkChrt1[lengths(XmarkChrt1) != 0]

#  YmarkChrt1_247<<-YmarkChrt1
#  XmarkChrt1_247<<-XmarkChrt1

  if(length(YmarkChrt1)>0) {

  # LEFT CHROMATID
  lapply(1:length(YmarkChrt1), function(s) mapply(function(x,y,z)
    graphics::polygon(x=x, y=y,
                      col = dfMarkColorInternal$markColor[match(     z   , dfMarkColorInternal$markName)],
                      lwd = lwd.chr,
                      border = dfMarkColorInternal$markBorderColor[match(z,dfMarkColorInternal$markName)]
    ),
    x=XmarkChrt1[[s]],
    y=YmarkChrt1[[s]],
    z=listOfdfMarkPosSq[[s]]$markName
  ) #m
  ) #l
  }
}

if(length(YmarkChrt2)>0) {
  YmarkChrt2 <- YmarkChrt2[lengths(YmarkChrt2) != 0]
  XmarkChrt2 <- XmarkChrt2[lengths(XmarkChrt2) != 0]

#  YmarkChrt2_247<<-YmarkChrt2
#  XmarkChrt2_247<<-XmarkChrt2
  if(length(YmarkChrt2)>0) {

  # RIGHT CHROMATID

  lapply(1:length(YmarkChrt1), function(s) mapply(function(x,y,z)
    graphics::polygon(x=x, y=y,
                      col = dfMarkColorInternal$markColor[match(     z   , dfMarkColorInternal$markName)],
                      lwd = lwd.chr,
                      border = dfMarkColorInternal$markBorderColor[match(z,dfMarkColorInternal$markName)]
    ),
    x=XmarkChrt2[[s]],
    y=YmarkChrt2[[s]],
    z=listOfdfMarkPosSq[[s]]$markName
  ) #m
  ) #l
  } # if
}

}

