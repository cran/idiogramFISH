#' sqPlotMarkCP
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
#' @param listMarkPosSq list of df. of mark pos.
#' @param chrWidth numeric, width of chr.
#' @param specialChrWidth numeric, width of chr.
#' @param yfactor yMark distortion based on canvas proportion
#' @param n numeric, to define vertices of rounded portions
#' @param lwd.chr thick of border line
#' @param listChrSize chr size df list
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
#'
plotSqMarks <- function(
  chromatidsCheck_1
  ,chromatidsCheck_2
  ,circularPlot
,squareness
,xMarkSq
,yMarkSq
,dfMarkColorInt
,lwd.marks2
,listMarkPosSq
,listChrSize
,specialChrWidth
,chrWidth
,yfactor
,markN
,ptsl
,bannedMarkName3
,y
,markLabelSize
,pattern
,separFactor
,labelSpacing
,circleCenter
,circleCenterY
,radius
,legend
,ylistTransChrSimple
,rotation
,labelOutwards
,x
,xModifierMH
,pts) {

if(chromatidsCheck_1) {

  if(circularPlot==FALSE) {

    if(squareness>20) {

      lapply(1:length(xMarkSq), function(w) mapply(function(x,y,z)
        graphics::polygon(
          x=x,
          y=y,
          col= dfMarkColorInt$markColor[match(     z   , dfMarkColorInt$markName)],
          lwd=lwd.marks2,
          border =
            dfMarkColorInt$markBorderColor[match(z,dfMarkColorInt$markName)]
        ), # pol
        x=xMarkSq[[w]],
        y=yMarkSq[[w]]
        ,z=listMarkPosSq[[w]]$markName

      ) # mapply
      ) # lapp

    } else { # squarenes < 20

      rounded<-roundMe(yMarkSq,xMarkSq,listChrSize,listMarkPosSq,specialChrWidth,chrWidth,squareness
                       ,yfactor,ptsl)

      lapply(1:length(xMarkSq), function(w)
        mapply(function(x,y,z)
          graphics::polygon(x=x, y=y,
                            col=dfMarkColorInt$markColor[match(z   ,dfMarkColorInt$markName)],
                            lwd=lwd.marks2,
                            border=dfMarkColorInt$markBorderColor[match(z,dfMarkColorInt$markName)]
          ), # pol
          x=rounded$roundedX[[w]],
          y=rounded$roundedY[[w]]
          ,z=listMarkPosSq[[w]]$markName
        ) # mapply
      ) # l
    }

  } else if(circularPlot) {

    if(squareness<=20){

      rounded  <- roundMe(yMarkSq,xMarkSq,listChrSize,listMarkPosSq,specialChrWidth
                          ,chrWidth,squareness,yfactor,ptsl
                          ,chrt=FALSE)

      xMarkSq <- rounded$roundedX
      yMarkSq <- rounded$roundedY

    }

    sqPlotMarkCP(bannedMarkName3
                 , xMarkSq, yMarkSq,
                 dfMarkColorInt,
                 listMarkPosSq,
                 chrWidth, #use for calc r2
                 markN,
                 lwd.marks2,#lwd.chr,
                 y,
                 markLabelSize,
                 pattern,
                 separFactor,
                 labelSpacing,circleCenter,circleCenterY,radius,
                 legend,ylistTransChrSimple,rotation=rotation
                 ,labelOutwards
                 ,x
                 ,addText=TRUE) #
  } # cP

} else if (chromatidsCheck_2) {

  #
  #   deals with squareness chrtSqMark
  #

  chrtdmap <- chrtSqMark(squareness,yMarkSq,xMarkSq,xModifierMH,dfMarkColorInt,lwd.marks2
                         ,listMarkPosSq,specialChrWidth,chrWidth,listChrSize,markN
                         ,pts)

  if(circularPlot==FALSE) {

    plotchrtds(chrtdmap$XmarkChrt1
               ,chrtdmap$YmarkChrt1
               ,chrtdmap$XmarkChrt2
               ,chrtdmap$YmarkChrt2
               ,dfMarkColorInt,lwd.marks2,listMarkPosSq
    )

  } else if(circularPlot) {

    # left chromatid and whole arm and label when inline

    sqPlotMarkCP(bannedMarkName3
                 , chrtdmap$XmarkChrt1
                 , chrtdmap$YmarkChrt1,
                 dfMarkColorInt,listMarkPosSq,
                 chrWidth
                 ,markN,
                 lwd.marks2
                 ,y,
                 markLabelSize,pattern,
                 separFactor,
                 labelSpacing,
                 circleCenter,circleCenterY,radius,
                 legend
                 ,ylistTransChrSimple,rotation=rotation,labelOutwards
                 ,x
                 ,addText=TRUE)

    # right chromatid only

    sqPlotMarkCP(bannedMarkName3
                 , chrtdmap$XmarkChrt2
                 , chrtdmap$YmarkChrt2,
                 dfMarkColorInt,listMarkPosSq,
                 chrWidth
                 ,markN,
                 lwd.marks2
                 ,y,
                 markLabelSize,pattern,
                 separFactor,
                 labelSpacing,
                 circleCenter,circleCenterY,radius,
                 legend
                 ,ylistTransChrSimple,rotation=rotation,labelOutwards
                 ,x)

  } # cP

} # chrtds
}

sqPlotMarkCP <- function(bannedMarkName
                         , xMark, yMark,
                        dfMarkColorInt
                        ,listMarkPosSq,
                        chrWidth
                        ,n,
                        lwd.chr
                        ,y,
                        markLabelSize
                        ,pattern,
                        separFactor,
                        labelSpacing,
                        circleCenter,circleCenterY,radius,
                        legend,ylistTransChr,rotation,labelOutwards
                       ,x
                       ,addText=FALSE) {

  yMarkPer <- markMapPer(yMark,y) # was t

  xlistNew <- suppressWarnings(xChrtdMarkMap(xMark,x,0) )

  ylistTransMark  <- transyListMark(yMarkPer,ylistTransChr)

  circleMapsMarks <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor
                                     ,ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

  drawPlotMark(circleMapsMarks,dfMarkColorInt,listMarkPosSq,lwd.chr)

  if(legend=="inline" & addText) {

    textyMarkPer<-centerMarkMapPer(yMark,y)

    textylistTransMark <- transyListMark(textyMarkPer,ylistTransChr)

    circleMapsLabels   <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor
                                         ,textylistTransMark,xlistNew,n,
                                         labelSpacing,chrWidth,rotation=rotation
                                         ,label=TRUE)

    circLabelMark(bannedMarkName,circleMapsLabels,listMarkPosSq,markLabelSize
                  ,pattern,labelOutwards,circleCenter,circleCenterY)
  }

} # FUN

chrtSqMark<-function(squareness,yMark,xMark,xModifier,dfMarkColorInt,lwd.chr
                     ,listMarkPosSq,specialChrWidth,chrWidth,listChrSize,n
                     ,pts) {

XmarkChrt1<-XmarkChrt2<-YmarkChrt1<-YmarkChrt2<-list()

if (squareness > 20 ) {

  for (s in 1:length(yMark) ) {

    markXYSq<-mapXYchromatidHolo(1 ,
                                 (length(yMark[[s]]) ) ,
                                 yMark[[s]],
                                 xMark[[s]],
                                 xModifier
    )

    XmarkChrt1[[s]]<-markXYSq$xCT1
    XmarkChrt2[[s]]<-markXYSq$xCT2

    YmarkChrt1[[s]]<-markXYSq$yCT1
    YmarkChrt2[[s]]<-markXYSq$yCT2

    attr(XmarkChrt1[[s]],"spname") <- attr(XmarkChrt2[[s]],"spname") <- attr(YmarkChrt1[[s]],"spname") <- attr(YmarkChrt2[[s]],"spname") <- attr(yMark[[s]],"spname")

  } # for s

} else {                  # squareness <20

  for (s in 1:length(yMark) ) {

    corr_index <- which(names(listChrSize) %in% names(listMarkPosSq)[[s]] )

    if(attr(listChrSize[[corr_index]],"ytitle")=="cM"){
      chrWidth2  <-specialChrWidth
    } else {
      chrWidth2 <- chrWidth
    }

    r2 <- chrWidth2/(squareness*2)

    markXYSq<-mapXYmarksRo(1 ,
                          length(yMark[[s]])  ,
                                   yMark[[s]],
                                   xMark[[s]],
                                   r2,
                                   xModifier,
                                   pts
    )

    # right or both:

    XmarkChrt1[[s]]<-markXYSq$markRightx
    YmarkChrt1[[s]]<-markXYSq$markRighty

    XmarkChrt2[[s]]<-markXYSq$markLeftx
    YmarkChrt2[[s]]<-markXYSq$markLefty

    attr(XmarkChrt1[[s]],"spname") <- attr(XmarkChrt2[[s]],"spname") <- attr(YmarkChrt1[[s]],"spname") <- attr(YmarkChrt2[[s]],"spname") <- attr(yMark[[s]],"spname")

  } # for s

} # squareness

chrtdmap<-list()

chrtdmap$YmarkChrt1<- YmarkChrt1[lengths(YmarkChrt1) != 0]
chrtdmap$XmarkChrt1<- XmarkChrt1[lengths(XmarkChrt1) != 0]
chrtdmap$YmarkChrt2<- YmarkChrt2[lengths(YmarkChrt2) != 0]
chrtdmap$XmarkChrt2<- XmarkChrt2[lengths(XmarkChrt2) != 0]

return(chrtdmap)
}

plotchrtds <- function(XmarkChrt1,YmarkChrt1,XmarkChrt2,YmarkChrt2
                    ,dfMarkColorInt,lwd.chr,listMarkPosSq) {
  # LEFT CHROMATID
  lapply(1:length(YmarkChrt1), function(s) mapply(function(x,y,z)
    graphics::polygon(x=x, y=y,
                      col = dfMarkColorInt$markColor[match(     z   , dfMarkColorInt$markName)],
                      lwd = lwd.chr,
                      border = dfMarkColorInt$markBorderColor[match(z,dfMarkColorInt$markName)]
    ),
    x=XmarkChrt1[[s]],
    y=YmarkChrt1[[s]],
    z=listMarkPosSq[[s]]$markName
  ) #m
  ) #l

  # RIGHT CHROMATID
  lapply(1:length(YmarkChrt1), function(s) mapply(function(x,y,z)
    graphics::polygon(x=x, y=y,
                      col = dfMarkColorInt$markColor[match(     z   , dfMarkColorInt$markName)],
                      lwd = lwd.chr,
                      border = dfMarkColorInt$markBorderColor[match(z,dfMarkColorInt$markName)]
    ),
    x=XmarkChrt2[[s]],
    y=YmarkChrt2[[s]],
    z=listMarkPosSq[[s]]$markName
  ) #m
  ) #l
} # if

roundMe <- function(yMark,xMark,listChrSize,listMarkPosSq,specialChrWidth,chrWidth
                    ,squareness,yfactor,ptsl,chrt=FALSE) {

  roundedX <- roundedY <- rounded<- list()

  for (s in 1:length(yMark) ) {

    corr_index<-which(names(listChrSize) %in% names(listMarkPosSq)[[s]] )

    if(attr(listChrSize[[corr_index]],"ytitle")=="cM"){
      chrWidth2  <-specialChrWidth
    } else {
      chrWidth2 <- chrWidth
    }

    r2 <- chrWidth2/(squareness*2)

    if(chrt==FALSE) {

      xyCoords <- mapXY(1 , (length(yMark[[s]]) ) ,
                      yMark[[s]]
                      ,yMark[[s]] ,
                      xMark[[s]],
                      yfactor,r2,
                      ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]] )
    } else {
      xyCoords <- mapXY(1 , (length(yMark[[s]]) ) ,
                        yMark[[s]]
                        ,yMark[[s]] ,
                        xMark[[s]],
                        yfactor,r2,
                        ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]]
                        ,chrt)
    }

    roundedX[[s]]<-xyCoords$roundedX
    roundedY[[s]]<-xyCoords$roundedY

    attr(roundedY[[s]],"spname") <- attr(yMark[[s]],"spname")
    attr(roundedX[[s]],"spname") <- attr(xMark[[s]],"spname")

  } # for

  names(roundedY) <- names(yMark)
  rounded$roundedX<-roundedX
  rounded$roundedY<-roundedY

  return(rounded)
}
