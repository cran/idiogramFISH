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
#' @param ylist y coords
#' @param ylistNew y
#' @param x only d.f. of this cen. type
#' @param y intervals
#' @param n font
#' @param r3 modifier of pos.
#' @param shrinkFactor numeric
#' @param circleCenterY numeric
#' @param separFactor factor
#' @param ylistTrans ylist
#' @param xlistNew xlist
#' @param n number
#' @param circleMaps maps
#' @param chrColor chr color
#' @param cenBorder character, cen border color
#' @param cenColor2 character, cen. color
#' @param cfunction character, name of function to apply
#' @param lwd.chr numeric, width of line
#' @param chrColor character, chr. color
#' @param chrId character, type of chr. name
#' @param chrNames list, resulting from applyMapCircle
#' @param chrWidth character, chr. width
#' @param circleCenter numeric, modify center of circle of chr.
#' @param circleMaps list, resulting from applyMapCircle
#' @param circleMapsMarksCr list, resulting from applyMapCircle
#' @param circleMapsOTUname list, resulting from applyMapCircle
#' @param colBorderCr character, border color
#' @param colCr character color of mark
#' @param dfMarkColorInternal data.frame of mark charact.
#' @param diffXRounded numeric, difference of chr. width to rounded corner
#' @param firstXchrEach list, first element from xlistNewChr
#' @param firstYchrEach list, first element from xlistNewChr
#' @param fixCenBorder2 boolean, change cen border
#' @param indexIdTextSize numeric, text size
#' @param labelSpacing numeric, spacing of labels to chr. or mark.
#' @param parparlistOfdfMarkPosDataCen list, of d.f.s of marks
#' @param listOfdfMarkPos list, of d.f.s of marks
#' @param listOfdfMarkPosSq list, of d.f.s of marks
#' @param mapCircle function name
#' @param markDistType character, type of distance to mark
#' @param markLabelSize numeric, size of mark label
#' @param monocenNames character, vector with names of OTUs
#' @param n numeric, number of vertices
#' @param OTUplacing boolean use number instead of OTU name
#' @param OTUlabelSpacing numeric, spacing for OTU name
#' @param OTUsrt numeric, angle for OTU name
#' @param OTUTextSize numeric, font size of OTU
#' @param pattern character, regex pattern to remove from mark names
#' @param position numeric, indexer for karyotype position
#' @param radius numeric, radius
#' @param rad list, list of radius for dots marks
#' @param radiusMap function, map radius in circle
#' @param radPerCr list, radius percentages
#' @param separFactor numeric, separation of kar. in circle
#' @param shrinkFactor numeric, percentage in decimal of whole chr.
#' @param unlist boolean, for unlisting x or y coordinates
#' @param xfactor numeric, modify y x aspect
#' @param xlist list, x coordinates
#' @param xlistMarkCen list, marks
#' @param xlistNew list, transformed coordinates
#' @param xlistNewCen list, transformed coordinates
#' @param xlistNewChr list, transformed coordinates
#' @param xMark list, mark coords.
#' @param xMarkCr list, mark coords.
#' @param xMarkList list, mark coords.
#' @param xRounded list, transformed coords.
#' @param y list, chr coords.
#' @param ylistNew list, chr coords.
#' @param ylistNewCen list, chr coords.
#' @param ylistNewChr list, chr coords.
#' @param ylistTrans list, trans .chr coords.
#' @param ylistTransChr list, trans .chr coords.
#' @param yMark list, mark coords.
#' @param yMarkCen list, mark coords.
#' @param yMarkPer list, mark coords in percentage.
#' @param OTUlegendHeight numeric
#' @param labelOutwards srt
#'
#' @return plot
#' @importFrom graphics mtext
#' @importFrom grDevices colorRampPalette

yVertoHor<-function(y,monocenNames){
  ylistNew<-list()

  for (s in 1:length(y)) {

    if(names(y[s]) %in% monocenNames){ # mono test

      ylistNew[[s]]<-y[[s]]

      if(length(y[[s]] ) >= (2*2) ){
        diffPrevious<-0

        for (i in seq(1, (length(y[[s]])-1 ),by=2 ) ) {
          ylistNew[[s]][[i]]   <- ylistNew[[s]][[i]]   - min(y[[s]][[i]], na.rm=T)
          ylistNew[[s]][[i+1]] <- ylistNew[[s]][[i+1]] - min(y[[s]][[i]], na.rm=T)
        }
        i<-3
        for (i in seq(3, (length(y[[s]])-1 ),by=2 ) ) {
          diffPrevious <- diffPrevious + max(ylistNew[[s]][[i-1]], na.rm=T) - min(ylistNew[[s]][[i-2]], na.rm=T)

          ylistNew[[s]][[i]]   <- ylistNew[[s]][[i]]   + diffPrevious
          longSize<-max(ylistNew[[s]][[i]], na.rm=T) - min(ylistNew[[s]][[i]], na.rm=T)
          ylistNew[[s]][[i+1]] <- ylistNew[[s]][[i+1]] + diffPrevious
        }
        for (c in 1:length(y[[s]])){
          attr(ylistNew[[s]][[c]], "chrName1") <- attr(y[[s]][[c]], "chrName1")
        }
      }
    } else { # end mono beg. holo
    ylistNew[[s]]<-y[[s]]
    if(length(y[[s]] ) >= 2 ) {
      diffPrevious<-0
      for (i in 2:length(y[[s]] )) {
        diffPrevious<-diffPrevious + max(ylistNew[[s]][[i-1]], na.rm=T) - min(ylistNew[[s]][[i-1]], na.rm=T)
        ylistNew[[s]][[i]]<-ylistNew[[s]][[i]]+diffPrevious
      }
      for (c in 1:length(y[[s]])){
        attr(ylistNew[[s]][[c]], "chrName1") <- attr(y[[s]][[c]], "chrName1")
      }
    }
  } # else holo
  } # for s
  return(ylistNew)
} # fun

xHortoVer<-function(xlist,shrink=0){
  xlistNew<-list()

  for (s in 1:length(xlist)){
    xlistNew[[s]]<-xlist[[s]]

    for (i in 1:length(xlist[[s]])){
      minChro <- min(xlistNew[[s]][[i]], na.rm=T)
      maxChro <- max(xlistNew[[s]][[i]], na.rm=T)
      xlistNew[[s]][[i]] <- xlistNew[[s]][[i]] - minChro + (shrink * (maxChro-minChro) )
      attr(xlistNew[[s]][[i]],"rowIndex") <- attr(xlist[[s]][[i]],"rowIndex")          # i
    }
    names(xlistNew)[s] <- names(xlist[s])
    attr(xlistNew[[s]],"spname") <- attr(xlist[[s]],"spname")
  } # for
  return(xlistNew)
} # fun

xHortoVerMid<-function(xlist,shrink=0){
  xlistNew<-list()

  for (s in 1:length(xlist)){
    xlistNew[[s]]<-xlist[[s]]

    for (i in 1:length(xlist[[s]])){
      minChro <- min(xlistNew[[s]][[i]], na.rm=T)
      maxChro <- max(xlistNew[[s]][[i]], na.rm=T)

      xlistNew[[s]][[i]] <- xlistNew[[s]][[i]] - minChro + (shrink * (maxChro-minChro) )

      attr(xlistNew[[s]][[i]],"rowIndex") <- attr(xlist[[s]][[i]],"rowIndex")          # i
    }
    names(xlistNew)[s] <- names(xlist[s])
    attr(xlistNew[[s]],"spname") <- attr(xlist[[s]],"spname")
  } # for
  return(xlistNew)
} # fun

xHortoVerRoundCen<-function(xlist,diffXRounded){
  xlistNew<-list()

  for (s in 1:length(xlist)){
    xlistNew[[s]]<-xlist[[s]]
    for (i in 1:length(xlist[[s]])){
      minSpecies<-min(xlistNew[[s]][[i]], na.rm=T)
      xlistNew[[s]][[i]] <- xlistNew[[s]][[i]]-minSpecies + diffXRounded
    }
    names(xlistNew)[s] <- names(xlist[s])
  } # for

  return(xlistNew)
} # fun

xHortoVerRoundCenExt<-function(xlist,diffXRounded){
  xlistNew<-list()

  for (s in 1:length(xlist)){
    xlistNew[[s]]<-xlist[[s]]
    for (i in 1:length(xlist[[s]])){
      minSpecies <- min(xlistNew[[s]][[i]], na.rm=T)
      maxX<-max(xlistNew[[s]][[i]]-minSpecies + diffXRounded, na.rm=T)
      xlistNew[[s]][[i]] <- xlistNew[[s]][[i]]-minSpecies +  maxX

    }
    names(xlistNew)[s] <- names(xlist[s])
  } # for

  return(xlistNew)
} # fun

xHortoVerDots<-function(xMarkList,x){
  xlistNew<-list()

  for (s in 1:length(xMarkList)){
    xlistNew[[s]]<-list()
    currName<-attr(xMarkList[[s]],"spname")
    corrIndex<-which(names(x)%in% currName) # x5 x
# i
    for (i in 1:length(xMarkList[[s]])){
      currChrNameB<-attr(xMarkList[[s]][[i]],"rowIndex")
      corrIndexChrNameB<-which(names(x[[corrIndex]]) %in% currChrNameB) # x5 x

      xlistNew[[s]][[i]]<-xMarkList[[s]][[i]]

      minXChr<- min(x[[corrIndex]][[corrIndexChrNameB]])

      if (length(xMarkList[[s]][[i]])>1 ){
      for (d in 1:length(xMarkList[[s]][[i]]) ) {

        xlistNew[[s]][[i]][[d]]<-xlistNew[[s]][[i]][[d]]-minXChr

        } #3 f
      } else {
        xlistNew[[s]][[i]]<-xlistNew[[s]][[i]]-minXChr
      }
    } #2 f
    attr(xlistNew[[s]],"spname")<-attr(xMarkList[[s]],"spname")
  } #1 for

  return(xlistNew)
} # fun

mapCircle<-function(x,y,n,radius,circleCenter,circleCenterY,position,separFactor,
                    labelSpacing,chrWidth,rotation) {

  r2 <- radius*position*chrWidth*2 + position*separFactor + chrWidth*labelSpacing

  yValue2<-list()
  xValue2<-list()
  if (length(y)>1){
    for (i in 1:(length(y)-1) ){
      yValue2[[i]] <- seq(y[i],y[i+1], length.out=n) * 2 # stands for 2pi = 360 degrees
      xValue2[[i]] <- seq(x[i],x[i+1], length.out=n)
    }
  } else {
    yValue2[[1]] <- y[1] * 2 # stands for 2pi = 360 degrees
    xValue2[[1]] <- x[1]
  }
  x1b<- ( r2+unlist(xValue2)  ) * sin(unlist(yValue2)*pi -(rotation*pi) ) + circleCenter #0
  y1b<- ( r2+unlist(xValue2)  ) * cos(unlist(yValue2)*pi -(rotation*pi) ) + circleCenterY #1
  xyCircle<-list()
  xyCircle$x<-x1b
  xyCircle$y<-y1b

  attr(xyCircle, "chrName1") <- attr(y, "chrName1")

  # attr(xyCircle,"positionB")
  return(xyCircle)
}

mapRadius<-function(x,y,n,radius,circleCenter,circleCenterY, position,separFactor,labelSpacing,chrWidth,rotation) {
  newrad <- radius*position*chrWidth*2 + position*separFactor + chrWidth*labelSpacing + x
  # newrad <- radius + position + position*chrWidth + separFactor + chrWidth*labelSpacing +x
  # newrad <- radius + position*separFactor*chrWidth + chrWidth*labelSpacing + x
  newrad <- 2*pi*newrad * y
  # attr(xyCircle,"positionB")
  return(newrad)
}

transYList<-function(ylistNew,shrinkFactor,monocenNames) {
  ylistTrans<-list()
  for (s in 1:length(ylistNew)){
    yMin<-min(unlist(ylistNew[[s]]), na.rm=T  )
    ylistTrans[[s]] <- lapply(ylistNew[[s]], function(y) y - yMin )
  }
  for (s in 1:length(ylistNew)){
    yMax<-max(unlist(ylistTrans[[s]]), na.rm=T  )
    ylistTrans[[s]] <- lapply(ylistTrans[[s]], function(y) y/yMax )
  }
  for (s in 1:length(ylistTrans)){
    ylistTrans[[s]] <- lapply(ylistTrans[[s]], function(y) y*shrinkFactor )

    if(names(ylistNew[s]) %in% monocenNames) {                         # mono test

      for (c in seq(1, (length(ylistTrans[[s]] ) ), by=2 ) ) {
        ylistTrans[[s]][[c]]   <- (ylistTrans[[s]][[c]])   +   ( (   ( (1 - shrinkFactor)/2 )   * (c*2 - 1) ) / length(ylistTrans[[s]] ) )
        attr(ylistTrans[[s]][[c]], "chrName1") <- attr(ylistNew[[s]][[c]], "chrName1")
        ylistTrans[[s]][[c+1]] <- (ylistTrans[[s]][[c+1]]) +   ( (   ( (1 - shrinkFactor)/2 )   * (c*2 - 1) ) / length(ylistTrans[[s]] ) )
      }

      ylistTrans[[s]]<-lapply(ylistTrans[[s]], function(x) x+  ( ( ( 1 - shrinkFactor)/2     ) / length(ylistTrans[[s]] )  ) )

    } else { # monocen                                                - else holo

      for (c in 1:(length(ylistTrans[[s]] ) ) ) {
        ylistTrans[[s]][[c]]  <- (ylistTrans[[s]][[c]]) +      (   ( (1 - shrinkFactor)/2 )   * (c*2 - 1) ) / length(ylistTrans[[s]] )
        attr(ylistTrans[[s]][[c]], "chrName1") <- attr(ylistNew[[s]][[c]], "chrName1")
      }


    }
    attr(ylistTrans[[s]],"positionnoNA")<- attr(ylistNew[[s]],"positionnoNA")
  } # big for
  return(ylistTrans)
} # fun


mapOTUnames<-function(firstYchrEach , firstXchrEach, ylistNewChr, n, radius, circleCenter, circleCenterY,
                      separFactor, OTUlabelSpacing, chrWidth,rotation) {

  circleMapsOTUname<-list()
  for (s in 1:length(firstYchrEach ) ) { # for polygon
    position<-as.numeric(attr(ylistNewChr[[s]],"positionnoNA") )
    circleMapsOTUname[[s]]<- mapCircle(x=firstXchrEach[[s]],
                                       y=firstYchrEach[[s]],
                                       n=n,
                                       radius=radius,
                                       circleCenter=circleCenter,
                                       circleCenterY=circleCenterY,
                                       position,
                                       separFactor,
                                       OTUlabelSpacing,
                                       chrWidth,
                                       rotation
    ) # mapCircle
    attr(circleMapsOTUname[[s]],"name") <- names(firstYchrEach[s])
    attr(circleMapsOTUname[[s]],"positionnoNA") <- position
  }
  return(circleMapsOTUname)
}

addOTUnames<-function(circleMapsOTUname,OTUTextSize,OTUsrt=0,OTUplacing="first",OTUfont2,OTUfamily2,
                      circleCenter,OTULabelSpacerx,circleCenterY,OTULabelSpacery,OTUlegendHeight,radius,
                      chrWidth,normalizeToOne,OTUcentered,OTUjustif,separFactor,labelSpacing) {
  maxPos<-numeric()
  for (s in 1:length(circleMapsOTUname ) ) {
    maxPos<-max(maxPos,as.numeric(attr(circleMapsOTUname[[s]],"positionnoNA")))

    centerX<-min(unlist(circleMapsOTUname[[s]]$x) )
    centerY<-min(unlist(circleMapsOTUname[[s]]$y) )

    graphics::text(x=centerX,
                   y=centerY
                   ,label= if(OTUplacing=="first"){
                      attr(circleMapsOTUname[[s]],"name")
                   } else if(OTUplacing=="number") {
                     attr(circleMapsOTUname[[s]],"positionnoNA")
                   } else {
                     ""
                   }
                   ,cex= OTUTextSize
                   # pos=4,
                   ,adj=0.5
                   ,srt=OTUsrt
                   ,font=   OTUfont2
                   ,family= OTUfamily2
    ) # text
  } # for

  if(OTUplacing=="number" | OTUplacing=="simple"){
    OTUlabelsright(circleMapsOTUname, circleCenter,OTULabelSpacerx,circleCenterY,OTULabelSpacery,
                   OTUlegendHeight,radius,chrWidth,OTUfont2,OTUfamily2,OTUTextSize,normalizeToOne,
                   OTUplacing,OTUcentered,OTUjustif,maxPos,separFactor,labelSpacing)
  }


} # fun


applyMapCircle<-function(radius,circleCenter,circleCenterY,separFactor,ylistTrans,xlistNew,n=NA,labelSpacing,
                         chrWidth,unlist=FALSE,cfunction=mapCircle,specialOTUNames=NA,chrWFactor=NA,rotation) {
  circleMaps<-list()
  # radius <- r3 + separFactor

  for (s in 1:length(ylistTrans)) { # for spescies
    circleMaps[[s]]<-list()
    labelSpacing2<-labelSpacing

    if(!is.na(specialOTUNames[1]) & specialOTUNames[1]!=""){
      if(names(ylistTrans[s]) %in%  specialOTUNames){
        labelSpacing2<-labelSpacing*chrWFactor
#        labelSpacing2I<<-labelSpacing2
      }
    }

    for (c in 1:length(ylistTrans[[s]] ) ) { # for polygon
      position<- as.numeric(attr(ylistTrans[[s]],"positionnoNA") )



      circleMaps[[s]][[c]]<- cfunction(x=if(unlist){unlist(xlistNew[[s]][[c]]  )} else{  xlistNew[[s]][[c]] },
                                       y=if(unlist){unlist(ylistTrans[[s]][[c]])} else{ylistTrans[[s]][[c]] },
                                       n=n,
                                       radius=radius,
                                       circleCenter=circleCenter,
                                       circleCenterY=circleCenterY,
                                       position,
                                       separFactor,
                                       labelSpacing2,
                                       chrWidth,
                                       rotation
      ) # mapCircle
    }
    attr(circleMaps[[s]],"positionB")<-s
    names(circleMaps)[s] <- names(ylistTrans[s])
  } # for
  return(circleMaps)
}
# # y<-yInternal
# s<-1
# a<-8
# newOrder

intercalate<-function(y,monocenNames){
  newOrder<-list()
  for (s in 1:length(y)) {
      if(names(y[s]) %in% monocenNames) { # mono test
        newOrder[[s]]<-list()

        for (a in 1:(length(y[[s]])/2 ) ){
          b<-a*2-1
          newOrder[[s]][[b]]<-y[[s]][[a]]
          names(newOrder[[s]])[b] <- names(y[[s]][a])
        }

        for (a in ( (length(y[[s]])/2 )+1):length(y[[s]]) ){
          b<-( a -  (length(y[[s]])/2 )  )*2
          newOrder[[s]][[b]]<-y[[s]][[a]]
          names(newOrder[[s]])[b] <- names(y[[s]][a])
        }
        # names(newOrder[[s]]) <- names(y[[s]])

    } else {
      newOrder[[s]]<-list()
      newOrder[[s]]<-y[[s]]
      names(newOrder[[s]]) <- names(y[[s]])
    }
    attr(newOrder[[s]],"positionnoNA")<- attr(y[[s]],"positionnoNA")
  } # for
  newOrder<-Filter(function(x) {length(x) >= 1}, newOrder)
  return(newOrder)
} # fun

drawPlot<-function(circleMaps,chrColor,lwd.chr,chrBorderColor2) {
  for (s in 1:length(circleMaps)){
    for (i in 1:length(circleMaps[[s]] ) ) {
      graphics::polygon(x=circleMaps[[s]][[i]]$x,
                        y=circleMaps[[s]][[i]]$y,
                        col=chrColor,
                        lwd=lwd.chr,
                        border=chrBorderColor2

      ) # polygon
    } # for
  } # for
}

drawPlotMark<-function(circleMaps,dfMarkColorInternal,listOfdfMarkPosSq,lwd.chr) {
  for (s in 1:length(circleMaps)){
    for (i in 1:length(circleMaps[[s]] ) ) {
      graphics::polygon(x=circleMaps[[s]][[i]]$x,
                        y=circleMaps[[s]][[i]]$y,
                        col = dfMarkColorInternal$markColor[match(     listOfdfMarkPosSq[[s]]$markName[[i]] ,
                                 dfMarkColorInternal$markName)],
                        lwd=lwd.chr,
                        border = dfMarkColorInternal$markBorderColor[match( listOfdfMarkPosSq[[s]]$markName[[i]]
                                                                             ,dfMarkColorInternal$markName)]
                        # ) # ifelse
      ) # polygon
    } # for
  } # for
} # fun

drawCenStyle<-function(circleMaps,defCenStyleCol,lwd.chr) {
  for (s in 1:length(circleMaps)){
    for (i in 1:length(circleMaps[[s]] ) ) {
      graphics::polygon(x=circleMaps[[s]][[i]]$x,
                        y=circleMaps[[s]][[i]]$y,
                        col = defCenStyleCol,
                        lwd=lwd.chr,
                        border = defCenStyleCol
                        # ) # ifelse
      ) # polygon
    } # for
  } # for
} # fun
# drawPlotMarkLine<-function(circleMaps,dfMarkColorInternal,listOfdfMarkPosSq,lwd.chr) {
#   for (s in 1:length(circleMaps)){
#     for (i in 1:length(circleMaps[[s]] ) ) {
#       graphics::lines(x=circleMaps[[s]][[i]]$x,
#                       y=circleMaps[[s]][[i]]$y,
#                       col = dfMarkColorInternal$markColor[match(     listOfdfMarkPosSq[[s]]$markName[[i]]   ,
#                                                                      dfMarkColorInternal$markName)],
#                       lwd=lwd.chr,
#       ) # polygon
#     } # for
#   } # for
# } # fun
drawPlotMarkLine<-function(circleMaps,defCenStyleCol,lwd.chr) {
  for (s in 1:length(circleMaps)){
    for (i in 1:length(circleMaps[[s]] ) ) {
      graphics::lines(x=circleMaps[[s]][[i]]$x,
                        y=circleMaps[[s]][[i]]$y,
                        col = defCenStyleCol,
                      lwd=lwd.chr,
      ) # polygon
    } # for
  } # for
} # fun


drawCen<-function(circleMaps,cenColor2,cenBorder,lwd.chr) {
  for (s in 1:length(circleMaps)){
    for (i in 1:length(circleMaps[[s]] ) ) {
      graphics::polygon(x=circleMaps[[s]][[i]]$x,
                        y=circleMaps[[s]][[i]]$y,
                        col = cenColor2,
                        lwd=lwd.chr,
                        border = cenBorder
      ) # polygon
    } # for
  } # for
} # fun

circLabelMark<-function(bannedMarkName,circleMaps,listOfdfMarkPos,markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY,iscM=FALSE,adj=0.5,iscMLeft=FALSE) {
  for (s in 1:length(circleMaps)){

    for (i in 1:length(circleMaps[[s]] ) ) {

      if( !listOfdfMarkPos[[s]]$markName[i] %in% bannedMarkName ){

      centerX<-mean(unlist(circleMaps[[s]][[i]]$x) )
      centerY<-mean(unlist(circleMaps[[s]][[i]]$y) )


      if (labelOutwards){
        delta_x = centerX - circleCenter
        delta_y = centerY - circleCenterY
        theta_radians = atan2(delta_y, delta_x)
        srt<-(theta_radians*180)/pi
        if(srt>90 & srt<=180){
          srt<-srt+180
        } else if (srt < -90 & srt >= -180) {
          srt<-srt+180
          }
      } else {
        srt<-0
      }

      if (iscM){

        minX<-min(unlist(circleMaps[[s]][[i]]$x) )
        maxX<-max(unlist(circleMaps[[s]][[i]]$x) )

        minY<-min(unlist(circleMaps[[s]][[i]]$y) )
        maxY<-max(unlist(circleMaps[[s]][[i]]$y) )

        xmoreDistant<-ifelse(abs(minX-circleCenter)  > abs(maxX-circleCenter),
                             centerX<-minX,
                             centerX<-maxX )
        ymoreDistant<-ifelse(abs(minY-circleCenterY) > abs(maxY-circleCenterY),
                             centerY<-minY,
                             centerY<-maxY )
      }
      if (iscMLeft){

        minX<-min(unlist(circleMaps[[s]][[i]]$x) )
        maxX<-max(unlist(circleMaps[[s]][[i]]$x) )

        minY<-min(unlist(circleMaps[[s]][[i]]$y) )
        maxY<-max(unlist(circleMaps[[s]][[i]]$y) )

        xmoreDistant<-ifelse(abs(minX-circleCenter)  > abs(maxX-circleCenter),
                             centerX<-maxX,
                             centerX<-minX )
        ymoreDistant<-ifelse(abs(minY-circleCenterY) > abs(maxY-circleCenterY),
                             centerY<-maxY,
                             centerY<-minY )
      }
      w=tryCatch(listOfdfMarkPos[[s]]$chrRegionOrig[i], error=function(e) {NA} )
      z=sub(pattern,"",listOfdfMarkPos[[s]]$markName[i])

      graphics::text(x=centerX,
                     y=centerY
                       ,label=ifelse(any(is.na(w),is.null(w) ),z,"")
                       ,cex=markLabelSize
                     # pos=4,
                       ,adj=adj
                       ,srt=srt
      ) # text
      } # if
    } # for i

  } # for
} # fun

circPlotDots <- function(circleMapsMarksCr,xfactor,radiusMap,
                         # radiusMapX,
                         colCr,colBorderCr,n){
lapply(1:length(circleMapsMarksCr), function(m)
  lapply(1:length(circleMapsMarksCr[[m]] ), function(u)
    mapply(function(x,y,
                    # radiusX,
                    radius,z,w) {
      pts2=seq(0, 2 * pi, length.out = n*4)
      xy2 <- cbind(x + radius * sin(pts2)*xfactor , y + radius * cos(pts2) )

      graphics::polygon(xy2[,1],
                        xy2[,2],
                        col=z,
                        border = w
      ) #p
    }, # f
    x=circleMapsMarksCr[[m]][[u]]$x,
    y=circleMapsMarksCr[[m]][[u]]$y,
    # radiusX=radiusMapX[[m]][[u]],
    radius=radiusMap[[m]][[u]],
    z=colCr[[m]][[u]],
    w=colBorderCr[[m]][[u]]
    ) # mapply
  ) # lapply
)
}

# attr(listChrCenter[[s]][[c]],"chrName")

plotChrNames<-function(chrNames, indexIdTextSize,chrId,monocenNames,chrColor) {
  labels<-list()

  for (s in 1:length(chrNames)){
    divisorL <- ifelse(names(chrNames[s]) %in% monocenNames, 2,1 )

    labels[[s]]<-list()
    # for (i in 1: (length(chrNames[[s]] )/ divisorL ) )  {
      for (i in seq(1, length(chrNames[[s]])  , by = divisorL ) )  {

      if (chrId=="original"){
        labels[[s]][[i]]<-attr(chrNames[[s]][[i]],"chrName")
      } else {
        labels[[s]][[i]]<- (i+ifelse(divisorL==2,1,0)) / divisorL
      }
      centerX<-mean(unlist(chrNames[[s]][[i]]$x) )
      centerY<-mean(unlist(chrNames[[s]][[i]]$y) )
      graphics::text(x=centerX,
                     y=centerY
                     ,label= labels[[s]][[i]]
                     ,cex=indexIdTextSize
                     # pos=4,
                     ,adj=0.5
                     ,col=colorRampPalette(c(chrColor, "black" ))(100)[50]
      ) # text
    } # for
  } # for
} # fun

#
#   attr chrNameB to yMark and xMark
#

addChrNameAttrMark<-function(xMark,yMark,x){
  markList<-list()
  for(i in 1:length(xMark)){
    currName<-attr(xMark[[i]],"spname")
    corrIndex<-which(names(x)%in% currName) # x5 x
    for (j in 1:length(xMark[[i]]) ) {
      for (k in 1:length( x[[corrIndex]] ) ){
        match<-which(xMark[[i]][[j]] %in% x[[corrIndex]][[k]] )
        if(length(match)>0){
          # print(k)
          attr(xMark[[i]][[j]],"chrNameB")<-k
          attr(yMark[[i]][[j]],"chrNameB")<-k
        } else {
          NA
        }
      }
    }
  } # for
  markList$xMark<-xMark
  markList$yMark<-yMark
  return(markList)
} # fun


addChrNameAttrMarkDots<-function(xMark,yMark,x){
  markList<-list()
  for(i in 1:length(xMark)){
    currName<-attr(xMark[[i]],"spname")
    corrIndex<-which(names(x)%in% currName) # x5 x
    for (j in 1:length(xMark[[i]]) ) {
      for (k in 1:length( x[[corrIndex]] ) ){
        distance<-xMark[[i]][[j]][[2]]-xMark[[i]][[j]][[1]]
        match<-which( (xMark[[i]][[j]][[1]] - distance/2 ) %in% x[[corrIndex]][[k]] )

        if(length(match)>0){
          # print(k)
          attr(xMark[[i]][[j]],"chrNameB")<-k
          attr(yMark[[i]][[j]],"chrNameB")<-k
        } else {
          NA
        }
      }
    }
  } # for
  markList$xMark<-xMark
  markList$yMark<-yMark
  return(markList)
} # fun

xMarkMap<-function(xMark,x, shrink) {
  xMarkList<-list()

  for( s in 1:length(xMark) ){
    xMarkList[[s]]<-xMark[[s]]

    corrIndex <- which( names(x) %in% attr(xMark[[s]],"spname")  )

    for ( m in 1:length(xMark[[s]] ) ){
      name <- attr(xMark[[s]][[m]],"rowIndex")

      minMark <- min(xMark[[s]][[m]], na.rm=T)
      maxMark <- max(xMark[[s]][[m]], na.rm=T)

      xMarkList[[s]][[m]] <- xMarkList[[s]][[m]] - min(unlist(x[[corrIndex]][name])  ) + ( (shrink/2) * (maxMark-minMark) )

      attr(xMarkList[[s]][[m]],"rowIndex")<-name
    }
    attr(xMarkList[[s]],"spname")<-attr(xMark[[s]],"spname")
  } # for
  return(xMarkList)
} # fun

xMarkMapLeft<-function(xMark,x) {
  xMarkList<-list()

  for( s in 1:length(xMark) ){
    xMarkList[[s]]<-xMark[[s]]

    corrIndex <- which( names(x) %in% attr(xMark[[s]],"spname")  )

    for ( m in 1:length(xMark[[s]] ) ){
      name <- attr(xMark[[s]][[m]],"rowIndex")

      xMarkList[[s]][[m]] <- xMarkList[[s]][[m]] - min(unlist(x[[corrIndex]][name])  )

      attr(xMarkList[[s]][[m]],"rowIndex")<-name
    }
    attr(xMarkList[[s]],"spname")<-attr(xMark[[s]],"spname")
  } # for
  return(xMarkList)
} # fun

markMapPer<-function(yMark,y){
  yMarkPer<-list()

  for( s in 1:length(yMark) ){
    yMarkPer[[s]]<-list()
    corrIndex <- which( names(y) %in% attr(yMark[[s]],"spname")  )

    for ( m in 1:length(yMark[[s]] ) ){
      name <- attr(yMark[[s]][[m]],"rowIndex")
      chrSize <- max(unlist(y[[corrIndex]][name] ) ) - min(unlist(y[[corrIndex]][name])  )
      distBeg <- min(yMark[[s]][[m]] ) -  (min(unlist(y[[corrIndex]][name])  ) )
      disBegPer<- distBeg/chrSize

      distEnd <- max(yMark[[s]][[m]])  -  (min(unlist(y[[corrIndex]][name])  ) )
      disEndPer<- distEnd/chrSize
      diffPer <- disEndPer - disBegPer

      diffReal<-distEnd-distBeg

      fac<-diffPer/diffReal
      fac<-ifelse(is.na(fac),0,fac)

        yMarkPer[[s]][[m]]<-  psum( ( ( yMark[[s]][[m]]  -  min(unlist(y[[corrIndex]][name] ) ) ) - distBeg ) * fac  , disBegPer, na.rm=T)

      attr(yMarkPer[[s]][[m]],"rowIndex")<-name
    }
    attr(yMarkPer[[s]],"spname")<-attr(yMark[[s]],"spname")
  } # for
  return(yMarkPer)
} # fun

markMapPerCen <- function(yMark,y){ # use y
  yMarkPer<-list()

  for( s in 1:length(yMark) ){
    yMarkPer[[s]]<-list()
    corrIndex <- which( names(y) %in% names(yMark)[s]  )
    for ( m in 1:length(yMark[[s]] ) ){
      # longArmIndex<-m*2-1
      armSize <- max(unlist(y[[corrIndex]][[m]] ) ) - min(unlist(y[[corrIndex]][[m]])  )

      distBeg <- min(yMark[[s]][[m]] ) -  (min(unlist(y[[corrIndex]][[m]])  ) )
      disBegPer <- distBeg/armSize
      # disBegPer<-1
      distEnd <- max(yMark[[s]][[m]])  -  (min(unlist(y[[corrIndex]][[m]])  ) )
      disEndPer <- distEnd/armSize
      diffPer <- disEndPer-disBegPer

      diffReal <- distEnd -  distBeg

      fac<-diffPer/diffReal
      fac<-ifelse(is.na(fac),0,fac)

      yMarkPer[[s]][[m]] <-  psum( ( ( yMark[[s]][[m]]  -  min(unlist(y[[corrIndex]][[m]] ) ) ) - distBeg ) * fac  , disBegPer , na.rm=T)
      attr(yMarkPer[[s]][[m]],"rowIndex")<-m
    }
    names(yMarkPer[[s]]) <- 1: (length(yMarkPer[[s]]) ) # was len y but 4 != 8
    names(yMarkPer)[s]<-names(y[s])
    # attr(yMarkPer[[s]],"spname") <- names(yMark[s])
    # attr(yMarkPer[[s]], "positionnoNA") <- s
  } # for s

  return(yMarkPer)
} # fun

markMapPercM<-function(yMark,y) {
  yMarkPer<-list()

  for( s in 1:length(yMark) ){
    yMarkPer[[s]]<-list()
    corrIndex <- which( names(y) %in% attr(yMark[[s]],"spname")  )

    for ( m in 1:length(yMark[[s]] ) ){
      name <- attr(yMark[[s]][[m]],"rowIndex")
      chrSize <- max(unlist(y[[corrIndex]][name] ) ) - min(unlist(y[[corrIndex]][name])  )
      distBeg <- min(yMark[[s]][[m]] ) -  (min(unlist(y[[corrIndex]][name])  ) )
      disBegPer<- distBeg/chrSize
      fac<-disBegPer/distBeg
      fac<-ifelse(is.na(fac),0,fac)
      yMarkPer[[s]][[m]]<- ( ( yMark[[s]][[m]]  -  min(unlist(y[[corrIndex]][name])  ) ) - distBeg ) * fac  + disBegPer
      attr(yMarkPer[[s]][[m]],"rowIndex")<-name
    }
    attr(yMarkPer[[s]],"spname") <- attr(yMark[[s]],"spname")
  } # for
  return(yMarkPer)
} # fun

markMapPerDots<-function(yMark,y)
{
  yMarkPer<-list()

  for( s in 1:length(yMark) ){
    yMarkPer[[s]]<-list()
    corrIndex <- which( names(y) %in% attr(yMark[[s]],"spname")  )

    for ( m in 1:length(yMark[[s]] ) ){

      name <- attr(yMark[[s]][[m]],"rowIndex")
      chrSize <- max(unlist(y[[corrIndex]][name] ) ) - min(unlist(y[[corrIndex]][name])  )
      distBeg <- min(yMark[[s]][[m]][[1]] ) -  (min(unlist(y[[corrIndex]][name])  ) )
      disBegPer<- distBeg/chrSize
      fac<-disBegPer/distBeg
      fac<-ifelse(is.na(fac),0,fac)
      yMarkPer[[s]][[m]]<-list()
      if (length(yMark[[s]][[m]]) > 1 ){
        yMarkPer[[s]][[m]][1:2]<- ( ( yMark[[s]][[m]][[1]]  -  min(unlist(y[[corrIndex]][name])  ) ) - distBeg ) * fac  + disBegPer
      } else {
        yMarkPer[[s]][[m]]<- ( ( yMark[[s]][[m]][[1]]  -  min(unlist(y[[corrIndex]][name])  ) ) - distBeg ) * fac  + disBegPer
      }
      attr(yMarkPer[[s]][[m]],"rowIndex")<-name
    }
    attr(yMarkPer[[s]],"spname") <- attr(yMark[[s]],"spname")
  } # for
  return(yMarkPer)
} # fun

radDotsPer<-function(rad,y) {
  radPer<-list()

  for( s in 1:length(rad) ){
    radPer[[s]]<-list()
    corrIndex <- which( names(y) %in% attr(rad[[s]],"spname")  )

    for ( m in 1:length(rad[[s]] ) ){
      name <- attr(rad[[s]][[m]],"rowIndex")
      chrSize <- max(unlist(y[[corrIndex]][name] ) ) - min(unlist(y[[corrIndex]][name])  )
      radSize <- rad[[s]][[m]][[1]]
      radPerC<- radSize/chrSize
      radPer[[s]][[m]]<-list()
      if (length(rad[[s]][[m]])>1){
        radPer[[s]][[m]][1:2]<- radPerC
      } else {
        radPer[[s]][[m]] <- radPerC
      }
    }
  } # for
  return(radPer)
} # fun

centerMarkMapPer<-function(yMark,y){
  yMarkPer<-list()

  for( s in 1:length(yMark) ){
    yMarkPer[[s]]<-list()
    corrIndex <- which( names(y) %in% attr(yMark[[s]],"spname")  )

    for ( m in 1:length(yMark[[s]] ) ){
      name <- attr(yMark[[s]][[m]],"rowIndex")
      chrSize <- max(unlist(y[[corrIndex]][name] ) ) - min(unlist(y[[corrIndex]][name])  )
      distBeg <- min(yMark[[s]][[m]] ) -  (min(unlist(y[[corrIndex]][name])  ) )
      disBegPer<- distBeg/chrSize
      distEnd <- max(yMark[[s]][[m]])  -  (min(unlist(y[[corrIndex]][name])  ) )
      disEndPer<- distEnd/chrSize
      diffPer<-disEndPer-disBegPer
      diffReal<-distEnd-distBeg
      fac<-diffPer/diffReal
      fac<-ifelse(is.na(fac),0,fac)
      center<-(disBegPer+disEndPer) / 2
      len<- length(yMark[[s]][[m]] )

      yMarkPer[[s]][[m]]<- rep(center,len)

      attr(yMarkPer[[s]][[m]],"rowIndex")<-name
    }
    attr(yMarkPer[[s]],"spname")<-attr(yMark[[s]],"spname")
  } # for
  return(yMarkPer)
} # fun

#
#   trans Marks
#

transyListMark<-function(yMarkPer,ylistTransChr){

ylistTransMark<-list()

  for (s in 1:length(yMarkPer) ){
    ylistTransMark[[s]]<-list()
    corrIndex <- which( names(ylistTransChr) %in% attr(yMarkPer[[s]],"spname") )

    for (m in 1: length(yMarkPer[[s]]) ) {
      name<-NULL
      corrIndexMark<-NULL
      name<-attr(yMarkPer[[s]][[m]],"rowIndex")
      corrIndexMark <- which( names(ylistTransChr[[corrIndex]]) %in% name )

      if (length(corrIndexMark)>0){
        chrSize <- max(unlist(ylistTransChr[[corrIndex]][corrIndexMark] ) ) - min(unlist(ylistTransChr[[corrIndex]][corrIndexMark])  )

        ylistTransMark[[s]][[m]] <- yMarkPer[[s]][[m]] * chrSize
        ylistTransMark[[s]][[m]] <- ylistTransMark[[s]][[m]] + min(unlist(ylistTransChr[[corrIndex]][corrIndexMark] ) )
      }
    }
    attr(ylistTransMark[[s]],"spname")<-attr(yMarkPer[[s]],"spname")
    attr(ylistTransMark[[s]],"positionnoNA")<-attr(ylistTransChr[[corrIndex]],"positionnoNA")

  } # for
  return(ylistTransMark)
}

transyListCen<-function(yMarkPer,ylistTransChr){

  ylistTransMark<-list()

  for (s in 1:length(yMarkPer) ) {
    ylistTransMark[[s]]<-list()
    # corrIndex <- which( names(ylistTransChr) %in% attr(yMarkPer[[s]],"spname") )
    corrIndex <- which( names(ylistTransChr) %in% names(yMarkPer)[s] )
    # if(length(corrIndex)>0 ) {
    for (m in 1: length(yMarkPer[[s]]) ) {
      # name<-NULL
      # corrIndexMark<-NULL
      # name<-attr(yMarkPer[[s]][[m]],"rowIndex")
      # corrIndexMark <- which( names(ylistTransChr[[corrIndex]]) %in% name )

      if (length(m)>0){
        intercaIndex<-m*2-1
        chrSize <- max(unlist(ylistTransChr[[corrIndex]][[intercaIndex]] ) ) - min(unlist(ylistTransChr[[corrIndex]][[intercaIndex]] )  )

        ylistTransMark[[s]][[m]] <- yMarkPer[[s]][[m]] * chrSize
        ylistTransMark[[s]][[m]] <- ylistTransMark[[s]][[m]] + min(unlist(ylistTransChr[[corrIndex]][[intercaIndex]] ) )
      }
    } # m
    # attr(ylistTransMark[[s]],"spname")<-attr(yMarkPer[[s]],"spname")
    attr(ylistTransMark[[s]],"positionnoNA") <- attr(ylistTransChr[[corrIndex]],"positionnoNA")
    names(ylistTransMark[[s]]) <- 1: (length(ylistTransMark[[s]]) )

  } # for s
  # }
  return(ylistTransMark)
}


# radPerCr<-radPerCr125
# yMarkPerCr<-yMarkPerCr116
# ylistTransChr<-ylistTransChr135

transRadDots<-function(radPerCr,yMarkPerCr,ylistTransChr){
  radTrans<-list()
  # s<-1
  for (s in 1:length(radPerCr) ){
    radTrans[[s]]<-list()
    corrIndex <- which( names(ylistTransChr) %in% attr(yMarkPerCr[[s]],"spname") )
    for (m in 1:length(radPerCr[[s]])) {

      name<-NULL
      name<-attr(yMarkPerCr[[s]][[m]],"rowIndex")
      corrIndexMark<-NULL
      corrIndexMark <- which( names(ylistTransChr[[corrIndex]]) %in% name )

      # m<-1
      # chrSize <- max(unlist(ylistTransChr[[1]][1] ) ) - min(unlist(ylistTransChr[[1]][1])  )

      chrSize <- max(unlist(ylistTransChr[[corrIndex]][corrIndexMark] ) ) - min(unlist(ylistTransChr[[corrIndex]][corrIndexMark])  )
#      chrSize1018 <<- chrSize

      radTrans[[s]][[m]]<-list()
      if(length(radPerCr[[s]][[m]])>1){
        radTrans[[s]][[m]][1:2] <- radPerCr[[s]][[m]][[1]] * chrSize
      } else {
        radTrans[[s]][[m]]<- radPerCr[[s]][[m]] * chrSize
      }
    }
    attr(radTrans[[s]],"spname")<-attr(yMarkPerCr[[s]],"spname")
    attr(radTrans[[s]],"positionnoNA")<-attr(ylistTransChr[[corrIndex]],"positionnoNA")
  } # for
  return(radTrans)
}


transyListMarkDots<-function(yMarkPer,ylistTransChr){

  ylistTransMark<-list()

  for (s in 1:length(yMarkPer) ){
    ylistTransMark[[s]]<-list()
    corrIndex <- which( names(ylistTransChr) %in% attr(yMarkPer[[s]],"spname") )
    for (m in 1:length(yMarkPer[[s]])) {
      name<-NULL
      name<-attr(yMarkPer[[s]][[m]],"rowIndex")
      corrIndexMark<-NULL
      corrIndexMark <- which( names(ylistTransChr[[corrIndex]]) %in% name )


      chrSize <- max(unlist(ylistTransChr[[corrIndex]][corrIndexMark] ) ) - min(unlist(ylistTransChr[[corrIndex]][corrIndexMark])  )

      ylistTransMark[[s]][[m]]<-list()

      if (length(yMarkPer[[s]][[m]])>1 ){
        ylistTransMark[[s]][[m]][1:2] <- yMarkPer[[s]][[m]][[1]] * chrSize
        ylistTransMark[[s]][[m]][1:2] <- ylistTransMark[[s]][[m]][[1]] + min(unlist(ylistTransChr[[corrIndex]][corrIndexMark] ) )
      } else {
        ylistTransMark[[s]][[m]] <- yMarkPer[[s]][[m]] * chrSize
        ylistTransMark[[s]][[m]] <- ylistTransMark[[s]][[m]] + min(unlist(ylistTransChr[[corrIndex]][corrIndexMark] ) )
      }
      attr(ylistTransMark[[s]][[m]],"rowIndex")<-attr(yMarkPer[[s]][[m]],"rowIndex")
    }
    attr(ylistTransMark[[s]],"spname")<-attr(yMarkPer[[s]],"spname")
    attr(ylistTransMark[[s]],"positionnoNA")<-attr(ylistTransChr[[corrIndex]],"positionnoNA")

  } # for
  return(ylistTransMark)
}

mapChrCenter <- function(ylistTransChr){

  listChrCenter<-list()

  for (s in 1:length(ylistTransChr) ){
    listChrCenter[[s]]<-list()
    for (c in 1:length(ylistTransChr[[s]])) {

      chrSize <- max(unlist(ylistTransChr[[s]][[c]] ) ) - min(unlist(ylistTransChr[[s]][[c]])  )

      listChrCenter[[s]][[c]] <- rep( (.5 * chrSize) + min(unlist(ylistTransChr[[s]][[c]] ) ),2 ) # was .5
      # attr(listChrCenter[[s]][[c]],"chrName") <- names(ylistTransChr[[s]])[[c]]
      attr(listChrCenter[[s]][[c]], "chrName1") <- attr(ylistTransChr[[s]][[c]], "chrName1")
    }
    attr(listChrCenter[[s]],"positionnoNA")<- attr(ylistTransChr[[s]],"positionnoNA")

  } # for
  return(listChrCenter)
}

oneDot<-function(xMarkCr){
  oneDotXList <- list()
  for (s in 1:length(xMarkCr)){
    oneDotXList[[s]]<- list()
    for (m in 1:length(xMarkCr[[s]])){
      both<-unlist(xMarkCr[[s]][[m]])
      oneDotXList[[s]][[m]]<-  sum(both)/2
      attr(oneDotXList[[s]][[m]],"rowIndex") <- attr(xMarkCr[[s]][[m]],"rowIndex")
    }
    attr(oneDotXList[[s]],"spname") <- attr(xMarkCr[[s]],"spname")
  }
  return(oneDotXList)
}

drawCenMarks <- function(circleMaps,dfMarkColorInternal,parparlistOfdfMarkPosDataCen,lwd.chr,fixCenBorder2,chrColor) {
  for (s in 1:length(circleMaps)){
    for (m in 1:length(circleMaps[[s]] ) ) {
      graphics::polygon(x=circleMaps[[s]][[m]]$x,
                        y=circleMaps[[s]][[m]]$y,
                        col = dfMarkColorInternal$markColor[match(parparlistOfdfMarkPosDataCen[[s]]$markName[[m]],
                                                                  dfMarkColorInternal$markName)] ,
                        lwd=lwd.chr,
                        border = ifelse(fixCenBorder2,
                                        chrColor,
                                        dfMarkColorInternal$markBorderColor[match(parparlistOfdfMarkPosDataCen[[s]]$markName[[m]],
                                                                                  dfMarkColorInternal$markName)] # z outside
                        ) # ifelse
      ) # polygon
    } # for
  } # for
} # fun

OTUlabelsright<-function(y, circleCenter,OTULabelSpacerx,circleCenterY,OTULabelSpacery,OTUlegendHeight,radius,
                         chrWidth,font,family,OTUTextSize,normalizeToOne,OTUplacing,OTUcentered,OTUjustif,maxPos,separFactor,labelSpacing) {
#  yLabel<<-y
  if (OTUcentered){
    labelx1 <- circleCenter + OTULabelSpacerx
  } else {
    labelx1 <-(circleCenter + radius*chrWidth*2*maxPos+maxPos*separFactor+ chrWidth*labelSpacing + OTULabelSpacerx)
  }
  labelx  <- rep(labelx1, length(y) )

  # message(crayon::green(paste0("legend right section part 3 " ) ) )

  center <- (circleCenterY + OTULabelSpacery)

  if(is.na(OTUlegendHeight)){
    OTUlegendHeight<-normalizeToOne
  } else {
    OTUlegendHeight<-OTUlegendHeight#*normalizeToOne
  }

  lHeights<- sapply( OTUlegendHeight, function(x) x + ( (0:(length(y)-1) ) *OTUlegendHeight*2 ) )

  blabely <- lHeights-min(lHeights)
  halfmaxLH <- max(blabely)/2
  labely <- blabely +  center - halfmaxLH

  OTUNamesVec<-character()
  for (s in 1:length(y)){
    if(OTUplacing=="number"){
      OTUNamesVec[s]<-paste(attr(y[[s]],"positionnoNA") , attr(y[[s]],"name")  )
    } else {
      OTUNamesVec[s]<-paste(attr(y[[s]],"name")  )
    }
  }
  graphics::text(x=labelx, # was1
                 y=labely ,
                 labels= OTUNamesVec
                 ,font=   font
                 ,family= family
                 ,cex=OTUTextSize
                 # col="black",
                 ,adj=OTUjustif # left 2 (0 is left or bottom when 2)
  ) # graphics::text # pos4 is to the right of coords
}# end of function

psum <- function(...,na.rm=FALSE) {
  rowSums(do.call(cbind,list(...)),na.rm=na.rm)
}

