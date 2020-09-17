# plotlabelsright
#' This is an internal function to plot labels to the right, when "aside" for
#' its position
#'
#' It returns a graphic element with the legends
#'
#' @keywords internal
#'
#' @param x The x axis coordinates of chromosomes
#' @param y The y axis coordinates of chromosomes
#' @param markLabelSpacer distance from right chr to legend
#' @param chrWidth chr widht
#' @param dfMarkColorInternal data.frame of mark characteristics
#' @param allMarkMaxSize maximum size of marks
#' @param normalizeToOne transformation value of karyotype height
#' @param markLabelSize font size of legends
#' @param xfactor relative proportion of x vs y axes
#' @param legendWidth factor to increase width of legend squares and dots
#' @param legendHeight factor to increase height of legend squares and dots
#' @param n numeric, vertices for round parts
#' @param pattern, character, regex to remove from markNames
#' @param legendYcoord numeric modify Y position of legend
#' @param useOneDot boolean, when \code{TRUE} plots only one dot

#' @importFrom graphics polygon text
#'
#' @return Returns a graphics element
#'

plotlabelsright<-function(maxx,y, markLabelSpacer,chrWidth,dfMarkColorInternal,allMarkMaxSize,normalizeToOne,
                          markLabelSize,xfactor,legendWidth,legendHeight,n,pattern,legendYcoord,useOneDot,dotsAsOval,circularPlot) {


  miny<-(min(unlist(y)) )

  chrWidth<-chrWidth*legendWidth

  labelx<-(maxx+markLabelSpacer)+(c(0, chrWidth, chrWidth,0)+0)

  labelx<-t(replicate(nrow(dfMarkColorInternal),labelx) )

  if(is.na(legendHeight)){
    if(exists("allMarkMaxSize")){
#      allMarkMaxSize43<<-allMarkMaxSize
#      normalizeToOne44<<-normalizeToOne
      legendHeight<-allMarkMaxSize*normalizeToOne
    } else {
      legendHeight<-1*normalizeToOne
    }
  } else {
    legendHeight<-legendHeight*normalizeToOne
  }
  # message(crayon::green(paste0("legend right section part 3 " ) ) )
#  legendHeight51<<-legendHeight

  labely<- sapply( c(0,0,legendHeight,legendHeight), function(x) x + ( (0:(nrow(dfMarkColorInternal)-1) ) * (legendHeight*2) )
                   ) + miny + legendYcoord


  #
  #   labelx and y to matrix
  #

  if(!inherits(labely,"matrix") ) {
    labely<-t(as.matrix(labely) )
  }

  if(!inherits(labelx,"matrix") ) {
    labelx<-t(as.matrix(labelx) )
  }
  #
  # remove dots
  #
  labelytoplot<-labely[which(dfMarkColorInternal$style!="dots"),]
  labelxtoplot<-labelx[which(dfMarkColorInternal$style!="dots"),]

  #
  #   labelxplot and y to matrix
  #

  ifelse(inherits(labelytoplot,"matrix"),
         labelytoplot<-base::split(labelytoplot, row(labelytoplot) ),
         labelytoplot<-list(t(as.matrix(labelytoplot) ) )
  )

  ifelse(
         inherits(labelxtoplot,"matrix"),
         labelxtoplot<-base::split(labelxtoplot, row(labelxtoplot) ),
         labelxtoplot<-list(t(as.matrix(labelxtoplot) ) )
  )
  # squares labels

  if(length(dfMarkColorInternal$markName[which(dfMarkColorInternal$style!="dots")] ) > 0 ) {

    marks <- dfMarkColorInternal$markColor[which(dfMarkColorInternal$style!="dots")]

    borders <-  dfMarkColorInternal$markBorderColor[which(dfMarkColorInternal$style!="dots") ]

  graphics::text(x=t(labelx[which(dfMarkColorInternal$style!="dots"),2]), # was1
                 y=t(
                   (c(labely[which(dfMarkColorInternal$style!="dots"),1]+
                        labely[which(dfMarkColorInternal$style!="dots"),3]
                   )/2)-.01
                 ) ,
                 labels= sub(pattern,"",dfMarkColorInternal$markName[which(dfMarkColorInternal$style!="dots")]),
                 cex=markLabelSize,
                 col="black",
                 pos=4
  ) # graphics::text # pos4 is right

  mapply(function(x,y,z,w) graphics::polygon(x=x,
                                           y=y,
                                           col=z,
                                           lwd=rep(.5,length(marks) )
                                           ,border=w
  ), # polygon
  x = labelxtoplot,
  y = labelytoplot
  ,z = marks
  ,w = borders
  ) # mapply

  } # if len


  ##########################################
  # circular labels to the right DOTS
  ##########################################

  {
    labelxdiff <- (max(labelx) - min(labelx) )


    if(useOneDot==FALSE){
      diffxQuar<-labelxdiff/4
      xcenters<- c( ( min(labelx) + diffxQuar), (min(labelx)+3*diffxQuar) )
    } else {
      diffxHalf<-labelxdiff/2
      xcenters<- ( min(labelx) + diffxHalf)
    }
    listOfxcenters <- rep(list(xcenters), nrow(dfMarkColorInternal[which(dfMarkColorInternal$style=="dots"),] ) )

    labelydiffs<-labely[which(dfMarkColorInternal$style=="dots"),3]-labely[which(dfMarkColorInternal$style=="dots"),2]
    labelydiffhalf<-labelydiffs[1]/2

    ycenters<-labely[which(dfMarkColorInternal$style=="dots"),2]+labelydiffhalf

    if(useOneDot==FALSE){
      listOfycenters<-lapply(ycenters, function(x) rep(x,2) )
    } else {
      listOfycenters<-lapply(ycenters, function(x) rep(x,1) )
    }

    rad<-labelydiffhalf
    radX <- labelxdiff/2

    if(circularPlot | dotsAsOval==FALSE){
      radX<-rad
    }

    yfactor<-1

    if(length(listOfxcenters)>0){
      lapply(1:length(listOfxcenters), function(u) {
        mapply(function(x,y,radiusX,radius,z,w) {
          pts2=seq(0, 2 * pi, length.out = n)
          xy2 <- cbind(x + (radiusX * sin(pts2)*xfactor) , y + (radius * cos(pts2)*yfactor ) )
          graphics::polygon(xy2[,1],
                            xy2[,2],
                            col=z,
                            border = w)
        },
        x= listOfxcenters[[u]],
        y= listOfycenters[[u]],
        radiusX= radX,
        radius= rad,

        z= dfMarkColorInternal$markColor[which(dfMarkColorInternal$style=="dots")][[u]]
        , w=  dfMarkColorInternal$markBorderColor[which(dfMarkColorInternal$style=="dots")][[u]]
        ) # mapply
      } # fun
      ) # lapply
      graphics::text(x=t(labelx[which(dfMarkColorInternal$style=="dots"),2]),
                     y=t(
                       c(labely[which(dfMarkColorInternal$style=="dots"),1]+
                           labely[which(dfMarkColorInternal$style=="dots"),3]
                       )/2-.01
                     ) ,
                     labels=sub(pattern,"",dfMarkColorInternal$markName[which(dfMarkColorInternal$style=="dots")]),
                     cex=markLabelSize,
                     col="black",
                     pos=4
      ) # graphics::text # pos4 is right
    } # len xcenters
  }# circ right
}# end of function

