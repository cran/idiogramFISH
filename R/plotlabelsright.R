# plotlabelsright
#' This is an internal function to plot labels to the right, when "aside" for its position
#'
#' It returns a graphic element with the legends
#'
#' @keywords internal
#'
#' @param x The x axis coordinates of chromosomes
#' @param y The y axis coordinates of chromosomes
#' @param markLabelSpacer distance from right chr to legend
#' @param chrWidth chr widht
#' @param dfMarkColorInternal dataframe of mark characteristics
#' @param allMarkMaxSize maximum size of marks
#' @param normalizeToOne transformation value of karyotype height
#' @param markLabelSize font size of legends
#' @param xfactor relative proportion of x vs y axes
#' @param legendWidth factor to increase width of legend squares and dots
#' @param legendHeight factor to increase height of legend squares and dots
#' @param n numeric, vertices for round parts

#' @importFrom graphics polygon text
#'
#' @return Returns a graphics element
#'

plotlabelsright<-function(x,y, markLabelSpacer,chrWidth,dfMarkColorInternal,allMarkMaxSize,normalizeToOne,
                          markLabelSize,xfactor,legendWidth,legendHeight,n) {
  maxx<-(max(unlist(x)) )

  miny<-(min(unlist(y)) )
  chrWidth<-chrWidth*legendWidth
  labelx<-(maxx+markLabelSpacer)+(c(0, chrWidth, chrWidth,0)+0)

  labelx<-t(replicate(nrow(dfMarkColorInternal),labelx) )
  if(is.na(legendHeight)){
    if(exists("allMarkMaxSize")){
      legendHeight<-allMarkMaxSize*normalizeToOne
    } else {
      legendHeight<-1*normalizeToOne
    }
  } else {
    legendHeight<-legendHeight*normalizeToOne
  }

  labely<- sapply(c(0,0,legendHeight,legendHeight), function(x) x + 0:(nrow(dfMarkColorInternal)-1)*(legendHeight*2) ) + miny

  # remove the dot ones
  labelytoplot<-labely[which(dfMarkColorInternal$style!="dots"),]
  labelxtoplot<-labelx[which(dfMarkColorInternal$style!="dots"),]

  ifelse(class(labelytoplot)=="matrix",
         labelytoplot<-base::split(labelytoplot, row(labelytoplot) ),
         labelytoplot<-list(t(as.matrix(labelytoplot) ) )
  )

  ifelse(class(labelxtoplot)=="matrix",
         labelxtoplot<-base::split(labelxtoplot, row(labelxtoplot) ),
         labelxtoplot<-list(t(as.matrix(labelxtoplot) ) )
  )
  # squares of labels
  mapply(function(x,y,z,w) graphics::polygon(x=x,
                                           y=y,
                                           col=z,
                                           lwd=.5
                                           # , border=z
                                           ,border=w
  ), # polygon
  x=labelxtoplot,
  y=labelytoplot
  ,z=dfMarkColorInternal$markColor[which(dfMarkColorInternal$style!="dots")]
  ,w=ifelse(dfMarkColorInternal$markColor[which(dfMarkColorInternal$style!="dots")]=="white",
           "black",
           dfMarkColorInternal$markColor[which(dfMarkColorInternal$style!="dots")]
           )#ifelse
  ) # mapply

  # text of labels
  graphics::text(x=t(labelx[which(dfMarkColorInternal$style!="dots"),2]), # was1
                 y=t(
                   (c(labely[which(dfMarkColorInternal$style!="dots"),1]+
                        labely[which(dfMarkColorInternal$style!="dots"),3]
                   )/2)-.01
                 ) ,
                 labels=dfMarkColorInternal$markName[which(dfMarkColorInternal$style!="dots")], cex=markLabelSize, col="black", pos=4
  ) # graphics::text # pos4 is right

  ##################
  # circular labels to the right
  ##################

  {
    labelxdiff<-(max(labelx) - min(labelx) )
    diffxQuar<-labelxdiff/4
    xcenters<- c((min(labelx)+diffxQuar),(min(labelx)+3*diffxQuar) )

    listOfxcenters<-rep(list(xcenters), nrow(dfMarkColorInternal[which(dfMarkColorInternal$style=="dots"),] ) )

    labelydiffs<-labely[which(dfMarkColorInternal$style=="dots"),3]-labely[which(dfMarkColorInternal$style=="dots"),2]
    labelydiffhalf<-labelydiffs[1]/2

    ycenters<-labely[which(dfMarkColorInternal$style=="dots"),2]+labelydiffhalf
    listOfycenters<-lapply(ycenters, function(x) rep(x,2) )

    rad<-min(labelydiffhalf, (diffxQuar) )


    yfactor<-1
    # xfactor<-(xsizeplot/ysizeplot  )/dotRoundCorr

    if(length(listOfxcenters)>0){
      lapply(1:length(listOfxcenters), function(u) {
        mapply(function(x,y,r,z,w) {
          pts2=seq(0, 2 * pi, length.out = n)
          xy2 <- cbind(x + (r * sin(pts2)*xfactor) , y + (r * cos(pts2)*yfactor ) )
          graphics::polygon(xy2[,1],
                            xy2[,2],
                            col=z,
                            border = w)
        },
        x= listOfxcenters[[u]],
        y= listOfycenters[[u]],
        r= rad,
        z= dfMarkColorInternal$markColor[which(dfMarkColorInternal$style=="dots")][[u]]
        ,w= ifelse(dfMarkColorInternal$markColor[which(dfMarkColorInternal$style=="dots")][[u]]=="white",
                         "black",
                         dfMarkColorInternal$markColor[which(dfMarkColorInternal$style=="dots")][[u]]
        ) # ifelse
        ) # mapply
      } # fun
      ) # lapply

      graphics::text(x=t(labelx[which(dfMarkColorInternal$style=="dots"),2]),
                     y=t(
                       c(labely[which(dfMarkColorInternal$style=="dots"),1]+
                           labely[which(dfMarkColorInternal$style=="dots"),3]
                       )/2-.01
                     ) ,
                     labels=dfMarkColorInternal$markName[which(dfMarkColorInternal$style=="dots")],
                     cex=markLabelSize,
                     col="black",
                     pos=4
      ) # graphics::text # pos4 is right
    }
  }# circ right
}# end of function

