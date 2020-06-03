#' plotDotMarks
#' This is an internal function that plot dot marks
#'
#' It returns a plot
#'
#' @keywords internal
#'
#' @param xMarkCr x coords
#' @param yMarkCr y coords
#' @param rad radius
#' @param colCr color
#' @param n vertices number
#' @param xfactor aspect
#' @param colBorderCr border color
#' @param circularPlot boolean
#' @param y y of chr.
#' @param x x of chr.
#' @param radius numeric
#' @param circleCenter numeric
#' @param circleCenterY numeric
#' @param separFactor numeric
#' @param chrWidth numeric
#' @param listOfdfMarkPosCr list of d.f.
#' @param markLabelSize numeric
#' @param pattern numeric
#' @param labelSpacing numeric
#' @param useOneDot boolean
#' @param legend character
#' @param ylistTransChr list of trans. coords
#' @param rotation rotation
#' @param labelOutwards srt
#'
#' @return plot
#' @importFrom graphics polygon text
#'

plotDotMarks <- function(bannedMarkName,xMarkCr,yMarkCr, rad, colCr,n,xfactor,colBorderCr,circularPlot, y, x ,
                         radius,circleCenter,circleCenterY,separFactor,
                         chrWidth,listOfdfMarkPosCr,markLabelSize,pattern,labelSpacing,useOneDot,legend,ylistTransChr,rotation,
                         labelOutwards){

#  # xMarkCrInternal<<-xMarkCr
#  # yMarkCrInternal<<-yMarkCr
#  # radInternal<<-rad
#  # colCrInternal<<-colCr
#  # nInternal<<-n
#  # xfactorInternal<<-xfactor
#  # colBorderCrInternal<<-colBorderCr

  if(circularPlot==FALSE){

  lapply(1:length(xMarkCr), function(m)
    lapply(1:length(xMarkCr[[m]] ), function(u)
      mapply(function(x,y,radius,z,w) {
        pts2=seq(0, 2 * pi, length.out = n*4)
        xy2 <- cbind(x + radius * sin(pts2)*xfactor , y + radius * cos(pts2) )

        graphics::polygon(xy2[,1],
                          xy2[,2],
                          col=z,
                          border = w
        ) #p
      }, # f
      x=xMarkCr[[m]][[u]],
      y=yMarkCr[[m]][[u]],
      radius=rad[[m]][[u]],
      z=colCr[[m]][[u]],
      w=colBorderCr[[m]][[u]]
      ) # mapply
    ) # lapply
  ) # la

  } else { # circ TRUE

    #
    # horizontal coordinates x to x as if all chr. in a vertical column
    #

    # xlistNewCr<-xHortoVerDots(xMarkCr)
    if(useOneDot){
      xMarkCr<-oneDot(xMarkCr)
      yMarkCr<-oneDot(yMarkCr)
      rad<-oneDot(rad)
    }

    xlistNewCr<-xHortoVerDots(xMarkCr,x)

    #
    # transform position of mark to percentage of chr.
    #

    yMarkPerCr<-markMapPerDots(yMarkCr,y)

    #
    #   radius size as percentage of chr.
    #

    radPerCr<-radDotsPer(rad,y)

    #
    # chr positions
    #

    #
    #  transform to the coord of circle 0 to 1
    #

    ylistTransMarkCr<-transyListMarkDots(yMarkPerCr,ylistTransChr)

    radTransCr <- transRadDots(radPerCr,yMarkPerCr,ylistTransChr)

    circleMapsMarksCr<- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMarkCr,
                                       xlistNewCr,2,0,chrWidth,unlist=TRUE, mapCircle,rotation=rotation)

    radiusMap<-applyMapCircle(radius,circleCenter,circleCenterY,separFactor,radTransCr,xlistNewCr,2,    0,            chrWidth,
                              unlist=TRUE, mapRadius,rotation=rotation)

    circPlotDots(circleMapsMarksCr,   xfactor,radiusMap,   colCr=colCr,colBorderCr=colBorderCr, n)

if(legend=="inline"){
    circleMapsLabelsCr <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMarkCr,xlistNewCr,n,labelSpacing,chrWidth,
                                         unlist=TRUE,mapCircle,rotation=rotation )
    circLabelMark(bannedMarkName,circleMapsLabelsCr,listOfdfMarkPosCr,markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY)
}

  } # circ
} # fun
