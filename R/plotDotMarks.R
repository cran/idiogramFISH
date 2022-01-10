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

plotDotMarks <- function(bannedMarkName,xMarkCr,yMarkCr, rad, radX, colCr,n,xfactor,colBorderCr,circularPlot, y, x ,
                         radius,circleCenter,circleCenterY,separFactor,
                         chrWidth,listOfdfMarkPosCr,markLabelSize,pattern,labelSpacing,useOneDot,legend,ylistTransChr,rotation,
                         labelOutwards,dotsAsOval){


  if(circularPlot==FALSE) {


    if(useOneDot){
      xMarkCr<-lapply(xMarkCr, function(x)
        lapply(x, function(u) mean(unlist(u) ) ) )

      yMarkCr<-lapply(yMarkCr, function(x)
        lapply(x, function(u) unlist(u[1]) ) )

      rad <-lapply(rad, function(x)
        lapply(x, function(u) unlist(u[1]) )
        )
      radX <-lapply(radX, function(x)
        lapply(x, function(u) unlist(u[1]) )
      )
      colCr <-lapply(colCr, function(x)
        lapply(x, function(u) unlist(u[1]) )
      )
      colBorderCr <-lapply(colBorderCr, function(x)
        lapply(x, function(u) unlist(u[1]) )
      )
    }

    if( dotsAsOval==FALSE ) {
      radX <- rad
    }

    #
    # transform this into a map function to make ovals in circ plot
    #

  lapply(1:length(xMarkCr), function(m)
    lapply(1:length(xMarkCr[[m]] ), function(u)
      mapply(function(x,y,radiusX,radiusY,z,w) {
        pts2=seq(0, 2 * pi, length.out = n*4)

        xy2 <- cbind(x + radiusX * sin(pts2)*xfactor , y + radiusY * cos(pts2) )

        graphics::polygon(xy2[,1],
                          xy2[,2],
                          col=z,
                          border = w
        ) #p
      }, # f
      x=xMarkCr[[m]][[u]],
      y=yMarkCr[[m]][[u]],
      radiusX=radX[[m]][[u]],
      radiusY=rad[[m]][[u]],
      z=colCr[[m]][[u]],
      w=colBorderCr[[m]][[u]]
      ) # mapply
    ) # lapply
  ) # la

  } else { # circ TRUE

    #
    # horizontal coordinates x to x as if all chr. in a vertical column
    #

    if(useOneDot){
      xMarkCr<-oneDot(xMarkCr)
      yMarkCr<-oneDot(yMarkCr)
      rad<-oneDot(rad)
      # radX<-oneDot(radX)
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

    radiusMap<-applyMapCircle(radius,circleCenter,circleCenterY,separFactor,radTransCr,xlistNewCr,2
                              , 0,chrWidth,
                              unlist=TRUE, mapRadius,rotation=rotation)

    circPlotDots(circleMapsMarksCr,   xfactor,radiusMap,   colCr=colCr,colBorderCr=colBorderCr, n)

if(legend=="inline"){

    circleMapsLabelsCr <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMarkCr,xlistNewCr,n,labelSpacing,chrWidth,
                                         unlist=TRUE,mapCircle,rotation=rotation )

    circLabelMark(bannedMarkName,circleMapsLabelsCr,listOfdfMarkPosCr,markLabelSize,pattern
                  ,labelOutwards,circleCenter,circleCenterY)
}

  } # circ
} # fun
