#' rulerPlot
#' This is an internal function that plots rulers
#'
#' It returns rulers (axis)
#'
#' @keywords internal
#'
#' @param ycoord y coords
#' @param listOfdfChromSize list of all d.f chr size
#' @param listOfdfChromSizeCenType only d.f. of this cen. type
#' @param fromZerotoMax intervals
#' @param rulerNumberSize font
#' @param rulerPosMod modifier of pos.
#' @param rulerPos pos. of ruler
#' @param ruler.tck tick size and orient.
#'
#' @return axis
#' @importFrom graphics axis
#'

rulerPlot<-function(ycoord,listOfdfChromSize,listOfdfChromSizeCenType,fromZerotoMax,rulerNumberSize,rulerPosMod,rulerPos,ruler.tck,lwd.chr){
  for (i in 1:length(ycoord) ) {
    corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfChromSizeCenType)[[i]] )
    divisor2<-as.numeric(attr(listOfdfChromSize[[corr_index]],"divisor"))
    if ( attr(listOfdfChromSizeCenType[[i]], "ytitle" )=="cM" ) {
      labels<-unlist(fromZerotoMax[[i]] )*divisor2
    } else if( attr(listOfdfChromSizeCenType[[i]], "ytitle" )=="Mb" ) {
      labels<-unlist(fromZerotoMax[[i]] )*divisor2/1e6
    } else  { # ytitle notmb
      labels<-unlist(fromZerotoMax[[i]] )*divisor2
    }

    #
    #     long tick and labels
    #

    graphics::axis(side=2, at=unlist(ycoord[[i]]),
                   labels = labels
                   ,cex.axis=rulerNumberSize
                   ,las=1
                   ,line=rulerPosMod
                   ,pos= rulerPos
                   ,tck=ruler.tck
                   ,lwd=lwd.chr
                   ,lwd.ticks = lwd.chr
    )
    # ) # l
  } # FOR
}
