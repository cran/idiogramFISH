#' rulerTitle
#' This is an internal function that plots rulers
#'
#' It returns rulers (axis)
#'
#' @keywords internal
#'
#' @param ycoord y coords
#' @param listOfdfChromSizeCenType only d.f. of this cen. type
#' @param MbUnit intervals
#' @param ylabline font
#' @param specialyTitle modifier of pos.
#' @param yTitle pos. of ruler
#'
#' @return plot
#' @importFrom graphics mtext

rulerTitle<-function(ycoord,listOfdfChromSizeCenType,MbUnit,specialyTitle,yTitle,ylabline){
  for (i in 1:length(listOfdfChromSizeCenType)){
    if( attr(listOfdfChromSizeCenType[[i]], "ytitle" )=="Mb" ){
      graphics::mtext(MbUnit,
                      side=2,
                      line=ylabline
                      ,at= max(unlist(ycoord[[i]]),na.rm= TRUE )
      ) # MTEXT
    } # if Mb
    else if ( attr(listOfdfChromSizeCenType[[i]], "ytitle" )=="cM" ) {

      graphics::mtext(specialyTitle,
                      side=2,
                      line=ylabline
                      ,at= max(unlist(ycoord[[i]]),na.rm= TRUE )
      ) # MTEXT
    } # if sp cM
    else {
      graphics::mtext(yTitle,
                      side=2,
                      line=ylabline
                      ,at= max(unlist(ycoord[[i]]),na.rm= TRUE )
      ) # MTEXT
    } # else microm ( not mb no cM)
  } # for
}
