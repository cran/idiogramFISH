#' rulerTitle
#' This is an internal function that plots rulers
#'
#' It returns rulers (axis)
#'
#' @keywords internal
#'
#' @param xmnoNA x coords
#' @param ymnoNA y coords
#' @param chrSpacing chr. spacing
#' @param listOfdfChromSizeCenType only d.f. of this cen. type
#' @param MbUnit intervals
#' @param xPosRulerTitle font
#' @param specialyTitle modifier of pos.
#' @param yTitle pos. of ruler
#' @param rulerTitleSize font size
#'
#' @return plot
#' @importFrom graphics text

# rulerTitle<-function(ycoord,listOfdfChromSizeCenType,MbUnit,specialyTitle,yTitle,xPosRulerTitle,rulerTitleSize){
rulerTitle<-function(xmnoNA,ymnoNA,chrSpacing,yPosRulerTitle,listOfdfChromSizeCenType
                     ,MbUnit,specialyTitle,yTitle,xPosRulerTitle,rulerTitleSize,verticalPlot){

  amount<-if(verticalPlot){length(listOfdfChromSizeCenType)} else {1}

  for (s in 1:amount ) {
    if( attr(listOfdfChromSizeCenType[[s]], "ytitle" )=="Mb" ){
      unit<-MbUnit
    } else if ( attr(listOfdfChromSizeCenType[[s]], "ytitle" )=="cM" ) {
      unit<-specialyTitle
    } else {
      unit<-yTitle
    } # else microm ( not mb no cM)
    # graphics::mtext(unit,
    #                 side=2,
    #                 line=xPosRulerTitle
    #                 ,cex=rulerTitleSize
    #                 ,at= max(unlist(ycoord[[s]]),na.rm= TRUE )
    # ) # MTEXT

    graphics::text( min(xmnoNA[[s]][,3]) - ( chrSpacing* xPosRulerTitle ) ,
                   max(ymnoNA[[s]] ) + (yPosRulerTitle/3) ,
                   labels = unit
                   ,cex=rulerTitleSize
    ) # end graphics::text
  } # for
}
