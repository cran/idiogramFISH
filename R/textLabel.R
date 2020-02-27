#' textLabel
#' This is an internal function that plot labels of cM marks
#'
#' It returns a plot
#'
#' @keywords internal
#'
#' @param xMark x coords
#' @param yMark y coords
#' @param listOfdfChromSize radius
#' @param listOfdfMarkPos color
#' @param specialChrSpacing vertices number
#' @param chrSpacing aspect
#' @param markLabelSize size of mark label font
#' @param pattern pattern to remove from mark name
#' @param isCentromeric use TRUE for cen. marks
#'
#' @return plotted text
#' @importFrom graphics polygon text
#'

textLabel<-function(xMark,yMark,listOfdfChromSize,listOfdfMarkPos,specialChrSpacing,chrSpacing,markLabelSize,pattern,isCentromeric=FALSE){
# lapply(1:length(xMark), function(s)
  component<-ifelse(isCentromeric,3,2)
  for (s in 1:length(xMark) ){
    corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPos)[[s]] )
    if(attr(listOfdfChromSize[[corr_index]],"ytitle")=="cM"){
      chrSpacing2<-specialChrSpacing
    } else {
      chrSpacing2<-chrSpacing
    }
    lapply(1:length(xMark[[s]]), function (m)
      mapply(function(x,y,z) graphics::text(x=x,
                                            y=y,
                                            label=z,
                                            cex=markLabelSize,
                                            # pos=4,
                                            adj=0
      ),
      x=xMark[[s]][[m]][1]+chrSpacing2*.1,
      y=( yMark[[s]][[m]][1] + yMark[[s]][[m]][component] ) /2,
      z=sub(pattern,"",listOfdfMarkPos[[s]]$markName[m])
      # t
      ) #m
    )# l
  } # for )# l
}
