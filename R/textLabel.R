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

textLabel<-function(xMark,yMark,listOfdfChromSize,listOfdfMarkPos,specialChrSpacing,
                    chrSpacing,markLabelSize,pattern,bannedMarkName,
                    isCentromeric=FALSE,markNewLine2,mylheight2,xsize=0) {
  component<-ifelse(isCentromeric,3,2)
  for (s in 1:length(xMark) ) {
    corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPos)[[s]] )
    if(attr(listOfdfChromSize[[corr_index]],"ytitle")=="cM"){
      chrSpacing2<-specialChrSpacing
    } else {
      chrSpacing2<-chrSpacing
    }
      for ( m in 1:length(xMark[[s]] ) ) {

        if( !listOfdfMarkPos[[s]]$markName[m] %in% bannedMarkName ){

          squareSide<-attr(yMark[[s]][[m]],"squareSide")

          if(is.null(squareSide) ) {
            x = xMark[[s]][[m]][1] + chrSpacing2*.1
            # y = ( yMark[[s]][[m]][1] + yMark[[s]][[m]][component] ) /2
            y = ( max(unlist(yMark[[s]][[m]])) + min(unlist(yMark[[s]][[m]])) )/2
            z=sub(pattern,"",listOfdfMarkPos[[s]]$markName[m])
            w=tryCatch(listOfdfMarkPos[[s]]$chrRegionOrig[m], error=function(e) {NA} )
            v=0


          } else if (attr(yMark[[s]][[m]],"squareSide") =="right"){
            x = xMark[[s]][[m]][1] + chrSpacing2*.1
            # y = ( yMark[[s]][[m]][1] + yMark[[s]][[m]][component] ) /2
            y = ( max(unlist(yMark[[s]][[m]])) + min(unlist(yMark[[s]][[m]])) )/2
            z=sub(pattern,"",listOfdfMarkPos[[s]]$markName[m])
            w=tryCatch(listOfdfMarkPos[[s]]$chrRegionOrig[m], error=function(e) {NA} )
            v=0


          } else if (attr(yMark[[s]][[m]],"squareSide") =="left"){
            x = min(xMark[[s]][[m]]) - chrSpacing2*.1
            # y = ( yMark[[s]][[m]][1] + yMark[[s]][[m]][component] ) /2
            y = ( max(unlist(yMark[[s]][[m]])) + min(unlist(yMark[[s]][[m]])) )/2
            z=sub(pattern,"",listOfdfMarkPos[[s]]$markName[m])
            w=tryCatch(listOfdfMarkPos[[s]]$chrRegionOrig[m], error=function(e) {NA} )
            v=1

          } else if (attr(yMark[[s]][[m]],"squareSide") =="exProtein"){
            x = max(unlist(xMark[[s]][[m]])) + chrSpacing2*.15
            y = ( max(unlist(yMark[[s]][[m]])) + min(unlist(yMark[[s]][[m]])) )/2
            z=sub(pattern,"",listOfdfMarkPos[[s]]$markName[m])
            w=tryCatch(listOfdfMarkPos[[s]]$chrRegionOrig[m], error=function(e) {NA} )
            v=0

          } else if (attr(yMark[[s]][[m]],"squareSide") =="inProtein"){
            x = max(unlist(xMark[[s]][[m]])) + xsize + chrSpacing2*.15
            y = ( max(unlist(yMark[[s]][[m]])) + min(unlist(yMark[[s]][[m]])) )/2
            z=sub(pattern,"",listOfdfMarkPos[[s]]$markName[m])
            w=tryCatch(listOfdfMarkPos[[s]]$chrRegionOrig[m], error=function(e) {NA} )
            v=0
          }

          label=tryCatch(ifelse(any(is.na(w),is.null(w) ),z,""), error=function(e){""} )

          if(!is.na(markNewLine2) ){
          label <- gsub(markNewLine2,"\n",label)
          par(lheight=mylheight2) # closer together
          }
          graphics::text(x=x,
                         y=y,
                         label= label #ifelse(is.na(w),z,"")
                         ,cex=markLabelSize
                         ,adj=v
          )

        } # if
    } # for m
  } # for s )# l
}

textLabelCen<-function(xMark,yMark,listOfdfChromSize,listOfdfMarkPos,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName){

  for (s in 1:length(xMark) ) {

    corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPos)[[s]] )
    if(attr(listOfdfChromSize[[corr_index]],"ytitle")=="cM"){
      chrSpacing2<-specialChrSpacing
    } else {
      chrSpacing2<-chrSpacing
    }


    for ( m in 1:length(xMark[[s]] ) ) {
      if( !listOfdfMarkPos[[s]]$markName[m] %in% bannedMarkName ){

      x = max(xMark[[s]][[m]])+chrSpacing2*.1
      y = min(yMark[[s]][[m]])+  (max(yMark[[s]][[m]]) - min(yMark[[s]][[m]]) )/2
      z = sub(pattern,"",listOfdfMarkPos[[s]]$markName[m] )
      w = tryCatch(listOfdfMarkPos[[s]]$chrRegionOrig[m], error=function(e) {NA} )

        graphics::text(x=x,
                       y=y,
                       label=ifelse(any(is.na(w),is.null(w) ),z,"") #ifelse(is.na(w),z,"")
                       ,cex=markLabelSize
                       ,adj=0
        )
      } # if
    } # for m
  } # for s )# l
}

textLabelLeft<-function(xMark,yMark,listOfdfChromSize,listOfdfMarkPos,specialChrSpacing,chrSpacing,markLabelSize,pattern,
                        bannedMarkName,isCentromeric=FALSE ) {
  component<-ifelse(isCentromeric,3,2)
  for (s in 1:length(xMark) ){
    corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPos)[[s]] )

    if(attr(listOfdfChromSize[[corr_index]],"ytitle")=="cM"){
      chrSpacing2<-specialChrSpacing
    } else {
      chrSpacing2<-chrSpacing
    }
    for ( m in 1:length(xMark[[s]] ) ) {

      if( !listOfdfMarkPos[[s]]$markName[m] %in% bannedMarkName ){
      mapply(function(x,y,z) graphics::text(x=x,
                                              y=y,
                                              label=z,
                                              cex=markLabelSize,
                                              adj=1 # left
        ),
        x=  xMark[[s]][[m]][2] - chrSpacing2*.1, # left
        y=( yMark[[s]][[m]][1] + yMark[[s]][[m]][component] ) /2,
        z=sub(pattern,"",listOfdfMarkPos[[s]]$markName[m])
      ) # m
    # )# l
    } # if
    } # for m
  } # for s )# l
}
