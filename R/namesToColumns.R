#' FUNCTION that modifies marks' names into columns
#'
#' @description Reads a data.frame with marks' of styles 
#' \code{downArrow,upArrow,cM,cMLeft} 
#' positions. It separates names in columns, avoiding overlap when multiple 
#' close names
#' @description Exceptionally this function requires the column style in the 
#' data.frame of marks' 
#' positions.
#' @description Returns a data.frame
#'
#' @keywords data.frame marks
#'
#' @param marksDf data.frame with columns: \code{markName,style,markPos}
#' @param dfChrSize data.frame, size of chr. Same of plot.
#' @param markType character, use 
#' \code{c("downArrow","upArrow","cM","cMLeft")} or a subset
#' @param amountofSpaces numeric, number of spaces for each column
#' @param colNumber numeric, number of columns
#' @param protruding numeric, same as plot, minimal protruding for arrow 
#' marks, equivalent to cM 
#' protruding
#' @param protrudingInt numeric, spacing of columns in terms of width of chr. 
#' percent 1 = 100%. 
#' Defaults to \code{0.5}
#' @param circularPlot boolean, use \code{TRUE} for circular plots. Use 
#' \code{FALSE} otherwise
#' @param rotation numeric, same as plot, anti-clockwise rotation, defaults to 
#' \code{0.5} which 
#' rotates chr. from top to -90 degrees. (-0.5*\eqn{\pi} )
#' @param defaultStyleMark character, if some data in column style missing 
#' fill with this one. 
#' Defaults to \code{"square"}
#' @param orderBySize boolean, use same as in plot. Defaults to \code{TRUE}
#' @param halfModUp numeric, for circ. plots, when plotting several 
#' chromosomes in a circular 
#' plot, using a small value \code{0.05} corrects for alignment problems of 
#' \code{upArrows, cM} 
#' labels. Defaults to \code{NA}
#' @param halfModDown numeric, for circ. plots, when plotting several 
#' chromosomes in a circular 
#' plot, using a small value \code{0.05} corrects for alignment problems of 
#' \code{downArrows, 
#' cMLeft} labels. Defaults to \code{NA}
#' @param rotatMod numeric, for circ. plots, when rotation != 0 (diff.), 
#' corrects alignment of 
#' labels. Defaults to \code{0}
#' @export
#'
#' @return data.frame

namesToColumns <- function(marksDf, dfChrSize, markType=c("downArrow","upArrow","cMLeft","cM"),
                           amountofSpaces=13,colNumber=2,protruding=0.5,
                           protrudingInt=0.5,circularPlot=TRUE,rotation=0.5,
                           defaultStyleMark = "square",orderBySize=TRUE,
                           halfModDown=NA,halfModUp=NA, rotatMod=0
                           ) {


  # if style column does not exist stop
{
  if( "style" %in% colnames(marksDf) ) {
    if(length(marksDf[which(is.na(marksDf$style) ),]$style) >0 ) {
      marksDf[which(is.na(marksDf$style) ),]$style <- defaultStyleMark
    }
  } else {
    message(crayon::red("exceptionally, style column is mandatory, needs some downArrow, upArrow, cMLeft or cM, stopping!"))
    return(marksDf)
  }
}
if(! "chrName" %in% colnames(marksDf) ) {
      message(crayon::red("chrName column is mandatory in marks d.f., stopping!"))
      return(marksDf)
}
  ## d.f. chr size to list
{
  dfChrSize <- makeNumCols(dfChrSize)

  listOfdfChromSize <- dfToListColumn(dfChrSize)

  # add attr. monocen or holocen based on columns
  listOfdfChromSize <- addAttributesDfChrSizeSimple(listOfdfChromSize) # this makes char. cols. adds cen.

  # str(listOfdfChromSize)

  # add chrSize col. for monocen.
  listOfdfChromSize <- addChrSizeColumn(listOfdfChromSize) # now makes numeric 1st

  # define ordering of chr. length or name
  totalLength<-lapply(listOfdfChromSize, function(x) tryCatch(x$chrSize, error=function(e) NA)  )
  ifelse(
      inherits(totalLength, "matrix"),
      totalLength <- base::split(totalLength, col(totalLength) )
      ,NA
  )

  if(orderBySize==TRUE) {
    orderlist<-lapply(totalLength, function(x) order(x, decreasing = TRUE) )
  } else { # if not want to order by size, set order by name of chro
    orderlist<-lapply(listOfdfChromSize, function(x) tryCatch(order(x$chrName), error=function(e) NA ) )
  } # else

  #   add column of new chro index to data.frames and order !!
  listOfdfChromSize <- addNeworderColumn(listOfdfChromSize,orderlist)

  # markPos d.f. to List
  listOfdfMarks <- dfToListColumn(marksDf)

  # transfers neworder column to markposlist # requires column chrName
  listOfdfMarks <- newOrderColumn(listOfdfChromSize,listOfdfMarks)
  # protrudingInt<-protruding <- .5
  proVec <- seq(protruding, (protrudingInt * (colNumber-1) ) + protruding, length.out = colNumber)

  if(exists("listOfdfMarks") ) {
    listOfChecksChr<-checkNameChrDfMarks(listOfdfChromSize,listOfdfMarks)
    listOfdfChromSize<-listOfChecksChr[[1]]
    listOfdfMarks<-listOfChecksChr[[2]]

    if(length(listOfdfMarks)==0){
      remove(listOfdfMarks)
      message(crayon::red("stopping!"))
      return(marksDf)
    }
  }
}
  # begin loop by SPS (OTU)
for ( k in 1:length(listOfdfChromSize ) ) {
    spindex <- which(names(listOfdfMarks) %in% names(listOfdfChromSize)[[k]] )
    if(length(spindex)>0){
    # k<-1

    # sp index for marks
    if(circularPlot){
      if(attr(listOfdfChromSize[[k]],"cenType")=="monocen" ) {
        chrsize <- sum(as.numeric(as.character(listOfdfChromSize[[k]]$chrSize) ) ) #+
          # sum(as.numeric(as.character(listOfdfChromSize[[k]]$centromereSize) ) )
      } else if(attr(listOfdfChromSize[[k]],"cenType")=="holocen" ) {
        chrsize <- sum(as.numeric(as.character(listOfdfChromSize[[k]]$chrSize) ) )
      }

      accuChrSize<-0
      half <- chrsize/2


    }

    # split marks based on chr.
    listOfdfMarks[[spindex]] <- dfToListColumn(listOfdfMarks[[spindex]], "chrName")

    # split chrDF based on chr.
    listOfdfChromSize[[k]]   <- dfToListColumn(listOfdfChromSize[[k]], "chrName")
    # for each CHR. in dfchr
    for ( l in 1 : length(listOfdfChromSize[[k]]) ) {   # important

      # get chr index for dfMarks
      chrIndex <- which(names(listOfdfMarks[[spindex]]) %in% names(listOfdfChromSize[[k]])[[l]] )

      if(length(chrIndex)>0) {

      # if column pro does not exist, create
      if(any(!colnames(listOfdfMarks[[spindex]][[chrIndex]]) %in% "protruding")){
        listOfdfMarks[[spindex]][[chrIndex]]$protruding<-NA
      }

      # create markPos2 disregarding arm -  and sort
      if(attr(listOfdfChromSize[[k]][[l]],"cenType")=="monocen" ) {
        listOfdfMarks[[spindex]][[chrIndex]]$markPos2 <- listOfdfMarks[[spindex]][[chrIndex]]$markDistCen
        listOfdfMarks[[spindex]][[chrIndex]] <- listOfdfMarks[[spindex]][[chrIndex]][order(listOfdfMarks[[spindex]][[chrIndex]]$chrRegion,
                                                                                           listOfdfMarks[[spindex]][[chrIndex]]$markPos2),]

      } else if(attr(listOfdfChromSize[[k]][[l]],"cenType")=="holocen" ) {

        listOfdfMarks[[spindex]][[chrIndex]]$markPos2 <- listOfdfMarks[[spindex]][[chrIndex]]$markPos

        listOfdfMarks[[spindex]][[chrIndex]] <- listOfdfMarks[[spindex]][[chrIndex]][order(listOfdfMarks[[spindex]][[chrIndex]]$markPos2),]
      }

      if(circularPlot) {

        listOfdfMarks[[spindex]][[chrIndex]]$newPos <- as.numeric(NA)

        if(attr(listOfdfChromSize[[k]][[l]],"cenType")=="monocen" ) {

          las <- listOfdfChromSize[[k]][[l]]$longArmSize
          cenSize <- listOfdfChromSize[[k]][[l]]$centromereSize

          tryCatch(listOfdfMarks[[spindex]][[chrIndex]][which(listOfdfMarks[[spindex]][[chrIndex]]$chrRegion == "q"),]$markPos2<-
                     las -
                     listOfdfMarks[[spindex]][[chrIndex]][which(listOfdfMarks[[spindex]][[chrIndex]]$chrRegion == "q"),]$markPos2,
                   error = function(e) {""} )
          tryCatch(listOfdfMarks[[spindex]][[chrIndex]][which(listOfdfMarks[[spindex]][[chrIndex]]$chrRegion == "p"),]$markPos2<-
                     las + #cenSize +
                     listOfdfMarks[[spindex]][[chrIndex]][which(listOfdfMarks[[spindex]][[chrIndex]]$chrRegion == "p"),]$markPos2,
                   error = function(e) {""} )
        } # monocen

          # As downArrow marks labels are at the end, account for that
          if("downArrow" %in% markType ) {

            subsetA <- which(listOfdfMarks[[spindex]][[chrIndex]]$style %in% "downArrow")

            markSizeA <- tryCatch(listOfdfMarks[[spindex]][[chrIndex]][subsetA,]$markSize, error=function(e){0})

            tryCatch(listOfdfMarks[[spindex]][[chrIndex]][subsetA,]$markPos2 <-
                       psum(listOfdfMarks[[spindex]][[chrIndex]][subsetA,]$markPos,markSizeA,na.rm=TRUE)
                     ,error=function(e){0} )
          }

        # markPos2 might have change - sort again
        if(attr(listOfdfChromSize[[k]][[l]],"cenType")=="monocen" ) {
          listOfdfMarks[[spindex]][[chrIndex]] <- listOfdfMarks[[spindex]][[chrIndex]][order(listOfdfMarks[[spindex]][[chrIndex]]$chrRegion,
                                                                                             listOfdfMarks[[spindex]][[chrIndex]]$markPos2),]
        } else if(attr(listOfdfChromSize[[k]][[l]],"cenType")=="holocen" ) {
          listOfdfMarks[[spindex]][[chrIndex]] <- listOfdfMarks[[spindex]][[chrIndex]][order(listOfdfMarks[[spindex]][[chrIndex]]$markPos2),]
        }

      if(rotation!=0){

          halfRot <- rotation / 2
          halfRot <- halfRot + (rotatMod * halfRot)

          listOfdfMarks[[spindex]][[chrIndex]]$markPos2 <- listOfdfMarks[[spindex]][[chrIndex]]$markPos2 + accuChrSize

          st <- chrsize * halfRot

          listOfdfMarks[[spindex]][[chrIndex]][which(listOfdfMarks[[spindex]][[chrIndex]]$markPos2 > st ),]$newPos <-
            listOfdfMarks[[spindex]][[chrIndex]][which(listOfdfMarks[[spindex]][[chrIndex]]$markPos2 > st ),]$markPos2-st

          newst <- chrsize - st + accuChrSize

          listOfdfMarks[[spindex]][[chrIndex]][which(listOfdfMarks[[spindex]][[chrIndex]]$markPos2 < st ),]$newPos <-
            listOfdfMarks[[spindex]][[chrIndex]][which(listOfdfMarks[[spindex]][[chrIndex]]$markPos2 < st ),]$markPos2+newst+accuChrSize

        } else { # rotation 0
          listOfdfMarks[[spindex]][[chrIndex]]$newPos <- listOfdfMarks[[spindex]][[chrIndex]]$markPos2 + accuChrSize
        } #
      } else {# c p
        listOfdfMarks[[spindex]][[chrIndex]]$newPos <- listOfdfMarks[[spindex]][[chrIndex]]$markPos2
      }

      if("downArrow" %in% markType | "cMLeft" %in% markType ) {

        subset1 <- which(listOfdfMarks[[spindex]][[chrIndex]]$style %in% "downArrow" | listOfdfMarks[[spindex]][[chrIndex]]$style %in% "cMLeft")

        #column size
        # Separate names in columns
        if(length(subset1)>0) {

          names<-listOfdfMarks[[spindex]][[chrIndex]][subset1,]$markName
          counterCol<-1:colNumber
          i = 0
          for (j in 1:length(names)){
            i = i + 1
            if (i > length(counterCol)){
              i = 1
            }
            listOfdfMarks[[spindex]][[chrIndex]][subset1,]$markName[j] <-
              # gsub("(.*)",paste0(paste0(rep(" ",amountofSpaces * (counterCol[i]-1) ), collapse = ""),
              #                        "\\1",
              #                        paste0(rep(" ",amountofSpaces*(colNumber-counterCol[i]) ), collapse = "")),names[j]
              #      ) #gsub
            gsub("(.*)",paste0(paste0(rep(" ",amountofSpaces*(colNumber-counterCol[i]) ), collapse = "")
            ,"\\1",
            paste0(rep(" ",amountofSpaces * (counterCol[i]-1) ), collapse = "")
            ),names[j]
            )#gsub
            listOfdfMarks[[spindex]][[chrIndex]][subset1,]$protruding[j] <- proVec[i]
          }

          # # fix first half ( 0h to  6h) i.e. invert short & long
          if (circularPlot) {
            if(!is.na(halfModDown)){
              half2 <- half + (halfModDown*half)
            } else {
              half2 <- half
            }
            if(nrow(listOfdfMarks[[spindex]][[chrIndex]][subset1,] ) >= colNumber){

              toMod <- which( listOfdfMarks[[spindex]][[chrIndex]][subset1,]$newPos > half2 )
              if(length(toMod)>0){
                listOfdfMarks[[spindex]][[chrIndex]][subset1,][toMod,]$markName <-
                  sub("(\\s+)?([[:alnum:]_]+)(\\s+)?","\\3\\2\\1",listOfdfMarks[[spindex]][[chrIndex]][subset1,][toMod,]$markName)
              }
            }
          } # circ plot
        } # subset1
      }

      # add spaces before and after name for outer names (upArrow) ######################################

      if("upArrow" %in% markType | "cM" %in% markType) {
        subset2<- which(listOfdfMarks[[spindex]][[chrIndex]]$style %in% "upArrow" | listOfdfMarks[[spindex]][[chrIndex]]$style %in% "cM")

        if(length(subset2)>0) {
          # Separate names in  columns
          names<-listOfdfMarks[[spindex]][[chrIndex]][subset2,]$markName
          counterCol<-1:colNumber
          i = 0
          for (j in 1:length(names)){
            i = i + 1
            if (i > length(counterCol)){
              i = 1
            }
            listOfdfMarks[[spindex]][[chrIndex]][subset2,]$markName[j] <-
              gsub("(.*)",paste0(paste0(rep(" ",amountofSpaces * (counterCol[i]-1) ), collapse = ""),
                                 "\\1",
                                 paste0(rep(" ",amountofSpaces*(colNumber-counterCol[i]) ), collapse = "")),names[j]
              )#gsub
            listOfdfMarks[[spindex]][[chrIndex]][subset2,]$protruding[j] <- proVec[i]
          }

          # fix first half ( 0h to  6h) i.e. invert short & long
          if (circularPlot) {
            if(!is.na(halfModUp)){
              half2 <- half + (halfModUp*half)
            } else {
              half2 <- half
            }
            if(nrow(listOfdfMarks[[spindex]][[chrIndex]][subset2,] ) >= colNumber){
                toMod <- which(listOfdfMarks[[spindex]][[chrIndex]][subset2,]$newPos > half2 )
                if(length(toMod)>0){
#                  ini<<-listOfdfMarks[[spindex]][[chrIndex]][subset2,][toMod,]
                  listOfdfMarks[[spindex]][[chrIndex]][subset2,][toMod,]$markName <-
                    sub("(\\s+)?([[:alnum:]_]+)(\\s+)?","\\3\\2\\1",listOfdfMarks[[spindex]][[chrIndex]][subset2,][toMod,]$markName)
#                  fini<<-listOfdfMarks[[spindex]][[chrIndex]][subset2,][toMod,]
                }
            }
            # sub("(\\s+)?([[:alnum:]_]+)(\\s+)?","\\3\\2\\1","ini   ")
          } # circ plot
        } # subset2
      }

  # if(exists("toRemoveMPMono")){
  #       if( toRemoveMPMono ) {
  #         listOfdfMarks[[spindex]][[chrIndex]]$markPos2 <- NULL
  #       }
  # }

  } # chrindex
      if(circularPlot){
        # as.numeric(as.character(dfChrSize[which(dfChrSize$OTU %in% OTU & dfChrSize$chrName %in% chrN ),]$chrSize ) )
        accuChrSize <- accuChrSize + as.numeric(listOfdfChromSize[[k]][[l]]$chrSize) #+ as.numeric(listOfdfChromSize[[k]][[l]]$centromereSize)
        # print(accuChrSize)
      }
} # for chr. l

    listOfdfMarks[[k]] <- dplyr::bind_rows(listOfdfMarks[[k]], .id = NULL)
} # spindex
} # for species k

  if("OTU" %in% colnames(marksDf)){
    marksDf <- dplyr::bind_rows(listOfdfMarks, .id = NULL)
  } else {
    marksDf <- dplyr::bind_rows(listOfdfMarks, .id = "OTU")
  }
  marksDf$newPos<-NULL
  marksDf$markPos2<-NULL
  marksDf$neworder<-NULL

  # if style col
  return(marksDf)
}


