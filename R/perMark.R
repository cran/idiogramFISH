#' @name perMark
#' @title FUNCTION perMark
#' @description calculates fraction (%) of chromosome for each mark
#'
#' @param dfMarkPos data.frame of marks' position
#' @param listOfdfChromSize list data.frames of chr. sizes. Require col. \code{chrSize} for all. Use \code{armRatioCI}
#'
#' @keywords percentage span mark chromosome
#' @examples
#' # Beginning with two data.frames with column OTU
#' dfOfChrSize$OTU<-1
#' dfOfMarks2$OTU<-1
#'
#' # add column chrSize if missing
#' dfOfChrSizeWithChrSize <- armRatioCI(dfOfChrSize)
#'
#' #data.frame of chr. size to list
#' listOfdfChr<-idiogramFISH:::dfToListColumn(dfOfChrSizeWithChrSize)
#'
#' perMark(dfOfMarks2,listOfdfChr)
#'
#' @return list
#' @rdname perMark
#' @importFrom stats setNames
#' @export
#'
#'

perMark<-function(dfMarkPos,listOfdfChromSize) {

bandList <- unique(dfMarkPos$markName) # 1247
perList<-list()

for (i in 1:length(listOfdfChromSize)) { # 1223
  spname <- names(listOfdfChromSize)[i]

  perList[[i]]<-setNames(data.frame(matrix(ncol = nrow( listOfdfChromSize[[i]])  , nrow = 0)),
                         listOfdfChromSize[[i]]$chrName  )

  names(perList)[i]<-names(listOfdfChromSize)[i]

  for (band in bandList){

    for (chrName in listOfdfChromSize[[i]]$chrName ) {

      chrSize <- listOfdfChromSize[[i]][which(listOfdfChromSize[[i]]$chrName %in% chrName ),]$chrSize

      perList[[i]]["chrSize",as.character(chrName)]<-chrSize

      # print(paste("chr",chrName) )
      markSize<-NA
      allMarks<-NA

      allMarks <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                            dfMarkPos$chrName %in% chrName &
                                            dfMarkPos$markName %in% band),]$markSize

      markSize <- sum(dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                                dfMarkPos$chrName %in% chrName &
                                                dfMarkPos$markName %in% band),]$markSize, na.rm=T)
      chrRegion<-NA
      # band<-"B mark"
      chrRegion<-dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                           dfMarkPos$chrName %in% chrName &
                                           dfMarkPos$markName %in% band),]$chrRegion
      if (length(markSize) ) {
        if(any(is.na(allMarks)) ){
          message(crayon::blue( "In Monocen. kar. (columns shortArmSize etc) cen. marks do not have size, see ruler" ) )

          message(crayon::blue(paste(spname,"- No data of size, for mark", band ,"chr",chrName,"region",chrRegion[is.na(allMarks)] ))
          )
        }
        perList[[i]][as.character(band),as.character(chrName)] <- markSize
      }
    }
    perList[[i]][paste0(band,"_per"),] <- perList[[i]][as.character(band),] / perList[[i]]["chrSize",]

  } # for band

} # for s
return(perList)
} # fun
