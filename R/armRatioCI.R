#' @name armRatioCI
#' @aliases swapChrRegionDfSizeAndMarks
#' @title FUNCTIONS armRatioCI and swapChrRegionDfSizeAndMarks.
#' @description armRatioCI: reads a data.frame and produces AR (arm ratio), CI
#' (centromeric index) , Guerra and Levan classifications.
#' @description swapChrRegionDfSizeAndMarks: reads data.frames to swap arms
#'
#' @param dfChrSize name of data.frame with columns: shortArmSize, longArmSize
#' @param dfMarkPos name of data.frame of marks
#' @param rnumeric boolean, returns only numeric AR, CI
#' @param chrNamesToSwap name of chr. names to swap arms
#'
#' @keywords data.frame size arm
#' @examples
#' armRatioCI(dfOfChrSize)
#' armRatioCI(bigdfOfChrSize)
#'
#' @seealso \code{\link{chrbasicdatamono}}
#' @references LEVAN A, FREDGA K, SANDBERG AA (1964) NOMENCLATURE FOR
#' CENTROMERIC POSITION ON CHROMOSOMES. Hereditas 52:201–220.
#' @references Guerra. 1986. Reviewing the chromosome nomenclature of Levan et
#' al. Braz. Jour. Gen. Vol IX, 4, 741-743
#'
#' @return data.frame (armRatioCI)
#' @rdname armRatioCI
#' @export
#'
armRatioCI <- function(dfChrSize, rnumeric=FALSE){
  dfChrSize<-as.data.frame(dfChrSize)
  # message("\nCalculating chromosome indexes\n")
  if(!"shortArmSize" %in% colnames(dfChrSize)) {
    message("\nSorry, shortArmSize column, mandatory\n")
    return(data.frame())
  }
  if(!"longArmSize" %in% colnames(dfChrSize)) {
    message("\nSorry, longArmSize column, mandatory\n")
    return(data.frame())
  }

  dfChrSize$smallest<-pmin(dfChrSize$shortArmSize, dfChrSize$longArmSize)
  dfChrSize$largest <-pmax(dfChrSize$shortArmSize, dfChrSize$longArmSize)
  dfChrSize$chrSize <-dfChrSize$smallest+dfChrSize$largest
  dfChrSize<-dfChrSize[which(!is.na(dfChrSize$chrSize)),]

  if(!identical(dfChrSize$smallest,dfChrSize$shortArmSize) ){
    dfChrSize$diffSmallShort <- dfChrSize$shortArmSize-dfChrSize$smallest
    message(crayon::red("\nERROR in short/long arm classif. It will not be fixed\nChr. (cen) indexes will not be calculated") )
    attr(dfChrSize, "indexStatus")<-"failure"
    return(dfChrSize)
  }

  if("OTU" %in% colnames(dfChrSize)){message(paste("\nCalculating chromosome indexes in",unique(dfChrSize$OTU) )
                                             ) }

  dfChrSize$AR<-format(round(dfChrSize$largest/dfChrSize$smallest,1),nsmall = 1)
  dfChrSize$CI<-format(round(dfChrSize$smallest*100/(dfChrSize$longArmSize+dfChrSize$shortArmSize),1),nsmall = 1)
  dfChrSize$ARnum<-as.numeric(dfChrSize$AR)
  dfChrSize$CInum<-as.numeric(dfChrSize$CI)

  dfChrSize$Guerra<-ifelse(dfChrSize$ARnum<=1.49999999999,"M",
                             ifelse(dfChrSize$ARnum>=1.5 & dfChrSize$ARnum<=2.999999999, "SM",
                                    ifelse(dfChrSize$ARnum>=3, "A","" )
                                   )
                            )
  dfChrSize$Levan<-ifelse(dfChrSize$ARnum==1,"M",
                            ifelse(dfChrSize$ARnum>1 & dfChrSize$ARnum<1.7,"m",
                              ifelse(dfChrSize$ARnum==1.7, "m-sm",
                                  ifelse(dfChrSize$ARnum>1.7 & dfChrSize$ARnum<3, "sm",
                                         ifelse(dfChrSize$ARnum==3,"sm-st",
                                                ifelse(dfChrSize$ARnum>3 & dfChrSize$ARnum<7,"st",
                                                       ifelse(dfChrSize$ARnum==7,"st-t",
                                                              ifelse(dfChrSize$ARnum>7,"t","")
                                                       )
                                                )
                                         )
                                  )
                            )
                      )
  )

  if(rnumeric){
    dfChrSize$CI<-dfChrSize$CInum
    dfChrSize$CInum<-NULL
    dfChrSize$AR<-dfChrSize$ARnum
    dfChrSize$ARnum<-NULL
  }
  attr(dfChrSize, "indexStatus")<-"success"
  return(dfChrSize)
}
#'
#' @rdname armRatioCI
#' @return list of data.frames (swapChrRegionDfSizeAndMarks)
#' @examples
#' swapChrRegionDfSizeAndMarks(dfOfChrSize,dfOfMarks,"1")
#' @export
swapChrRegionDfSizeAndMarks<- function(dfChrSize,dfMarkPos,chrNamesToSwap){
  dfChrSize$chrName<-as.character(dfChrSize$chrName)
  dfMarkPos$chrName<-as.character(dfMarkPos$chrName)

  for (c in chrNamesToSwap){
    index<-NULL
    markIndex<-NULL
    index<-which(dfChrSize$chrName %in% c )
    if(length(index)>0){
      dfChrSize$shortArmSize2<-dfChrSize$longArmSize
      dfChrSize[index,]$longArmSize<-dfChrSize[index,]$shortArmSize
      dfChrSize[index,]$shortArmSize<-dfChrSize[index,]$shortArmSize2
      dfChrSize$shortArmSize2<-NULL
    }
    if(!missing(dfMarkPos)){
      markIndex<-which(dfMarkPos$chrName %in% c)
      if(length(markIndex)>0){
        dfMarkPos$chrRegion2 <- dfMarkPos$chrRegion
        dfMarkPos[markIndex,]$chrRegion2[dfMarkPos[markIndex,]$chrRegion %in% "p"]<-"q"
        dfMarkPos[markIndex,]$chrRegion2[dfMarkPos[markIndex,]$chrRegion %in% "q"]<-"p"
        dfMarkPos[markIndex,]$chrRegion <- dfMarkPos[markIndex,]$chrRegion2
        dfMarkPos$chrRegion2<-NULL
      }
    }
  }
  mylist<-list(dfChrSize,dfMarkPos)
  names(mylist)<-c("dfChrSize","dfMarkPos")
  return(mylist )
}


