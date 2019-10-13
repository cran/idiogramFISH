#' FUNCTION to calculate Arm Ratio and C.I. and morphological categories
#'
#' This function reads a dataframe and produces AR (r), CI, Guerra and Levan
#' classifications
#'
#' @param dfOfChrSize name of dataframe with columns: shortArmSize, longArmSize
#'
#' @keywords dataframe size arm
#' @export
#' @examples
#' armRatioCI(dfOfChrSize)
#' armRatioCI(bigdfOfChrSize)
#' @seealso \code{\link{chrbasicdatamono}}
#' @references LEVAN A, FREDGA K, SANDBERG AA (1964) NOMENCLATURE FOR
#' CENTROMERIC POSITION ON CHROMOSOMES. Hereditas 52:201–220.
#' @references Guerra. 1986. Reviewing the chromosome nomenclature of Levan et
#' al. Braz. Jour. Gen. Vol IX, 4, 741-743
#'
#' @return data.frame

armRatioCI<- function(dfOfChrSize){
  # message(crayon::black("\nCalculating chromosome indexes\n") )
  dfOfChrSize$smallest<-pmin(dfOfChrSize$shortArmSize, dfOfChrSize$longArmSize)
  if(!identical(dfOfChrSize$smallest,dfOfChrSize$shortArmSize) ){
    message(crayon::red("\nERROR in short/long arm classif., It will not be fixed\nChr. (cen) indexes will not be calculated\n") )
    if("OTU" %in% colnames(dfOfChrSize)){message(crayon::red(paste("in",unique(dfOfChrSize$OTU) ))) }
    return(dfOfChrSize)
  }
  if("OTU" %in% colnames(dfOfChrSize)){message(crayon::black(paste("\nCalculating chromosome indexes in",unique(dfOfChrSize$OTU) ))) }

  dfOfChrSize$largest <-pmax(dfOfChrSize$shortArmSize, dfOfChrSize$longArmSize)
  dfOfChrSize$AR<-format(round(dfOfChrSize$largest/dfOfChrSize$smallest,1),nsmall = 1)
  dfOfChrSize$CI<-format(round(dfOfChrSize$smallest*100/(dfOfChrSize$longArmSize+dfOfChrSize$shortArmSize),1),nsmall = 1)
  dfOfChrSize$ARnum<-as.numeric(dfOfChrSize$AR)
  dfOfChrSize$CInum<-as.numeric(dfOfChrSize$CI)
  dfOfChrSize$Guerra<-ifelse(dfOfChrSize$ARnum<=1.49999999999,"M",
                             ifelse(dfOfChrSize$ARnum>=1.5 & dfOfChrSize$ARnum<=2.999999999, "SM",
                                    ifelse(dfOfChrSize$ARnum>=3, "A","" )
                                   )
                            )
  dfOfChrSize$Levan<-ifelse(dfOfChrSize$ARnum==1,"M",
                            ifelse(dfOfChrSize$ARnum>1 & dfOfChrSize$ARnum<1.7,"m",
                              ifelse(dfOfChrSize$ARnum==1.7, "m-sm",
                                  ifelse(dfOfChrSize$ARnum>1.7 & dfOfChrSize$ARnum<3, "sm",
                                         ifelse(dfOfChrSize$ARnum==3,"sm-st",
                                                ifelse(dfOfChrSize$ARnum>3 & dfOfChrSize$ARnum<7,"st",
                                                       ifelse(dfOfChrSize$ARnum==7,"st-t",
                                                              ifelse(dfOfChrSize$ARnum>7,"t","")
                                                       )
                                                )
                                         )
                                  )
                            )
                      )
  )
  return(dfOfChrSize)
}
