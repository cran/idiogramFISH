#' Function to calculate Arm Ratio and C.I. and categorical morphologies
#'
#' This function reads a dataframe and produces AR (r), CI, Guerra and Levan
#' classifications
#'
#' @param dfOfChrSize name of dataframe with columns: shortArmSize, longArmSize
#'
#' @keywords dataframe size arm
#' @export
#' @examples
#' data(dfOfChrSize)
#' armRatioCI(dfOfChrSize)
#' @references LEVAN A, FREDGA K, SANDBERG AA (1964) NOMENCLATURE FOR
#' CENTROMERIC POSITION ON CHROMOSOMES. Hereditas 52:201–220.
#' @references Guerra. 1986. Reviewing the chromosome nomenclature of Levan et
#' al. Braz. Jour. Gen. Vol IX, 4, 741-743
#'
#' @return data.frame

armRatioCI<- function(dfOfChrSize){
  message(crayon::black("\nCalculating chromosome indexes\n") )
  dfOfChrSize$smallest<-pmin(dfOfChrSize$shortArmSize, dfOfChrSize$longArmSize)
  if(!identical(dfOfChrSize$smallest,dfOfChrSize$shortArmSize) ){
    message(crayon::red("\nERROR in short/long arm classif., It will not be fixed\nWill not calculate cen. ind.\n") )
    return(dfOfChrSize)
  }
  dfOfChrSize$largest <-pmax(dfOfChrSize$shortArmSize, dfOfChrSize$longArmSize)
  dfOfChrSize$AR<-format(round(dfOfChrSize$largest/dfOfChrSize$smallest,1),nsmall = 1)
  dfOfChrSize$CI<-format(round(dfOfChrSize$smallest*100/(dfOfChrSize$longArmSize+dfOfChrSize$shortArmSize),1),nsmall = 1)
  dfOfChrSize$Guerra<-ifelse(dfOfChrSize$AR<=1.49999999999,"M",
                             ifelse(dfOfChrSize$AR>=1.5 & dfOfChrSize$AR<=2.999999999, "SM",
                                    ifelse(dfOfChrSize$AR>=3, "A","" )
                                   )
                            )
  dfOfChrSize$Levan<-ifelse(dfOfChrSize$AR==1,"M",
                            ifelse(dfOfChrSize$AR>1 & dfOfChrSize$AR<1.7,"m",
                              ifelse(dfOfChrSize$AR==1.7, "m-sm",
                                  ifelse(dfOfChrSize$AR>1.7 & dfOfChrSize$AR<3, "sm",
                                         ifelse(dfOfChrSize$AR==3,"sm-st",
                                                ifelse(dfOfChrSize$AR>3 & dfOfChrSize$AR<7,"st",
                                                       ifelse(dfOfChrSize$AR==7,"st-t",
                                                              ifelse(dfOfChrSize$AR>7,"t","")
                                                       )
                                                )
                                         )
                                  )
                            )
                      )
  )
  return(dfOfChrSize)
}
