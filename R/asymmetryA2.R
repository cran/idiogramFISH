#' Function to calculate karyotype asymmetry
#'
#' @description This function reads a dataframe  with columns:
#'   \code{shortArmSize} and \code{longArmSize} and optionally \code{OTU} and
#'   returns a list with the A2 values for the OTUs
#'
#' @description If several species present, use column "OTU".
#'
#' @param dfChrSize name of dataframe
#'
#' @keywords dataframe size arm
#' @importFrom stats sd
#' @export
#' @examples
#' asymmetryA2(dfOfChrSize)
#' asymmetryA2(dfChrSizeHolo)
#' @references Romero-Zarco. 1986. A New Method for Estimating Karyotype
#'   Asymmetry. Taxon Vol. 35, No. 3  pp. 526-530
#'
#' @return list

asymmetryA2<- function(dfChrSize){
  message(crayon::black("\nCalculating karyotype index A2\n") )
  dfChrSize<-as.data.frame(dfChrSize)
  if(!"chrSize" %in% colnames(dfChrSize) ){
    dfChrSize$chrSize<-dfChrSize$shortArmSize+dfChrSize$longArmSize
  }
  if("OTU" %in% colnames(dfChrSize)){
    listOfdfChromSize<-base::split(dfChrSize, dfChrSize$OTU )
    names(listOfdfChromSize)<-unique(dfChrSize$OTU)
  } else {
    listOfdfChromSize<-list(dfChrSize)
    names(listOfdfChromSize)<-1
  }
  asymmetry<-list()
  stDevForSps<-sapply(listOfdfChromSize, function(x) sd(x$chrSize))
  meanForSps<-sapply(listOfdfChromSize, function(x) mean(x$chrSize))
  asymmetry$A2<-format(round(stDevForSps / meanForSps,2),nsmall=2 )
  return(asymmetry)
}
