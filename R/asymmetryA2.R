#' FUNCTION to calculate karyotype asymmetry
#'
#' @description This function reads a dataframe  with columns:
#'   \code{shortArmSize} and \code{longArmSize} and optionally \code{OTU} and
#'   returns a list with the A2 values for the OTUs
#'
#' @description If several species present, use column "OTU".
#' \deqn{A_{2} = \frac{sCL}{xCL}}{%
#'      A2 = sCL / xCL}
#' @description (s = std dev, CL = chr. length, x = mean)
#' @description related to:
#' \deqn{CV_{CL} = A_{2} * 100}{%
#'       CVCL = A2 * 100}
#' @description(CV = coeff. var.)
#'
#'
#' @param dfChrSize name of dataframe
#'
#' @keywords dataframe size arm
#' @importFrom stats sd
#' @export
#' @examples
#' asymmetryA2(dfOfChrSize)
#' as.data.frame(asymmetryA2(bigdfOfChrSize))
#' asymmetryA2(dfChrSizeHolo)
#' as.data.frame(asymmetryA2(bigdfChrSizeHolo))
#' @seealso \code{\link{chrbasicdatamono}}
#' @seealso \code{\link{chrbasicdataHolo}}
#' @references Romero-Zarco. 1986. A New Method for Estimating Karyotype
#'   Asymmetry. Taxon Vol. 35, No. 3  pp. 526-530
#'
#' @return list

asymmetryA2<- function(dfChrSize){
  message(crayon::black("Calculating karyotype index A2\n") )
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
