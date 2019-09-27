#' Function to calculate karyotype asymmetry A and A2
#'
#' @description This function reads a dataframe  with columns:
#' \code{shortArmSize} and \code{longArmSize}
#' @description If several species present, use
#' column \code{OTU}.
#' @description It returns a list with the A and A2 indices
#'
#' @description A2 = sCL / xCL (s = std dev, CL= chr. length, x= mean)
#' @description related to: CVCL= A2 *100 (CV = coeff. var.)
#'
#' @param dfChrSize name of dataframe
#'
#' @keywords dataframe size arm
#' @export
#' @examples
#' data(dfOfChrSize)
#' asymmetry(dfOfChrSize)
#' @description A: Watanabe et al. (1999) asymmetry of karyotype ranging from 0
#' (symmetric) to 1 (asymmetric)
#'
#' @references Watanabe K, Yahara T, Denda T, Kosuge K. (1999) Chromosomal
#' evolution in the genus Brachyscome (Asteraceae, Astereae): Statistical tests
#' regarding correlation between changes in karyotype and habit using
#' phylogenetic information. Journal of Plant Research 112: 145-161.
#' 10.1007/PL00013869
#' @references A2: Romero-Zarco. 1986. A New Method for Estimating Karyotype
#' Asymmetry. Taxon Vol. 35, No. 3  pp. 526-530
#' @return list

asymmetry<- function(dfChrSize){
  message(crayon::black("\nCalculating karyotype indexes\n") )
  dfChrSize<-as.data.frame(dfChrSize)
  if("OTU" %in% colnames(dfChrSize)){
    listOfdfChromSize<-base::split(dfChrSize, dfChrSize$OTU )
    names(listOfdfChromSize)<-unique(dfChrSize$OTU)
  } else {
    listOfdfChromSize<-list(dfChrSize)
    names(listOfdfChromSize)<-1
  }
  for (s in 1: length(listOfdfChromSize)){
    listOfdfChromSize[[s]]$smallest<-pmin(listOfdfChromSize[[s]]$shortArmSize,listOfdfChromSize[[s]]$longArmSize)
    listOfdfChromSize[[s]]$largest <-pmax(listOfdfChromSize[[s]]$shortArmSize,listOfdfChromSize[[s]]$longArmSize)
    listOfdfChromSize[[s]]$length <-listOfdfChromSize[[s]]$shortArmSize+listOfdfChromSize[[s]]$longArmSize
    listOfdfChromSize[[s]]$Aeach<- mapply(function(X,Y) (X-Y)/(X+Y), X=listOfdfChromSize[[s]]$largest, Y=listOfdfChromSize[[s]]$smallest)
    if(!identical(listOfdfChromSize[[s]]$smallest,listOfdfChromSize[[s]]$shortArmSize) ){
        message(crayon::red("\nERROR in short/long arm classif., It will not be fixed\nWill not calculate kar. ind.")
        )# cat
        if("OTU" %in% colnames(listOfdfChromSize[[s]]) ){message(crayon::red(paste("in OTU",unique(listOfdfChromSize[[s]]$OTU) )) ) }
        return(NULL)
    } # fi
  } # for
  asymmetry<-list()
  asymmetry$A<-format(round(sapply(listOfdfChromSize, function(x) mean(x$Aeach)),2),nsmall=2)
  stDevForSps<-sapply(listOfdfChromSize, function(x) stats::sd(x$length))
  meanForSps<-sapply(listOfdfChromSize, function(x) mean(x$length))
  asymmetry$A2<-format(round(stDevForSps / meanForSps,2),nsmall=2 )
  return(asymmetry)
}


