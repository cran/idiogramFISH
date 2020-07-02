#' @name asymmetry
#' @aliases asymmetryA2
#' @title FUNCTIONS asymmetry and asymmetryA2.
#' @description asymmetry: calculates karyotype asymmetry A and A2.
#' @description asymmetryA2: calculates karyotype asymmetry A2
#'
#' @description This functions read a data.frame  with columns:
#' \code{shortArmSize} and \code{longArmSize}
#' @description If several species present, use
#' column \code{OTU}.
#' @description It returns a list with the A and A2 indices
#'
#' \deqn{A = \frac{\sum_{i=1}^{n} \frac{longArm_{i} - shortArm_{i}}{CL_{i}} 
#' }{n} }{%
#'      A = Sum (longArm - shortArm / CL) / n }
#' @description A: Watanabe et al. (1999) asymmetry of karyotype ranging from 0
#' (symmetric) to 1 (asymmetric)
#' \deqn{A_{2} = \frac{sCL}{xCL}}{%
#'      A2 = sCL / xCL}
#' @description (s = std dev, CL = chr. length, x = mean) (Romero-Zarco 1986)
#'
#' @description related to:
#' \deqn{CV_{CL} = A_{2} * 100}{%
#'       CVCL = A2 * 100}
#' @description(CV = coeff. var.) (Paszko 2006)
#'
#' @param dfChrSize name of data.frame
#'
#' @keywords data.frame size arm
#' @importFrom stats sd
#' @export
#' @examples
#' asymmetry(dfOfChrSize)
#' myAlist<-asymmetry(bigdfOfChrSize)
#' as.data.frame(myAlist)
#' @seealso \code{\link{chrbasicdatamono}}
#'
#' @references Watanabe K, Yahara T, Denda T, Kosuge K. 1999. Chromosomal
#' evolution in the genus Brachyscome (Asteraceae, Astereae): Statistical tests
#' regarding correlation between changes in karyotype and habit using
#' phylogenetic information. Journal of Plant Research 112: 145-161.
#' 10.1007/PL00013869
#' @references A2: Romero-Zarco. 1986. A New Method for Estimating Karyotype
#' Asymmetry. Taxon Vol. 35, No. 3  pp. 526-530
#' @references Paszko B. 2006. A critical review and a new proposal of 
#' karyotype asymmetry 
#' indices. Plant Syst Evol 258:39–48.
#' @rdname asymmetry
#' @export
#' @return list

asymmetry<- function(dfChrSize){
  message(crayon::black("Calculating karyotype indexes A and A2\n") )
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
        message(crayon::red("\nERROR in short/long arm classif. It will not be fixed\nWill not calculate kar. ind.")
        ) # m
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
#' @rdname asymmetry
#' @examples
#' asymmetryA2(dfOfChrSize)
#' as.data.frame(asymmetryA2(bigdfOfChrSize))
#' asymmetryA2(dfChrSizeHolo)
#' as.data.frame(asymmetryA2(bigdfChrSizeHolo))
#' @seealso \code{\link{chrbasicdatamono}}
#' @seealso \code{\link{chrbasicdataHolo}}
#' @export
#'
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




