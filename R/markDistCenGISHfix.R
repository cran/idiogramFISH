#' markDistCenGISHfix
#' This is an internal function that fixes markDistCen when GISH
#'
#' @keywords internal
#'
#' @param dfArmGISHInternalMonocen df of GISH OF ARM
#' @param dfChrSizeInternal d.f. of chr. size
#' @param markDistType character
#' @param columnArmSize column rel. to arm.
#'
#' @return data.frame

markDistCenGISHfix<-function(dfArmGISHInternalMonocen,dfChrSizeInternal
                             ,columnArmSize,markDistType
                             ,listOfdfChromSize){

  dfArmGISHInternalMonocen$r2<-as.numeric(NA)
  dfArmGISHInternalMonocen$markSizeProtein<-as.numeric(NA)
  dfArmGISHInternalMonocen$markDistCenProtein<-as.numeric(NA)

  for(i in 1:length(dfArmGISHInternalMonocen$OTU)){
  corr_index <- which(names(listOfdfChromSize) %in% dfArmGISHInternalMonocen$OTU[i] )
  dfArmGISHInternalMonocen$r2[i] <- as.numeric(attr(listOfdfChromSize[[corr_index]],"r2"))
  }

    dfArmGISHInternalMonocen$markSize <- dfChrSizeInternal[match(interaction(dfArmGISHInternalMonocen[c("OTU","chrName")] ),
                                                             interaction(dfChrSizeInternal[c("OTU","chrName") ] )
    ),][,columnArmSize]

    dfArmGISHInternalMonocen$markSizeProtein<-dfArmGISHInternalMonocen$markSize-(dfArmGISHInternalMonocen$r2*2)

    dfArmGISHInternalMonocen$markDistCen <- 0



    if(markDistType=="cen") { # center

      dfArmGISHInternalMonocen$markDistCen <- dfChrSizeInternal[match(interaction(dfArmGISHInternalMonocen[c("OTU","chrName")] ),
                                                                    interaction(dfChrSizeInternal[c("OTU","chrName") ] )
      ),][,columnArmSize]/2

    } # if cen

    dfArmGISHInternalMonocen$markDistCenProtein <- dfArmGISHInternalMonocen$markDistCen + dfArmGISHInternalMonocen$r2

    return(dfArmGISHInternalMonocen)
}
