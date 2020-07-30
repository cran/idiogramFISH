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

markDistCenGISHfix<-function(dfArmGISHInternalMonocen,dfChrSizeInternal,columnArmSize,markDistType){

    dfArmGISHInternalMonocen$markSize <- dfChrSizeInternal[match(interaction(dfArmGISHInternalMonocen[c("OTU","chrName")] ),
                                                             interaction(dfChrSizeInternal[c("OTU","chrName") ] )
    ),][,columnArmSize]

    dfArmGISHInternalMonocen$markDistCen<-0


    if(markDistType=="cen") { # center

      dfArmGISHInternalMonocen$markDistCen <- dfChrSizeInternal[match(interaction(dfArmGISHInternalMonocen[c("OTU","chrName")] ),
                                                                    interaction(dfChrSizeInternal[c("OTU","chrName") ] )
      ),][,columnArmSize]/2

    } # if cen

    return(dfArmGISHInternalMonocen)
}
