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

markDistCenGISHfix <- function(dfArmGISHInternalMonocen, dfChrSizeInternal,
                               columnArmSize, markDistType,
                               listOfdfChromSize, addR2 = TRUE) {
  dfArmGISHInternalMonocen$r2 <- as.numeric(NA)
  dfArmGISHInternalMonocen$markSizeProtein <- as.numeric(NA)
  dfArmGISHInternalMonocen$markDistCenProtein <- as.numeric(NA)

  for (i in seq_along(dfArmGISHInternalMonocen$OTU)) {
    corr_index <- which(names(listOfdfChromSize) %in% dfArmGISHInternalMonocen$OTU[i])
    if (addR2) {
      dfArmGISHInternalMonocen$r2[i] <- as.numeric(attr(listOfdfChromSize[[corr_index]], "r2"))
    }
  }

  dfArmGISHInternalMonocen$markSize <- dfChrSizeInternal[match(
    interaction(dfArmGISHInternalMonocen[c("OTU", "chrName")]),
    interaction(dfChrSizeInternal[c("OTU", "chrName")])
  ), ][, columnArmSize]
  dfArmGISHInternalMonocen$markSizeOrig <- NA

  if (addR2) {
    dfArmGISHInternalMonocen$markSizeProtein <- dfArmGISHInternalMonocen$markSize - (dfArmGISHInternalMonocen$r2 * 2)
  }
  dfArmGISHInternalMonocen$markDistCen <- 0



  if (markDistType == "cen") {

    dfArmGISHInternalMonocen$markDistCen <- dfChrSizeInternal[match(
      interaction(dfArmGISHInternalMonocen[c("OTU", "chrName")]),
      interaction(dfChrSizeInternal[c("OTU", "chrName")])
    ), ][, columnArmSize] / 2
  }

  if (addR2) {
    dfArmGISHInternalMonocen$markDistCenProtein <- dfArmGISHInternalMonocen$markDistCen + dfArmGISHInternalMonocen$r2
  }
  return(dfArmGISHInternalMonocen)
}
