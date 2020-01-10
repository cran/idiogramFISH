#' Function to check names of chromosomes among data.frames
#'
#' This function reads two data.frames the one with chromosome sizes
#' \code{dfChrSize} and one df with data of marks (sites) both have to have the
#' column \code{chrName} and if several species, \code{OTU}. The functions
#' returns \code{TRUE} if names of OTUs in data.frame of marks are included in
#' data.frame of chr. size.
#'
#' @param dfChrSize data.frame, of chr. size with columns: OTU (optional),
#'   chrName, for chr. with cen.: shortArmSize, longArmSize, for holoc.: chrSize
#' @param dfMarks data.frame, of marks (sites): OTU (opt / mandat. if in
#'   dfChrSize), chrName markName markSize, for chr. with cen.:  chrRegion (p,q)
#'   markDistCen, for holoc: markPos
#'
#' @keywords data.frame check
#' @keywords internal
#' @return logical
#'
checkNameOTUdfMarks<- function(dfChrSize,dfMarks){
  pars <- as.character(match.call()[-1])
  if("OTU" %in% colnames(dfChrSize)){
    if("OTU" %in% colnames(dfMarks)){
      message(crayon::black(paste("\nDataframe of chr. size and df of",pars[2],"have the OTU column\nComparing OTU (species) names among them: ")
      )) # message
      if (length(setdiff(dfMarks$OTU,dfChrSize$OTU) )>0){
        diff<-setdiff(dfMarks$OTU,dfChrSize$OTU)
        message(crayon::red(paste(c("\nERROR:",diff,"OTU(s) of",pars[2],"data.frame NOT in Chr. size (main) data.frame"), sep=" ", collapse = " ")
            ))#message
        return(FALSE)
      } else {#fi
        message(crayon::black(paste(c("\nCheck OK, OTUs of data.frame ",pars[2]," present in Chr. size (main) data.frame"), sep=" ", collapse = " ")
                              )#b
        )#m
        return(TRUE)
      }
    } else {#fi # OTU  in df marks
      message(crayon::red(paste("\nERROR: Dataframe of chr. size has the OTU column\nbut Df ",pars[2],"does not")
          ) )#message
      return(FALSE)
    } #else
  } else { # 1 fi OTU NOT PRESENT IN DF CHR SIZE
    if("OTU" %in% colnames(dfMarks)){
      message(crayon::red(paste("\nERROR: Dataframe ",pars[2],"has the OTU column\nbut Df of Chr. size (main) has not")
      ) )#message
      return(FALSE)
    } # fi
    else {
      message(crayon::black(paste("Check OK, Nor Dataframe ",pars[2],"nor Df of Chr. size (main) have the OTU column") )
      )#message
      return(TRUE)
    }# else
  } # else
}# fun
