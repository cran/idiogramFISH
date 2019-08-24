#' Function to check names of OTUs (species) among dataframes
#'
#' This function reads two dataframes the one with chromosome sizes, and one df
#' with data of marks (sites) both have to have the column \code{chrName} and if
#' several species, \code{OTU}. The functions returns \code{TRUE} if names of
#' chromosomes in dataframe of marks are included in dataframe of chr. size.
#'
#' @param dfChrSize dataframe, with columns: OTU (optional), chrName, for chr.
#'   with cen.: shortArmSize, longArmSize, for holoc.: chrSize
#' @param dfMarks dataframe, of marks (sites): OTU (opt / mandat. if in
#'   dfChrSize), chrName markName markSize, for chr. with cen.:  markArm (p,q)
#'   markDistCen, for holoc: markPos
#'
#' @keywords dataframe check
#' @export
#'
#' @examples
#' data(dfOfChrSize,dfOfMarks,dfOfCenMarks)
#' checkNameChrDfMarks(dfOfChrSize,dfOfMarks)
#' checkNameChrDfMarks(dfOfChrSize,dfMarks=dfOfCenMarks)
#' @return logical
#'
checkNameChrDfMarks<- function(dfChrSize,dfMarks){
  pars <- as.character(match.call()[-1])
  if("OTU" %in% colnames(dfChrSize)){
    listOfdfChromSize<-base::split(dfChrSize, dfChrSize$OTU )
    if("OTU" %in% colnames(dfMarks)){
      listOfdfMarks<-base::split(dfMarks, dfMarks$OTU )
    } else {
      message(crayon::red("OTUs in main dataframe but missing in df of marks")
      )#c
      result<-FALSE
    } # OTU in dfMarks
  } else { # no OTU in dfChrSize
    listOfdfChromSize<-list(dfChrSize)
    names(listOfdfChromSize)<-1
    listOfdfMarks<-list(dfMarks)
    names(listOfdfMarks)<-1
  } # else
  message("\n")
  message(crayon::black(paste(("\nComparing chromosome names among dataframes:\n") ) )
     )# message
  result<-logical()

  for (s in 1:length(listOfdfChromSize)) {
      name<-names(listOfdfChromSize)[[s]]
      if (length(which(names(listOfdfMarks)==name) )==0){} else {
        message(crayon::black(paste(c("\nComparing OTU named: ", name,"of main dataframe with correspondent dataframe ",pars[2]," "), sep=" ", collapse = " ")
                               )
        )#m
        if (length(setdiff(listOfdfMarks[[which(names(listOfdfMarks)==name)]]$chromName,
                           listOfdfChromSize[[which(names(listOfdfChromSize)==name)]]$chromName))>0){
              message(crayon::red(paste(c("\nERROR Divergences Chromosome Name:", setdiff(listOfdfMarks[[which(names(listOfdfMarks)==name)]]$chromName,
                                             listOfdfChromSize[[which(names(listOfdfChromSize)==name)]]$chromName
                  ) ), sep=" ", collapse = " " )
                )
              ) #message
              message(crayon::black(paste(c("\nthere are chromosome Name(s) -see above - of dataframe ",
                pars[2],"missing in the dataframe of chromosome sizes dfChrSize"), sep=" ", collapse = " " ) )
              )#message
              result<-c(result,FALSE)
        } else {
            message(crayon::black(paste("\n No divergence"))
            )#m
            result<-c(result,TRUE)
        } # else
      } # else
  } # for
  message(crayon::black(paste("\nChecks done for ",pars[2]) )
  )# message
  return(all(result))
} # fun

