#' Function to check names of OTUs (species) among dataframes
#'
#' This function reads two dataframes the one with chromosome sizes, and one df
#' with data of marks (sites) both have to have the column \code{chrName} and if
#' several species, \code{OTU}. The functions returns a filtered list with 1. data.frame of chrSizes,
#' 2. data.frame of marks, 3. mark names 4. max mark size.
#'
#' @param listOfdfChromSize list of data.frames, with columns: OTU (optional), chrName, for chr.
#'   with cen.: shortArmSize, longArmSize, for holoc.: chrSize
#' @param listOfdfMarks list of data.frames of marks (sites): OTU (opt / mandat. if in
#'   dfChrSize), chrName markName markSize, for chr. with cen.:  chrRegion (p,q)
#'   markDistCen, for holoc: markPos
#'
#' @keywords internal
#'
#' @return list
#'
#'
checkNameChrDfMarks<- function(listOfdfChromSize,listOfdfMarks){
  pars <- as.character(match.call()[-1])
  message(crayon::black(paste(("\nComparing chromosome names among dataframes:\n") ) )
     )# message

  # result<-logical()
  markNames<-character()
  markSize <-numeric()

  for (s in 1:length(listOfdfChromSize)) {
    name<-names(listOfdfChromSize)[[s]]
    if(length(listOfdfChromSize[[s]]$chrName)!=length(unique(listOfdfChromSize[[s]]$chrName) ) ) {
      message(crayon::yellow(
        paste("\nWARNING Chromosome Names in dataframe",name,"duplicated - Error when d.f. of marks present")
      )) #m
      # message(crayon::red(
      #   paste("\ndata of OTU",name,"removed")
      # ) ) #m
      # listOfdfChromSize[s]<-NA
    } # duplicated chr. names fi

      if (length(which(names(listOfdfMarks)==name) )!=0 ) {

        message(crayon::black(paste(c("\nComparing OTU named: ", name,"of main dataframe with correspondent dataframe ",
                                      pars[2],

                                      " "),
                                    sep=" ",
                                    collapse = " ")
        )
        ) #m

        if (length(setdiff(listOfdfMarks[[which(names(listOfdfMarks)==name)]]$chrName,
                           listOfdfChromSize[[which(names(listOfdfChromSize)==name)]]$chrName
                           ) # setdiff
                   )>0) {

              message(crayon::red(paste(c("\nERROR Divergences Chromosome Name:",
                                          setdiff(listOfdfMarks[[which(names(listOfdfMarks)==name)]]$chrName,
                                             listOfdfChromSize[[which(names(listOfdfChromSize)==name)]]$chrName
                                          ) # setdiff
                  ), sep=" ", collapse = " " )
                )
              ) #message

          message(crayon::red(paste(c("\nERROR: There are chromosome Name(s) -see above - of OTU ",
                # pars[2],
                name
                 ,"missing in the dataframe of chromosome sizes\n this OTU's marks will be removed"), sep=" ", collapse = " " ) )
              )#message

          listOfdfMarks[which(names(listOfdfMarks)==name)]<-NULL
              # result<-c(result,FALSE)
        } else {
            message(crayon::black(paste("\n No divergence"))
            )#m
            # result<-c(result,TRUE)

          if(class(listOfdfMarks[[which(names(listOfdfMarks)==name)]])=="data.frame"){
            markNames   <- c(markNames,unique(listOfdfMarks[[which(names(listOfdfMarks)==name)]]$markName) )
            # }
            # if(class(listOfdfMarks[[which(names(listOfdfMarks)==name)]])=="data.frame"){
            #
            if(length(listOfdfMarks[[which(names(listOfdfMarks)==name)]]$markSize)>0){
              markSize <- c(markSize,max(listOfdfMarks[[which(names(listOfdfMarks)==name)]]$markSize, na.rm=TRUE) )
              # , error=function(e) NULL ) )
            } # length >0
          } # is data.frame

        } # else
    } # if chr. with mark present
  } # for

  resultList<-list(listOfdfChromSize,listOfdfMarks,markNames,markSize)
  return(resultList)
  message(crayon::black(paste("\nChecks done for ",pars[2]) )
  )# message
} # fun

