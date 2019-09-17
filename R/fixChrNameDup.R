# fixChrNameDup
#' This is an internal function fixes duplicated names when no marks
#'
#' It returns a list of dataframes
#'
#' @keywords internal
#'
#' @param listOfdfChromSize list of df of chr size
#' @param mybooleanChrName test for absence of marks
#'
#' @import crayon
#'
#' @return Returns a graphics element
#'
#'
# mybooleanChrName<-!exists("listOfdfMarkPosSq") & !exists("listOfdfMarkPosCr") & !exists("listOfdfDataCen")

fixChrNameDup<-function(listOfdfChromSize, mybooleanChrName){
  if(mybooleanChrName){
  for (s in 1:length(listOfdfChromSize)) {
    if(length(listOfdfChromSize[[s]]$chrName)!=length(unique(listOfdfChromSize[[s]]$chrName) )){
      message(crayon::red(
        paste0("\nWarning: Chromosome names duplicated in dataframe ",names(listOfdfChromSize)[[s]],
               ", will be renamed\n this correction is available when no marks to be plotted, otherwise, is fatal")
      ) ) #m
      string<-listOfdfChromSize[[s]]$chrName
      mstring <- make.unique(as.character(string) )
      mstring<-sub("(.*)(\\.)([0-9]+)","\\1_\\3",mstring)
      y <- rle(string)
      tmp <- !duplicated(string) & (string %in% y$values[y$lengths>1])
      mstring[tmp]<-gsub("(.*)","\\1_0", mstring[tmp])
      end <- sub(".*_([0-9]+)","\\1",grep("_([0-9]*)$",mstring,value=T) )
      beg <- sub("(.*_)[0-9]+","\\1",grep("_([0-9]*)$",mstring,value=T) )
      newend <- as.numeric(end)+1
      mstring[grep("_([0-9]*)$",mstring)]<-paste0(beg,newend)
      listOfdfChromSize[[s]]$chrName<-mstring
    }
  } # SPS
    return(listOfdfChromSize)
  } # if
  else{
    return(listOfdfChromSize)
  }
}
