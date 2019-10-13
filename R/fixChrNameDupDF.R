# fixChrNameDupDF
#' This is an internal function fixes duplicated names when no marks
#'
#' It returns a list of dataframes
#'
#' @keywords internal
#'
#' @param list1dfChromSize list of 1 df of chr size
#' @param mybooleanChrName test for absence of marks
#'
#' @import crayon
#'
#' @return Returns a graphics element
#'
#'
# mybooleanChrName<-!exists("listOfdfMarkPosSq") & !exists("listOfdfMarkPosCr") & !exists("listOfdfDataCen")

fixChrNameDupDF<-function(list1dfChromSize, mybooleanChrName){
  dfChromSize<-list1dfChromSize[[1]]
  if(class(dfChromSize)=="data.frame"){
  OTUname<-names(list1dfChromSize)
  if(mybooleanChrName){
  # for (s in 1:length(listOfdfChromSize)) {
    if(length(dfChromSize$chrName)!=length(unique(dfChromSize$chrName) )){
      message(crayon::yellow(
        paste0("\nWarning: Chromosome names duplicated in dataframe ",names(list1dfChromSize),
               ", will be renamed\n this correction is available when no marks to be plotted, otherwise, is fatal")
      ) ) #m
      string<-dfChromSize$chrName
      mstring <- make.unique(as.character(string) )
      mstring<-sub("(.*)(\\.)([0-9]+)","\\1_\\3",mstring)
      y <- rle(string)
      tmp <- !duplicated(string) & (string %in% y$values[y$lengths>1])
      mstring[tmp]<-gsub("(.*)","\\1_0", mstring[tmp])
      end <- sub(".*_([0-9]+)","\\1",grep("_([0-9]*)$",mstring,value=T) )
      beg <- sub("(.*_)[0-9]+","\\1",grep("_([0-9]*)$",mstring,value=T) )
      newend <- as.numeric(end)+1
      mstring[grep("_([0-9]*)$",mstring)]<-paste0(beg,newend)
      dfChromSize$chrName<-mstring
    } # if
  # } # SPS
    list1dfChromSize<-list(dfChromSize)
    names(list1dfChromSize)<-OTUname
    return(list1dfChromSize)
  } # if
  else{
    return(list1dfChromSize)
  }
  } # data.frame
  else{
    return(list1dfChromSize)
  }
} # fun
