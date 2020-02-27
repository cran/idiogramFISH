#' @name genBankReadIF
#' @title FUNCTION genBankReadIF
#' @description genBankReadIF: creates a list of data.frames from a genbank format file
#'
#' @param filename.gb name of file to read, downloaded from genBank
#'
#' @keywords genBank
#' @export
#' @rdname genBankReadIF
#' @export
#' @importFrom tidyr fill
#' @importFrom dplyr %>% group_by summarise mutate arrange
#' @return list
genBankReadIF<-function(filename.gb){
  value <- field <- NULL
# @importFrom magrittr %>%
  requireNamespace("dplyr")
  if(requireNamespace("tidyr")==FALSE){
    message(crayon::red("You need to install tidyr"))
    return(NULL)
  } else {
    requireNamespace("tidyr")
  }
  readed<-readLines(filename.gb)
  assemblyPresence<-grep("Genome-Assembly-Data",readed)
  if(length(assemblyPresence)>0){
    assemblyPresence<-TRUE
  } else {
    assemblyPresence<-FALSE
  }
  #fIRST PART
  delimiter1stPart<-ifelse(assemblyPresence,"##Genome-Assembly-Data-START##","##Genome-Annotation-Data-START##")
  end1stpart <- grep(delimiter1stPart, readed)-1
  posSecond<-gregexpr(pattern ='[[:alnum:]]+',readed[1])[[1]][[2]]
  firstcolumn <-substr(readed[1:end1stpart], 1,posSecond-1)
  firstcolumn<-trimws(firstcolumn)
  secondcolumn <-substr(readed[1:end1stpart],posSecond, nchar(readed) )
  secondcolumn<-trimws(secondcolumn)
  gbdf<-data.frame(field=firstcolumn,value=secondcolumn, stringsAsFactors = F)
  gbdf$field[gbdf$field!=""]<-make.unique(gbdf$field[gbdf$field!=""])
  gbdf$field[gbdf$field==""]<-NA
  gbdf<-gbdf %>%
    fill(field) %>%
    group_by(field) %>%
    summarise(value = paste(value, collapse = "; ")) %>%
    mutate(field = factor(field, levels=unique(gbdf$field) ) ) %>%
    arrange(field)

  listOfDfs<-list()
  listOfDfs$gbdfMain<-gbdf

  # SECOND PART
  if(assemblyPresence){
    sectionString<-"##Genome-Assembly-Data"
    begAsData <- grep(paste0(sectionString,"-START##"), readed)+1
    endAsData <- grep(paste0(sectionString,"-END##"), readed)-1
    mypattern<-'::'
    posSecond2<-gregexpr(pattern = mypattern ,readed[begAsData+0]) [[1]][[1]]+2 # column char

    interval<-(begAsData):endAsData
    firstcolumn2 <-substr(readed[interval], 1,posSecond2-3)
    firstcolumn2<-trimws(firstcolumn2)
    secondcolumn2 <-substr(readed[interval],posSecond2, nchar(readed[(begAsData+1):endAsData]) )
    secondcolumn2 <-trimws(secondcolumn2)
    gbdf<-data.frame(field=firstcolumn2,value=secondcolumn2, stringsAsFactors = F)
    # gbdf<-data.frame(field=firstcolumn,value=secondcolumn, stringsAsFactors = F)
    gbdf$field[gbdf$field!=""]<-make.unique(gbdf$field[gbdf$field!=""])
    gbdf$field[gbdf$field==""]<-NA
    gbdf<-gbdf %>%
      fill(field) %>%
      group_by(field) %>%
      summarise(value = paste(value, collapse = "; ")) %>%
      mutate(field = factor(field, levels=unique(gbdf$field) ) ) %>%
      arrange(field)

    # gbdf2<-collapseRows(gbdf2)
    listOfDfs$gbdfAssemblyMeta<-gbdf
  }
  # THIRD PART
  ##Genome-Annotation-Data-START##
  mypattern<-'::'
  sectionString<-"##Genome-Annotation-Data"
  begAsData3 <- grep(paste0(sectionString,"-START##"), readed)+1
  endAsData3 <- grep(paste0(sectionString,"-END##"), readed)-1
  posSecond3 <- gregexpr(pattern = mypattern,readed[begAsData3+1])[[1]][[1]]+2

  interval<-(begAsData3):endAsData3
  firstcolumn3 <-substr(readed[interval], 1,posSecond3-3)
  firstcolumn3 <-trimws(firstcolumn3)
  secondcolumn3 <-substr(readed[interval],posSecond3, nchar(readed[interval]) )
  secondcolumn3 <-trimws(secondcolumn3)
  gbdf3<-data.frame(field=firstcolumn3,value=secondcolumn3, stringsAsFactors = F)

  gbdf3$field[gbdf3$field!=""]<-make.unique(gbdf3$field[gbdf3$field!=""])
  gbdf3$field[gbdf3$field==""]<-NA
  gbdf3<-gbdf3 %>%
    fill(field) %>%
    group_by(field) %>%
    summarise(value = paste(value, collapse = "; ")) %>%
    mutate(field = factor(field, levels=unique(gbdf3$field) ) ) %>%
    arrange(field)

  listOfDfs$gbdfAnnoMeta<-gbdf3

  # FEATURES             Location/Qualifiers
  "CONTIG      join(CP009939.1:1..52166)"

  # FORTH PART
  ##Genome-Annotation-Data-START##
  string4<-"Location/Qualifiers"
  begAsData4 <- grep(string4, readed)+1
  endstring4<-"CONTIG"
  endAsData4 <- grep(endstring4, readed)-1
  #
  #   limit readed lines
  # if(!missing(maxNumber)){
  #   endAsData4 <- min(maxNumber,endAsData4)
  # }
  mypattern<-"[[:alnum:]]+"
  posSecond4 <- gregexpr(pattern = mypattern,readed[begAsData4] ) [[1]][[2]]
  firstcolumn4 <-substr(readed[(begAsData4):endAsData4], 1,posSecond4-1)
  firstcolumn4 <-trimws(firstcolumn4)
  interval<-(begAsData4):endAsData4
  secondcolumn4 <-substr(readed[interval],posSecond4, nchar(readed[interval]) )
  secondcolumn4 <-trimws(secondcolumn4)

  gbdf4 <- data.frame(field=firstcolumn4,value=secondcolumn4, stringsAsFactors = F)
  gbdf4$field[gbdf4$field!=""]<-make.unique(gbdf4$field[gbdf4$field!=""])
  gbdf4$field[gbdf4$field==""]<-NA
  gbdf4<-gbdf4 %>%
    fill(field) %>%
    group_by(field) %>%
    summarise(value = paste(value, collapse = "; ")) %>%
    mutate(field = factor(field, levels=unique(gbdf4$field) ) ) %>%
    arrange(field)

  # gbdf4<-gbdf4 %>%
  #   fill(field)  %>%
  #   group_by(field) %>%
  #   summarise(value = paste(value, collapse = "; "))

  ###########################
  features<-unique(firstcolumn4)[unique(firstcolumn4)!=""]

  for (feature in features) {
    # if(readCDS) {

    gbdf4CDS<-gbdf4[which(gbdf4$field %in% grep(feature, gbdf4$field, value=TRUE) ),]

    # createfields
    mypattern<-"/[[:alnum:]_]+="
    fieldNamesCDS<-regmatches(gbdf4CDS$value, gregexpr(mypattern, gbdf4CDS$value))
    fieldNamesCDS<-lapply(fieldNamesCDS, function(x) gsub("/|=","",x) )

    gbdf4CDS$value<-paste(gbdf4CDS$value,"/")

    pattern2<-'=.*?/'
    fieldValuesCDS<-regmatches(gbdf4CDS$value, gregexpr(pattern2, gbdf4CDS$value))
    fieldValuesCDS<-lapply(fieldValuesCDS, function(x) gsub('\"',"",x) )
    fieldValuesCDS<-lapply(fieldValuesCDS, function(x) gsub('=|; /',"",x) )
    fieldValuesCDS<-lapply(fieldValuesCDS, function(x) gsub('\\s/$',"",x) )

    firstField<-sub(";.*","",gbdf4CDS$value, perl=T)
    firstFieldBac<-firstField
    firstFieldPrefix<-NULL
    firstFieldPrefix<-sub("([[:alpha:]]+)?.*","\\1",firstFieldBac)
    firstField<-gsub("complement\\(|\\)","",firstField)
    firstField<-gsub("join\\(|\\)","",firstField)
    firstField<-gsub("<","",firstField)

    beginSeq<- sub("\\..*","", firstField,perl=T)
    endSeq<- sub("[[:digit:]]+\\.+>?([[:digit:]]+)","\\1", firstField,perl=T)
    endSeq<- sub("\\,.*","\\1", endSeq,perl=T)

    dflistCDS<-list()
    for (i in 1:length(fieldNamesCDS)){
      dflistCDS[[i]]<-as.data.frame(t(as.data.frame(fieldValuesCDS[[i]]) ), stringsAsFactors = F)
      colnames(dflistCDS[[i]])<-fieldNamesCDS[[i]]
    }

    dfCDS <- dplyr::bind_rows(dflistCDS)
    dfCDS$begin<-beginSeq
    dfCDS$end<-endSeq
    dfCDS$seqType<-firstFieldPrefix
    listOfDfs[[feature]] <- dfCDS
    # }
  }
  return(listOfDfs)
}
