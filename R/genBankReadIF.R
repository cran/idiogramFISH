#' @name genBankReadIF
#' @title FUNCTION genBankReadIF
#' @description genBankReadIF: creates a list of data.frames from a genbank
#' format file or a
#' rentrez object
#' @description Requires tidyr
#'
#' @param filename.gb name of file to read, downloaded from genBank, or,
#' object from \code{rentrez::entrez_fetch( db="nuccore",} \code{ id="theID",} \code{ rettype="gbwithparts",} \code{ retmode = "text" )}
#'
#' @keywords genBank
#' @export
#' @rdname genBankReadIF
#' @export
#' @importFrom tidyr fill
#' @importFrom dplyr %>% group_by summarise mutate arrange
#' @return list
#'
genBankReadIF <- function(filename.gb) {

  value <- field <- NULL

  # requireNamespace("dplyr")
  #
  # if(requireNamespace("tidyr")==FALSE){
  #   message(crayon::red("You need to install tidyr"))
  #   return(NULL)
  # } else {
  #   requireNamespace("tidyr")
  # }
  # filename.gb<-dataChr.gb
  if( nchar(filename.gb) > 300 ){
    #
    #   not a filename but txt data
    #
    # filename.gb<-nostoc_seqs
    readed <- unlist(strsplit(filename.gb,"\n") )
    begSeq <- grep("ORIGIN",readed)
    readed <- readed[1:(begSeq)]
  } else {
    # filename.gb<-"gitNo/nostoc.gb"
    readed<-readLines(filename.gb)
  }

  # if(length(assemblyPresence)>0){
  #   assemblyPresence<-TRUE
  # } else {
  #   assemblyPresence<-FALSE
  # }
  #fIRST PART
  # delimiter1stPart <- ifelse(assemblyPresence,"##Genome-Assembly-Data-START##","##Genome-Annotation-Data-START##")
  delimiter1stPart<- "##"
  end1stpart <- (grep(delimiter1stPart, readed)-1)[1]
  posSecond <- gregexpr(pattern ='[[:alnum:]]+',readed[1])[[1]][[2]] # column pos
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
  # View(gbdf)
  listOfDfs$gbdfMain<-gbdf

  # SECOND PART
  assemblyPresence <- any(grepl("Genome-Assembly-Data",readed) )

  if(assemblyPresence) {
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
  annotationPresence <- any(grepl("Genome-Annotation-Data",readed) )

  if(annotationPresence) {
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

  gbdf3 <- data.frame(field=firstcolumn3,value=secondcolumn3, stringsAsFactors = F)

  gbdf3$field[gbdf3$field!=""]<-make.unique(gbdf3$field[gbdf3$field!=""])
  gbdf3$field[gbdf3$field==""]<-NA
  gbdf3<-gbdf3 %>%
    fill(field) %>%
    group_by(field) %>%
    summarise(value = paste(value, collapse = "; ")) %>%
    mutate(field = factor(field, levels=unique(gbdf3$field) ) ) %>%
    arrange(field)

  listOfDfs$gbdfAnnoMeta<-gbdf3
  }

  # FEATURES             Location/Qualifiers
  # "CONTIG      join(CP009939.1:1..52166)"

  # FORTH PART
  ##Genome-Annotation-Data-START##

  locationPresence <- any(grepl("Location/Qualifiers",readed) )

  if(locationPresence) {

  string4<-"Location/Qualifiers"
  begAsData4 <- grep(string4, readed)+1
  endstring4<-"CONTIG|ORIGIN"
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

    fieldNamesCDS <- lapply(fieldNamesCDS, function(x) make.unique(x, sep = '_' ) )

    gbdf4CDS$value<-paste(gbdf4CDS$value,"/")
    pattern2<-'=.*?/'

    fieldValuesCDS<-regmatches(gbdf4CDS$value, gregexpr(pattern2, gbdf4CDS$value))
    fieldValuesCDS<-lapply(fieldValuesCDS, function(x) gsub('\"',"",x) )
    fieldValuesCDS<-lapply(fieldValuesCDS, function(x) gsub('=|; /',"",x) )
    fieldValuesCDS<-lapply(fieldValuesCDS, function(x) gsub('\\s/$',"",x) )

    firstField<-sub(";.*","",gbdf4CDS$value, perl=T)

    compleBool<-grepl("complement",firstField)
    joinBool  <-grepl("join",firstField)
    commaPos  <-grep(",",firstField)

    if(length(commaPos)>0) {
      fieldValuesCDS<-insertInList(fieldValuesCDS,commaPos,fieldValuesCDS[commaPos])
      fieldNamesCDS <-insertInList(fieldNamesCDS ,commaPos, fieldNamesCDS[commaPos])
      compleBool    <-insertInList(compleBool ,commaPos, compleBool[commaPos])
      joinBool      <-insertInList(joinBool ,commaPos, joinBool[commaPos])
    }

    joinElements   <-  grep(",",firstField, value=T)
    joinElementsAC <-  sub(".*\\,","", joinElements)

    if(length(commaPos)>0) {
      firstField<-insertInList(firstField,commaPos,joinElementsAC)
    }

    firstField<-gsub("complement\\(|\\)","",firstField)
    firstField<-gsub("join\\(|\\)","",firstField)
    firstField<-gsub("<","",firstField)

    beginSeq<- sub("\\..*","", firstField,perl=T)
    # as.numeric(beginSeq)
    endSeq<- sub("[[:digit:]]+\\.+>?([[:digit:]]+)","\\1", firstField,perl=T)
    endSeq<- sub("\\,.*","\\1", endSeq,perl=T)
    endSeq<- gsub("\\)","",   endSeq,perl=T)
    # as.numeric(endSeq)

    dflistCDS<-list()
    for (i in 1:length(fieldNamesCDS)){
      dflistCDS[[i]]<-as.data.frame(t(as.data.frame(fieldValuesCDS[[i]]) ), stringsAsFactors = F)
      colnames(dflistCDS[[i]])<-fieldNamesCDS[[i]]
    }

    dfCDS <- dplyr::bind_rows(dflistCDS)
    dfCDS$begin<-beginSeq
    dfCDS$end<-endSeq
    dfCDS$isComplement<-compleBool
    dfCDS$isJoin <- joinBool

    listOfDfs[[feature]] <- dfCDS
  } # big for
  } # locationP
  return(listOfDfs)
} # fun
