#' @name perMark
#' @title FUNCTION perMark
#' @description calculates fraction (%) of chromosome for each mark
#'
#' @param dfMarkPos data.frame, of marks' position
#' @param listOfdfChromSize list of data.frames or data.frame of chr. sizes.
#' @param bToRemove character vector, bands to ignore
#' @param result character, type of return, \code{"data.frame"} or \code{"list"}
#'
#' @keywords percentage span mark chromosome
#' @examples
#' load(system.file("shinyApps", "iBoard/www/rda/monoholoCS.rda", package = "idiogramFISH") )
#' load(system.file("shinyApps", "iBoard/www/rda/monoholoMarks.rda", package = "idiogramFISH") )
#' monoholoMarks2 <- fillMarkInfo(monoholoMarks,monoholoCS)
#' perMark(monoholoMarks2,monoholoCS, result="data.frame")
#'
#' @return list
#' @rdname perMark
#' @importFrom stats setNames
#' @export
#'
perMark <- function(dfMarkPos,listOfdfChromSize, result="list", bToRemove="") {

  if(inherits(listOfdfChromSize, "list")==FALSE ) {
    listOfdfChromSize <- dfToListColumn(listOfdfChromSize)

    if(!"OTU" %in% colnames(dfMarkPos)){
      message(crayon::blue("listOfdfChromSize not a list & dfMarkPos without OTU column, dfMarkPos OTU will be 1"))
      dfMarkPos$OTU<-1
    }
  }

  for (s in 1:length(listOfdfChromSize)){
    dfChromSize <- fixChrNameDupDF(listOfdfChromSize[s], TRUE)
    listOfdfChromSize[[s]]<-dfChromSize[[1]]

    # remove empty columns

    listOfdfChromSize[[s]][sapply(listOfdfChromSize[[s]], function(x) all(is.na(x)))] <- NULL

    # add chrSize column

    if(!"chrSize" %in% colnames(listOfdfChromSize[[s]] ) ) {
      listOfdfChromSize[[s]]$chrSize<- listOfdfChromSize[[s]]$shortArmSize+listOfdfChromSize[[s]]$longArmSize
    }
  }

  message(crayon::blue("\nCalculating position of each mark in terms of % of chromosome"))

  perList  <-list()

  for (i in 1:length(listOfdfChromSize)) { # 1223
    # i<-1
    spname <- names(listOfdfChromSize)[i]

    dup_chr<-any(duplicated(listOfdfChromSize[[i]]$chrName) )

    if(dup_chr==FALSE) {
      perList[[i]]<-setNames(data.frame(matrix(ncol = nrow( listOfdfChromSize[[i]])  , nrow = 0)),
                             listOfdfChromSize[[i]]$chrName  )

      names(perList)[i]<-names(listOfdfChromSize)[i]

      bandList <- unique(dfMarkPos[which(dfMarkPos$OTU %in% spname),]$markName) # 1247

      if("chrRegion" %in% colnames(dfMarkPos) ){
        bandList <-   unique(dfMarkPos[which(
          dfMarkPos$OTU %in% spname &
          !dfMarkPos$chrRegion %in% "cen"),]$markName)
      }

      bandList <- setdiff(bandList,bToRemove)

      # band<-"5S"
      for (band in bandList) {

        chrName <- listOfdfChromSize[[i]]$chrName[1]

        for (chrName in listOfdfChromSize[[i]]$chrName ) {

          chrSize <- listOfdfChromSize[[i]][which(listOfdfChromSize[[i]]$chrName %in% chrName ),]$chrSize

          perList[[i]]["chrSize",as.character(chrName)]<-chrSize

          # print(paste("chr",chrName) )
          markSize<-NA
          allMarks<-NA

          allMarks <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                        dfMarkPos$chrName %in% chrName &
                                        dfMarkPos$markName %in% band),]$markSize

          markSize <- sum(dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                            dfMarkPos$chrName %in% chrName &
                                            dfMarkPos$markName %in% band),]$markSize, na.rm=T)
          chrRegion<-NA
          # band<-"B mark"
          if("chrRegion" %in% colnames(dfMarkPos)) {
          chrRegion<-dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                       dfMarkPos$chrName %in% chrName &
                                       dfMarkPos$markName %in% band),]$chrRegion

          }

          if (length(markSize) ) {
            if(any(is.na(allMarks)) ) {
              if("chrRegion" %in% colnames(dfMarkPos)) {
                if(!is.na(chrRegion[is.na(allMarks)]=="cen" ) ) {
                if(chrRegion[is.na(allMarks)]=="cen"){
                  message(crayon::blue( "mark's style 'cen' does not have size, no % calculated, see ruler") )
                }
                } else {
                  message(crayon::blue(paste(spname,"- No data of size, for mark"
                                             , band
                                             ,"chr",chrName
                                             ,"region",chrRegion[is.na(allMarks)]
                                             ))
                  )
                }
              }
            }
            perList[[i]][as.character(band),as.character(chrName)] <- markSize
          }
        } # for chrName

        perList[[i]][paste0(band,"_per"),] <- perList[[i]][as.character(band),] / perList[[i]]["chrSize",]
        perList[[i]] <- perList[[i]][rowSums(perList[[i]]) != 0, ]

      } # for band

    } else {# dup False
      message(crayon::red(paste0("chrNames duplicated in: ",spname) ) )
    }

  } # for s
  if(result=="data.frame"){
    dflist2 <- lapply(perList, function(x) {
      if(nrow(x)){
      a<-cbind(markName=row.names(x),x)
      row.names(a)<-1:nrow(a)
      a
      } } )
    dflist2     <- dflist2[which(!sapply(dflist2, is.null)) ] # remove no-marks sps.
    df       <- plyr::rbind.fill(mapply( function(x,y) cbind(OTU=x,y), x = names(dflist2), y = (dflist2), SIMPLIFY = F ) )
    df[df==0]<-NA

    otherc <- sort(setdiff(colnames(df),c("OTU","markName") ) )
    numeric_c <- as.character(sort(as.numeric(otherc[which(!is.na(suppressWarnings(as.numeric(otherc) ) ) ) ] ) ) )
    not_num   <- sort(otherc[which(is.na(suppressWarnings(as.numeric(otherc) ) ) ) ] )
    df <- df[,c("OTU","markName", numeric_c,not_num)]
    listOfMarks <- base::split(df, factor(df[,"OTU"],levels = unique(df[,"OTU"])  ) )
    nam<-names(listOfMarks)
    i<-0
    for (df in listOfMarks ) {
      i<-i+1
      listOfMarks[[i]] <- plyr::rbind.fill(df[which(df$markName=="chrSize"),]
                            ,df[which(df$markName!="chrSize"),][order(df$OTU[which(df$markName!="chrSize")],
                                                               df$markName[which(df$markName!="chrSize")] ),]
      )
    }
    names(listOfMarks)<-nam
    plist <- rbind.fill(listOfMarks[sort(names(listOfMarks) )])

    df <- makeNumCols(plist)
    # df <- df[order(df$OTU, df$markName ),]
    return(df)
  } else {
    return(perList)
  }
} # fun
