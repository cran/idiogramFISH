#' @name posCalc
#' @aliases fillMarkInfo
#' @title FUNCTION posCalc and fillMarkInfo
#' @description calculates position of marks in fraction of (%) chromosome units (0-1)
#'
#' @param dfMarkPos data.frame of marks' position
#' @param listOfdfChromSize list (for \code{posCalc}) or data.frames of chr. sizes.
#' @param dfChrSize data.frame of chr. sizes
#' @param markDistType markDistType character, if \code{"cen"} = the distance you provided in data.frame (\code{dfMarkPos}) column \code{markDistCen}
#' or \code{markPos}  is to the center of the mark, if \code{"beg"} = the distance you provided is to the
#'   beginning of the mark (Default)
#' @param bToRemove, character, bands to remove from calc. of pos.
#' @param origin, character, For non-monocentric chr. (for holocentrics only) Use \code{"b"} (default) if distance to mark in (\code{"markPos"} column in \code{"dfMarkPos"}) data.frame measured from bottom of chromosome, use \code{"t"} for distance to mark from top of chr.
#' @param showBandList, boolean, show row of all bands in tibble, see \code{"result"}
#' @param result character, use \code{"tibble"} to get results in tibble, \code{"data.frame"}, or other string results in a \code{list}
#'
#' @keywords position mark chromosome fraction
#' @examples
#' load(system.file("shinyApps", "iBoard/www/rda/monoholoCS.rda", package = "idiogramFISH") )
#' load(system.file("shinyApps", "iBoard/www/rda/monoholoMarks.rda", package = "idiogramFISH") )
#' monoholoMarks2 <- fillMarkInfo(monoholoMarks,monoholoCS)
#' posCalc(monoholoMarks2,monoholoCS, result="data.frame")
#'
#' @return list, tibble
#' @rdname posCalc
#' @importFrom tidyr as_tibble unnest pivot_wider
#' @importFrom plyr rbind.fill
#' @importFrom rlang .data
#' @export
#'
posCalc <- function(dfMarkPos, listOfdfChromSize
                    , bToRemove=""
                    , markDistType="beg"
                    , origin="b"
                    , showBandList=FALSE
                    , result="tibble"  ) {

if(!inherits(listOfdfChromSize, "list") ) {
    # listOfdfChromSize <- armRatioCI(listOfdfChromSize)
    listOfdfChromSize <- dfToListColumn(listOfdfChromSize)
    # listOfdfChromSize <- dfToListColumn(listOfdfChromSize)
    if(!"OTU" %in% colnames(dfMarkPos)){
      message(crayon::blue("listOfdfChromSize not a list & dfMarkPos without OTU column, dfMarkPos OTU will be 1"))
      dfMarkPos$OTU<-1
    }
  }

for( s in 1:length(listOfdfChromSize) ) {
# s<-1
    # dfChromSize <- fixChrNameDupDF(listOfdfChromSize[s], TRUE)
    dfChromSize <- fixChrNameDupDF(listOfdfChromSize[s], TRUE)
    listOfdfChromSize[[s]]<-dfChromSize[[1]]

    listOfdfChromSize[[s]][sapply(listOfdfChromSize[[s]], function(x) all(is.na(x)) ) ] <- NULL

    if(!"chrSize" %in% colnames(listOfdfChromSize[[s]] ) ) {
      listOfdfChromSize[[s]]$chrSize<- listOfdfChromSize[[s]]$shortArmSize+listOfdfChromSize[[s]]$longArmSize
    }
} # for

  posListTibb <- posList<-list()
  bandList    <- unique(dfMarkPos$markName)
  bandList    <- setdiff(bandList,bToRemove)

  for (s in 1:length(listOfdfChromSize) ) {

    spname  <- names(listOfdfChromSize)[s]

    dup_chr <- any(duplicated(listOfdfChromSize[[s]]$chrName) )

    if(dup_chr == FALSE) {

    posListTibb[[s]] <- tidyr::as_tibble(sapply(as.character(listOfdfChromSize[[s]]$chrName), function(x) list() ))

    posList[[s]]     <- list()

    if("shortArmSize" %in% colnames(listOfdfChromSize[[s]] ) ) {
      # chrName<-"3"
      for (chrName in listOfdfChromSize[[s]]$chrName ) {

          chrSize      <- listOfdfChromSize[[s]][which(listOfdfChromSize[[s]]$chrName %in% chrName ),]$chrSize
          shortArmSize <- listOfdfChromSize[[s]][which(listOfdfChromSize[[s]]$chrName %in% chrName ),]$shortArmSize
          longArmSize  <- listOfdfChromSize[[s]][which(listOfdfChromSize[[s]]$chrName %in% chrName ),]$longArmSize

          markPos     <- numeric()

          allMarksPos <- numeric()

          bandListUpdate <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                             dfMarkPos$chrName %in% chrName &
                                             dfMarkPos$markName %in% bandList),]$markName

          allMarksPos    <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                        dfMarkPos$chrName %in% chrName &
                                        dfMarkPos$markName %in% bandList),]$markDistCen

          if(length(allMarksPos) ) {

          allMarksSize <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                           dfMarkPos$chrName %in% chrName &
                                           dfMarkPos$markName %in% bandList),]$markSize

          chrRegion <- character()
          # band<-"B mark"
          chrRegion <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                         dfMarkPos$chrName %in% chrName &
                                         dfMarkPos$markName %in% bandList),]$chrRegion

          for (i in 1:length(allMarksPos) ) {
            if (chrRegion[i]=="p") {

              if (markDistType=="beg"){
              # band start
              markPos[i] <- sum(shortArmSize, -1*allMarksPos[i], -1*allMarksSize[i], na.rm=TRUE)
              } else {
              # center
              markPos[i] <- sum(shortArmSize, -1*allMarksPos[i], -1*(allMarksSize[i]/2), na.rm=TRUE)
              }

            } else if (chrRegion[i]=="q") {

              if (markDistType=="beg") {
                # band start
                markPos[i] <- sum(chrSize, -1*longArmSize, allMarksPos[i], na.rm=TRUE)
              } else {
                # center
                markPos[i] <- sum(chrSize, -1*longArmSize, allMarksPos[i], allMarksSize[i]/2, na.rm=TRUE)
              }

            } else if (chrRegion[i]=="cen") {
              markPos[i] <- shortArmSize
            }
          } # for
          remove(allMarksPos)
          } # if any mark

          if (length(markPos) ) {
            # print(chrName)
            # posList[[s]][[as.character(chrName)]]$chrSize <- chrSize
            posList[[s]][[as.character(chrName)]][[1]]      <- markPos
            names(posList[[s]][[as.character(chrName)]])[1] <- paste0(bandListUpdate, collapse=",")
            posList[[s]][[as.character(chrName)]][[2]]      <- markPos/chrSize
            names(posList[[s]][[as.character(chrName)]])[2] <- paste0("frac",bandListUpdate, collapse=",")

            # posListTibb[[s]][1,as.character(chrName)][[1]] <- list(chrSize)
            posListTibb[[s]][1,as.character(chrName)][[1]]   <-list(markPos/chrSize)
            posListTibb[[s]][2,as.character(chrName)][[1]]   <-list(bandListUpdate)
            if(showBandList){
              posListTibb[[s]][3,as.character(chrName)][[1]] <-list(bandList)
            }
            remove(markPos)
          } # markPos

        } #for chr

      } else { # if monocen else holocen

        for (chrName in listOfdfChromSize[[s]]$chrName ) {

          chrSize <- listOfdfChromSize[[s]][which(listOfdfChromSize[[s]]$chrName %in% chrName ),]$chrSize

          markPos     <- numeric()

          allMarksPos <-numeric()

          bandListUpdate <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                              dfMarkPos$chrName %in% chrName &
                                              dfMarkPos$markName %in% bandList),]$markName


          allMarksPos    <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                           dfMarkPos$chrName %in% chrName &
                                           dfMarkPos$markName %in% bandList),]$markPos

          if(length(allMarksPos)){

          allMarksSize <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                            dfMarkPos$chrName %in% chrName &
                                            dfMarkPos$markName %in% bandList),]$markSize

            for (i in 1:length(allMarksPos) ) {
              if (origin=="b"){

                if (markDistType=="beg"){
                   markPos[i] <- sum(chrSize, -1* allMarksPos[i], -1* allMarksSize[i], na.rm=TRUE)
                } else { # cen
                   markPos[i] <- chrSize - allMarksPos[i]
                }
              } else { #  b  t
                markPos[i] <- allMarksPos[i]
              }

            } # for mark
          remove(allMarksPos)
          } # any mark

          if (length(markPos) ) {
            # print(chrName)
            # posList[[s]][[as.character(chrName)]]$chrSize <- chrSize
            posList[[s]][[as.character(chrName)]][[1]]     <- markPos
            names(posList[[s]][[as.character(chrName)]])[1]<- paste0(bandListUpdate, collapse=",")
            posList[[s]][[as.character(chrName)]][[2]]     <- markPos/chrSize
            names(posList[[s]][[as.character(chrName)]])[2]<- paste0("frac",bandListUpdate, collapse=",")

            # posListTibb[[s]][1,as.character(chrName)][[1]] <- list(chrSize)
            posListTibb[[s]][1,as.character(chrName)][[1]] <- list(markPos/chrSize)
            posListTibb[[s]][2,as.character(chrName)][[1]] <- list(bandListUpdate)
            if(showBandList){
              posListTibb[[s]][3,as.character(chrName)][[1]] <- list(bandList)
            }
            remove(markPos)
          } # markPos

        } #for chr

      } # end mono holo

    names(posList)[s]     <- names(listOfdfChromSize[s])
    names(posListTibb)[s] <- names(listOfdfChromSize[s])

    } else { # dup False
      message(crayon::red(paste0("chrNames duplicated in: ",spname) ) )
    }
  } # for s

  dflist  <- lapply(posListTibb, function(x) as.data.frame(t(data.frame(x , check.names = F) ) ) )
  dflist2 <- lapply(dflist, function(x) {cbind(chrName=row.names(x),x)} )
  df3     <- plyr::rbind.fill(mapply( function(x,y) cbind(OTU=x,y), x = names(dflist2), y = (dflist2), SIMPLIFY = F ) )
  colnames(df3)[3:4] <- c("pos","markName")
  df3     <- df3[which(!sapply(df3[,3], is.null)) ,] # remove no-marks chr.
  df3     <- as.data.frame(df3 %>% unnest(cols = c(.data$pos, .data$markName)) )
  df3$poschar <- as.character(df3$pos)
  wide <- df3 %W>%
    pivot_wider(names_from = chrName, values_from = .data$pos)  %>%
    unnest(cols = everything() )
  first_c <- c("OTU","markName","poschar")
  otherc <- sort(setdiff(colnames(wide), first_c) )
  numeric_c <- as.character(sort(as.numeric(otherc[which(!is.na(suppressWarnings(as.numeric(otherc) ) ) ) ] ) ) )
  not_num   <- sort(otherc[which(is.na(suppressWarnings(as.numeric(otherc) ) ) ) ] )
  wide <- wide[,c("OTU","markName", numeric_c,not_num)]
  wide <- wide[order(wide$OTU, wide$markName ),]

  if(result=="tibble") {
    return(posListTibb)
  } else if(result=="data.frame") {
    return(wide)
  } else {
    return(posList)
  }
} # fun
#'
#' @rdname posCalc
#' @return data.frame of marks
#' @export

fillMarkInfo2 <- function(dfMarkPos, dfChrSize) {

if("OTU" %in% colnames(dfMarkPos)){
  listOfMarks<-base::split(dfMarkPos, factor(dfMarkPos[,"OTU"],levels = unique(dfMarkPos[,"OTU"])  ) )
  names(listOfMarks) <- unique(dfMarkPos$OTU)
} else {
  message(crayon::green("OTU column not found, adding"))
  listOfMarks <- list(dfMarkPos)
  names(listOfMarks)   <- 1
  listOfMarks[[1]]$OTU <- 1
  dfChrSize$OTU        <- 1
}

for( s in 1:length(listOfMarks) ) {
  listOfMarks[[s]][sapply(listOfMarks[[s]], function(x) all(is.na(x)))] <- NULL

  if("markDistCen" %in% colnames(listOfMarks[[s]] ) ) {
    listOfMarks[[s]]$markDistCen <- ifelse(is.na(listOfMarks[[s]]$markDistCen) &
                                           listOfMarks[[s]]$chrRegion %in% c("p","q"),
                                           0 ,
                                           listOfMarks[[s]]$markDistCen
    )

    for (m in 1:length(listOfMarks[[s]]$markSize) ) {
      if(is.na(listOfMarks[[s]]$markSize[m] ) ) {
        if(listOfMarks[[s]]$chrRegion[m]=="p" ) {
          listOfMarks[[s]]$markSize[m] <- dfChrSize[match(interaction(listOfMarks[[s]][m,c("OTU","chrName")] ),
                                                          interaction(dfChrSize[c("OTU","chrName")] )
          ),][,"shortArmSize"]
        } else if (listOfMarks[[s]]$chrRegion[m]=="q" ) {
          listOfMarks[[s]]$markSize[m] <- dfChrSize[match(interaction(listOfMarks[[s]][m,c("OTU","chrName")] ),
                                                          interaction(dfChrSize[c("OTU","chrName") ] )
          ),][,"longArmSize"]
        }
      }
    } #for
  } # if mdc
}
dfMarks2 <- plyr::rbind.fill(listOfMarks)
return(dfMarks2)
}
#'
#' @rdname posCalc
#' @return data.frame of marks
#' @export
fillMarkInfo <- function(dfMarkPos,dfChrSize
                         ,markDistType = "beg"
                         ,origin = "b") {
  dfMarkPosInternal <- dfMarkPos

  dfChrSizeInternal <- dfChrSize # 128

  dfMarkPosInternal[dfMarkPosInternal==""] <- NA

  copyDfMarkPosInternal1 <- dfMarkPosInternal <- makeNumCols(dfMarkPosInternal)

  if(is.null(copyDfMarkPosInternal1$markPos)){
    copyDfMarkPosInternal1$markPos<-NA
  }
  if(is.null(copyDfMarkPosInternal1$markSize)){
    copyDfMarkPosInternal1$markSize<-NA
  }
  if(is.null(copyDfMarkPosInternal1$markDistCen)){
    copyDfMarkPosInternal1$markDistCen<-NA
  }

  #
  # requires chrRegion

  if("chrRegion" %in% colnames(copyDfMarkPosInternal1) ) {

    dfCenMarksInternal <- copyDfMarkPosInternal1[which(copyDfMarkPosInternal1$chrRegion=="cen"),]

    if(nrow(dfCenMarksInternal)==0 ){
      remove(dfCenMarksInternal)
    }

    dfpGISHInternal <- copyDfMarkPosInternal1[which(copyDfMarkPosInternal1$chrRegion %in% "p" &
                                                      is.na(copyDfMarkPosInternal1$markSize) &
                                                      is.na(copyDfMarkPosInternal1$markDistCen)
    ),]
    if(nrow(dfpGISHInternal)==0 ){
      remove(dfpGISHInternal)
    }

    dfqGISHInternal <- copyDfMarkPosInternal1[which(copyDfMarkPosInternal1$chrRegion %in% "q" &
                                                      is.na(copyDfMarkPosInternal1$markSize) &
                                                      is.na(copyDfMarkPosInternal1$markDistCen)
    ),]
    if(nrow(dfqGISHInternal)==0 ){
      remove(dfqGISHInternal)
    }

    dfwholeGISHInternal <- copyDfMarkPosInternal1[which(copyDfMarkPosInternal1$chrRegion %in% "w" &
                                                          is.na(copyDfMarkPosInternal1$markSize) &
                                                          (is.na(copyDfMarkPosInternal1$markDistCen) |
                                                             is.na(copyDfMarkPosInternal1$markPos) )
    ),]

    if(nrow(dfwholeGISHInternal)==0 ){
      remove(dfwholeGISHInternal)
    }
  } else {
    remove(copyDfMarkPosInternal1) # absence of chrRegion
  }

  # } # df mark pos

  ##############################################################################
  #
  #   adds name of otu when missing 690
  #
  ##############################################################################

  listOfdfMarkPosInternal<-dfToListColumn(dfMarkPosInternal)

  # dfMarkPosInternal <- dplyr::bind_rows(listOfdfMarkPosInternal, .id = "OTU")

  dfMarkPosInternal <- suppressWarnings(bind_rows( (lapply(
    listOfdfMarkPosInternal, function(x) { mutate(x, across(.cols=everything(), as.character) ) } ) )
    ,.id = "OTU") )

  dfMarkPosInternal <- makeNumCols(dfMarkPosInternal)

  ### 725 743

  if (exists("dfCenMarksInternal")) {

    parlistOfdfMarkPosDataCen <- dfToListColumn(dfCenMarksInternal)

    # dfCenMarksInternal <- dplyr::bind_rows(parlistOfdfMarkPosDataCen, .id = "OTU")

    dfCenMarksInternal <- suppressWarnings(bind_rows( (lapply(
      parlistOfdfMarkPosDataCen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
      ,.id = "OTU") )

    dfCenMarksInternal <- makeNumCols(dfCenMarksInternal)

    # important has OTU column
    parlistOfdfMarkPosDataCen <- dfToListColumn(dfCenMarksInternal)
    # parlistOfdfMarkPosDataCen <- dfToListColumn(dfCenMarksInternal)
    # remove(parlistOfdfMarkPosDataCen) # diverges from pI

  } # df of marks

  cendfs <- mget(ls(pattern = "^dfCenMarksInternal" ) )

  if(length(cendfs) ) {

    # dfCenMarksInternal <- suppressWarnings(dplyr::bind_rows(cendfs) )

    dfCenMarksInternal <- suppressWarnings(bind_rows( (lapply(
      cendfs, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
    ) )

    dfCenMarksInternal <- makeNumCols(dfCenMarksInternal)

  }

  #800 add OTU

  listOfdfChromSize <- dfToListColumn(dfChrSizeInternal) # adds OTU as name of list

  # dfChrSizeInternal <- dplyr::bind_rows(listOfdfChromSize, .id = "OTU") # names of list to column
  dfChrSizeInternal <- suppressWarnings(bind_rows( (lapply(
    listOfdfChromSize, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
    , .id = "OTU") )

  dfChrSizeInternal <- makeNumCols(dfChrSizeInternal)


  for (i in 1:length(listOfdfChromSize)) {

    #
    # remove columns without info. per karyotype
    #

    listOfdfChromSize[[i]][listOfdfChromSize[[i]]==""]<-NA
    listOfdfChromSize[[i]]<-  listOfdfChromSize[[i]][, !apply(is.na(listOfdfChromSize[[i]]), 2, all)]

    # Does the data.frame have short and long info?
    message("\nChecking columns from listOfdfChromSize\n") # mess

    #################################################################################################
    #
    #   let see if it is monocen
    #

    if(length( setdiff(c("chrName", "shortArmSize","longArmSize"),
                       colnames(listOfdfChromSize[[i]]) ) )==0 ) {
      message("\nChecking mandatory columns from listOfdfChromSize for chr. with cen.: \nchrName, shortArmSize,longArmSize,\n (column OTU  is necessary if more than one species)\n"
      ) # mess
      message(crayon::green(paste("\nOTU ",names(listOfdfChromSize)[[i]],"has all columns with info to have monocen. If not, you have to clean your data"))
      )# message
      attr(listOfdfChromSize[[i]], 'cenType') <- "monocen"

    } # if monocen success

    ##############################################################################################3
    #   let see if it is holocen
    #

    else if(length( setdiff(c("chrName", "chrSize"),
                            colnames(listOfdfChromSize[[i]]) ) )==0 ){
      message("\nChecking mandatory columns from listOfdfChromSize for chr. without cen.: \nchrName, chrSize,\n (column OTU  is necessary if more than one species)\n"
      ) # mess
      message(crayon::green(paste(c("\nOTU ",names(listOfdfChromSize)[[i]]," has all columns with info to have holocen. If not, you have to clean your data")))
      ) # message
      attr(listOfdfChromSize[[i]], 'cenType') <- "holocen"
    }
  }

  {
    monocenNames<-makeVectorNames(listOfdfChromSize,"cenType","monocen")

    holocenNames<-makeVectorNames(listOfdfChromSize,"cenType","holocen")
  }

  #################################### 936

  if (exists("dfpGISHInternal")) {

    listOfdfpGISHInternal<-dfToListColumn(dfpGISHInternal)

    # monocen

    listOfdfpGISHInternalMonocen<-listOfdfpGISHInternal[which(names(listOfdfpGISHInternal) %in% monocenNames)]

    # names(listOfdfpGISHInternalMonocen)
    if(length(listOfdfpGISHInternalMonocen)==0){
      remove(listOfdfpGISHInternalMonocen)
    } else {
      listOfdfpGISHInternalMonocen<-Filter(function(x) {nrow(x) >= 1}, listOfdfpGISHInternalMonocen)

      # dfpGISHInternalMonocen <- dplyr::bind_rows(listOfdfpGISHInternalMonocen, .id = "OTU")

      dfpGISHInternalMonocen <- suppressWarnings(bind_rows( (lapply(
        listOfdfpGISHInternalMonocen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfpGISHInternalMonocen <- makeNumCols(dfpGISHInternalMonocen)

      # dfpGISHInternalMonocen$chrRegion<-"p"
    } # else

    # P marks of Holocen MUST NOt exist

    checkArmHolocenError(listOfdfpGISHInternal,holocenNames)

  } #   if (exists("dfpGISHInternal")){

  ##################################################################

  if (exists("dfqGISHInternal")){

    listOfdfqGISHInternal<-dfToListColumn(dfqGISHInternal)

    # monocen

    listOfdfqGISHInternalMonocen<-listOfdfqGISHInternal[which(names(listOfdfqGISHInternal) %in% monocenNames)]

    if(length(listOfdfqGISHInternalMonocen)==0){
      remove(listOfdfqGISHInternalMonocen)
    } else {

      listOfdfqGISHInternalMonocen <- Filter(function(x) {nrow(x) >= 1}, listOfdfqGISHInternalMonocen)

      # dfqGISHInternalMonocen <- dplyr::bind_rows(listOfdfqGISHInternalMonocen, .id = "OTU")

      dfqGISHInternalMonocen <- suppressWarnings(bind_rows( (lapply(
        listOfdfqGISHInternalMonocen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfqGISHInternalMonocen <- makeNumCols(dfqGISHInternalMonocen)

    } # else

    # q marks of Holocen MUST NOt exist

    checkArmHolocenError(listOfdfqGISHInternal,holocenNames)

  } #   if (exists("dfpGISHInternal")){

  ###########################################3

  if(exists("dfwholeGISHInternal")) {

    listOfdfwholeGISHInternal<-dfToListColumn(dfwholeGISHInternal)

    ###########################################################################################################################3
    #
    # MONOCEN GISH TO P Q CEN
    #

    listOfdfwholeGISHMonocen<-listOfdfwholeGISHInternal[which(names(listOfdfwholeGISHInternal) %in% monocenNames)]

    if(length(listOfdfwholeGISHMonocen)==0) {
      remove(listOfdfwholeGISHMonocen)
    } else {
      listOfdfwholeGISHMonocen <- Filter(function(x) {nrow(x) >= 1}, listOfdfwholeGISHMonocen)

      #
      #   p part
      #

      listOfdfpGISHInternalMonocen2 <- listOfdfwholeGISHMonocen

      # dfpGISHInternalMonocen2 <- dplyr::bind_rows(listOfdfpGISHInternalMonocen2, .id = "OTU")

      dfpGISHInternalMonocen2 <- suppressWarnings(bind_rows( (lapply(
        listOfdfpGISHInternalMonocen2, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfpGISHInternalMonocen2 <- makeNumCols(dfpGISHInternalMonocen2)

      dfpGISHInternalMonocen2$chrRegion     <- "p"
      dfpGISHInternalMonocen2$chrRegionOrig <- "w"

      #
      #   q part
      #

      listOfdfqGISHInternalMonocen2 <- listOfdfwholeGISHMonocen

      # dfqGISHInternalMonocen2 <- dplyr::bind_rows(listOfdfqGISHInternalMonocen2, .id = "OTU")

      dfqGISHInternalMonocen2 <- suppressWarnings(bind_rows( (lapply(
        listOfdfqGISHInternalMonocen2, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfqGISHInternalMonocen2 <- makeNumCols(dfqGISHInternalMonocen2)

      dfqGISHInternalMonocen2$chrRegion<-"q"
      dfqGISHInternalMonocen2$chrRegionOrig<-"w"

      #
      # cen part
      #

      listOfdfCenMarksInternal2 <- listOfdfwholeGISHMonocen

      # dfCenMarksInternal2 <- dplyr::bind_rows(listOfdfCenMarksInternal2, .id = "OTU")

      dfCenMarksInternal2<- suppressWarnings(bind_rows( (lapply(
        listOfdfCenMarksInternal2, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfCenMarksInternal2 <- makeNumCols(dfCenMarksInternal2)

      dfCenMarksInternal2$chrRegion<-"cen"

      # dfCenMarksInternal2$chrRegionOrig<-"w" leaving this hides w names completely in inline

      cendfs <- mget(ls(pattern = "^dfCenMarksInternal" ) )

      if(length(cendfs) ) {
        # dfCenMarksInternal <- suppressWarnings(dplyr::bind_rows(cendfs) )

        dfCenMarksInternal<- suppressWarnings(bind_rows((lapply(
          cendfs, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

        dfCenMarksInternal <- makeNumCols(dfCenMarksInternal)

      }

    } # else

    #
    # HOLOCEN
    #
    ###########################################################################################################

    listOfdfwholeGISHHolocen<-listOfdfwholeGISHInternal[which(names(listOfdfwholeGISHInternal) %in% holocenNames)]

    if(length(listOfdfwholeGISHHolocen)==0){
      remove(listOfdfwholeGISHHolocen)

    } else {

      # dfwholeGISHHolocen <- dplyr::bind_rows(listOfdfwholeGISHHolocen, .id = "OTU")

      dfwholeGISHHolocen<- suppressWarnings(bind_rows( (lapply(
        listOfdfwholeGISHHolocen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        ,.id = "OTU" ) )

      dfwholeGISHHolocen <- makeNumCols(dfwholeGISHHolocen)

      #
      # remake sizes df
      #

      dfwholeGISHHolocen$markSize <- dfChrSizeInternal[match(interaction(dfwholeGISHHolocen[c("OTU","chrName")] ),
                                                             interaction(dfChrSizeInternal[c("OTU","chrName") ] )
      ),]$chrSize
      #
      # dfwholeGISHHolocen$markSizeProtein<-dfwholeGISHHolocen$markSize-(dfwholeGISHHolocen$r2*2)
      #
      dfwholeGISHHolocen$markPos <- 0

      if(markDistType=="cen") { # center
        dfwholeGISHHolocen$markPos <- dfChrSizeInternal[match(interaction(dfwholeGISHHolocen[c("OTU","chrName")] ),
                                                              interaction(dfChrSizeInternal[c("OTU","chrName") ] )
        ),]$chrSize/2
      }

      # dfwholeGISHHolocen$markPosProtein <- dfwholeGISHHolocen$markPos + dfwholeGISHHolocen$r2


      #
      #   merge dfMarkPosInternal and dfwholeGISHHolocen
      #

      if(exists("dfMarkPosInternal") & exists("dfwholeGISHHolocen") ) {
        # dfMarkPosInternal <- dplyr::bind_rows(dfMarkPosInternal,dfwholeGISHHolocen)

        dfMarkPosInternal<- suppressWarnings(bind_rows((lapply(
          list(dfMarkPosInternal,dfwholeGISHHolocen), function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

        dfMarkPosInternal <- makeNumCols(dfMarkPosInternal)

      }
      if(!exists("dfMarkPosInternal") & exists("dfwholeGISHHolocen") ) {
        dfMarkPosInternal <- dfwholeGISHHolocen
      }

    } #     if(length(listOfdfwholeGISHHolocen)==0){

  }  #  end   if(exists("dfwholeGISHInternal")){

  #################################################################################################################3
  #
  #   merge p
  #

  #1220

  gishMonocenDfsP <- mget(ls(pattern = "^dfpGISHInternalMonocen" ) )

  if(length(gishMonocenDfsP) ) {
    # MdfpGISHInternalMonocen <- suppressWarnings(dplyr::bind_rows(gishMonocenDfsP) )

    MdfpGISHInternalMonocen<- suppressWarnings(bind_rows((lapply(
      gishMonocenDfsP, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    MdfpGISHInternalMonocen <- makeNumCols(MdfpGISHInternalMonocen)

  }

  if(exists("MdfpGISHInternalMonocen") ) {
    #
    #   divisor not used see 990
    #
    MdfpGISHInternalMonocen <- markDistCenGISHfix(MdfpGISHInternalMonocen,dfChrSizeInternal
                                                  ,"shortArmSize",markDistType="beg"
                                                  ,listOfdfChromSize,addR2=FALSE)
  } # p gish


  ############################################################################################
  # q

  gishMonocenDfsQ <- mget(ls(pattern = "^dfqGISHInternalMonocen" ) )

  if(length(gishMonocenDfsQ) ) {
    # MdfqGISHInternalMonocen <- suppressWarnings(dplyr::bind_rows(gishMonocenDfsQ) )

    MdfqGISHInternalMonocen<- suppressWarnings(bind_rows((lapply(
      gishMonocenDfsQ, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    MdfqGISHInternalMonocen <- makeNumCols(MdfqGISHInternalMonocen)

  }

  if(exists("MdfqGISHInternalMonocen") ) {
    #
    #   divisor not used see 990
    #
    MdfqGISHInternalMonocen <- markDistCenGISHfix(MdfqGISHInternalMonocen,dfChrSizeInternal
                                                  ,"longArmSize",markDistType="beg"
                                                  ,listOfdfChromSize,addR2 = FALSE)
  } # q gish

  ##################################################################################################
  #
  #       merging p and q
  #
  ##################################################################################################
  #
  gishMonocenDfsPQ <- mget(ls(pattern = "^Mdf" ) )

  if(length(gishMonocenDfsPQ) ) {
    # dfMarkPosInternal2 <- suppressWarnings(dplyr::bind_rows(gishMonocenDfsPQ) )

    dfMarkPosInternal2<- suppressWarnings(bind_rows((lapply(
      gishMonocenDfsPQ, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    dfMarkPosInternal2 <- makeNumCols(dfMarkPosInternal2)

  }

  #
  #    merge dfMarkPosInternal2 dfMarkPosInternal  dfMarkPosInternal3
  #

  mDfMarkPosI <- mget(ls(pattern = "^dfMarkPosInternal" ) )

  if(length(mDfMarkPosI) ) {
    #
    #   rev gish must be first to be background color
    #
    dfMarkPosInternal<- suppressWarnings(bind_rows(rev(lapply(
      mDfMarkPosI, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    dfMarkPosInternal <- makeNumCols(dfMarkPosInternal)

    # dfMarkPosInternal <- suppressWarnings(dplyr::bind_rows(rev(mDfMarkPosI) ) )
  }

  #
  #     DF OF marks to list
  #

  if (exists("dfMarkPosInternal") ) {

    dfMarkPosInternal <- unique(dfMarkPosInternal)

    listOfdfMarkPosInternal <- dfToListColumn(dfMarkPosInternal)

    #
    #              monocen marks list
    #

    parlistOfdfMarkPosMonocen <- listOfdfMarkPosInternal[which(names(listOfdfMarkPosInternal) %in% monocenNames)]

    if(length(parlistOfdfMarkPosMonocen)==0){
      remove(parlistOfdfMarkPosMonocen)
    } else {
      for (i in 1:length(parlistOfdfMarkPosMonocen)) {

        #
        #   requires chrRegion
        #
        missingCol <-setdiff(c("chrRegion"),
                             colnames(parlistOfdfMarkPosMonocen[[i]]) )
        if(length (missingCol )==0 ) {
          parlistOfdfMarkPosMonocen[[i]]  <- parlistOfdfMarkPosMonocen[[i]][which(parlistOfdfMarkPosMonocen[[i]]$chrRegion!="cen"),]
        } else {
          message(crayon::red("missing column chrRegion in dfMarkPos, unable to plot monocen. marks"
          ))
        }
      } # for

      parlistOfdfMarkPosMonocen<-Filter(function(x) {nrow(x) >= 1}, parlistOfdfMarkPosMonocen)

      if(length(parlistOfdfMarkPosMonocen)==0){
        remove(parlistOfdfMarkPosMonocen)
      }
    } # else

    # if(exists("parlistOfdfMarkPosMonocen")) {
    # }

    #
    #                holocen marks list
    #

    parlistOfdfMarkPosHolocen <- listOfdfMarkPosInternal[which(names(listOfdfMarkPosInternal) %in% holocenNames)]

    if(length(parlistOfdfMarkPosHolocen)==0){
      remove(parlistOfdfMarkPosHolocen)
    }

  } # end missing dfMarkPosInternal

  #
  #   for each d.f. of dfmarkpos check columns
  #

  ############################################################################################################################
  #
  #   Monocen check marks
  #

  if(exists("parlistOfdfMarkPosMonocen")) {
    message(
      "\nChecking mandatory columns from dfMarkPos: chrName, markName, chrRegion,markDistCen\n (column OTU  is necessary if more than one species)\nmarkSize can be absent when cM style"
    )# cat

    for (i in 1:length(parlistOfdfMarkPosMonocen ) ) {

      parlistOfdfMarkPosMonocen[[i]][parlistOfdfMarkPosMonocen[[i]]==""] <- NA
      parlistOfdfMarkPosMonocen[[i]] <- parlistOfdfMarkPosMonocen[[i]][, !apply(is.na(parlistOfdfMarkPosMonocen[[i]]), 2, all)]

      #
      #   rename column markpos if necessary
      #

      if(!"markDistCen" %in% colnames(parlistOfdfMarkPosMonocen[[i]]) & "markPos" %in% colnames(parlistOfdfMarkPosMonocen[[i]])  ){
        message(crayon::red(
          paste(c("Column markPos in d.f. of marks of OTU",names(parlistOfdfMarkPosMonocen)[[i]]
                  ,"renamed to markDistCen")))
        ) # mess
        colnames(parlistOfdfMarkPosMonocen[[i]])[which(names(parlistOfdfMarkPosMonocen[[i]])=="markPos")]<-"markDistCen"
      }

      #
      #   REMOVE GISH DATA incomplete duplicated data
      #

      parlistOfdfMarkPosMonocen[[i]] <- parlistOfdfMarkPosMonocen[[i]][setdiff(1:length(parlistOfdfMarkPosMonocen[[i]]$chrRegion),
                                                                               which(parlistOfdfMarkPosMonocen[[i]]$chrRegion %in% "p" &
                                                                                       is.na(parlistOfdfMarkPosMonocen[[i]]$markSize) &
                                                                                       is.na(parlistOfdfMarkPosMonocen[[i]]$markDistCen)
                                                                               ) ) ,]

      parlistOfdfMarkPosMonocen[[i]] <- parlistOfdfMarkPosMonocen[[i]][setdiff(1:length(parlistOfdfMarkPosMonocen[[i]]$chrRegion),
                                                                               which(parlistOfdfMarkPosMonocen[[i]]$chrRegion %in% "q" &
                                                                                       is.na(parlistOfdfMarkPosMonocen[[i]]$markSize) &
                                                                                       is.na(parlistOfdfMarkPosMonocen[[i]]$markDistCen)
                                                                               ) ) ,]

      parlistOfdfMarkPosMonocen[[i]] <- parlistOfdfMarkPosMonocen[[i]][setdiff(1:length(parlistOfdfMarkPosMonocen[[i]]$chrRegion),
                                                                               which(parlistOfdfMarkPosMonocen[[i]]$chrRegion %in% "w"
                                                                               ) ) ,]

      #
      #   column error check
      #

      missingCol<-setdiff(c("chrName", "markName", "chrRegion","markDistCen"),
                          colnames(parlistOfdfMarkPosMonocen[[i]]) )

      if(length (missingCol )>0 ) {
        message(crayon::red(paste(c("ERROR Missing columns in d.f. of marks of OTU"
                                    ,names(parlistOfdfMarkPosMonocen)[[i]] ,":"
                                    ,missingCol) , sep="\n", collapse = " "
        )
        )
        ) # cat
        message(crayon::red(paste("\nERRORS PRESENT, see above, dfMarksPos of OTU"
                                  , names(parlistOfdfMarkPosMonocen)[[i]]
                                  ,"REMOVED\n")
        ) ) #m
        parlistOfdfMarkPosMonocen[[i]]<-NA
      } # fi setdiff
      #
      #   column without error
      #
      else { # if no error

        if(markDistType=="cen") { # this is from center
          #
          #   fix bug when markDistType is cen (center) but cM style of marks have NA in markSize column
          #
          # halfMarkSize<-ifelse(is.na(parlistOfdfMarkPosMonocen[[i]]$markSize/2),0,(parlistOfdfMarkPosMonocen[[i]]$markSize/2) )
          if("markSize" %in% colnames(parlistOfdfMarkPosMonocen[[i]])){
            parlistOfdfMarkPosMonocen[[i]]$markDistCen <- psum(parlistOfdfMarkPosMonocen[[i]]$markDistCen,
                                                               ( - parlistOfdfMarkPosMonocen[[i]]$markSize/2),
                                                               na.rm=TRUE)
          }
        } # if
      } # else No Error
    } # for each data.frame of Marks of Monocen

    parlistOfdfMarkPosMonocen<-parlistOfdfMarkPosMonocen[!is.na(parlistOfdfMarkPosMonocen)]
    # do as before with holo 27/09
  } # fi parlistOfdfMarkPosMonocen

  ##################################################################################################################
  #
  #   holocen check mark 1517
  #

  if(exists("parlistOfdfMarkPosHolocen")){
    message("\nChecking mandatory columns from dfMarkPos (without cen.): chrName, markName, markPos\n (column OTU  is necessary if more than one species)\nmarkSize column is not necessary for style of mark cM"
    ) # mess

    for (i in 1:length(parlistOfdfMarkPosHolocen ) ) {

      parlistOfdfMarkPosHolocen[[i]][parlistOfdfMarkPosHolocen[[i]]==""]<-NA
      parlistOfdfMarkPosHolocen[[i]]<-  parlistOfdfMarkPosHolocen[[i]][, !apply(is.na(parlistOfdfMarkPosHolocen[[i]]), 2, all)]

      #
      #   REMOVE GISH DATA incomplete duplicated data
      #

      parlistOfdfMarkPosHolocen[[i]] <- parlistOfdfMarkPosHolocen[[i]][setdiff(1:length(parlistOfdfMarkPosHolocen[[i]]$chrName),
                                                                               which(parlistOfdfMarkPosHolocen[[i]]$chrRegion %in% "w" &
                                                                                       is.na(parlistOfdfMarkPosHolocen[[i]]$markSize )
                                                                               ) ) ,]

      #
      #   rename column markdistcen if necessary
      #

      if(!"markPos" %in% colnames(parlistOfdfMarkPosHolocen[[i]]) & "markDistCen" %in% colnames(parlistOfdfMarkPosHolocen[[i]])  ){
        message(crayon::red(paste(c("Columns markDistCen in d.f. of marks of OTU",names(parlistOfdfMarkPosHolocen)[[i]] ,"renamed to markPos")))
        ) # mess
        colnames(parlistOfdfMarkPosHolocen[[i]])[which(names(parlistOfdfMarkPosHolocen[[i]])=="markDistCen")]<-"markPos"
      }

      #
      #   column error
      #

      # if(length (setdiff(c("chrName", "markName", "markPos","markSize"),
      if(length (setdiff(c("chrName", "markName", "markPos"),
                         colnames(parlistOfdfMarkPosHolocen[[i]]) ) )>0 ){
        message(crayon::red(paste(c("ERROR Missing columns:",
                                    # setdiff(c("chrName", "markName", "markPos","markSize"),
                                    setdiff(c("chrName", "markName", "markPos"),
                                            colnames(parlistOfdfMarkPosHolocen[[i]]) ) ) , sep="\n", collapse = " " )
        )
        ) # cat
        message(crayon::red(paste("\nERRORS PRESENT, see above, dfMarksPos of OTU", names(parlistOfdfMarkPosHolocen)[[i]] ,"REMOVED\n")
        ) ) #m
        # parlistOfdfMarkPosHolocen<-parlistOfdfMarkPosHolocen[-i]
        parlistOfdfMarkPosHolocen[[i]]<-NA
      } # fi
      #
      #   column without error
      #

      else { # if no error
        message(paste("\nOK marks of OTU",names(parlistOfdfMarkPosHolocen)[[i]],"checked \n")
        )  #m
        if(any(is.na(parlistOfdfMarkPosHolocen[[i]]$markPos))){
          message(crayon::blue(paste("\nholocen. mark(s) without pos. might get unexpected results\n")
          ))
        }
        if(origin=="t"){
          parlistOfdfMarkPosHolocen[[i]]$markPos2<-parlistOfdfMarkPosHolocen[[i]]$markPos
          parlistOfdfMarkPosHolocen[[i]]$chrSize<-
            dfChrSizeInternal[match(interaction( parlistOfdfMarkPosHolocen[[i]][c("OTU", "chrName")]), # divisor
                                    interaction( dfChrSizeInternal[c("OTU", "chrName")] )# divisor
            ),]$chrSize

          if(markDistType=="beg"){
            if("markSize" %in% colnames(parlistOfdfMarkPosHolocen[[i]])){
              # markSize2<-ifelse(is.na( parlistOfdfMarkPosHolocen[[i]]$markSize  ) ,0, ( parlistOfdfMarkPosHolocen[[i]]$markSize ) )
              parlistOfdfMarkPosHolocen[[i]]$markPos<- psum(parlistOfdfMarkPosHolocen[[i]]$chrSize,
                                                            - parlistOfdfMarkPosHolocen[[i]]$markPos2,
                                                            - parlistOfdfMarkPosHolocen[[i]]$markSize,
                                                            na.rm=TRUE)
            } # markSize column exist

          } else if(markDistType=="cen"){
            if("markSize" %in% colnames(parlistOfdfMarkPosHolocen[[i]])){
              # halfMarkSize<-ifelse(is.na(parlistOfdfMarkPosHolocen[[i]]$markSize/2) ,0, ( (parlistOfdfMarkPosHolocen[[i]]$markSize/2) ) )
              parlistOfdfMarkPosHolocen[[i]]$markPos<-psum( parlistOfdfMarkPosHolocen[[i]]$chrSize,
                                                            - parlistOfdfMarkPosHolocen[[i]]$markPos2,
                                                            (- parlistOfdfMarkPosHolocen[[i]]$markSize/2),
                                                            na.rm=TRUE)
            } # col markSize exists
          } # cen

        } else if (origin=="b") { # if t else b

          if(markDistType=="cen") { # center
            if("markSize" %in% colnames(parlistOfdfMarkPosHolocen[[i]])){

              parlistOfdfMarkPosHolocen[[i]]$markPos <- psum(parlistOfdfMarkPosHolocen[[i]]$markPos,
                                                             (- parlistOfdfMarkPosHolocen[[i]]$markSize/2),
                                                             na.rm=TRUE)
            } # if col markSize exist
          } # cen
        } # origin b
      } # else No Error
    } # for each data.frame of Marks of Monocen

    parlistOfdfMarkPosHolocen<-parlistOfdfMarkPosHolocen[!is.na(parlistOfdfMarkPosHolocen)]

  } # fi holocen exists



  ################################################################################################################################
  #
  #   cen Mark check
  #

  if(exists("parlistOfdfMarkPosDataCen")) {
    message("\nChecking mandatory columns from dfCenMarks: chrName, markName\n (column OTU  is necessary if more than one species)\n")


    for (i in 1:length(parlistOfdfMarkPosDataCen)){
      #
      #   columns with error
      #

      if(length(setdiff(c("chrName", "markName"),
                        colnames(parlistOfdfMarkPosDataCen[[i]]) ) )>0 ){
        message(crayon::red(paste(c("ERROR Missing columns:",
                                    setdiff(c("chrName", "markName"),
                                            colnames(parlistOfdfMarkPosDataCen[[i]]) ),"in OTU", names(parlistOfdfMarkPosDataCen)[[i]]   ), sep="\n", collapse = " " )
        )
        ) # cat
        message(crayon::red(paste("\nERRORS PRESENT, see above, dfCenMarks of OTU", names(parlistOfdfMarkPosDataCen)[[i]] ,"REMOVED\n")
        ) ) #m
        parlistOfdfMarkPosDataCen[[i]] <- NA

      } # fi

      #
      #   columns without error
      #

      else { # if no error
        message(paste("\nOK cen. marks of OTU",names(parlistOfdfMarkPosDataCen)[[i]],"checked \n")
        ) # mess
      } # else
    } # for

    parlistOfdfMarkPosDataCen<-parlistOfdfMarkPosDataCen[!is.na(parlistOfdfMarkPosDataCen)]

  } # fi   if(exists("parlistOfdfMarkPosDataCen"))

  ##############################################################################################################
  #
  #   OTU cross check of d.fs
  #

  if(exists("parlistOfdfMarkPosMonocen")){

    parlistOfdfMarkPosMonocen<- filterExtraOTU(listOfdfChromSize,parlistOfdfMarkPosMonocen)

  } # exists

  if(exists("parlistOfdfMarkPosHolocen")){
    # message("\n####\ndfMarkPos exists, if error will be removed\n")

    parlistOfdfMarkPosHolocen<- filterExtraOTU(listOfdfChromSize,parlistOfdfMarkPosHolocen)

  } # exists

  #
  #     check chromosomes names  from d.f. marks to chr. size. d.f.
  #

  if(exists("parlistOfdfMarkPosMonocen") ) {

    listOfChecksChr   <-checkNameChrDfMarks(listOfdfChromSize,parlistOfdfMarkPosMonocen)
    listOfdfChromSize <-listOfChecksChr[[1]]

    parlistOfdfMarkPosMonocen <-listOfChecksChr[[2]]

    if(length(parlistOfdfMarkPosMonocen)==0){
      remove(parlistOfdfMarkPosMonocen)
    } else {

      #
      #  allMarkNames creation
      #

      allMarkNames <- unique(listOfChecksChr[[3]])


      allMarkNamesInProtein <- allMarkNames[which(allMarkNames %in% grep("inProtein", allMarkNames
                                                                         , value=TRUE, invert = FALSE) ) ]

      markNamesCentromere <- allMarkNamesInProtein[
        which(allMarkNamesInProtein %in% grep("inProteinCentromere", allMarkNamesInProtein
                                              , value=TRUE, invert = FALSE) ) ]

      allMarkNamesInProtein <- allMarkNamesInProtein[
        which(allMarkNamesInProtein %in% grep("inProteinCentromere", allMarkNamesInProtein
                                              , value=TRUE, invert = TRUE) ) ]

      allMarkNames <- allMarkNames[which(allMarkNames %in% grep("inProtein", allMarkNames
                                                                , value=TRUE, invert = TRUE) ) ]

      if(exists("allMarkNames")) {if(!length(allMarkNames)){remove(allMarkNames)} }

      if(length(listOfChecksChr[[4]])>0) {
        allMarkMaxSize<-max(listOfChecksChr[[4]], na.rm=TRUE)
      }
    }
  } # parlistOfdfMarkPosMonocen


  if(exists("parlistOfdfMarkPosHolocen") ) {

    listOfChecksChr  <-checkNameChrDfMarks(listOfdfChromSize,parlistOfdfMarkPosHolocen)
    listOfdfChromSize<-listOfChecksChr[[1]]

    parlistOfdfMarkPosHolocen<-listOfChecksChr[[2]]

    if(length(parlistOfdfMarkPosHolocen)==0){
      remove(parlistOfdfMarkPosHolocen)
    } else {

      if(exists("allMarkNames")) {
        allMarkNames<-unique(c(allMarkNames,listOfChecksChr[[3]] ) )
      } else {
        allMarkNames<-unique(listOfChecksChr[[3]] )
      }

      allMarkNamesInProtein2 <- allMarkNames[which(allMarkNames %in% grep("inProtein", allMarkNames
                                                                          , value=TRUE, invert = FALSE) ) ]

      aMNList <- ls(pattern = "^allMarkNamesInProtein" )

      if(length(aMNList)){
        aMNList <- lapply(mget(aMNList ), function(x) unname(x) )
        allMarkNamesInProtein <- suppressWarnings(unlist(aMNList) )
        remove(allMarkNamesInProtein2)
      }

      allMarkNames <- allMarkNames[which(allMarkNames %in% grep("inProtein", allMarkNames
                                                                , value=TRUE, invert = TRUE) ) ]

      if(exists("allMarkNames")) {if(!length(allMarkNames)) {remove(allMarkNames)} }

      if(length(listOfChecksChr[[4]])>0){
        if (exists("allMarkMaxSize")){
          allMarkMaxSize<-max(c(allMarkMaxSize,max(listOfChecksChr[[4]], na.rm=TRUE) ), na.rm=TRUE)
        } else {
          allMarkMaxSize<-max(listOfChecksChr[[4]], na.rm=TRUE)
        }
      }
    }
  } # holocen

  if(exists("parlistOfdfMarkPosDataCen") ) {

    listOfChecksChr  <- checkNameChrDfMarks(listOfdfChromSize,parlistOfdfMarkPosDataCen)

    listOfdfChromSize<- listOfChecksChr[[1]]

    parlistOfdfMarkPosDataCen<-listOfChecksChr[[2]]

    if(length(parlistOfdfMarkPosDataCen)==0){
      remove(parlistOfdfMarkPosDataCen)
    } else {

      if(exists("allMarkNames")) {

        allMarkNames <-unique(c(allMarkNames,listOfChecksChr[[3]] ) )
      } else {
        allMarkNames <-unique(listOfChecksChr[[3]])
      }

      allMarkNamesInProtein3 <- allMarkNames[which(allMarkNames %in% grep("inProtein", allMarkNames
                                                                          , value=TRUE, invert = FALSE) ) ]

      allMarkNamesInProtein3 <- allMarkNamesInProtein3[
        which(allMarkNamesInProtein3 %in% grep("inProteinCentromere", allMarkNamesInProtein3
                                               , value=TRUE, invert = TRUE) ) ]

      aMNList <- ls(pattern = "^allMarkNamesInProtein" )

      #
      # last AMNIP
      #

      if(length(aMNList)){
        aMNList <- lapply(mget(aMNList ), function(x) unname(x) )
        allMarkNamesInProtein <- suppressWarnings(unlist(aMNList) )
        if(length(allMarkNamesInProtein)==0){
          remove(allMarkNamesInProtein)
        }
        remove(allMarkNamesInProtein3)
      }

      if(exists("allMarkNames")) {if(!length(allMarkNames)){remove(allMarkNames)} }

      cenMarkNames <- unique(listOfChecksChr[[3]])

    } # parlistOfdfMarkPosDataCen 0
  } # parlistOfdfMarkPosDataCen

  ###############################################################################
  #
  #   remake dfMarkPosInternal (for per. mark) after filtering from lists
  #

  mlists <- ls(pattern = "^parlistOfdfMarkPos" )

  if(length(mlists)) {

    plist <- lapply(mget(mlists ), function(x) unname(x) )

    #
    #   last dfMarkPosInternal
    #
    # dfMarkPosInternal <- suppressWarnings(dplyr::bind_rows(plist) )

    plist <- plyr::rbind.fill(lapply(plist, plyr::rbind.fill) )

    dfMarkPosInternal <- makeNumCols(plist)

  }
  return(dfMarkPosInternal)
}
