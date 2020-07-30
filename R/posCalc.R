#' @name posCalc
#' @title FUNCTION posCalc
#' @description calculates position of marks in fraction of (%) chromosome units (0-1)
#'
#' @param dfMarkPos data.frame of marks' position
#' @param listOfdfChromSize list data.frames of chr. sizes. Require col. \code{chrSize} for all. Use \code{armRatioCI}
#' @param markDistType markDistType character, if \code{"cen"} = the distance you provided in data.frame (\code{dfMarkPos}) column \code{markDistCen}
#' or \code{markPos}  is to the center of the mark, if \code{"beg"} = the distance you provided is to the
#'   beginning of the mark (Default)
#' @param bToRemove, character, bands to remove from calc. of pos.
#' @param origin, For non-monocentric chr. (for holocentrics only) Use \code{"b"} (default) if distance to mark in (\code{"markPos"} column in \code{"dfMarkPos"}) data.frame measured from bottom of chromosome, use \code{"t"} for distance to mark from top of chr.
#' @param result character, use \code{"tibble"} to get results in tibble, other string results in a \code{list}
#'
#' @keywords position mark chromosome fraction
#' @examples
#' # Monocentrics - Beginning with two data.frames with column OTU
#' require(idiogramFISH)
#'
#' dfOfChrSize$OTU<-1
#' dfOfMarks2$OTU<-1
#'
#' # check missing data
#'
#' dfOfMarks2[which(is.na(dfOfMarks2$markSize) & dfOfMarks2$chrRegion %in% c("p","q") ),]
#' # markSize and markDistCen missing
#'
#' # add markDistCen
#' dfOfMarks2$markDistCen <- ifelse( is.na(dfOfMarks2$markDistCen) &
#' dfOfMarks2$chrRegion %in% c("p","q"),
#' 0,
#' dfOfMarks2$markDistCen)
#'
#' # add markSize
#' XshortSize <- dfOfChrSize[which(dfOfChrSize$chrName=="X"),]$shortArmSize
#' dfOfMarks2[which(dfOfMarks2$chrName=="X" & dfOfMarks2$chrRegion=="p")  ,]$markSize<-XshortSize
#'
#' # add column chrSize if missing
#' dfOfChrSizeWithChrSize <- armRatioCI(dfOfChrSize)
#'
#' # data.frame of chr. size to list
#' listOfdfChr <- idiogramFISH:::dfToListColumn(dfOfChrSizeWithChrSize)
#'
#' ti<-posCalc(dfOfMarks2,listOfdfChr)
#' as.list(ti[[1]][,1])
#' posCalc(dfOfMarks2,listOfdfChr, result="list")
#'
#' # holocentrics example
#'
#' dfChrSizeHolo$OTU<-2
#' dfMarkPosHolo$OTU<-2
#'
#' # data.frame of chr. size to list
#' listOfdfChrHolo <- idiogramFISH:::dfToListColumn(dfChrSizeHolo)
#'
#' ti2<-posCalc(dfMarkPosHolo,listOfdfChrHolo)
#' ti2
#'
#' posCalc(dfMarkPosHolo,listOfdfChrHolo, result="list")
#'
#' as.list(ti[[1]][,1] )
#'
#' @return list, tibble
#' @rdname posCalc
#' @importFrom tidyr as_tibble
#' @export
#'
#'
posCalc <- function(dfMarkPos,listOfdfChromSize,bToRemove="",markDistType="beg",origin="b", result="tibble"  ) {

  posListTibb<-posList<-list()
  bandList <- unique(dfMarkPos$markName)
  bandList<- setdiff(bandList,bToRemove)

  for (s in 1:length(listOfdfChromSize) ) {

    spname <- names(listOfdfChromSize)[s]

    posListTibb[[s]] <- tidyr::as_tibble(sapply(as.character(listOfdfChromSize[[s]]$chrName), function(x) list() ))

    posList[[s]]<-list()

    if("shortArmSize" %in% colnames(listOfdfChromSize[[s]] ) ) {
      # chrName<-"3"
      for (chrName in listOfdfChromSize[[s]]$chrName ) {

        chrSize <- listOfdfChromSize[[s]][which(listOfdfChromSize[[s]]$chrName %in% chrName ),]$chrSize

          shortArmSize<-listOfdfChromSize[[s]][which(listOfdfChromSize[[s]]$chrName %in% chrName ),]$shortArmSize
          longArmSize <-listOfdfChromSize[[s]][which(listOfdfChromSize[[s]]$chrName %in% chrName ),]$longArmSize

          markPos<-numeric()

          allMarksPos<-numeric()

          bandListUpdate <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                             dfMarkPos$chrName %in% chrName &
                                             dfMarkPos$markName %in% bandList),]$markName

          allMarksPos <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                        dfMarkPos$chrName %in% chrName &
                                        dfMarkPos$markName %in% bandList),]$markDistCen

          if(length(allMarksPos) ) {

          allMarksSize <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                           dfMarkPos$chrName %in% chrName &
                                           dfMarkPos$markName %in% bandList),]$markSize

          chrRegion<-character()
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
              markPos[i] <- shortArmSize - allMarksPos[i]
              }

            } else if (chrRegion[i]=="q"){
              markPos[i] <- chrSize - longArmSize + allMarksPos[i]

            } else if (chrRegion[i]=="cen"){
              markPos[i] <- shortArmSize
            }
          } # for
          remove(allMarksPos)
          } # if any mark

          if (length(markPos) ) {
            # print(chrName)
            # posList[[s]][[as.character(chrName)]]$chrSize <- chrSize
            posList[[s]][[as.character(chrName)]][[1]] <- markPos
            names(posList[[s]][[as.character(chrName)]])[1]<-paste0(bandListUpdate, collapse=",")
            posList[[s]][[as.character(chrName)]][[2]] <- markPos/chrSize
            names(posList[[s]][[as.character(chrName)]])[2]<-paste0("frac",bandListUpdate, collapse=",")

            # posListTibb[[s]][1,as.character(chrName)][[1]] <- list(chrSize)
            posListTibb[[s]][1,as.character(chrName)][[1]]<-list(markPos/chrSize)
            posListTibb[[s]][2,as.character(chrName)][[1]]<-list(bandListUpdate)
            posListTibb[[s]][3,as.character(chrName)][[1]]<-list(bandList)
            remove(markPos)
          } # markPos

        } #for chr

      } else {# if monocen else holocen

        for (chrName in listOfdfChromSize[[s]]$chrName ) {

          chrSize <- listOfdfChromSize[[s]][which(listOfdfChromSize[[s]]$chrName %in% chrName ),]$chrSize

          markPos <-numeric()

          allMarksPos <-numeric()

          bandListUpdate <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
                                              dfMarkPos$chrName %in% chrName &
                                              dfMarkPos$markName %in% bandList),]$markName


          allMarksPos <- dfMarkPos[which(dfMarkPos$OTU %in% spname &
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
            posList[[s]][[as.character(chrName)]][[1]] <- markPos
            names(posList[[s]][[as.character(chrName)]])[1]<-paste0(bandListUpdate, collapse=",")
            posList[[s]][[as.character(chrName)]][[2]] <- markPos/chrSize
            names(posList[[s]][[as.character(chrName)]])[2]<-paste0("frac",bandListUpdate, collapse=",")

            # posListTibb[[s]][1,as.character(chrName)][[1]] <- list(chrSize)
            posListTibb[[s]][1,as.character(chrName)][[1]]<-list(markPos/chrSize)
            posListTibb[[s]][2,as.character(chrName)][[1]]<-list(bandListUpdate)
            posListTibb[[s]][3,as.character(chrName)][[1]]<-list(bandList)
            remove(markPos)
          } # markPos

        } #for chr

      } # end mono holo

    names(posList)[s] <- names(listOfdfChromSize[s])

  } # for s

  if(result=="tibble"){
    return(posListTibb)
  } else {
    return(posList)
  }
} # fun
