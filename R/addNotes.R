# FUNCTIONS: addnotes
#' This is an internal function that adds notes
#'
#' @keywords internal
#'
#' @param notes x component of polygon
#' @param listOfdfChromSizenoNA y component of polygon
#' @param groupName y of chr
#' @param indexCount colors for marks
#' @param morphoCount list of df. of mark pos.
#' @param xmnoNA thick of border line
#' @param ymnoNA boolean
#' @param distTextChr numeric
#' @param chrIdCount numeric
#' @param notesPosX numeric
#' @param notesPosY numeric
#' @param notesTextSize numeric
#' @param defaultFontFamily2 character, regex
#' @param downNote numeric
#'
#' @return plot
#' @importFrom graphics text
#'

addNotes<-function(notes,listOfdfChromSizenoNA, groupName,indexCount,morphoCount,
                     xmnoNA,ymnoNA,distTextChr,chrIdCount, notesPosX,notesPosY,
                     notesTextSize,defaultFontFamily2,
                     OTUasNote, OTUfont, OTUfamily,
                     downNote,rightN,
                     leftNoteFont,
                     leftNoteFontUp,
                     noteFont,
                   parseTypes,
                   parseStr2lang){

if(!inherits(notes, "data.frame") ) {
      message(crayon::blurred("notes are not in a data.frame, ignoring notes"))
} else {

for (i in 1:length(listOfdfChromSizenoNA) ) {

if(rightN==FALSE){

  font1   <- ifelse(downNote, leftNoteFont,leftNoteFontUp)
  family1 <- defaultFontFamily2

  if(as.numeric(attr(listOfdfChromSizenoNA[[i]],"groupPresence") ) > 0 ) {
      ifelse(groupName,groupCount<-2,groupCount<-1)
  } else {
      groupCount=0
  } # end ifelse

      if(attr(listOfdfChromSizenoNA[[i]],"indexStatus")=="failure") {indexCount2=indexCount*0} else {indexCount2=indexCount}
      if(attr(listOfdfChromSizenoNA[[i]],"indexStatus")=="failure") {morphoCount2=morphoCount*0} else {morphoCount2=morphoCount}

  if(attr(xmnoNA[[i]],"cenType")=="holocen") {
        holocenDisCount <- morphoCount2 + indexCount2 #newDistVector #+bothAdd
      } else {
        holocenDisCount <- 0

  } # ifelse holocen


  # distTextChr<<-distTextChr
  # chrIdCount<<-chrIdCount
  # morphoCount2<<-morphoCount2
  # indexCount2<<-indexCount2
  # groupCount<<-groupCount
  # holocenDisCount<<-holocenDisCount

      if(downNote){
        yposNote <- min(ymnoNA[[i]]) - ( (distTextChr/3) * (chrIdCount + morphoCount2 + indexCount2 + groupCount + 3 - holocenDisCount) )
        mySign<-1
      } else {
        yposNote <- max(ymnoNA[[i]])
        mySign<- -1
      }

      xposNote <- min(xmnoNA[[i]]) + ((notesPosX/2) )

} else { # RIGHT NOTE TRUE

  font1   <- ifelse(OTUasNote, OTUfont, noteFont)
  family1 <- ifelse(OTUasNote, ifelse( !is.na(OTUfamily),  OTUfamily, defaultFontFamily2),defaultFontFamily2) # justif
  mySign <- 1

   if(attr(listOfdfChromSizenoNA[[i]],"cenType")=="monocen"){
      yposNote<-min(ymnoNA[[i]][,1])
    } else {
      yposNote<-(max(ymnoNA[[i]]) + min(ymnoNA[[i]]) ) /2
    }
    xposNote<-max(xmnoNA[[i]]) + (notesPosX/2)
}

 note <- notes[which(notes$OTU %in% names(listOfdfChromSizenoNA)[i] ), ]$note

 suppressWarnings(remove(nameWithVar))

 if(!is.null(note) & length(note)>0 ) {

        hasQuotes<-grepl("\\((.*?)\\)|'(.*?)'",note)

        #
        # CHECK  for need for italics
        #

        fbool<- rightN==FALSE & downNote & leftNoteFont==3
        sbool<- rightN==FALSE & downNote==FALSE & leftNoteFontUp==3
        tbool<- rightN        & downNote==FALSE & noteFont==3
        f2bool<- rightN        & downNote==FALSE & OTUfont==3 & OTUasNote

        if( fbool | sbool | tbool | f2bool ) if(parseStr2lang==FALSE) if(hasQuotes) {

              pattern1<-"\\s?\\((.*?)\\)\\s?|\\s?'(.*?)'\\s?"
              posPattern <- gregexpr(pattern1, note)

              notInParOrig <-   unlist(regmatches(note, posPattern, invert=T ) )
              notInPar <- notInParOrig[notInParOrig!=""]

              pattern2<-"\\((.*?)\\)|'(.*?)'"
              varNames    <- unlist(regmatches(note,gregexpr(pattern2,note ) ) ) # "\\((.*?)\\)|'(.*?)'"
              #
              # non pat to X
              #
              regmatches(note, posPattern, invert=T) <- Map(blanks2, lapply(regmatches(note, posPattern, invert = T), nchar) )
              #
              # add
              #
              regmatches(note, posPattern, invert=F) <-  list(paste0(',\"',varNames," \"")  )
              #
              # add
              #
              posXNotPattern2 <- gregexpr("[\u0168]+", note)
              regmatches(note, posXNotPattern2, invert=F) <-  list(paste0(',italic(\"',notInPar," \")" ) )
              #
              #   end
              #
              # nameWithVar<-paste0("paste(",note,")")
              nameWithVar<-str2lang(paste0("paste(",note,")") )

        }

        #
        #   type patterns
        #

        if( parseTypes & parseStr2lang==FALSE ) {
          if(exists("nameWithVar") ){
            italics1<-TRUE
          } else {
            italics1<-FALSE
          }

        # note<- "2A + 1C + 8D + 2F+ + 3F + 2FL+ [2A/35S + 1D/35S]"

        pattern1 <- "\\s?[0-9]{1,2}[A-Za-z]{1,2}[0+]{0,1}\\s?"
        posPattern <- gregexpr(pattern1, note)

        notInParOrig <-   unlist(regmatches(note, posPattern, invert=T ) )
        notInPar <- notInParOrig[notInParOrig!=""]
        notInPar <- sub("^\\["," [", notInPar)

        if(italics1==FALSE){
          notInParPl<-paste0(",plain(\"",notInPar,"\"),")
        } else {
          notInParPl<-paste0(",\"",notInPar,"\",")
          notInParPl<-gsub("^,\",ita",",ita",notInParPl)
        }

        #
        #   types
        #

        pattern2<-"[0-9]{1,2}[A-Za-z]{1,2}[0+]{0,1}"

        varNames    <- unlist(regmatches(note,gregexpr(pattern2,note ) ) ) # "\\((.*?)\\)|'(.*?)'"

        common  <- varNames[which(!varNames %in% grep("FL|F\\+", varNames, value=TRUE) )]

        # common  <- varNames[which(!varNames %in% grep("FL", varNames, value=TRUE) )]

        uncommon  <- varNames[which(varNames %in% grep("FL",   varNames, value=TRUE) )]

        uncommonF <- varNames[which(varNames %in% grep("F\\+", varNames, value=TRUE) )]

        commonPos    <- grep("FL|F\\+", varNames, invert = T)

        uncommonPos  <- grep("FL",   varNames)

        uncommonPosF <- grep("F\\+", varNames)

        if( length(uncommonPos) | length(uncommonPosF) ) {

        splUncommon <- strsplit(uncommon,"")

        splUncommonF <- strsplit(uncommonF,"")

        vecFL<-character()
        for (sp in splUncommon){
          vecFL<-c(vecFL,paste0(",\" ",sp[1],"\",","\"",sp[2],"\"","[","\"",sp[3],"\"","]^\"",ifelse(sp[4]==" "|is.na(sp[4]),"",sp[4] ),"\"" )  )
        }

        vecF<-character()
        for (sp in splUncommonF){
          vecF<-c(vecF,paste0(",\" ",sp[1],"\",","\"",sp[2],"\"","^\"",ifelse(sp[3]==" "|is.na(sp[3]),"",sp[3] ),"\"" )  )
        }

        #
        # non pattern to X
        #
        regmatches(note, posPattern, invert=T) <- Map(blanks2, lapply(regmatches(note, posPattern, invert = T), nchar) )
        #
        # replace with mod
        #
        common<-gsub("^","\" ",common)
        common<-gsub("$"," \"",common)

        # newVarNames<-c(common,vecFL)

        # newVarNames <- vector(mode="character",length(common)+length(vecFL) )
        newVarNames <- vector(mode="character",length(common)+length(vecFL)+length(vecF) )

        # newVarNames[-uncommonPos] <- common

        newVarNames[commonPos] <- common
        newVarNames[uncommonPos] <- vecFL
        newVarNames[uncommonPosF] <- vecF

        newVarNames<-gsub("^"," ",newVarNames)
        newVarNames<-gsub("$"," ",newVarNames)

        regmatches(note, posPattern, invert=F) <-  list(newVarNames  )

        #
        # add
        #
        posXNotPattern2 <- gregexpr("[\u0168]+", note)
        regmatches(note, posXNotPattern2, invert=F) <-  list(notInParPl)
        #
        #   end
        #
        note<-sub("\",$","",note)
        # note
        nameWithVar <- str2lang(paste0("paste(",note,")") )

        } # len uncommon

        } # pT

        if(!exists("nameWithVar")) {
          if(parseStr2lang==FALSE){
            nameWithVar<- note
          } else {
            nameWithVar<- str2lang(paste0("paste(",note,")") )
          }
        }

               graphics::text(
                             xposNote
                             ,yposNote +  ( (notesPosY/2)*mySign)
                             ,labels =  nameWithVar,
                              cex=notesTextSize,
                              adj=0
                              ,font=  font1
                              ,family = family1
                    ) # end graphics::text
         } # null
       } # for
     } # inherits d.f.
} # fun

blanks2 <- function(n) strrep("\u0168", n)
