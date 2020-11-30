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


      if(downNote){
        yposNote <- min(ymnoNA[[i]]) - ( (distTextChr/3) * (chrIdCount + morphoCount2 + indexCount2 + groupCount + 3 - holocenDisCount) )
        mySign <- -1
      } else {
        yposNote <- max(ymnoNA[[i]])
        mySign <- 1
      }

  xposNote <- min(xmnoNA[[i]]) + ((notesPosX/2) )

} else { # RIGHT NOTE TRUE

  font1   <- ifelse(OTUasNote, OTUfont, noteFont)
  family1 <- ifelse(OTUasNote, ifelse( !is.na(OTUfamily),  OTUfamily, defaultFontFamily2),defaultFontFamily2) # justif
  mySign <- 1

   if(attr(listOfdfChromSizenoNA[[i]],"cenType")=="monocen"){
      yposNote <- min(ymnoNA[[i]][,1])
    } else {
      yposNote <- (max(ymnoNA[[i]]) + min(ymnoNA[[i]]) ) /2
    }
    xposNote<-max(xmnoNA[[i]]) + (notesPosX/2)
}

 note <- notes[which(notes$OTU %in% names(listOfdfChromSizenoNA)[i] ), ]$note

 suppressWarnings(remove(nameWithVar))

 if(!is.null(note) & length(note)>0 ) {


        #
        # CHECK  for need of italics
        #

        fbool<- rightN==FALSE & downNote & leftNoteFont==3
        sbool<- rightN==FALSE & downNote==FALSE & leftNoteFontUp==3
        tbool<- rightN        & downNote==FALSE & noteFont==3
        f2bool<- rightN        & downNote==FALSE & OTUfont==3 & OTUasNote

        hasF<-grepl("FL|FL\\+|FL0|F\\+",note)
        hasQuotes<-grepl("\\((.*?)\\)|'(.*?)'",note)

        if( fbool | sbool | tbool | f2bool ) if(parseStr2lang==FALSE) if(hasQuotes & parseTypes) {
          # note<-"C. maxima 'Pink' (1A + 1B + 1C + 8D + 1F+ + 4F + 2FL)"
          if(hasF){
            message(crayon::blue("patterns FL FL+ FL0 or F+ detected and processed, avoid this using parseTypes=FALSE"))
          }
          nameWithVar <- processNameVarAndFormula(note)
        } else if (hasQuotes & parseTypes==FALSE) {
          nameWithVar <- processNameVar(note)
        }

        # if(T) if(T) if(F) {"b"} else if (T) {"a"}

        #
        #   type patterns
        #

        if(!any(fbool,sbool,tbool,f2bool) ) { # if all false
        if( parseTypes & parseStr2lang==FALSE & !exists("nameWithVar") & hasF ) {

          message(crayon::blue("patterns FL FL+ FL0 or F+ detected and processed, avoid this using parseTypes=FALSE"))

          noteLang <- formatFs(note,"FL|FL\\+|FL0|F\\+")
          nameWithVar<-str2lang(paste0("paste(",noteLang,")") )

        } # parsetypes TRUE
        }

        if(!exists("nameWithVar")) {
          if(parseStr2lang==FALSE){
            nameWithVar<- note
          } else { # parseStr2lang TRUE
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
               remove(nameWithVar)
         } # null note
       } # for sp
     } # inherits d.f. notes
} # fun

blanks2 <- function(n) strrep("\u0168", n)

blanks3 <- function(n) strrep("\u0169", n)

processNameVarAndFormula <- function(string, pattern="\\((.*?)\\)\\s?|\\s?'(.*?)'") {

  insideQuotesOrPar <- gregexpr(pattern, string)

  # not italics
  inQPOrig <-  unlist(regmatches(string, insideQuotesOrPar, invert=F ) )
  inQPOrig <- inQPOrig[inQPOrig!=""]

  # italics
  outQPOrig <-  unlist(regmatches(string, insideQuotesOrPar, invert=T ) )
  outQPOrig <- outQPOrig[outQPOrig!=""]

  regmatches(string, insideQuotesOrPar, invert=T) <- Map(blanks3, lapply(regmatches(string, insideQuotesOrPar, invert = T), nchar) )

  regmatches(string, insideQuotesOrPar, invert=F) <- Map(blanks2, lapply(regmatches(string, insideQuotesOrPar, invert = F), nchar) )

  posOfMatches <- gregexpr("[\u0168]+", string)

  inQPOrigLang <- formatFs(inQPOrig,"FL|FL\\+|FL0|F\\+")

  regmatches(string, posOfMatches, invert=F) <-  list(inQPOrigLang) #ok

  posOfNonMatches <- gregexpr("[\u0169]+", string)

  regmatches(string, posOfNonMatches, invert=F) <-  list(paste0(',italic(\"',outQPOrig," \")" ) )

  nameWithVar<-str2lang(paste0("paste(",string,")") )
  return(nameWithVar)
}

processNameVar <- function(string, pattern="\\((.*?)\\)\\s?|\\s?'(.*?)'") {
  insideQuotesOrPar <- gregexpr(pattern, string)

  # not italics
  inQPOrig <-  unlist(regmatches(string, insideQuotesOrPar, invert=F ) )
  inQPOrig <- inQPOrig[inQPOrig!=""]

  # italics
  outQPOrig <-  unlist(regmatches(string, insideQuotesOrPar, invert=T ) )
  outQPOrig <- outQPOrig[outQPOrig!=""]

  regmatches(string, insideQuotesOrPar, invert=T) <- Map(blanks3, lapply(regmatches(string, insideQuotesOrPar, invert = T), nchar) )

  regmatches(string, insideQuotesOrPar, invert=F) <- Map(blanks2, lapply(regmatches(string, insideQuotesOrPar, invert = F), nchar) )

  posOfMatches <- gregexpr("[\u0168]+", string)

  # inQPOrigLang <- formatFs(inQPOrig,"FL|FL\\+|FL0|F\\+")

  regmatches(string, posOfMatches, invert=F) <-  list(paste0(',plain(\"',inQPOrig," \")" ) )

  posOfNonMatches <- gregexpr("[\u0169]+", string)

  regmatches(string, posOfNonMatches, invert=F) <-  list(paste0(',italic(\"',outQPOrig," \")" ) )

  nameWithVar<-str2lang(paste0("paste(",string,")") )
  return(nameWithVar)
}

formatFs <- function(string,pattern){
  i<-0
  for (str in string){
    # str<-string
    i<-i+1
    strOrig <- str
    {
      posPattern <- gregexpr(pattern, str)

      regmatches(str, posPattern, invert=T) <- Map(blanks3, lapply(regmatches(str, posPattern, invert = T), nchar) )

      regmatches(str, posPattern, invert=F) <- Map(blanks2, lapply(regmatches(str, posPattern, invert = F), nchar) )

      #
      # change pattern match
      #

      patternMatch  <- unlist(regmatches(strOrig,gregexpr(pattern,strOrig ) ) ) # "\\((.*?)\\)|'(.*?)'"

      nonPatternMatch  <- unlist(regmatches(strOrig,gregexpr(pattern,strOrig ), invert = T ) ) # "\\((.*?)\\)|'(.*?)'"
      tryCatch(remove(FLmatches), warning=function(w) {""} )
      FLmatches  <- patternMatch[which(patternMatch %in% grep("FL",   patternMatch, value=TRUE) )]
      tryCatch(remove(F_matches), warning=function(w) {""} )
      F_matches  <- patternMatch[which(patternMatch %in% grep("F\\+", patternMatch, value=TRUE) )]

      FLmatchesPos  <- grep("FL",   patternMatch)

      F_matchesPos <- grep("F\\+", patternMatch)
    }

    if( length(FLmatchesPos) | length(F_matchesPos) ) {

      splFLmatches <- strsplit(FLmatches,"")

      splF_matches <- strsplit(F_matches,"")

      vecFL<-character()
      for (sp in splFLmatches){
        vecFL<-c(vecFL,paste0(#",\" ",sp[1],"\",",
          ",\"", sp[1],"\"",       # normal F
          "[","\"",sp[2],"\"","]", # subscript L
          ifelse(is.na(sp[3]),
                 paste0(",plain(\"\")"),   # empty
                 paste0("^\"",sp[3],"\""))# plus or zero superscript
        )
        )
      }
      vecFL
      vecF<-character()
      for (sp in splF_matches){
        vecF<-c(vecF,paste0(#",\" ",sp[1],"\",",
          ",\"",sp[1],"\"", # normal F
          "^\"",sp[2],"\"" # superscript plus
        )  )
      }
      vecF

      patternMatchLang <- vector(mode="character",length(vecFL)+length(vecF) )

      patternMatchLang[FLmatchesPos] <- vecFL
      patternMatchLang[F_matchesPos] <- vecF

      posOfMatches <- gregexpr("[\u0168]+", str)
      regmatches(str, posOfMatches, invert=F) <-  list(patternMatchLang  ) #ok
    }

    posOfNonMatches <- gregexpr("[\u0169]+", str)
    regmatches(str, posOfNonMatches, invert=F) <-  list(paste0(',plain(\"',nonPatternMatch,"\")," ) )

    string[i] <- str
  } # for str
  return(string)
}


