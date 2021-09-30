observeEvent(input$confirmSvg, {
  removeModal()
  updateRadioButtons(session,inputId = "pngorsvg", selected="svg")
  values[["pngorsvg"]] <- "svg"
  values[["decision"]] <- ". You did something not recommended, Now crashing?"
  values[["stop"]] <- FALSE
})

observeEvent(input$confirmPng, {
  removeModal()
  updateRadioButtons(session,inputId = "pngorsvg", selected="png")
  values[["stop"]] <- FALSE
  values[["pngorsvg"]] <- "png"
  values[["decision"]] <- ""
})

observeEvent(input$pngorsvg, ignoreInit = TRUE, {
  if(input$pngorsvg=="svg" & values[["number"]]==10) {
    showModal(modalDialog(
      title = "WARNING, Rstudio or browser crash risk"
      ,tagList("You selected to display a large chr. in .svg format"
               , br()
               , "you should leave .png in",tags$strong("Display"),"for thousands of marks"
               , br()
               , "and",tags$strong("download plot"), "as .svg if desired"
      )
      ,easyClose = TRUE,
      footer = list(
        actionButton("confirmSvg", "Choose .svg (not recommended)"),
        actionButton("confirmPng", "Leave .png as suggested")
      ) )
    )
    values[["stop"]] <- TRUE
    values[["decision"]] <- ""
  } else {
    values[["pngorsvg"]] <- input$pngorsvg
    values[["stop"]]     <- FALSE
    values[["decision"]] <- ""
  }
})

filenameR <- eventReactive({
  input$exampleButton
  values[["df1"]]
  values[["df1MStyle"]]
  values[["df1Mark"]]

  values[["notes"]]
  values[["leftNotes"]]
  values[["leftNotesUp"]]

  input$leftNoteFontUp
  input$leftNotesPosX
  input$leftNotesPosY
  input$leftNotesUpPosY

  input$leftNotesUpPosX
  input$notesPosY
  input$leftNoteFont
  input$noteFont
  input$leftNotesUpTextSize
  input$leftNotesTextSize

  input$verticalPlot
  input$karSpaceHor
  input$karAnchorLeft

  input$parseStr2lang
  input$moveAllKarValueHor
  input$moveAllKarValueY

  input$karHeight

  input$colorBorderMark
  input$lwd.marks
  input$gishCenBorder
  input$hideCenLines

  input$karHeiSpace
  input$amoSepar
  input$karSepar
  input$cenFormat
  input$cenFactor

  input$autoCenSize
  input$centromereSize

  as.character(input$legend)
  values[["mycolors"]]
  values[["markPer"]]
  values[["bToRemove"]]
  values[["bannedMarkName"]]
  values[["specialOTUNames"]]
  values[["missOTUspacings"]]
  values[["addMissingOTUAfter"]]

  input$specialyTitle
  input$specialChrWidth
  input$specialChrSpacing

  input$origin
  input$OTUfamily
  input$xModMonoHoloRate

  input$yTitle
  input$nameChrIndexPos

  input$perAsFraction

  input$chrColor
  input$cenColor
  input$chrId
  input$chrWidth
  input$chrSpacing
  input$squareness
  input$markDistType

  input$markN
  input$n
  input$orderChr
  input$useOneDot
  input$pMarkFac
  input$cMBeginCenter

  input$protruding
  input$arrowhead
  input$chrLabelSpacing
  input$labelSpacing

  input$rotation
  input$shrinkFactor
  input$separFactor
  input$radius
  input$circleCenter
  input$OTUsrt
  input$OTUjustif
  input$OTULabelSpacerx
  input$OTUlegendHeight
  input$OTUplacing
  input$morpho
  input$chrIndex
  input$karIndex
  input$karIndexPos
  input$chrSize
  input$showMarkPos
  input$indexIdTextSize
  input$chrSizeMbp
  input$chromatids

  input$holocenNotAsChromatids

  input$xModifier
  input$circularPlot
  input$addOTUName
  as.numeric(input$OTUfont)
  as.character(input$moveKarHor)
  input$mkhValue
  input$anchor
  input$moveAnchorV
  input$moveAnchorH
  input$OTUasNote
  input$labelOutwards
  input$notesTextSize
  input$notesPosX
  input$ruler
  input$rulerPos
  input$threshold
  input$ceilingFactor

  input$rulerInterval
  input$rulerIntervalMb
  input$rulerIntervalcM

  input$ruler.tck
  input$rulerNumberSize
  input$rulerNumberPos
  input$rulerTitleSize
  input$xPosRulerTitle
  input$legendWidth
  input$legendHeight
  input$pattern
  input$remSimiMarkLeg
  input$markLabelSize
  input$markLabelSpacer
  input$legendYcoord

  input$fixCenBorder
  input$chrBorderColor
  input$lwd.chr
  input$lwd.mimicCen
  input$lwd.cM
  input$distTextChr
  input$OTUTextSize
  input$xlimLeftMod
  input$xlimRightMod
  input$ylimBotMod
  input$ylimTopMod

  values[["pngorsvg"]]
}
,{
  filename <- tempfile(fileext=".txt")
  filenamePath <- normalizePath(filename, mustWork = F)
  filenamePath
})

observeEvent(input$left, {
  updateSliderInput(session, "hwModifier", value = isolate(input$hwModifier) - 0.2)
})

observeEvent(input$right, {
  updateSliderInput(session, "hwModifier", value = isolate(input$hwModifier) + 0.2)
})

observeEvent(input$centromereSize, {
  updateCheckboxInput(session, "autoCenSize", value = FALSE)
})

observeEvent(input$fontDecrease, {
  updateNumericInput(session, "rulerNumberSize", value = isolate(input$rulerNumberSize) - 0.2)
  updateNumericInput(session, "notesTextSize", value = isolate(input$notesTextSize) - 0.2)
  updateNumericInput(session, "OTUTextSize", value = isolate(input$OTUTextSize) - 0.2)
  updateNumericInput(session, "rulerTitleSize", value = isolate(input$rulerTitleSize) - 0.2)
  updateNumericInput(session, "markLabelSize", value = isolate(input$markLabelSize) - 0.2)
  updateNumericInput(session, "indexIdTextSize", value = isolate(input$indexIdTextSize) - 0.2)
})
observeEvent(input$fontIncrease, {
  updateNumericInput(session, "rulerNumberSize", value = isolate(input$rulerNumberSize) +0.2)
  updateNumericInput(session, "notesTextSize", value = isolate(input$notesTextSize) + 0.2)
  updateNumericInput(session, "OTUTextSize", value = isolate(input$OTUTextSize) + 0.2)
  updateNumericInput(session, "rulerTitleSize", value = isolate(input$rulerTitleSize) + 0.2)
  updateNumericInput(session, "markLabelSize", value = isolate(input$markLabelSize) + 0.2)
  updateNumericInput(session, "indexIdTextSize", value = isolate(input$indexIdTextSize) + 0.2)
})


add0<-"{"

addLibraryIdio<-"library(idiogramFISH)"





observeEvent(input$pngorsvgDown,
             {
               if(input$pngorsvgDown=="svg"){
                 values[["imageType"]]<-'image/svg+xml'

               } else {
                 values[["imageType"]]<-'image/png'
               }
})


observe({
  values[["mycolors"]] <- tryCatch(unlist(strsplit(input$mycolors,",") )
                                   , error=function(e){NULL} )

  values[["markPer"]] <- tryCatch(unlist(strsplit(input$markPer,",") )
                                   , error=function(e){NULL} )

  values[["bToRemove"]] <- tryCatch(unlist(strsplit(input$bToRemove,",") )
                                    , error=function(e){NULL} )

  values[["bannedMarkName"]] <- tryCatch(unlist(strsplit(input$bannedMarkName,",") )
                                    , error=function(e){NULL} )

  values[["specialOTUNames"]] <- tryCatch(unlist(strsplit(input$specialOTUNames,",") )
                                         , error=function(e){NULL} )

  values[["missOTUspacings"]] <- tryCatch(as.numeric(unlist(strsplit(input$missOTUspacings,",") ) )
                                          , error=function(e){NULL} )

  values[["addMissingOTUAfter"]] <- tryCatch(unlist(strsplit(input$addMissingOTUAfter,",") )
                                          , error=function(e){NULL} )

  values[["mycolors2"]] <- tryCatch(unlist(strsplit(input$mycolors2,",") )
                                    , error=function(e){NULL} )

  dfChrSizeName<- ifelse(  invalid(values[["df1"]] )
                           , shQuote("")
                           , ifelse(input$saveType=="rds"
                                    , input$chrfilename2
                                    , shQuote(paste0(isolate(input$chrfilename2),".csv" ) )
                           )
  )

  notesName<- tryCatch(ifelse(  invalid(values[["notes"]] )
                           , shQuote("")
                           , ifelse(input$saveType=="rds"
                                    , input$notesfilename2
                                    , shQuote(paste0(isolate(input$notesfilename2),".csv" ) )
                           )
  ), error=function(e){NULL} )

  leftNotesName<- tryCatch(ifelse(  invalid(values[["leftNotes"]] )
                       , shQuote("")
                       , ifelse(input$saveType=="rds"
                                , input$leftNotesfilename2
                                , shQuote(paste0(isolate(input$leftNotesfilename2),".csv" ) )
                       )
  ), error=function(e){NULL} )

  leftNotesUpName<- tryCatch(ifelse(  invalid(values[["leftNotesUp"]] )
                           , shQuote("")
                           , ifelse(input$saveType=="rds"
                                    , input$leftNotesUpfilename2
                                    , shQuote(paste0(isolate(input$leftNotesUpfilename2),".csv" ) )
                           )
  ), error=function(e){NULL} )

  dfMarkPosName<- ifelse(invalid(values[["df1Mark"]] )
                         ,shQuote("")
                         ,ifelse(input$saveType=="rds"
                                 ,input$markfilename2
                                 ,shQuote(paste0(isolate(input$markfilename2),".csv" ) )
                         )
  )

  dfMarkColorName<-  ifelse(invalid(values[["df1MStyle"]] )
                            ,shQuote("")
                            ,ifelse(input$saveType=="rds"
                                    ,input$MStylefilename2
                                    ,shQuote(paste0(isolate(input$MStylefilename2),".csv" ) )
                            )
  )

  mc <- match.call(plotIdiograms,
                   call("plotIdiograms",
                        dfChrSize    = dfChrSizeName

                        ,notes       = notesName
                        ,leftNotes   = leftNotesName
                        ,leftNotesUp = leftNotesUpName

                        ,dfMarkPos   = dfMarkPosName
                        ,dfMarkColor = dfMarkColorName,


                        colorBorderMark = shQuote(input$colorBorderMark),
                        lwd.marks       = input$lwd.marks,
                        gishCenBorder   = input$gishCenBorder,
                        hideCenLines    = input$hideCenLines,

                        leftNoteFontUp  = input$leftNoteFontUp,
                        leftNotesPosX   = input$leftNotesPosX ,
                        leftNotesPosY   = input$leftNotesPosY   ,
                        leftNotesUpPosY = input$leftNotesUpPosY,

                        leftNotesUpPosX     = input$leftNotesUpPosX,
                        notesPosY           = input$notesPosY   ,
                        leftNoteFont        = input$leftNoteFont,
                        noteFont            = input$noteFont,
                        leftNotesUpTextSize = input$leftNotesUpTextSize,
                        leftNotesTextSize   = input$leftNotesTextSize,

                        verticalPlot  = input$verticalPlot,
                        karSpaceHor   = input$karSpaceHor,
                        karAnchorLeft = input$karAnchorLeft,

                        parseStr2lang      = input$parseStr2lang,
                        moveAllKarValueHor = input$moveAllKarValueHor,
                        moveAllKarValueY   = input$moveAllKarValueY,

                        karHeight    = input$karHeight,
                        karHeiSpace  = input$karHeiSpace,
                        amoSepar = input$amoSepar,
                        karSepar = input$karSepar,
                        legend   = shQuote(input$legend),

                        mycolors = if(length( values[["mycolors"]] )==0){
                          shQuote("")
                        } else if (length( values[["mycolors"]] )==1) {
                          shQuote(values[["mycolors"]])
                        } else {
                          values[["mycolors"]]
                        },
                        markPer = if(length( values[["markPer"]] )==0){
                          shQuote("")
                        } else if (length( values[["markPer"]] )==1) {
                          shQuote(values[["markPer"]])
                        } else {
                          values[["markPer"]]
                        },
                        bToRemove = if(length( values[["bToRemove"]] )==0){
                          shQuote("")
                        } else if (length( values[["bToRemove"]] )==1) {
                          shQuote(values[["bToRemove"]])
                        } else {
                          values[["bToRemove"]]
                        },
                        bannedMarkName = if(length( values[["bannedMarkName"]] )==0){
                          shQuote("")
                        } else if (length( values[["bannedMarkName"]] )==1) {
                          shQuote(values[["bannedMarkName"]])
                        } else {
                          values[["bannedMarkName"]]
                        },
                        specialOTUNames = if(length( values[["specialOTUNames"]] )==0){
                          shQuote("")
                        } else if (length( values[["specialOTUNames"]] )==1) {
                          shQuote(values[["specialOTUNames"]])
                        } else {
                          values[["specialOTUNames"]]
                        },

                        missOTUspacings = if(length( values[["missOTUspacings"]] )==0){
                          0
                        } else {
                          values[["missOTUspacings"]]
                        },
                        addMissingOTUAfter = if(length( values[["addMissingOTUAfter"]] )==0){
                          shQuote("")
                        } else if (length( values[["addMissingOTUAfter"]] )==1) {
                          shQuote(values[["addMissingOTUAfter"]])
                        } else {
                          values[["addMissingOTUAfter"]]
                        },

                        specialyTitle     = shQuote(input$specialyTitle),
                        specialChrWidth   = input$specialChrWidth,
                        specialChrSpacing = input$specialChrSpacing,

                        origin    = shQuote(input$origin),
                        OTUfamily = shQuote(input$OTUfamily),
                        xModMonoHoloRate = input$xModMonoHoloRate,

                        yTitle          = input$yTitle,
                        nameChrIndexPos = input$nameChrIndexPos,

                        perAsFraction  = input$perAsFraction,

                        cenFormat = shQuote(input$cenFormat),
                        cenFactor = input$cenFactor,

                        autoCenSize    = input$autoCenSize,
                        centromereSize = input$centromereSize,

                        chrColor = shQuote(input$chrColor),
                        cenColor = shQuote(input$cenColor),

                        chrId      = shQuote(input$chrId),
                        chrWidth   = input$chrWidth,
                        chrSpacing = input$chrSpacing,

                        squareness   = input$squareness,
                        markDistType = shQuote(input$markDistType)

                        ,markN = input$markN
                        ,n = input$n

                        ,orderChr   = shQuote(input$orderChr)
                        ,useOneDot  = input$useOneDot

                        ,pMarkFac   = input$pMarkFac
                        ,protruding = input$protruding
                        ,arrowhead  = input$arrowhead

                        ,cMBeginCenter   = input$cMBeginCenter
                        ,chrLabelSpacing = input$chrLabelSpacing
                        ,labelSpacing    = input$labelSpacing

                        ,rotation        = input$rotation
                        ,shrinkFactor    = input$shrinkFactor
                        ,separFactor     = input$separFactor
                        ,radius          = input$radius
                        ,circleCenter    = input$circleCenter

                        ,OTUsrt          = input$OTUsrt
                        ,OTUjustif       = input$OTUjustif
                        ,OTULabelSpacerx = input$OTULabelSpacerx
                        ,OTUlegendHeight = input$OTUlegendHeight
                        ,OTUplacing      = shQuote(input$OTUplacing)

                        ,morpho      = shQuote(input$morpho),

                        chrIndex     = shQuote(input$chrIndex),
                        karIndex     = input$karIndex,
                        karIndexPos  = input$karIndexPos,

                        chrSize         = input$chrSize,
                        showMarkPos     = input$showMarkPos,
                        indexIdTextSize = input$indexIdTextSize,
                        chrSizeMbp   = input$chrSizeMbp,
                        chromatids   = input$chromatids,

                        holocenNotAsChromatids = input$holocenNotAsChromatids,
                        xModifier    = input$xModifier,

                        circularPlot = input$circularPlot,

                        addOTUName  = input$addOTUName,
                        OTUfont     = as.numeric(input$OTUfont),
                        moveKarHor  = shQuote(input$moveKarHor),
                        mkhValue    = input$mkhValue,
                        anchor      = input$anchor,
                        moveAnchorV = input$moveAnchorV,
                        moveAnchorH = input$moveAnchorH,

                        OTUasNote     = input$OTUasNote,
                        labelOutwards = input$labelOutwards,
                        notesTextSize = input$notesTextSize,
                        notesPosX     = input$notesPosX ,

                        ruler           = input$ruler,
                        rulerPos        = input$rulerPos,
                        threshold       = input$threshold,
                        ceilingFactor   = input$ceilingFactor,

                        rulerInterval   = input$rulerInterval,
                        rulerIntervalMb = input$rulerIntervalMb,
                        rulerIntervalcM = input$rulerIntervalcM,

                        ruler.tck       = input$ruler.tck,
                        rulerNumberSize = input$rulerNumberSize,
                        rulerNumberPos  = input$rulerNumberPos,

                        rulerTitleSize  = input$rulerTitleSize
                        ,xPosRulerTitle = input$xPosRulerTitle

                        ,legendWidth     = input$legendWidth
                        ,legendHeight    = input$legendHeight
                        ,pattern         = shQuote(input$pattern)
                        ,remSimiMarkLeg  = input$remSimiMarkLeg
                        ,markLabelSize   = input$markLabelSize
                        ,markLabelSpacer = input$markLabelSpacer
                        ,legendYcoord    = input$legendYcoord

                        ,fixCenBorder    = input$fixCenBorder
                        ,chrBorderColor  = shQuote(input$chrBorderColor)
                        ,lwd.chr         = input$lwd.chr
                        ,lwd.mimicCen    = input$lwd.mimicCen
                        ,lwd.cM          = input$lwd.cM

                        ,distTextChr = input$distTextChr
                        ,OTUTextSize = input$OTUTextSize

                        ,xlimLeftMod  = input$xlimLeftMod
                        ,xlimRightMod = input$xlimRightMod

                        ,ylimBotMod = input$ylimBotMod
                        ,ylimTopMod = input$ylimTopMod

                   )
  )
  mclist <- as.list( mc )
  mclist[1] <- NULL

  if (!invalid(!as.logical(input$keepDefault) ) ) {
    if ( !as.logical(input$keepDefault) )  {

      param_only <- gsub("Default","",names(paramValues) )

      for (i in 1:length(param_only)) {

        myl <- unlist(mclist[which(names(mclist)==param_only[i])] )

        myDef<-paramValues[i]

        if(!invalid(myl) ) {
          if(!invalid(myDef[[1]] ) ) {
            if (length( setequal(myl,myDef[[1]] ) ) ) {
              if( setequal(myl,myDef[[1]] ) ) {
                mclist[which(names(mclist)==param_only[i])]<-NA
              }
            }
            if (length( setequal(myl,paste0("'",myDef[[1]],"'" ) ) ) ) {
              if( setequal(myl,paste0("'",myDef[[1]],"'" ) ) ) {
                mclist[which(names(mclist)==param_only[i])]<-NA
              }
            }

          }
          if(length(setequal(myl,"") ) ) {
            if(setequal(myl,"") ){
              mclist[which(names(mclist)==param_only[i])]<-NA
            }
          }
          if(length(setequal(myl,"''") ) ) {
            if( setequal(myl,"''") ) {
              mclist[which(names(mclist)==param_only[i])]<-NA
            }
          }
        }
      }

      mclist<-mclist[which(sapply(mclist, unlist) != "''")]
      mclist<-mclist[which(!is.na(sapply(mclist, unlist) ) ) ]
    }
  }

  seq <- paste(names(mclist), mclist  , sep="=", collapse = ",\n")

  validate(
    need(try( "dfChrSize" %in% names(mclist) ), "")
  )

  if(!"dfChrSize" %in% names(mclist) ) {
    seq<-character()
  } else if (!is.na(mclist$dfChrSize) ) {
    if(mclist$dfChrSize=="''"){
      seq<-character()
    }
  } else {
    seq<-character()
  }

  addLibrarySvg<-"#install.packages('svglite')"
  addLibrarySvg<- ifelse(values[["pngorsvg"]]=="svg",addLibrarySvg,"")

  addLibrarySvg2<-"library(svglite)"
  addLibrarySvg2<- ifelse(values[["pngorsvg"]]=="svg",addLibrarySvg2,"")

  addSvgDev <- paste0('svg("dfOfChrSize.svg",width='
                  ,values[["mysvgwidth"]]
                  ,", height="
                  ,values[["mysvgheight"]],')'
  )
  addPngDev <- paste0('png("dfOfChrSize.png",width='
                  ,values[["mysvgwidth"]]*80
                  ,", height="
                  ,values[["mysvgheight"]]*80,')')

  addDev   <- ifelse(values[["pngorsvg"]]=="svg",addSvgDev,addPngDev)

  chrRds                 <- paste0("chrData     <- readRDS('",input$chrfilename2,".rds')\n")

  notesRds       <- ifelse(invalid(values[["notes"]] )
                           ,""
                           ,paste0("notes       <- readRDS('",input$notesfilename2,".rds')\n")
  )

  leftNotesRds   <- ifelse(invalid(values[["leftNotes"]] )
                           ,""
                           ,paste0("leftNotes   <- readRDS('",input$leftNotesfilename2,".rds')\n")
  )

  leftNotesUpRds <- ifelse(invalid(values[["leftNotesUp"]] )
                           ,""
                           ,paste0("leftNotesUp <- readRDS('",input$leftNotesUpfilename2,".rds')\n")
  )


  markRds  <- ifelse(invalid(values[["df1Mark"]] )
                   ,""
                   ,paste0("markData    <- readRDS('",input$markfilename2,".rds')\n")
  )


  mstyleRds<-  ifelse(invalid(values[["df1MStyle"]] )
                      ,""
                      ,paste0("MStyleData  <- readRDS('",input$MStylefilename2,".rds')\n")
  )

  rdsAdd  <- ifelse(input$saveType=="rds",chrRds,"")
  notesAdd<- ifelse(input$saveType=="rds",notesRds,"")
  leftNotesAdd  <- ifelse(input$saveType=="rds",leftNotesRds,"")
  leftNotesUpAdd<- ifelse(input$saveType=="rds",leftNotesUpRds,"")


  mrdsAdd <- ifelse(input$saveType=="rds",markRds,"")
  msrdsAdd<- ifelse(input$saveType=="rds",mstyleRds,"")

  block<-ifelse(input$asFile,"","#")

  strFun <- paste0(add0,"\n"
                   ,addLibrarySvg,"\n"
                   ,block
                   ,addLibrarySvg2,"\n"
                   ,addLibraryIdio,"\n"

                   ,rdsAdd
                   ,mrdsAdd
                   ,msrdsAdd
                   ,"\n"
                   ,notesAdd
                   ,leftNotesAdd
                   ,leftNotesUpAdd
                   ,block
                   ,ifelse(length(values[["mysvgwidth"]]),addDev,"")
                   ,"\n"
                   ,ifelse(length(seq),paste0("plotIdiograms(\n",seq,"\n)"),"")
                   ,"\n"
                   ,block
                   ,ifelse(length(values[["mysvgwidth"]]),"dev.off()","")
                   ,"\n}")

  strFun<-gsub("\n\n\n","\n",strFun)
  strFun<-gsub("\n\n","\n",strFun)

  spl1<-unlist(strsplit(strFun,"\n"))
  spl2<-character()

  for (s in spl1) {
    to_s <- sub("(.*)=.*","\\1",s )
    to_ss<- sprintf("%-*s%s%*s", 0, "", s, ifelse( (30-nchar(s) )<1,1,(30-nchar(s)) ), "")
    desc <- tryCatch(get(paste0(to_s,"Desc") ) ,error=function(e){""} )
    spl2 <- c(spl2,ifelse(desc=="",s,paste0(to_ss,"#",sub(paste0("`",to_s,"`:( \\([[:alnum:].`\u0022-]+\\))?"),"",desc )
    ) ) )
  }
  strFun <- paste(spl2,collapse = "\n")
  values[["strFun"]] <- strFun
  gc()

}) # observe 257
