#observe Plot

#observePlot

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
  values[["df1"]]        # data.frame of chr. size
  values[["df1MStyle"]]
  values[["df1Mark"]]
  input$karHeight
  input$karHeiSpace
  input$amoSepar
  input$karSepar
  input$cenFormat
  input$cenFactor

  as.character(input$legend)
  values[["mycolors"]]

  input$chrColor
  input$cenColor
  input$chrId
  input$chrWidth           # chr. width
  input$chrSpacing           # space am
  input$squareness
  input$markN
  input$n
  input$orderChr
  input$useOneDot
  input$pMarkFac
  input$cMBeginCenter

  input$protruding
  input$chrLabelSpacing
  input$labelSpacing

  input$rotation
  input$shrinkFactor
  input$radius
  input$circleCenter
  input$OTUsrt
  input$OTUjustif
  input$OTULabelSpacerx
  input$OTUlegendHeight
  input$OTUplacing
  input$morpho          # chr. morpho
  input$chrIndex            # cen. po
  input$karIndex
  input$chrSize           # add chr.
  input$indexIdTextSize
  input$chrSizeMbp        # add Mbp
  input$chromatids
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
  input$rulerPos              # pos
  input$ruler.tck          # size a
  input$rulerNumberSize        # fon
  input$xPosRulerTitle             #
  input$legendWidth            # w
  input$legendHeight
  input$pattern
  input$markLabelSize
  input$fixCenBorder      # use ch
  input$chrBorderColor
  input$lwd.chr
  input$distTextChr        # chr.
  input$OTUTextSize
  input$xlimLeftMod          # xli
  input$xlimRightMod          # xl
  input$ylimBotMod           # modify y
  input$ylimTopMod           # modify y

  values[["pngorsvg"]]
}
,{
  filename <- tempfile(fileext=".txt")
  filenamePath <- normalizePath(filename, mustWork = F)
  filenamePath
})

observeEvent(input$left, {
  updateSliderInput(session, "hwModifier", value = isolate(input$hwModifier) - 0.5)
})
observeEvent(input$right, {
  updateSliderInput(session, "hwModifier", value = isolate(input$hwModifier) + 0.5)
})


add0<-"{"

add2b<-"library(idiogramFISH)"

# curr_date <- eventReactive(list(input$btn, input$tabset), {
#   format(Sys.time(), "%c")
# })

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

  values[["mycolors2"]] <- tryCatch(unlist(strsplit(input$mycolors2,",") )
                                    , error=function(e){NULL} )

  dfChrSizeName<- ifelse(  invalid(values[["df1"]] )
                           , shQuote("")
                           , ifelse(input$saveType=="rds"
                                    , input$chrfilename2
                                    , shQuote(paste0(isolate(input$chrfilename2),".csv" ) )
                           )
  )

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
                        ,dfMarkPos   = dfMarkPosName
                        ,dfMarkColor = dfMarkColorName
                        ,karHeight   = input$karHeight,             # kar. height
                        karHeiSpace  = input$karHeiSpace,        # kar. height
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
                        cenFormat = shQuote(input$cenFormat),
                        cenFactor = input$cenFactor,

                        chrColor = shQuote(input$chrColor),
                        cenColor = shQuote(input$cenColor),

                        chrId      = shQuote(input$chrId),
                        chrWidth   = input$chrWidth,           # chr. width
                        chrSpacing = input$chrSpacing,           # space among chr.

                        squareness  = input$squareness
                        ,markN = input$markN
                        ,n = input$n

                        ,orderChr   = shQuote(input$orderChr)
                        ,useOneDot  = input$useOneDot
                        ,pMarkFac   = input$pMarkFac
                        ,protruding = input$protruding
                        ,cMBeginCenter = input$cMBeginCenter

                        ,chrLabelSpacing = input$chrLabelSpacing
                        ,labelSpacing = input$labelSpacing

                        ,rotation      = input$rotation
                        ,shrinkFactor  = input$shrinkFactor
                        ,radius        = input$radius
                        ,circleCenter  = input$circleCenter

                        ,OTUsrt          = input$OTUsrt
                        ,OTUjustif       = input$OTUjustif
                        ,OTULabelSpacerx = input$OTULabelSpacerx
                        ,OTUlegendHeight = input$OTUlegendHeight
                        ,OTUplacing      = shQuote(input$OTUplacing)

                        ,morpho      = shQuote(input$morpho),          # chr. morpho. classif. (Guerra, Levan, both, "" ) ver. >= 1.12 only

                        chrIndex     = shQuote(input$chrIndex),            # cen. pos. (CI, AR, both, "" ) ver. >= 1.12 only
                        karIndex     = input$karIndex,
                        chrSize      = input$chrSize,           # add chr. sizes under chr.
                        indexIdTextSize = input$indexIdTextSize,
                        chrSizeMbp   = input$chrSizeMbp,        # add Mbp sizes under chr. (see above)
                        chromatids   = input$chromatids,
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
                        rulerPos        = input$rulerPos,              # position of ruler
                        ruler.tck       = input$ruler.tck,          # size and orientation of ruler ticks
                        rulerNumberSize = input$rulerNumberSize        # font size of rulers
                        ,xPosRulerTitle = input$xPosRulerTitle             # pos of ruler title

                        ,legendWidth   = input$legendWidth            # width of legend items
                        ,legendHeight  = input$legendHeight            # width of legend items
                        ,pattern       = shQuote(input$pattern)
                        ,markLabelSize = input$markLabelSize

                        ,fixCenBorder   = input$fixCenBorder      # use chrColor as border color of cen. or cen. marks
                        ,chrBorderColor = shQuote(input$chrBorderColor)
                        ,lwd.chr        = input$lwd.chr

                        ,distTextChr = input$distTextChr        # chr. text separation
                        ,OTUTextSize = input$OTUTextSize

                        ,xlimLeftMod  = input$xlimLeftMod          # xlim left param.
                        ,xlimRightMod = input$xlimRightMod          # xlim left param.

                        ,ylimBotMod = input$ylimBotMod           # modify ylim bottom argument
                        ,ylimTopMod = input$ylimTopMod           # modify ylim top argument

                   )
  )
  mclist <- as.list( mc )
  mclist[1] <- NULL

  if (!invalid(!as.logical(input$keepDefault) ) ) {
    if ( !as.logical(input$keepDefault) )  {

      paramVec<-gsub("Default","",names(paramValues) )

      for (i in 1:length(paramVec)) {

        myl <- unlist(mclist[which(names(mclist)==paramVec[i])] )

        myDef<-paste0(paramVec[i],"Default" )

        if(!invalid(myl) ) {
          if(!invalid(get(myDef) ) ) {
            if (length( setequal(myl,get(myDef) ) ) ) {
              if( setequal(myl,get(myDef) ) ) {
                mclist[which(names(mclist)==paramVec[i])]<-NA
              }
            }
            if (length( setequal(myl,paste0("'",get(myDef),"'" ) ) ) ) {
              if( setequal(myl,paste0("'",get(myDef),"'" ) ) ) {
                mclist[which(names(mclist)==paramVec[i])]<-NA
              }
            }

          }
          if(length(setequal(myl,"") ) ) {
            if(setequal(myl,"") ){
              mclist[which(names(mclist)==paramVec[i])]<-NA
            }
          }
          if(length(setequal(myl,"''") ) ) {
            if( setequal(myl,"''") ) {
              mclist[which(names(mclist)==paramVec[i])]<-NA
            }
          }
        } # myl mydf
      } # for

      mclist<-mclist[which(sapply(mclist, unlist) != "''")]
      mclist<-mclist[which(!is.na(sapply(mclist, unlist) ) ) ]
      # mclist<-mclist[which(!is.na(sapply(names(mclist), unlist) ) ) ]
      # mclist<-mclist[which(!is.null(sapply(mclist, unlist) ) ) ]


    } # if
  } #  keep Default

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

  add1<-"#install.packages('svglite')"
  add1<- ifelse(values[["pngorsvg"]]=="svg",add1,"")

  add2<-"library(svglite)"
  add2<- ifelse(values[["pngorsvg"]]=="svg",add2,"")

  add3a <- paste0('svg("dfOfChrSize.svg",width='
                  ,values[["mysvgwidth"]]
                  ,", height="
                  ,values[["mysvgheight"]],')'
  )
  add3b <- paste0('png("dfOfChrSize.png",width='
                  ,values[["mysvgwidth"]]*80
                  ,", height="
                  ,values[["mysvgheight"]]*80,')')
  add3<- ifelse(values[["pngorsvg"]]=="svg",add3a,add3b)

  chrRds   <-paste0("chrData    <- readRDS('",input$chrfilename2,".rds')\n")

  markRds<- ifelse(invalid(values[["df1Mark"]] )
                   ,""
                   ,paste0("markData   <- readRDS('",input$markfilename2,".rds')\n")
  )


  mstyleRds<-  ifelse(invalid(values[["df1MStyle"]] )
                      ,""
                      ,paste0("MStyleData <- readRDS('",input$MStylefilename2,".rds')\n")
  )
  rdsAdd  <- ifelse(input$saveType=="rds",chrRds,"")
  mrdsAdd <- ifelse(input$saveType=="rds",markRds,"")
  msrdsAdd<- ifelse(input$saveType=="rds",mstyleRds,"")

  block<-ifelse(input$asFile,"","#")

  strFun <- paste0(add0,"\n"
                   ,add1,"\n"
                   ,block
                   ,add2,"\n"
                   ,add2b,"\n"
                   ,block
                   ,ifelse(length(values[["mysvgwidth"]]),add3,"")
                   # ,add3
                   ,"\n"
                   ,rdsAdd
                   ,mrdsAdd
                   ,msrdsAdd
                   ,ifelse(length(seq),paste0("plotIdiograms(",seq,")"),"")
                   ,"\n"
                   ,block
                   ,ifelse(length(values[["mysvgwidth"]]),"dev.off()","")
                   ,"\n}")

  strFun<-gsub("\n\n\n","\n",strFun)
  strFun<-gsub("\n\n","\n",strFun)
  values[["strFun"]]<-strFun
  gc()

}
) # observe
