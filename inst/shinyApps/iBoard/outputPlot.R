#output  plot

#output plot

output$idiogramPlot <- renderImage( {

  validate(need(try(values[["stop"]]==FALSE),"not ready" ) )
  validate(need(try(values[["df1"]]),"Start in Example or Nucleotide pages (left)" ))
  validate(need(try(inherits(values[["df1"]],"data.frame") )
                ,"Start in pages (left): Examples, Nucleotides or data.frames
              \n modify browser zoom with Ctrl [+/-]
              \n If you are in Rstudio Desktop Viewer, click its button 'Open Browser'
              for better memory management")
  )
  validate(need(try(nrow(values[["df1"]])>0 )
                ,"Start in pages (left): Examples, Nucleotides or data.frames
              \n modify browser zoom with Ctrl [+/-]
              \n If you are in Rstudio Desktop Viewer, click its button 'Open Browser'
              for better memory management")
  )
  Sys.sleep(0.2)

  showModal(modalDialog(
    title = "Plotting"
    ,tagList(
      paste("this pop-up will close after completion. Press ESC to wait in shiny app.")
      ,br()
      ,paste0("display: ",values[["pngorsvg"]],values[["decision"]] )
    )
    ,easyClose = TRUE
    ,footer = tagList(
      modalButton("Wait in shiny app")
    )
  ))

  info <- getCurrentOutputInfo()
  width  <- as.numeric(isolate(info$width()) )  * (as.numeric(input$widFactor)/100  )
  height <- as.numeric(isolate(info$width() ) ) *
    (as.numeric(input$heiFactor)*(as.numeric(input$widFactor)/100) )

  mysvgwidth  <- width/80
  mysvgheight <- height/80

  values[["mysvgwidth"]]  <- mysvgwidth
  values[["mysvgheight"]] <- mysvgheight
  values[["width"]]       <- width
  values[["height"]]      <- height

  # values$mem <- HTML(paste("Current memory used:",format(round(pryr::mem_used()/1000000, 2),nsmall=2),"Mb" ) )

  # thres<-ifelse(!invalid(as.integer(input$memmega)*1000000>0)
  #               ,as.integer(input$memmega)*1000000
  #               , memthreshold*1000000  )

  # if(as.integer(pryr::mem_used() ) < thres ) {

  #
  # display image
  #

  if(values[["pngorsvg"]]=="svg"){
    outfileSvg <- tempfile(fileext='.svg')
    # outfileSvg <- "plot.svg"
    values[["imageType"]]<-'image/svg+xml'
    svg(outfileSvg, width=mysvgwidth, height=mysvgheight )
  } else {
    outfilePng <- tempfile(fileext='.png')
    # outfilePng <- 'plot.png'
    values[["imageType"]]<-'image/png'
    png(outfilePng, width=width, height=height )
  }

  capture.output (
    plotIdiograms(
      dfChrSize  = values[["df1"]],# data.frame of chr. size
      dfMarkColor= values[["df1MStyle"]],  # d.f of mark style <- Optional
      dfMarkPos  = values[["df1Mark"]],     # df of mark positions (includes cen. marks)

      karHeight  = input$karHeight,              # kar. height
      karHeiSpace= input$karHeiSpace,              # kar. height
      amoSepar   = input$amoSepar,
      karSepar   = input$karSepar,
      legend     = as.character(input$legend),

      mycolors   = if(length( values[["mycolors"]] )==0 ){""} else { values[["mycolors"]]}
      ,
      cenFormat = input$cenFormat,
      cenFactor = input$cenFactor,

      chrColor   = input$chrColor,
      cenColor   = input$cenColor,

      chrId      = input$chrId,
      chrWidth   = input$chrWidth,           # chr. width
      chrSpacing = input$chrSpacing,           # space among chr.

      squareness = input$squareness
      ,markN     = input$markN
      ,n         = input$n
      ,orderChr  = input$orderChr
      ,useOneDot = input$useOneDot
      ,pMarkFac  = input$pMarkFac
      ,cMBeginCenter = input$cMBeginCenter

      ,protruding = input$protruding

      ,chrLabelSpacing = input$chrLabelSpacing
      ,labelSpacing    = input$labelSpacing

      ,rotation        = input$rotation
      ,shrinkFactor    = input$shrinkFactor
      ,radius          = input$radius
      ,circleCenter    = input$circleCenter
      ,OTUsrt          = input$OTUsrt
      ,OTUjustif       = input$OTUjustif
      ,OTULabelSpacerx = input$OTULabelSpacerx
      ,OTUlegendHeight = input$OTUlegendHeight
      ,OTUplacing      = input$OTUplacing

      ,morpho       = input$morpho,          # chr. morpho. classif. (Guerra, Levan, both, "" ) ver. >= 1.12 only
      chrIndex      = input$chrIndex,            # cen. pos. (CI, AR, both, "" ) ver. >= 1.12 only
      karIndex      = input$karIndex,
      chrSize       = input$chrSize,           # add chr. sizes under chr.
      indexIdTextSize= input$indexIdTextSize,
      chrSizeMbp    = input$chrSizeMbp,        # add Mbp sizes under chr. (see above)
      chromatids    = input$chromatids,
      xModifier     = input$xModifier,
      circularPlot  = input$circularPlot,

      addOTUName = input$addOTUName,
      OTUfont    = as.numeric(input$OTUfont),
      moveKarHor = as.character(input$moveKarHor),
      mkhValue   = input$mkhValue,
      anchor     = input$anchor,
      moveAnchorV = input$moveAnchorV,
      moveAnchorH = input$moveAnchorH,

      OTUasNote     = input$OTUasNote,
      labelOutwards = input$labelOutwards,
      notesTextSize = input$notesTextSize,
      notesPosX     = input$notesPosX ,

      ruler          = input$ruler,
      rulerPos       = input$rulerPos,              # position of ruler
      ruler.tck      = input$ruler.tck,          # size and orientation of ruler ticks
      rulerNumberSize= input$rulerNumberSize,        # font size of rulers
      rulerTitleSize = input$rulerTitleSize
      ,xPosRulerTitle= input$xPosRulerTitle             # pos of ruler title

      ,legendWidth     = input$legendWidth            # width of legend items
      ,legendHeight    = input$legendHeight
      ,pattern         = input$pattern
      ,markLabelSize   = input$markLabelSize

      ,fixCenBorder    = input$fixCenBorder      # use chrColor as border color of cen. or cen. marks
      , chrBorderColor = input$chrBorderColor
      ,lwd.chr         = input$lwd.chr
      ,distTextChr     = input$distTextChr        # chr. text separation
      ,OTUTextSize     = input$OTUTextSize

      ,xlimLeftMod     = input$xlimLeftMod          # xlim left param.
      ,xlimRightMod    = input$xlimRightMod          # xlim left param.

      ,ylimBotMod = input$ylimBotMod           # modify ylim bottom argument
      ,ylimTopMod = input$ylimTopMod           # modify ylim top argument
    ) # plot
    ,    file = (outfile <- file(
      filenameR(),"w")),type="message" #"output"
  ) # capture
  close(outfile)
  dev.off()

  # Return a list containing the filename

  removeModal()

  hwModifier1 <- ifelse(input$hwModifier==0,0.1,input$hwModifier)

  if(values[["pngorsvg"]]=="svg"){
    list(src = normalizePath(outfileSvg),
         contentType = 'image/svg+xml',
         width  = width *hwModifier1,
         height = height*hwModifier1,
         alt = "My plot"
    ) } else {
      list(src = normalizePath(outfilePng),
           contentType = 'image/png',
           width  = width *hwModifier1,
           height = height*hwModifier1,
           alt = "My plot"
      )
    }

} , deleteFile = TRUE
) # end plot or image


output$buttonScript <-  renderUI({
  validate(
    need(try(scriptR()), "")
  )
  downloadButton('downloadR', 'Download R-script')
})


output$downloadR <- downloadHandler(

  filename = "script.R",
  content = function(file) {
    writeLines(scriptR(), file
    )
  }
)

output$pngorsvgDownButton <- downloadHandler(
  filename <- function() {
    paste0("plot.",input$pngorsvgDown)
  },
  content = function(file) {

      if(input$pngorsvgDown=="svg" & values[["number"]]==10) {

        if(Sys.info()['sysname']=="Linux"){
          message1<-"Linux might NOT be able to display .svg with more than 200,000 polygons"
          message2<-"Search for a workaround to increase this number for your system"
        } else if(Sys.info()['sysname']=="Windows" ){
          message1<-""
          message2<-""
        } else {
          message1<-"Problems known in linux for opening large .svg"
          message2<-"You may have to search for a workaround for your system"
        }

        showModal(modalDialog(
          title = "Plotting to file, please wait."
          ,tagList("You selected to download a large chr. in .svg format"
                   , br()
                   , message1
                   , br()
                   , message2
                   , br()
                   ,"this pop-up will close after completion. Press ESC to wait in shiny app"
          )
          ,easyClose = TRUE,
          footer = list(
            modalButton("Wait in shiny app")
          )
        )
        )
        # Sys.sleep(5)
        # removeModal()
      } else {

    showModal(modalDialog(
      title = "Plotting to file, please wait"
      ,"this pop-up will close after completion. Press ESC to wait in shiny app"
      ,easyClose = TRUE
      ,footer = modalButton("Wait in shiny app")
    ) )

      }

    if(input$pngorsvgDown=="svg") {
      svg(file, width=values[["mysvgwidth"]], height=values[["mysvgheight"]] )
    } else {
      png(file, width=values[["width"]], height=values[["height"]] )
    }

    #
    # download image
    #

    plotIdiograms(
      dfChrSize  = values[["df1"]],
      dfMarkColor= values[["df1MStyle"]],
      dfMarkPos  = values[["df1Mark"]],

      karHeight  = input$karHeight,
      karHeiSpace= input$karHeiSpace,
      amoSepar   = input$amoSepar,
      karSepar   = input$karSepar,
      legend     = as.character(input$legend),

      mycolors   = if(length( values[["mycolors"]] )==0 ){""} else { values[["mycolors"]]}
      ,
      cenFormat = input$cenFormat,
      cenFactor = input$cenFactor,

      chrColor   = input$chrColor,
      cenColor   = input$cenColor,

      chrId      = input$chrId,
      chrWidth   = input$chrWidth,           # chr. width
      chrSpacing = input$chrSpacing,           # space among chr.

      squareness = input$squareness
      ,markN     = input$markN
      ,n         = input$n
      ,orderChr  = input$orderChr
      ,useOneDot = input$useOneDot
      ,pMarkFac  = input$pMarkFac
      ,cMBeginCenter = input$cMBeginCenter

      ,protruding = input$protruding

      ,chrLabelSpacing = input$chrLabelSpacing
      ,labelSpacing    = input$labelSpacing

      ,rotation        = input$rotation
      ,shrinkFactor    = input$shrinkFactor
      ,radius          = input$radius
      ,circleCenter    = input$circleCenter
      ,OTUsrt          = input$OTUsrt
      ,OTUjustif       = input$OTUjustif
      ,OTULabelSpacerx = input$OTULabelSpacerx
      ,OTUlegendHeight = input$OTUlegendHeight
      ,OTUplacing      = input$OTUplacing

      ,morpho       = input$morpho,          # chr. morpho. classif. (Guerra, Levan, both, "" ) ver. >= 1.12 only
      chrIndex      = input$chrIndex,            # cen. pos. (CI, AR, both, "" ) ver. >= 1.12 only
      karIndex     = input$karIndex,
      chrSize       = input$chrSize,           # add chr. sizes under chr.
      indexIdTextSize= input$indexIdTextSize,
      chrSizeMbp    = input$chrSizeMbp,        # add Mbp sizes under chr. (see above)
      chromatids    = input$chromatids,
      xModifier     = input$xModifier,
      circularPlot  = input$circularPlot,

      addOTUName = input$addOTUName,
      OTUfont    = as.numeric(input$OTUfont),
      moveKarHor = as.character(input$moveKarHor),
      mkhValue   = input$mkhValue,
      anchor     = input$anchor,
      moveAnchorV = input$moveAnchorV,
      moveAnchorH = input$moveAnchorH,

      OTUasNote     = input$OTUasNote,
      labelOutwards = input$labelOutwards,
      notesTextSize = input$notesTextSize,
      notesPosX     = input$notesPosX ,

      ruler          = input$ruler,
      rulerPos       = input$rulerPos,              # position of ruler
      ruler.tck      = input$ruler.tck,          # size and orientation of ruler ticks
      rulerNumberSize= input$rulerNumberSize,        # font size of rulers

      rulerTitleSize = input$rulerTitleSize
      ,xPosRulerTitle= input$xPosRulerTitle             # pos of ruler title

      ,legendWidth     = input$legendWidth            # width of legend items
      ,legendHeight    = input$legendHeight
      ,pattern         = input$pattern
      ,markLabelSize   = input$markLabelSize

      ,fixCenBorder    = input$fixCenBorder      # use chrColor as border color of cen. or cen. marks
      , chrBorderColor = input$chrBorderColor
      ,lwd.chr         = input$lwd.chr
      ,distTextChr     = input$distTextChr        # chr. text separation
      ,OTUTextSize     = input$OTUTextSize

      ,xlimLeftMod     = input$xlimLeftMod          # xlim left param.
      ,xlimRightMod    = input$xlimRightMod          # xlim left param.

      ,ylimBotMod = input$ylimBotMod           # modify ylim bottom argument
      ,ylimTopMod = input$ylimTopMod           # modify ylim top argument
    ) # plot

    dev.off()
    removeModal()
  }
  ,contentType = values[["imageType"]]
)
