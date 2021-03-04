server <- function(input, output, session) {
  source(file.path("local.R"), local = TRUE, chdir = TRUE)

  output$tabsetpanel1UI<-renderUI({
    tabsetPanel(id="tabsetpanel1"
                ,tabPanel("1. Load example"
                          ,tags$head(tags$style( HTML(' wpanel .tab-content {margin-left:50px;}')) )
                          ,value="example"
                          ,div(class = "wpanel",
                               uiOutput("examplepanel")
                          )
                ) # tP
                ,tabPanel("2. Chr. data data.frame", value="dfChrTab",
                          uiOutput("dfchrpanel") )
                ,tabPanel("3. Mark Pos. data d.f.",value="dfMarkTab",
                          uiOutput("dfmarkpanel") )
                ,tabPanel("4. Mark's Style", value="dfMSTab",
                          uiOutput("dfMStylepanel") )
    ) # tabsetPanel
  })

  output$tabsetpanel2UI<-renderUI({
  tabsetPanel(id="tabsetpanel2",
              tabPanel("1. Parameters", value="paramTab",
                       uiOutput("parameterPanel") )
              ,tabPanel("2. Log", value="log",
                        uiOutput("logpanel") )
              ,tabPanel("3. code", value="funString",
                        uiOutput("strpanel") )
  )# tabsetpanel
  })

  outputOptions(output, "tabsetpanel1UI", suspendWhenHidden = FALSE)
  outputOptions(output, "tabsetpanel2UI", suspendWhenHidden = FALSE)


  values <- reactiveValues()
  #
  #   first tab

  Current <- reactiveValues(
    Tab = "example"
  )

  Current <- reactiveValues(
    Tab2 = "paramTab"
  )

  #
  # first submenu, see global

  CurrentM <- reactiveValues(
    menu = "DFsMenu"
  )

  #
  #   update currentM and current tab reactive values
  #

  observeEvent(
    input[["tabsetpanel1"]],
    {
      Current$Tab <- input[["tabsetpanel1"]]
    }
  )

  observeEvent(
    input[["tabsetpanel2"]],
    {
      Current$Tab2 <- input[["tabsetpanel2"]]
    }
  )

  observeEvent(
    input[["tabs"]],
    {
      CurrentM$menu <- input[["tabs"]]
    }
  )

  #
  #   left submenus
  #

  output$menu1 <- renderMenu({
    sidebarMenu(
      menuItem("data.frame inputs", tabName="DFsMenu" #, icon = icon("equalizer", lib="glyphicon")
               # ,selected=T
      ) # menuitem
      ,menuItem("Param. & idiogram", tabName="parameterPlotMenu"   #, icon = icon("equalizer", lib="glyphicon")
      ,selected = T
      ) #menuitem
      ,menuItem("About", tabName="abouttab"   #, icon = icon("equalizer", lib="glyphicon")
      ) #menuitem
    )
  }) #output

  #
  #   jump among submenus or tabs
  #


  observeEvent(
    input[["jumpToPrevMenu"]],
    {
      tab_id_position <- match(CurrentM$menu, menulist) - 1
      if (tab_id_position == 0) tab_id_position <- length(menulist)
      CurrentM$menu <- menulist[tab_id_position]
      updateTabItems(session, "tabs", menulist[tab_id_position])
    }
  )

  observeEvent(
    input[["jumpToPrevMenu2"]],
    {
      tab_id_position <- match(CurrentM$menu, menulist) - 1
      if (tab_id_position == 0) tab_id_position <- length(menulist)
      CurrentM$menu <- menulist[tab_id_position]
      updateTabItems(session, "tabs", menulist[tab_id_position])
    }
  )

  observeEvent(
    input[["jumpToPrevMenu3"]],
    {
      tab_id_position <- match(CurrentM$menu, menulist) - 1
      if (tab_id_position == 0) tab_id_position <- length(menulist)
      CurrentM$menu <- menulist[tab_id_position]
      updateTabItems(session, "tabs", menulist[tab_id_position])
    }
  )

  observeEvent(
    input[["jumpToNextMenu"]],
    {
      tab_id_position <- match(CurrentM$menu, menulist) + 1
      if (tab_id_position > length(menulist)) tab_id_position <- 1
      CurrentM$menu <- menulist[tab_id_position]
      updateTabItems(session, "tabs", menulist[tab_id_position])
      updateTabItems(session, "tabsetpanel2", tablist2[1]) # avoids log error
    }
  )

  observeEvent(
    input[["jumpToNextMenu2"]],
    {
      tab_id_position <- match(CurrentM$menu, menulist) + 1
      if (tab_id_position > length(menulist)) tab_id_position <- 1
      CurrentM$menu <- menulist[tab_id_position]
      updateTabItems(session, "tabs", menulist[tab_id_position])
      updateTabItems(session, "tabsetpanel2", tablist2[1]) # avoids log error

    }
  )

  observeEvent(
    input[["jumpToNextMenu3"]],
    {
      tab_id_position <- match(CurrentM$menu, menulist) + 1
      if (tab_id_position > length(menulist)) tab_id_position <- 1
      CurrentM$menu <- menulist[tab_id_position]
      updateTabItems(session, "tabs", menulist[tab_id_position])
      updateTabItems(session, "tabsetpanel2", tablist2[1]) # avoids log error

    }
  )

  observeEvent(
    input[["jumpToNext"]],
    {
      tab_id_position <- match(Current$Tab, tablist) + 1
      if (tab_id_position > length(tablist)) tab_id_position <- 1
      Current$Tab <- tablist[tab_id_position]
      updateTabItems(session, "tabsetpanel1", tablist[tab_id_position])
    }
  )

  observeEvent(
    input[["jumpToPrev"]],
    {
      tab_id_position <- match(Current$Tab, tablist) - 1
      if (tab_id_position == 0) tab_id_position <- length(tablist)
      Current$Tab <- tablist[tab_id_position]
      updateTabItems(session, "tabsetpanel1", tablist[tab_id_position])
    }
  )

  observeEvent(
    input[["jumpToNext2"]],
    {
      tab_id_position <- match(Current$Tab2, tablist2) + 1
      if (tab_id_position > length(tablist2)) tab_id_position <- 1
      Current$Tab2 <- tablist2[tab_id_position]
      updateTabItems(session, "tabsetpanel2", tablist2[tab_id_position])
    }
  )

  observeEvent(
    input[["jumpToPrev2"]],
    {
      tab_id_position <- match(Current$Tab2, tablist2) - 1
      if (tab_id_position == 0) tab_id_position <- length(tablist2)
      Current$Tab2 <- tablist2[tab_id_position]
      updateTabItems(session, "tabsetpanel2", tablist2[tab_id_position])
    }
  )

# objects in global

  #
  # param presets
  #

  observeEvent(input$exampleButton, {

    updateNumericInput(session, "karHeight", value = karHeightVec[as.numeric(input$exampleId)]  )
    updateNumericInput(session, "karHeiSpace", value = karHeiSpaceVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "amoSepar", value = amoSeparVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "karSepar", value = karSeparVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "chrWidth",  value = chrWidthVec[as.numeric(input$exampleId)] )

    updateNumericInput(session, "squareness",  value = squarenessVec[as.numeric(input$exampleId)] )
    updateRadioButtons(session, "orderChr",  selected = ((orderChrVec[as.numeric(input$exampleId)] ) ) )
    updateCheckboxInput(session, "useOneDot",  value = useOneDotVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "pMarkFac",  value = pMarkFacVec[as.numeric(input$exampleId)] )


    #circularPlot
    updateNumericInput(session, "chrLabelSpacing",  value = chrLabelSpacingVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "rotation",  value = rotationVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "shrinkFactor",  value = shrinkFactorVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "radius",  value = radiusVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "circleCenter",  value = circleCenterVec[as.numeric(input$exampleId)] )

    updateNumericInput(session, "OTUsrt",  value = OTUsrtVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "OTUjustif",  value = OTUjustifVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "OTULabelSpacerx",  value = OTULabelSpacerxVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "OTUlegendHeight",  value = OTUlegendHeightVec[as.numeric(input$exampleId)] )
    updateRadioButtons(session, "OTUplacing",  selected = ((OTUplacingVec[as.numeric(input$exampleId)] ) ) )

    # general
    updateNumericInput(session, "chrSpacing",  value = as.numeric((chrSpacingVec[as.numeric(input$exampleId)] ) ) )
    updateRadioButtons(session, "morpho",  selected = ((morphoVec[as.numeric(input$exampleId)])))
    updateTextInput(session, "chrColor",  value = chrColorVec[as.numeric(input$exampleId)] )
    updateTextInput(session, "cenColor",  value = cenColorVec[as.numeric(input$exampleId)] )

    updateRadioButtons(session, "chrIndex",  selected = ((chrIndexVec[as.numeric(input$exampleId)] ) ) )
    updateCheckboxInput(session, "chrSize",  value = as.logical((chrSizeVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "indexIdTextSize",  value = as.numeric((indexIdTextSizeVec[as.numeric(input$exampleId)] ) ) )

    updateCheckboxInput(session, "chrSizeMbp",  value = as.logical((chrSizeMbpVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "distTextChr",  value = as.numeric((distTextChrVec[as.numeric(input$exampleId)] ) ) )

    updateCheckboxInput(session, "chromatids",  value = chromatidsVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "xModifier",  value = as.numeric(xModifierVec[as.numeric(input$exampleId)] ) )

    updateCheckboxInput(session, "circularPlot",  value = circularPlotVec[as.numeric(input$exampleId)] )

    updateCheckboxInput(session, "ruler",  value = rulerVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "rulerPos",  value = as.numeric((rulerPosVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "ruler.tck",  value = as.numeric((ruler.tckVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "rulerNumberSize",  value = as.numeric((rulerNumberSizeVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "xPosRulerTitle",  value = as.numeric((xPosRulerTitleVec[as.numeric(input$exampleId)] ) ) )

    updateRadioButtons(session, "legend",  selected = legendVec[as.numeric(input$exampleId)] )
    updateRadioButtons(session, "cenFormat",  selected = cenFormatVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "cenFactor",  value = cenFactorVec[as.numeric(input$exampleId)] )

    updateNumericInput(session, "legendWidth",  value = as.numeric((legendWidthVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "legendHeight", value = as.numeric((legendHeightVec[as.numeric(input$exampleId)] ) ) )

    updateCheckboxInput(session, "fixCenBorder",  value = as.logical((fixCenBorderVec[as.numeric(input$exampleId)] ) ) )
    updateCheckboxInput(session, "chrBorderColor",  value = ((chrBorderColorVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "lwd.chr",  value = as.numeric((lwd.chrVec[as.numeric(input$exampleId)] ) ) )

    updateNumericInput(session, "xlimLeftMod",  value = as.numeric((xlimLeftModVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "xlimRightMod",  value = xlimRightModVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "ylimBotMod",  value = ylimBotModVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "ylimTopMod",  value = as.numeric((ylimTopModVec[as.numeric(input$exampleId)] ) ) )

    updateNumericInput(session, "widFactor",  value = as.numeric((widFactorVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "heiFactor",  value = as.numeric((heiFactorVec[as.numeric(input$exampleId)] ) ) )

    updateTextInput(session, "mycolors", value = mycolorsVec[as.numeric(input$exampleId)])
    updateTextInput(session, "chrNamesToSwap", value = chrNamesToSwapVec[as.numeric(input$exampleId)])

    updateCheckboxInput(session, "addOTUName",  value = addOTUNameVec[as.numeric(input$exampleId)] )
    updateRadioButtons(session, "OTUfont",  selected = as.character(OTUfontVec[as.numeric(input$exampleId)] ) )

    updateTextInput(session, "moveKarHor", value = moveKarHorVec[as.numeric(input$exampleId)])
    updateNumericInput(session, "mkhValue",  value = mkhValueVec[as.numeric(input$exampleId)]  )
    updateCheckboxInput(session, "anchor",  value = anchorVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "moveAnchorV",  value = moveAnchorVVec[as.numeric(input$exampleId)]  )
    updateNumericInput(session, "moveAnchorH",  value = moveAnchorHVec[as.numeric(input$exampleId)]  )


    updateNumericInput(session, "notesTextSize",  value = notesTextSizeVec[as.numeric(input$exampleId)]  )
    updateNumericInput(session, "notesPosX",  value = notesPosXVec[as.numeric(input$exampleId)]  )

    updateCheckboxInput(session, "OTUasNote",  value = OTUasNoteVec[as.numeric(input$exampleId)] )


  }, ignoreInit = F )

  # observe({
  # })

  #
  #   update chr. data.frame
  #

  observeEvent(input$exampleButton, {
    values[["df1"]] <- get((chrDataVec[as.numeric(input$exampleId)] ) )
    values[["df1Name"]] <- (chrDataVec[as.numeric(input$exampleId)] )
    values[["df1Origin"]] <- "data.frame was loaded from package"
  } )

  observeEvent(input$file1, {
    inFile <- input[["file1"]]
    values[["df1"]] <- read.csv(inFile$datapath, header = TRUE)
    values[["df1Name"]] <- inFile$name
    values[["df1Origin"]] <- "file was loaded"
  })

  #
  #   update mark pos data.frame
  #

  observeEvent(input$exampleButton, {
    values[["df1Mark"]] <- get((markDataVec[as.numeric(input$exampleId)] ) )
    values[["df1MarkName"]] <- (markDataVec[as.numeric(input$exampleId)] )
    values[["df1MarkOrigin"]] <- "data.frame was loaded from package"
  })

  observeEvent(input$file1Mark, {
    inFile <- input[["file1Mark"]]
    values[["df1Mark"]] <- read.csv(inFile$datapath, header = TRUE)
    values[["df1MarkName"]] <- inFile$name
    values[["df1MarkOrigin"]] <- "file was loaded"
  })

  #
  #   update mark style data.frame
  #

  observeEvent(input$exampleButton, {
    values[["df1MStyle"]] <- tryCatch(get((MStyleDataVec[as.numeric(input$exampleId)] ) ), error=function(e){data.frame()} )
    values[["df1MStyleName"]] <- tryCatch( (MStyleDataVec[as.numeric(input$exampleId)] ), error=function(e){"no"} )
    values[["df1MStyleOrigin"]] <- "data.frame was loaded from package"
  })

  observeEvent(input$file1MStyle, {
    inFile <- input[["file1MStyle"]]
    values[["df1MStyle"]] <- read.csv(inFile$datapath, header = TRUE)
    values[["df1MStyleName"]] <- inFile$name
    values[["df1MStyleOrigin"]] <- "file was loaded"
  })

  #
  #   add or remove columns chr
  #

  observeEvent(input$addColumn,{

    final <- values[["df1"]]
    if(inherits(final,"data.frame")) {
    if(nrow(final)>0) {
    if(input$colType=="character"){
      final[,input$col] <- as.character(NA)
    } else {
      final[,input$col] <- as.numeric(NA)
    }
    } else {
      final <- data.frame(chrName="chrName"
                          ,shortArmSize="shortArmSize"
                          ,longArmSize="longArmSize"
                          ,chrSize="chrSize"
                          ,OTU="1"
      )
    }
    } else {

      final <- data.frame(chrName="chrName"
                          ,shortArmSize="shortArmSize"
                          ,longArmSize="longArmSize"
                          ,chrSize="chrSize"
                          ,OTU=1
                          )

    }
    values[["df1"]] <- final
})

  observeEvent(input$removeColumn,{
    final <- values[["df1"]]
    final[,input$col] <- NULL
    values[["df1"]] <- final
  })

  #
  #   add or remove columns mark
  #

  observe({
    swapI<- tryCatch(unlist(strsplit(input$chrNamesToSwap,",") )
                     , error=function(e){NULL} )

    values[["chrNamesToSwap"]] <- swapI
  })

  observeEvent(input$addColumnMark,{

    final <- values[["df1Mark"]]
    if(inherits(final,"data.frame")){
    if(nrow(final)>0) {
      if(input$colTypeMark=="character"){
        final[,input$colMark] <- as.character(NA)
      } else {
        final[,input$colMark] <- as.numeric(NA)
      }
    } else { # if no rows

      final <- data.frame(chrName="chrName"
                          ,markName="markName"
                          ,chrRegion="chrRegion"
                          ,markPos="pos."
                          ,markSize="markSize")

      if("OTU" %in% colnames( values[["df1"]] )) {
        final$OTU <-as.character(NA)
      }
    }

    } else {

      final <- data.frame(chrName="chrName"
                          ,markName="markName"
                          ,chrRegion="chrRegion"
                          ,markPos="pos."
                          ,markSize="markSize")

      if("OTU" %in% colnames( values[["df1"]] )) {
        final$OTU <-as.character(NA)
      }
    }
    values[["df1Mark"]] <- final
  })


  observeEvent(input$removeColumnMark,{

    final <- values[["df1Mark"]]
    final[,input$colMark] <- NULL
    values[["df1Mark"]] <- final
  })

  #
  #   add or remove columns mark style
  #

  observeEvent(input$addColumnMStyle,{

    final <- values[["df1MStyle"]]
    if(inherits(final,"data.frame")){
    if(nrow(final)>0){
      if(input$colTypeMStyle=="character"){
        final[,input$colMStyle] <- as.character(NA)
      } else {
        final[,input$colMStyle] <- as.numeric(NA)
      }
    } else {
      final <- data.frame(markName="markName",markColor="colorName",style="dots")
    }
    } else {
      final <- data.frame(markName="markName",markColor="colorName",style="dots")
    }
    values[["df1MStyle"]] <- final
  })

  observeEvent(input$removeColumnMStyle,{

    final <- values[["df1MStyle"]]
    final[,input$colMStyle] <- NULL
    values[["df1MStyle"]] <- final
  })

  #
  #   keep inputs chr
  #

  observe({
    if (!is.null(input$out)) {
      tryCatch(df <- hot_to_r(input$out), error = function(e) {NA; NA } )
      if (!is.function(df)) {
        values[["df1"]] <- df
      }
    }
  })

  #
  #   keep inputs mark
  #

  observe({
    if (!is.null(input$outMark)) {
      tryCatch(df <- hot_to_r(input$outMark), error = function(e) {NA; NA } )
      if (!is.function(df)) {
        values[["df1Mark"]] <- df
      }
    }
  })

  #
  #   keep inputs mark style
  #

  observe({
    if (!is.null(input$outMStyle)) {
      tryCatch(df <- hot_to_r(input$outMStyle), error = function(e) {NA; NA } )
      if (!is.function(df)) {
        values[["df1MStyle"]] <- df
      }
    }
  })

  #
  #   to reactive
  #

  observeEvent(input$exampleButton,{
    values[["dfList"]] <- list()
  })

  #
  #   download chr.
  #

  output$buttontable <-  renderUI({
    validate(
      need(try(values[["df1"]]), "") #was df
    )
    downloadButton('downloadCsv', 'Download table as .csv')
  })

  output$buttontable2 <-  renderUI({
    validate(
      need(try(values[["df1"]]), "") #wdf
    )
    if(input$saveType=="csv"){
      downloadButton('downloadCsv2', 'Download table as .csv')
    } else {
      downloadButton('downloadRds2', 'Download table as .rds')
    }
  })

  #
  #   download mark
  #

  output$buttontableMark <-  renderUI({
    validate(
      need(try(values[["df1Mark"]]), "")
    )
    if (nrow(values[["df1Mark"]]) > 0 ) {
    downloadButton('downloadCsvMark', 'Download table as .csv')
    }
  })

  output$buttontableMark2 <-  renderUI({
    validate(
      need(try(values[["df1Mark"]]), "")
    )
    if (nrow(values[["df1Mark"]]) > 0 ) {
    if(input$saveType=="csv"){
      downloadButton('downloadCsvMark2', 'Download table as .csv')
    } else {
      downloadButton('downloadRdsMark2', 'Download table as .rds')
    }
    }
  })

  #
  #   download mark style
  #

  output$buttontableMStyle <-  renderUI({
    validate(
      need(try(values[["df1MStyle"]]), "")
    )
    if (nrow(values[["df1MStyle"]]) > 0 ) {
    downloadButton('downloadCsvMStyle', 'Download table as .csv')
    }
  })

  output$buttontableMStyle2 <-  renderUI({
    validate(
      need(try(values[["df1MStyle"]]), "")
    )
    if (nrow(values[["df1MStyle"]]) > 0 ) {
      if(input$saveType=="csv"){
        downloadButton('downloadCsvMStyle2', 'Download table as .csv')
      } else {
        downloadButton('downloadRdsMStyle2', 'Download table as .rds')
      }
    }
  })

  #
  #   downloadHandler chr
  #

  output$downloadCsv <- downloadHandler(
    filename = function() {
      # paste0(input$chrfilename)
      paste0(input$chrfilename,".csv")
    },
    content = function(file) {
      write.csv(values[["df1"]], file, na="", # wdf
                row.names = FALSE, quote = TRUE)
    }
  )

  output$downloadCsv2 <- downloadHandler(
    filename = function() {
      paste0(input$chrfilename2,".csv")
    },
    content = function(file) {
      write.csv(values[["df1"]], file, na="", # wdf
                row.names = FALSE, quote = TRUE)
    }
  )

  output$downloadRds2 <- downloadHandler(
    filename = function() {
      paste0(input$chrfilename2,".rds")
    },
    content = function(file) {
      saveRDS(values[["df1"]], file,
                )
    }
  )

  observeEvent(input$chrfilename,
               {
                 updateTextInput(session = session,
                                     inputId = "chrfilename2",
                                     value = isolate(input$chrfilename)
                 )
               }
  )

  observeEvent(input[["chrfilename2"]],
               {
                 updateTextInput(session = session,
                                     inputId = "chrfilename",
                                     value = isolate(input$chrfilename2)
                 )
               })

  #
  #   downloadHandler Mark
  #

  output$downloadCsvMark <- downloadHandler(
    filename = function() {
      # paste0(input$markfilename)
      paste0(input$markfilename,".csv")
    },
    content = function(file) {
      write.csv(values[["df1Mark"]], file, na="",
                row.names = FALSE, quote = TRUE)
    }
  )

  output$downloadCsvMark2 <- downloadHandler(
    filename = function() {
      # paste0(input$markfilename2)
      paste0(input$markfilename2,".csv")
    },
    content = function(file) {
      write.csv(values[["df1Mark"]], file, na="",
                row.names = FALSE, quote = TRUE)
    }
  )

  output$downloadRdsMark2 <- downloadHandler(
    filename = function() {
      paste0(input$markfilename2,".rds")
    },
    content = function(file) {
      saveRDS(values[["df1Mark"]], file,
      )
    }
  )

  observeEvent(input$markfilename,
               {
                 updateTextInput(session = session,
                                 inputId = "markfilename2",
                                 value = isolate(input$markfilename)
                 )
               }
  )

  observeEvent(input[["markfilename2"]],
               {
                 updateTextInput(session = session,
                                 inputId = "markfilename",
                                 value = isolate(input$markfilename2)
                 )
               })

  #
  #   downloadHandler MStyle style
  #

  output$downloadCsvMStyle <- downloadHandler(
    filename = function() {
      # paste0(input$MStylefilename)
      paste0(input$MStylefilename,".csv")
    },
    content = function(file) {
      write.csv(values[["df1MStyle"]], file, na="",
                row.names = FALSE, quote = TRUE)
    }
  )

  output$downloadCsvMStyle2 <- downloadHandler(
    filename = function() {
      # paste0(input$MStylefilename2)
      paste0(input$MStylefilename2,".csv")
    },
    content = function(file) {
      write.csv(values[["df1MStyle"]], file, na="",
                row.names = FALSE, quote = TRUE)
    }
  )

  output$downloadRdsMStyle2 <- downloadHandler(
    filename = function() {
      paste0(input$MStylefilename2,".rds")
    },
    content = function(file) {
      saveRDS(values[["df1MStyle"]], file,
      )
    }
  )


  observeEvent(input$MStylefilename,
               {
                 updateTextInput(session = session,
                                 inputId = "MStylefilename2",
                                 value = isolate(input$MStylefilename)
                 )
               }
  )

  observeEvent(input[["MStylefilename2"]],
               {
                 updateTextInput(session = session,
                                 inputId = "MStylefilename",
                                 value = isolate(input$MStylefilename2)
                 )
               })

  #
  #   render chr table
  #

  output$out <- renderRHandsontable({
    validate(need(try(values[["df1"]]),"Hint: Start in previous tab (left) loading example" ))
    if(invalid(values[["df1"]])) {
      return()
    } else {
      hot <- rhandsontable(values[["df1"]], width = 700)
      hot
    }
  })

  #
  #   render mark table
  #

  output$outMark <- renderRHandsontable({
    validate(need(try(values[["df1Mark"]]),"Hint: Start in first tab (left) loading example" ))
    if(invalid(values[["df1Mark"]])) {
      return()
    } else {
      hot <- rhandsontable(values[["df1Mark"]], width = 700)
      hot
    }
  })

  #
  #   render mark style table
  #

  output$outMStyle <- renderRHandsontable({
    validate(need(try(values[["df1MStyle"]] ),"Hint: Colors can be added in next page under 'Color' (optional)" ))
    if(invalid(values[["df1MStyle"]] ) ) {
      return(data.frame())
    } else if(nrow(values[["df1MStyle"]])>0 & "style" %in% colnames(values[["df1MStyle"]] ) ) {

      Options<-c("dots","square","squareLeft", "cM","cMLeft","cenStyle", "upArrow", "downArrow"
                 ,"exProtein","inProtein")

      tmpExampleTable <- rhandsontable(values[["df1MStyle"]],
                                       rowHeaders = NULL,
                                       stretchH = "all",
                                       selectCallback = TRUE
                                       , width = 400, height = 400
                                       ) %>%
        hot_col("style", allowInvalid = FALSE, type = "dropdown"
                , readOnly = TRUE)
      if(!is.null(input$outMStyle_select$select$r)) {
      tmpExampleTable <- hot_col(tmpExampleTable,
                                 col = "style",
                                 allowInvalid = FALSE,
                                 type = "dropdown",
                                 source = Options) %>%
        hot_cell(row = input$outMStyle_select$select$r
        , col = "style",
        readOnly = FALSE)
      }
      tmpExampleTable
    } else {
      hot <- rhandsontable(values[["df1MStyle"]])
      hot
    }
    # }  else {
      # data.frame()
    # }
  })

  #
  #   first tab choose example
  #

  output$examplepanel <- renderUI({
    fluidRow(
      column(width=4,
             br()
             ,wellPanel(
               h4("Choose an example")
               ,helpText("this will overwrite any data you have entered in other tabs")
               ,radioButtons("exampleId","",
                            c("monocentric k."=1,
                              "holocentric k."=2,
                              "multiple mono. k."=3,
                              "multiple holo. k."=4,

                              "holocentric k. mycolors"=5
                              ,"GISH"=6
                              ,"GISH holoc."=7
                              ,"circular Plot"=8
                              )
                            ,selected = 1
                            )
               ,actionButton("exampleButton",
                             "Load Example",
                             class = "btn-success")
             ) # wP
      ) # c
    ) # fR
  })

  #
  #   second tab chr data
  #

  output$dfchrpanel <- renderUI({

    fluidRow(
      column(width=2
             ,br()
                ,wellPanel(
                h4("Save data")
                ,helpText("optional")
                ,textInput("chrfilename",
                           "File name",
                           value = "chrData"
                           ,width = "50%")
                ,uiOutput("buttontable")
              ) # end wellpanel
              ,wellPanel(
                h4("Upload data")
                ,helpText("optional")
                ,fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                )
              ) # end wellpanel
      ) #end column
      ,column(width=2,
             br()
             ,wellPanel(
               h4("Add/Remove Row")
               ,helpText(""
                        , p("Right-click inside the table to delete/insert rows.")
               )
             )
             # ,br()
             ,wellPanel(
               h4("Add/Remove Column")
               ,helpText("Use unique names")
               ,textInput("col",
                          "Col. name",
                          value = "newCol"
                          ,width = "50%")
               ,radioButtons("colType","Col. data type",c("character","numeric") )
               ,actionButton("addColumn",
                            "Add")
               ,actionButton("removeColumn",
                             "Remove")
             ) # end wellpanel
             # br()
      ) # c

      ,column(width=5,
              br()
              ,fluidRow(
                column(width=6
                       ,wellPanel(
                         h4("swap arms")
                         ,textInput('chrNamesToSwap', 'chr. names', "1,2")
                         ,actionButton("swapButton","Swap!",icon = icon("rotate") )
                       ))
              )
              ,wellPanel(
                h2("Chr. data data.frame")
                ,helpText(paste(values[["df1Name"]], values[["df1Origin"]] ) )
              )
              ,rHandsontableOutput("out")
      ) # col
    ) # end fluidrow
  })

  output$dfmarkpanel <- renderUI({
    fluidRow(
      column(width=2
             ,br()
             ,wellPanel(
               h4("Save data")
               ,helpText("optional")
               ,textInput("markfilename",
                          "File name",
                          value = "markData"
                          ,width = "50%")
               # ,actionButton("saveChrData",
               # "Save")
               ,uiOutput("buttontableMark")
             ) # end wellpanel
             ,wellPanel(
               h4("Upload data")
               ,helpText("optional")
               ,fileInput("file1Mark", "Choose CSV File",
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
               )
             ) # end wellpanel
      ) #end column
      ,column(width=2,
              br()
              ,wellPanel(
                h4("Add/Remove Row")
                ,helpText(""
                          , p("Right-click inside the table to delete/insert rows.")
                )
              )
              # ,br()
              ,wellPanel(
                h4("Add/Remove Column")
                ,helpText("Use unique names")
                ,textInput("colMark",
                           "Col. name",
                           value = "newCol"
                           ,width = "50%")
                ,radioButtons("colTypeMark","Col. data type",c("character","numeric") )
                ,actionButton("addColumnMark",
                              "Add")
                ,actionButton("removeColumnMark",
                              "Remove")
              ) # end wellpanel
              # br()
      ) # c

      ,column(width=5,
              br()
              ,wellPanel(
                h2("Mark pos. data.frame")
                ,helpText(paste(values[["df1MarkName"]], values[["df1MarkOrigin"]] ) )
              )
              ,rHandsontableOutput("outMark")
      ) # col
    ) # end fluidrow
  })



  # dfMStylepanel


  #
  #   mark style
  #

  output$dfMStylepanel <- renderUI({
    fluidRow(
      column(width=2
             ,br()
             ,wellPanel(
               h4("Save data")
               ,helpText("optional")
               ,textInput("MStylefilename",
                          "File name",
                          value = "MStyleData"
                          ,width = "50%")
               # ,actionButton("saveChrData",
               # "Save")
               ,uiOutput("buttontableMStyle")
             ) # end wellpanel
             ,wellPanel(
               h4("Upload data")
               ,helpText("optional")
               ,fileInput("file1MStyle", "Choose CSV File",
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
               )
             ) # end wellpanel
      ) #end column
      ,column(width=2,
              br()
              ,wellPanel(
                h4("Add/Remove Row")
                ,helpText(""
                          , p("Right-click inside the table to delete/insert rows.")
                )
              )
              # ,br()
              ,wellPanel(
                h4("Add/Remove Column")
                ,helpText("Use unique names")
                ,textInput("colMStyle",
                           "Col. name",
                           value = "newCol"
                           ,width = "50%")
                ,radioButtons("colTypeMStyle","Col. data type",c("character","numeric") )
                ,actionButton("addColumnMStyle",
                              "Add")
                ,actionButton("removeColumnMStyle",
                              "Remove")
              ) # end wellpanel
              # br()
      ) # c

      ,column(width=5,
              br()
              ,wellPanel(
                h2("Mark style data.frame")
                ,helpText(paste(values[["df1MStyleName"]], values[["df1MStyleOrigin"]] ) )
              )
              ,rHandsontableOutput("outMStyle")
      ) # col
    ) # end fluidrow
  })

#
  #
  #   parameters ###################
  #

  output$circParam = renderUI({
      wellPanel(
        h4("circular plot param."),
        splitLayout(
          div(
            title="`chrLabelSpacing`: numeric, for `circularPlot=TRUE`. Spacing of chr. labels. Defaults to `0.5`",
          numericInput("chrLabelSpacing"
                       ,"chr. name dist."
                       ,chrLabelSpacingDefault
                       , min = 0.25, max = 10, step=0.25
                       )
          )
          ,div(
            title="`rotation`: numeric, anti-clockwise rotation, defaults to `0.5` which rotates first chr. from top to -90 degrees. (-0.5*π = 9 o'clock)"
        ,numericInput("rotation"
                      ,"Rotation"
                      ,rotationDefault
                      , min = 0, max = 2*pi, step=0.05
        )
          )
      )
      ,splitLayout(
        div(title="`shrinkFactor`:	numeric, for `circularPlot=TRUE` percentage of usage of circle. Defaults to `0.9`
", numericInput("shrinkFactor"
                     ,"shrink kar."
                     ,shrinkFactorDefault
                     , min = 0.2, max = 1, step=0.05
        )
        )
,div(title="`radius`: numeric, for `circularPlot=TRUE`. Affects radius of karyotypes. Defaults to `0.5`
"
        ,numericInput("radius"
                      ,"radius"
                      ,radiusDefault
                      , min = 0.25, max = 30, step=0.05
        )
)
      ) #sL
      ,splitLayout(
        div(title="`circleCenter`:	numeric, for `circularPlot=TRUE`. Affects coordinates of center of circles. Affects `legend='aside'` position."
            ,numericInput("circleCenter"
                     ,"coord. center X-axis"
                     ,circleCenterDefault
                     , min = -5, max = 15, step=0.5
        )
        )
      ) #sL
      ) # wP
    # }
  })


  output$circParamOTU = renderUI({
    # if(input$circularPlot) {
      wellPanel(
        h3("OTU name"),
        h5("(circ. plot)")
        ,fluidRow(
          column(6
                 ,div(title='`OTUplacing`:	character, for `circularPlot=TRUE`. location of OTU name. Defaults to `"first"`, which plots name of OTU near first chr. `"number"` places number near 1st chr. and index and name of OTU to the right or center. `"simple"` places name of OTU to the right or center without numbering. See also `OTUcentered`'
                      ,radioButtons("OTUplacing","position"
                                    ,c("first","number","simple")
                                    ,selected=OTUplacingDefault
                      )
                 )
          ),column(6,
                  div(title="`OTUsrt`: numeric, for `circularPlot=TRUE` and `OTUplacing='first'` Angle to use for OTU names. Defaults to `0`. See `OTUplacing`"

          ,numericInput("OTUsrt"
                       ,"Angle"
                       ,OTUsrtDefault
                       , min = -360, max = 360, step=1
          )
          )
          ,div(title='`OTUjustif`: numeric, for `circularPlot=TRUE` and `OTUplacing="number"` or `"simple"`. Justification of OTU name. `0` = left (Default); use `0.5` for centered. See `?text` -> `adj`
'
          , numericInput("OTUjustif"
                                  ,"Justif."
                                  ,OTUjustifDefault
                                  , min = 0, max = 1, step=0.5
          )
          )
        )
        )
        ,splitLayout(
          div(title='`OTULabelSpacerx`: numeric, for `circularPlot=TRUE` and `OTUplacing="number"` or `"simple"`. Modifies x names position'
          ,numericInput("OTULabelSpacerx"
                        ,HTML(paste("position X-axis") )
                        ,OTULabelSpacerxDefault
                        , min = -10, max = 10, step=.5
          )
          )
          ,div(title='`OTUlegendHeight`: numeric, for `circularPlot=TRUE` and `OTUplacing="number"` or `"simple"`. Modifies y names separation'
          , numericInput("OTUlegendHeight"
                         ,HTML(paste("vert. size") )
                         ,OTUlegendHeightDefault
                         , min = 0.5, max = 10, step=0.5
          )
          )
        ) # sL
      ) #wp
    # }
  } )

  # "",

  output$imageWidth<- renderUI({
    px<-session$clientData$output_idiogramPlot_width*(as.numeric(input$widFactor)/100)
    HTML(paste(paste("Width:",px,"px"),paste(px/80,"in"), sep ="<br/>" ) )
  })

  output$imageHeight<- renderUI({
    px <-session$clientData$output_idiogramPlot_width*(as.numeric(input$heiFactor)*(as.numeric(input$widFactor)/100)
    )
    HTML(paste(paste("Heigth:"
          ,px,"px"),paste(px/80,"in"), sep ="<br/>" ) )
  })

observeEvent(input$exampleButton,{
  filename <- tempfile(fileext=".txt")
  # filename <- "mylog.txt"
  filenamePath <- normalizePath(filename, mustWork = F)

  values[["outfile"]] <- filenamePath

})

add0<-"{"
add1<-"#install.packages('svglite')"
add2<-"library(svglite)"
add2b<-"library(idiogramFISH)"

curr_date <- eventReactive(list(input$btn, input$tabset), {

  format(Sys.time(), "%c")
})



observeEvent({
  input$swapButton
  } ,{
  # observeEvent(input$chrNamesToSwap,{
  chrNamesToSwapPar <- if( length( values[["chrNamesToSwap"]] )==0 ) {
    "" } else {
      values[["chrNamesToSwap"]]
    }

  tryCatch(dfList <- swapChrRegionDfSizeAndMarks(values[["df1"]], values[["df1Mark"]], chrNamesToSwapPar)
           , error=function(e) {"waiting for d.f."} )

  tryCatch(values[["dfList"]] <- dfList
           , error=function(e) {"waiting for d.f."} )

    if(inherits(values[["dfList"]]$dfChrSize, "data.frame" ) ) {
df1<-values[["dfList"]]$dfChrSize
  values[["df1"]] <- df1
  df1Mark<-  values[["dfList"]]$dfMarkPos
  values[["df1Mark"]] <-df1Mark

    }
})

observe({
  values[["mycolors"]] <- tryCatch(unlist(strsplit(input$mycolors,",") )
                                   , error=function(e){NULL} )

  dfChrSizeName<- ifelse(invalid(values[["df1"]] )
                         ,shQuote("")
                         ,ifelse(input$saveType=="rds"
                         ,input$chrfilename2
                   ,shQuote(paste0(isolate(input$chrfilename2),".csv" ) )
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
                      dfChrSize  = dfChrSizeName
                      ,dfMarkPos = dfMarkPosName
                      ,dfMarkColor= dfMarkColorName
                      ,karHeight=input$karHeight,             # kar. height
                      karHeiSpace = input$karHeiSpace,        # kar. height
                      amoSepar = input$amoSepar,
                      karSepar = input$karSepar,
                      legend = shQuote(input$legend),
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

                      chrWidth = input$chrWidth,           # chr. width
                      chrSpacing = input$chrSpacing,           # space among chr.

                      squareness = input$squareness
                      ,orderChr = shQuote(input$orderChr)
                      ,useOneDot = input$useOneDot
                      ,pMarkFac = input$pMarkFac

                      ,chrLabelSpacing = input$chrLabelSpacing
                        ,rotation = input$rotation
                        ,shrinkFactor  = input$shrinkFactor
                        ,radius   = input$radius
                        ,circleCenter  = input$circleCenter

                        ,OTUsrt = input$OTUsrt
                        ,OTUjustif  = input$OTUjustif
                        ,OTULabelSpacerx= input$OTULabelSpacerx
                        ,OTUlegendHeight= input$OTUlegendHeight
                        ,OTUplacing  = shQuote(input$OTUplacing)

                      ,morpho=shQuote(input$morpho),          # chr. morpho. classif. (Guerra, Levan, both, "" ) ver. >= 1.12 only
                      chrIndex=shQuote(input$chrIndex),            # cen. pos. (CI, AR, both, "" ) ver. >= 1.12 only
                      chrSize = input$chrSize,           # add chr. sizes under chr.
                      indexIdTextSize = input$indexIdTextSize,
                      chrSizeMbp = input$chrSizeMbp,        # add Mbp sizes under chr. (see above)
                      chromatids = input$chromatids,
                      xModifier = input$xModifier,

                      circularPlot = input$circularPlot,

                      addOTUName = input$addOTUName,
                      OTUfont = as.numeric(input$OTUfont),
                      moveKarHor = shQuote(input$moveKarHor),
                      mkhValue = input$mkhValue,
                      anchor = input$anchor,
                      moveAnchorV = input$moveAnchorV,
                      moveAnchorH = input$moveAnchorH,

                      OTUasNote = input$OTUasNote,
                      notesTextSize = input$notesTextSize,
                      notesPosX = input$notesPosX ,

                      ruler = input$ruler,
                      rulerPos = input$rulerPos,              # position of ruler
                      ruler.tck = input$ruler.tck,          # size and orientation of ruler ticks
                      rulerNumberSize=input$rulerNumberSize        # font size of rulers
                      ,xPosRulerTitle = input$xPosRulerTitle             # pos of ruler title

                      ,legendWidth=input$legendWidth            # width of legend items
                      ,legendHeight=input$legendHeight            # width of legend items

                      ,fixCenBorder = input$fixCenBorder      # use chrColor as border color of cen. or cen. marks
                      ,chrBorderColor = shQuote(input$chrBorderColor)
                      ,lwd.chr = input$lwd.chr

                      ,distTextChr = input$distTextChr        # chr. text separation

                      ,xlimLeftMod = input$xlimLeftMod          # xlim left param.
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
  # print(str(mclist))

  if(!"dfChrSize" %in% names(mclist) ){
    seq<-character()
  } else if (mclist$dfChrSize=="''"){
    seq<-character()
  }


  # print(seq)

  add3 <- paste0('svg("dfOfChrSize.svg",width=',values[["mysvgwidth"]],", height=",values[["mysvgheight"]],')')

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

  block<-ifelse(input$asSvg,"","#")

  values[["strFun"]] <- paste0(add0,"\n"
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



}
) # observe

output$downloadR <- downloadHandler(

  filename = "script.R",
  content = function(file) {
    writeLines(scriptR(), file
              )
  }
)

scriptR <- reactive({
  df <- values[["strFun"]]
  df
})

output$buttonScript <-  renderUI({
  validate(
    need(try(scriptR()), "")
  )
  downloadButton('downloadR', 'Download R-script')
})


  # output$idiogramPlot <- renderPlot( {
output$idiogramPlot <- renderImage( {
  # input$goButton

  # isolate ({

validate(need(try(values[["df1"]]),"Start in previous page (left) with data.frames" ))
validate(need(try(inherits(values[["df1"]],"data.frame") ),"Start in previous page (left) with data.frames" ))
validate(need(try(nrow(values[["df1"]])>0 ),"Start in previous page (left) with data.frames" ))

      width  <- session$clientData$output_idiogramPlot_width*(as.numeric(input$widFactor)/100 )
      height <- session$clientData$output_idiogramPlot_width*(as.numeric(input$heiFactor)*(as.numeric(input$widFactor)/100)
      )

      mysvgwidth <- width/80
      mysvgheight <- height/80

      values[["mysvgwidth"]]<-mysvgwidth
      values[["mysvgheight"]]<-mysvgheight

      outfileSvg <- tempfile(fileext='.svg')

      # Generate the svg
      svg(outfileSvg, width=mysvgwidth, height=mysvgheight )

capture.output (
    # isolate(
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

                  chrWidth   = input$chrWidth,           # chr. width
                  chrSpacing = input$chrSpacing,           # space among chr.

                  squareness = input$squareness
                  ,orderChr  = input$orderChr
                  ,useOneDot = input$useOneDot
                  ,pMarkFac = input$pMarkFac

                  ,chrLabelSpacing = input$chrLabelSpacing
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
                  notesTextSize = input$notesTextSize,
                  notesPosX     = input$notesPosX ,

                  ruler          = input$ruler,
                  rulerPos       = input$rulerPos,              # position of ruler
                  ruler.tck      = input$ruler.tck,          # size and orientation of ruler ticks
                  rulerNumberSize= input$rulerNumberSize        # font size of rulers
                  ,xPosRulerTitle= input$xPosRulerTitle             # pos of ruler title

                  ,legendWidth     = input$legendWidth            # width of legend items
                  ,legendHeight    = input$legendHeight

                  ,fixCenBorder    = input$fixCenBorder      # use chrColor as border color of cen. or cen. marks
                  , chrBorderColor = input$chrBorderColor
                  ,lwd.chr         = input$lwd.chr
                  ,distTextChr     = input$distTextChr        # chr. text separation

                  ,xlimLeftMod     = input$xlimLeftMod          # xlim left param.
                  ,xlimRightMod    = input$xlimRightMod          # xlim left param.

                  ,ylimBotMod = input$ylimBotMod           # modify ylim bottom argument
                  ,ylimTopMod = input$ylimTopMod           # modify ylim top argument
    ) # plot
    ,    file = (outfile <- file(values[["outfile"]],"w")),type="message" #"output"
 ) # capture
# message(values[["outfile"]])
close(outfile)
dev.off()

# Return a list containing the filename
list(src = normalizePath(outfileSvg),
     contentType = 'image/svg+xml',
     width = width,
     height = height,
     alt = "My plot")

  # }) # isolate
} , deleteFile = T
) # end plot or image^

  filenameR <- eventReactive({input$exampleButton
    values[["df1"]]# data.frame of chr. s
    values[["df1MStyle"]]  # d.f of mark
    values[["df1Mark"]]     # df of mark
    input$karHeight              # kar. h
    input$karHeiSpace              # kar.
    input$amoSepar
    input$karSepar
    input$cenFormat
    input$cenFactor

    as.character(input$legend)
    values[["mycolors"]]

    input$chrColor
    input$cenColor
    input$chrWidth           # chr. width
    input$chrSpacing           # space am
    input$squareness
    input$orderChr
    input$useOneDot
    input$pMarkFac
    input$chrLabelSpacing
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
    input$notesTextSize
    input$notesPosX
    input$ruler
    input$rulerPos              # pos
    input$ruler.tck          # size a
    input$rulerNumberSize        # fon
    input$xPosRulerTitle             #
    input$legendWidth            # w
    input$legendHeight
    input$fixCenBorder      # use ch
    input$chrBorderColor
    input$lwd.chr
    input$distTextChr        # chr.
    input$xlimLeftMod          # xli
    input$xlimRightMod          # xl
    input$ylimBotMod           # modify y
    input$ylimTopMod           # modify y
                             }
                             ,{
    f<-values[["outfile"]]
  })

output$log <- renderText({
    rawText <- filenameR()
    validate(need(try(readLines(rawText),TRUE ), message = FALSE) )

      replacedText <- paste(tryCatch(readLines(rawText ),error=function(e){"nothing to write"} )
                                     , collapse = "\n")
      replacedText <- gsub("\\\033\\[[[:digit:]]{2}m","",replacedText )
      return(replacedText)
    }

)

output$code <- renderText({
  return(values[["strFun"]])
})

  output$logpanel = renderUI({ # was treechar2
    wellPanel(
      h2("log")
      ,verbatimTextOutput("log")
    ) # end
  }) # end output

  output$clip <- renderUI({
    rclipButton("clipbtn", "Copy code", values[["strFun"]], icon("clipboard"))
  })

  output$markdown <- renderUI({
    htmltools::tags$iframe(src = "README2.html", width = '100%',  height = 1000,  style = "border:none;")
    # HTML(markdown::markdownToHTML(knit('about.Rmd', quiet = TRUE)))
  })

  output$strpanel = renderUI({ # was treechar2
    fluidRow(
      column(width=6
    ,wellPanel(
      rclipboardSetup()
      ,splitLayout(
      uiOutput("clip")
      ,div(style = 'max-width:100px;'
      ,uiOutput("buttonScript")
      )
      ,checkboxInput("asSvg","to .svg (keeps size)",TRUE)
      ,checkboxInput("keepDefault","keep default values",FALSE)
      ,radioButtons("saveType","download format",c("csv","rds"),"csv",inline = T)
      )
      ,verbatimTextOutput("code")

    ) # end
      ),
    column(width=2
           ,wellPanel(
             h4("Save chr. data")
             ,helpText("")
             ,textInput("chrfilename2",
                        "File name",
                        value = "chrData"
                        ,width = "50%")
             ,uiOutput("buttontable2")
           ) # end wellpanel
           ,wellPanel(
             h4("Save mark pos. data")
             ,helpText("")
             ,textInput("markfilename2",
                        "File name",
                        value = "markData"
                        ,width = "50%")
             ,uiOutput("buttontableMark2")
           ) # end wellpanel
           ,wellPanel(
             h4("Save mark style data")
             ,helpText("")
             ,textInput("MStylefilename2",
                        "File name",
                        value = "MStyleData"
                        ,width = "50%")
             ,uiOutput("buttontableMStyle2")
           ) # end wellpanel
           )
    # ,column(3
    #         ,wellPanel(
    #           h4("Save chr. data")
    #           ,helpText("optional")
    #           ,textInput("",
    #                      "File name",
    #                      value = ""
    #                      ,width = "50%")
    #
    #         ) # wP
    # )
    ) # fR
  }) # end output

  output$parameterPanel = renderUI({
    fluidRow(
      column(width=2
             ,br()
             ,wellPanel(
               fluidRow(
                 column(6
                        ,h4("Notes")
                        ,div(style= "margin-top: 15px;margin-bottom:-10px"
                             ,title="`notesTextSize`: (`0.4`) numeric, font size of notes, see `notes`"
                             ,numericInput("notesTextSize"
                                           ,HTML(paste("font", "size", sep = "<br/>") )
                                           ,notesTextSizeDefault, min = 0.1, max = 10, step=0.1)
                        )
                 ),column(6
                          ,div(style= "margin-top: -15px; margin-bottom:0px"
                               ,title="`OTUasNote`: (`FALSE`) See also `notes`. If `TRUE` OTU name is written to the right, as `notes`."
                               ,checkboxInput("OTUasNote",
                                              HTML(paste("OTU as note", "(right)", sep = "<br/>") )
                                              ,OTUasNoteDefault)
                          )
                          ,div(style= "margin-top:-10px;margin-bottom:-15px"
                               ,title="`notesPosX`: (`0.5`) numeric, moves right notes in the x axis"
                               ,numericInput("notesPosX"
                                             ,HTML(paste("right notes", "horiz. pos.", sep = "<br/>") )
                                             ,notesPosXDefault, min = -20, max = 20, step=0.1
                               )
                          )
                 )
               )
             ) # wP

             ,wellPanel(style= "margin-top: -10px"
               ,splitLayout(
                 h4("Layout"),
                 div(title='`circularPlot`:	boolean, if `TRUE` chromosomes are plotted in concentric circles. Defaults to `FALSE`. See `verticalPlot`'
                     ,checkboxInput("circularPlot",
                                    HTML(paste("Circular", "Plot", sep = "<br/>") )
                                    ,circularPlotDefault)
                 )
               )

               #
               #    circ. plot
               #
               ,uiOutput("circParam")
               ,uiOutput("circParamOTU")
             )


             ,wellPanel(
               splitLayout(
                 h4("Anchor")
                 ,  div(title='`anchor`: boolean, when TRUE, plots a parent progeny structure in karyotypes in `moveKarHor`
'
                        ,checkboxInput("anchor", "Show anchor", anchorDefault)
                 )
               )
               ,splitLayout(
                 div(title="`moveKarHor`: character, OTUs' names of karyotypes that should be moved horizontally. See `mkhValue`
"
                     ,textInput("moveKarHor",
                                "kar. to move",
                                value = moveKarHorDefault
                                ,width = "100%")
                 )
                 ,div(title="`mkhValue`: numeric, value to move kar. hor. See `moveKarHor`
"
                      ,numericInput("mkhValue", "move kar.", mkhValueDefault, min = -10, max = 10, step=0.25)
                 )
               ) #sL
               ,splitLayout(
                 div(title='`moveAnchorV`: numeric, displace anchor vertical portion to right or left. See `anchor`
'
                     ,numericInput("moveAnchorV", "move anchor ver.", moveAnchorVDefault, min = -10, max = 10, step=0.25)
                 )
                 ,div(title='`moveAnchorH`: numeric, displace anchor horizontal portion to right or left. See `anchor`'
                      ,numericInput("moveAnchorH", "move anchor hor.", moveAnchorHDefault, min = -10, max = 10, step=0.25)
                 )
               )
             ) #wp
      ), # col
      column(width=2
             ,br()
             ,wellPanel(style = "padding: 10px 5px 0px 5px;margin-bottom: 20px" # trbl
               ,fluidRow(
                 column(6
                       , h4("Marks")
                 ,div(title='`useOneDot`: boolean, use one dot instead of two in style of marks dots. Defaults to `FALSE`
'          ,checkboxInput("useOneDot",
                          "one dot only",
                          useOneDotDefault
)
                 )
),column(6,
div(style= "margin-bottom:-5px"
  ,title="`pMarkFac` numeric, fraction of chr. size for `exProtein` style marks. Defaults to `0.25`"
    ,numericInput("pMarkFac"
                  ,HTML(paste("exProtein"
                              ,"mark size", sep = "<br/>") )
                  ,pMarkFacDefault,min=0.05,max=1,step=0.05)
)
               )
             )
)
,wellPanel(style = "background: #F7DCDA; margin-top:-15px; padding: 0px 5px 0px 5px"
           ,h4("Colors")
           ,splitLayout(
             div(title='`chrColor`: (`"gray"`) Determines the color of chromosomes
'
                 ,textInput('chrColor', 'chrom.', chrColorDefault)
             )
             ,div(title='`cenColor`: Determines the color of centromeres. if GISH use `NULL`. Defaults to `chrColor`
'
                  ,textInput('cenColor', 'centrom.', cenColorDefault)
             )
           ) # sL
           ,splitLayout(
             div(title='`fixCenBorder`: boolean, when `TRUE` uses `chrColor` as centromere (and cen. mark) border color. See also `cenColor`, `chrColor`, `colorBorderMark`, `borderOfWhiteMarks`. No default value.
'
                 , checkboxInput("fixCenBorder"
                                 ,HTML(paste("chr. color as", "border color","of cen.", sep = "<br/>") )
                                 ,fixCenBorderDefault)
             )
             ,div(title='`chrBorderColor`: character, color for border of chromosomes, defaults to `chrColor`'
                  ,textInput("chrBorderColor","border",chrBorderColorDefault)
             )
           ) # sL
           ,div(style= "margin-bottom:-5px"
           ,title="`mycolors`: optional, character vector with colors' names, which are associated automatically with marks according to their order in the data.frame of position of marks. See this ordering with `unique(dfMarkPos$markName)`. Argument example: `mycolors = c(\"red\",\"chartreuse3\",\"dodgerblue\")`. Not mandatory for plotting marks, package has default colors.
          "
                ,textInput('mycolors'
                           ,HTML(paste("Marks"
                                       ,tags$p("(optional, see data.frame page)", style = "font-size: 80%; font-weight: normal;")
                                       ,tags$p("here, only comma separated", style = "font-size: 80%;")
                                       # , sep = "<br/>"
                           ) )
                           , mycolorsDefault)
           )
) # wP
,wellPanel(style="margin-top:-15px;"
    ,h4("Indices & Info.")
    ,splitLayout(
    div(title='`morpho`: (`"both"`) character, if `"both"` (default) prints the Guerra (1986) and Levan (1964) classif. of cen. position.  , use also `"Guerra"` or `"Levan"` or `""` for none. See `?armRatioCI` also (function).
'
        ,radioButtons("morpho","Morphology"
                      ,c("Guerra (1986)"="Guerra","Levan (1974)"="Levan"
                         , "both"="both","none"="")
                      ,selected = morphoDefault
        )
    ),
    div(title='`chrIndex`: (`"both"`) character, add arm ratio with `"AR"` and centromeric index with `"CI"`, or `"both"` (Default), or `""` for none to each chromosome [@Levan1964]. See `armRatioCI`also.
'
        ,radioButtons("chrIndex","Add AR & CI"
                      ,c("Chr. Index"="CI","Arm Ratio"="AR", "both"="both","none"="")
                      ,selected = chrIndexDefault )
    )
  ) # sL
  , fluidRow(
    column(6
           ,div(title='`indexIdTextSize`: numeric, font size of chr. and kar. indices and
                   chromosome name. Defaults to `1`'
               ,numericInput("indexIdTextSize",
                             HTML(paste("font","size",sep = "<br/>") )
                             ,indexIdTextSizeDefault
                             , min = 0.05, max = 5, step=0.05)
           )
           ,div(style="margin-top:-10px;"
           ,title='`chrSize`: boolean, when `TRUE` adds total chr size under each chr. Defautls to `FALSE`
'
                ,checkboxInput("chrSize",
                               HTML(paste("Show Chrom.", "size", sep = "<br/>") )
                               ,chrSizeDefault)
           ),
           div(style="margin-top:-15px;"
           ,title="`chrSizeMbp`: boolean, when `TRUE` adds total Mbp chr. size to each chr. provided, there is a `Mbp` column in `dfChrSize` data.frame. Defaults to `FALSE`. If data in columns `shortArmSize`, or col. `chrSize` is in millions ('Mbp'). Use `chrSize=TRUE` not this one (not column `Mbp`, you don't need this).
"
               ,checkboxInput("chrSizeMbp",
                              HTML(paste("Show Chr. Size"
                                         , "in Mbp"
                                         ,tags$p("(column required)", style = "font-size: 80%;")
                                         , sep = "<br/>") )
                              ,chrSizeMbpDefault)
           )
    )

    ,column(6
            ,div(title='`distTextChr`: Vertical distance from indices (text) to the chromosome.
'
                 , numericInput("distTextChr"
                                ,HTML(paste("chr. to text", "separation"
                                            , sep = "<br/>") )
                                , distTextChrDefault, min = 0.2, max = 5, step=0.1)
            )
            ,div(style="margin-top:-10px;"
                 ,title='`addOTUName`: (`TRUE`) If `TRUE` adds name of species (OTU) under karyotype
'
                 , checkboxInput("addOTUName"
                                 ,HTML(paste("Show OTU", "name"
                                             , sep = "<br/>") )
                                 ,addOTUNameDefault)
            )
            ,div(title='`OTUfont`: numeric, `1` for normal, `2` for bold, `3` for italics, `4` for bold-italics
'
                 ,radioButtons("OTUfont","OTU font type",
                               c("normal"="1","bold"="2","italics"="3","bold italics"="4"),
                               selected=OTUfontDefault)
            )
    )
  )
) #wP
      ) # col2
,column(width=8
        ,br()
        ,fluidRow(
          # wellPanel( #style = "background: #FAFAFA",
            div(
              style = "margin-bottom:-20px"
            ,fluidRow(
              column(3
                  ,wellPanel(style = "padding: 0px 5px 0px 5px"
                    ,fluidRow(
                      column(6,
                             h4("Ruler")
                             ,div(
                               title='`rulerPos`: (`-0.5`) Absolute position of ruler, corresponds to "pos" argument of the function `axis` of R plots'
                               ,numericInput("rulerPos", "modify pos.", rulerPosDefault, min = -5, max = 5, step=0.1) ,
                             )
                             ,div(style = "margin-top:-15px;margin-bottom:-10px",
                                  title='`ruler.tck`: (`-0.02`) tick size of ruler, corresponds to "tck" argument of `axis` function
'
                                  ,numericInput("ruler.tck", "modify ticks", ruler.tckDefault, min = -5, max = 5, step=0.005)
                             )
                      ) #c
                      ,
                      column(6
                             ,div(title='`ruler`: (`TRUE`) When `TRUE` displays ruler to the left of karyotype, when `FALSE` shows no ruler
'
                                  ,checkboxInput("ruler",
                                                 "Show ruler",rulerDefault)
                             )
                             ,div(style = "margin-top:-12px",
                                  title="`rulerNumberSize`: (`1`) Size of number's font in ruler
"
                                  ,numericInput("rulerNumberSize"
                                                , "font size", rulerNumberSizeDefault, min = .1, max = 5, step=0.1) ,
                             )
                             ,div(style = "margin-top:-15px;margin-bottom:-10px",
                                  title="`xPosRulerTitle`: (`2.6`) Modifies the horizontal position of the title of rulers (Mb, etc). Moves to left from 1st chr. in `chrSpacing` times"
                                  ,numericInput("xPosRulerTitle"
                                                , "pos. of title", xPosRulerTitleDefault, min = -5, max = 5, step=0.1)
                             )
                      ) # c6
                    ) # fR
                  ) #wp
                  ,fluidRow(
                    column(6
                           ,wellPanel(style = "margin-top:-15px;padding: 0px 5px 0px 5px"
                             ,h4("Chromatids")
                             ,div(title='`chromatids`: boolean, when `TRUE` shows separated chromatids. Defaults to `TRUE`'
                                  ,checkboxInput("chromatids",
                                                 HTML(paste("Show separ.") )
                                                 ,chromatidsDefault)
                             )
                             ,div(title='`xModifier`: numeric, for `chromatids=TRUE`, separation among chromatids.
                   Quotient for `chrWidth`. Defaults to `12 = chrWidth/12`'
                                  ,numericInput("xModifier"
                                                ,HTML(paste("Separation") )
                                                ,xModifierDefault
                                                ,min = 2, max = 25, step=0.5)
                             )
                           ) # wP
                    )
                    ,column(6
                            ,wellPanel(style = "margin-top:-15px;padding: 0px 5px 0px 5px"
                                       ,h4("Centromere")
                                       ,div(style = "margin-top:-25px"
                                            ,title='`cenFormat`: boolean, when "triangle", cen. has triangular aspect. When "rounded", it has rounded aspect (Default). "inProtein" for using the mark with style of same name.'
                                            ,radioButtons("cenFormat","",c("rounded","triangle","inProtein")
                                                          ,selected=cenFormatDefault)
                                       )
                                       ,div(style = "margin-top:-15px;margin-bottom:-10px",
                                            title='`cenFactor`: numeric, modifies any cen. mark and cen. size. Defaults to `1`'
                                            ,numericInput("cenFactor", "modify size"
                                                          , cenFactorDefault, min = 0, max = 10, step=0.25)
                                       )
                            ) # wP
                    ) # c
                  ) # fR
          ) # c3
          ,column(3
                  ,wellPanel(
                    h4("Chromosomes")
                    ,fluidRow(
                      column(6
                             ,div(title='`orderChr`: (`size`) character, when `"size"`, sorts chromosomes by total length from the largest to the smallest. `"original"`: preserves d.f. order. `"name"`: sorts alphabetically; `"group"`: sorts by group name; `"chrNameUp"`: sorts according to column `chrNameUp`. See `chrNameUp`'
                                  ,radioButtons(
                                    "orderChr",
                                    "order by:"
                                    ,c("size"="size","as in d.f."="original", "alphab."="name"
                                       ,"group"="group"
                                       ,"col. chrNameUp"="chrNameUp")
                                    ,selected = orderChrDefault )
                             )
                             ,div(style="margin-bottom:-20px;"
                             ,title='`squareness`: (`4`) Squared or rounded vertices when marks of the "square" style (defined in data.frame passed to `dfMarkColor`). Affects chromosomes also. Smaller numbers = more rounded
'
                                  ,numericInput("squareness",
                                                HTML(paste("Vertices", "squareness", sep = "<br/>") )
                                                ,squarenessDefault
                                                , min = 1, max = 21, step=0.5)
                             )
                      )
                      ,column(6
                              , div(title='`lwd.chr`: (`0.5`) width of border lines for chr. and marks when related param. absent.
'
                                    ,numericInput("lwd.chr","border width",lwd.chrDefault,min=0,max=4,step=0.25)
                              )
                              ,div(title='`chrWidth`: (`0.5`) Determines the width of chromosomes
'
                                   ,numericInput("chrWidth", "Width", chrWidthDefault, min = 0.1, max = 5, step=0.05)
                              )
                              ,div(title='`chrSpacing`: (`0.5`) Determines the horizontal spacing among chromosomes
'
                                   ,numericInput("chrSpacing", "horiz. spacing", chrSpacingDefault, min = 0.1, max = 5, step=0.1)
                              )
                      ) #c6
                    ) # fR
                  ) # wP
          ) # c3
          ,column(3
                  ,wellPanel(style = "padding: 0px 5px 0px 5px"
                             ,fluidRow(
                               column(6
                                      ,h4("Karyotypes")
                                      ,div(style="margin-top:-10px;"
                                           ,title='`karHeight`: (`2`) Vertical size of karyotypes considering only chromosomes. for ex `karHeight = 1`
'
                                           ,numericInput("karHeight"
                                                         , HTML(paste("Height") )
                                                         ,karHeightDefault, min = 0.5, max = 50, step=0.5)
                                      )
                                      ,
                                      div(style="margin-top:-12px;margin-bottom:-10px"
                                          ,title='`karHeiSpace`: (`2.5`) Vertical size of karyotypes including spacer. for ex `karHeiSpace = 1.2`. Use with `karSepar=FALSE`
'
                                          ,numericInput("karHeiSpace"
                                                        , HTML(paste("Height with","space", sep = "<br/>") )
                                                        , karHeiSpaceDefault, min = 2, max = 150, step=0.5)
                                      )
                               )
                               ,column(6,
                                       div(title='`karSepar`: (`TRUE`) If `TRUE` reduces the space among karyotypes. `FALSE` = equally sized karyotypes or `TRUE` = equally spaced karyotypes. Incompatible with `addMissingOTUAfter`'
                                           ,checkboxInput("karSepar"
                                                          ,HTML(paste("equally spaced","kar.", sep = "<br/>") )
                                                          ,karSeparDefault)
                                       )
                                       ,div(style= "margin-bottom:-10px"
                                         ,title='`amoSepar`: (`9`) For `karSepar = TRUE`, if zero, no space among karyotypes. Amount of separation.  if overlap, increase this and `karHeiSpace`'
                                         ,numericInput("amoSepar"
                                                       ,HTML(paste("Vert.","separ.", sep = "<br/>") )
                                                       , amoSeparDefault, min = 0, max = 15, step=0.5)
                                       )
                               )
                             )
                  ) #Wp
                  ,wellPanel(style = "margin-top:-15px;padding: 0px 5px 0px 5px"
                             ,fluidRow(
                               column(6
                                      ,h4("Legend")
                                      ,div(
                                        title='`legend`: (`"aside"`) If you wanto to plot the names of marks near each chromosome use `legend = "inline"`, to the right of karyotypes use `legend = "aside"`, otherwise use `legend = ""` for no legend. See `markLabelSpacer`'
                                           ,radioButtons("legend","Pos.",c("aside","inline","none"),selected = legendDefault )
                                      )
                               )
                               ,column(6
                                       ,div(#style = "margin-top:-8px"
                                            title='`legendWidth`: (`1.7`) numeric, factor to modify the width of the square and dots of legend. For `legend="aside"`.'
                                            ,numericInput("legendWidth", "width", legendWidthDefault, min = 0.25, max = 5, step=0.05)
                                       )
                                       ,div(style = "margin-top:-12px"
                                            ,title='`legendHeight`: (`NA`) numeric, factor to modify the height of the square and dots of legend. For `legend="aside"`.'
                                            ,numericInput("legendHeight", "height"
                                                          , legendHeightDefault
                                                          , min = 0.25, max = 5, step=0.05)
                                       )
                               )
                             ) # fR
                  ) #wP
          ) # c3
          ,column(3
                  ,wellPanel(
                    h4("Plot dimensions"),
                    fluidRow(
                      column(6
                             ,div(style="margin-bottom:-10px"
                             ,numericInput("widFactor", "width %",
                                           80, min = 5, max = 100, step=5)
                             )
                             ,          uiOutput("imageWidth")
                      )
                      ,column(6
                              ,div(style="margin-bottom:-10px"
                                   ,numericInput("heiFactor", "height ratio",
                                            0.5, min = 0.05, max = 20, step=0.05)
                              )
                              ,          uiOutput("imageHeight")
                      )
                    )
                  ) # wP

                  ,wellPanel(style="margin-top:-15px;padding: 0px 5px 0px 5px"
                    ,h4("Margins"),
                    fluidRow(
                      column(6,
                             div(style = "margin-top:-10px",
                                 title='`ylimBotMod`:	(`0.2`) modify `ylim` bottom component of plot adding more space
'
                                 ,numericInput("ylimBotMod", "bottom",
                                               ylimBotModDefault, min = -5, max = 5, step=0.5)
                             )
                             ,div(style = "margin-top:-15px",
                                  title='`ylimTopMod`: (`0.2`) modify `ylim` top component of plot adding more space.
'
                                  ,numericInput("ylimTopMod", "top"
                                                , ylimTopModDefault, min = -5, max = 5, step=0.2)
                             )
                      ) #sL

                      , column(6,
                               div(style = "margin-top:-10px",
                                   title='`xlimLeftMod`: (`1`) modifies `xlim` left (first) component of the plot as in any "R-plot"
'
                                   ,numericInput("xlimLeftMod", "left",
                                                 xlimLeftModDefault, min = -5, max = 5, step=0.5)
                               )
                               ,div(style = "margin-top:-15px",
                                    title='`xlimRightMod`:	(`2`) `xlim` (right) modification by adding space to the right of idiograms
'
                                    ,numericInput("xlimRightMod", "right",
                                                  xlimRightModDefault, min = -5, max = 5, step=0.5)
                               )
                      ) #
                    ) # fR
                  ) #wp
          ) #c3

        ) #fr
          )
          ) #div
        ) # fR
        ,fluidRow(
          wellPanel(
            style = "overflow-y:auto;text-align:center;" # margin-top:-12px;

            # ,actionButton("goButton", "Go!", class = "btn-success")

            ,div(style = 'overflow-y:auto;overflow-x:auto;min-width:1030px;'
                 ,plotOutput("idiogramPlot"
                             ,height = "auto"
                 )
            ) #d
          # ) # wp
        ) # fR
      ) #c8
    ) # fR
  })

  outputOptions(output, "dfchrpanel", suspendWhenHidden = FALSE)
  outputOptions(output, "dfmarkpanel", suspendWhenHidden = FALSE)
  outputOptions(output, "dfMStylepanel", suspendWhenHidden = FALSE)
  outputOptions(output, "parameterPanel", suspendWhenHidden = FALSE)
  outputOptions(output, "logpanel", suspendWhenHidden = FALSE)
  outputOptions(output, "strpanel", suspendWhenHidden = FALSE)

}
