server <- function(input, output, session) {

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

  #
  #   objects to load
  #
{
  chrDataVec <- c("dfOfChrSize",
                   "dfChrSizeHolo",
                   "bigdfOfChrSize",
                   "bigdfChrSizeHolo"

                  ,"dfChrSizeHolo"
                  ,"parentalAndHybChrSize"
                  ,"parentalAndHybHoloChrSize"
                  ,"traspadf"
                  )

  markDataVec <- c("dfOfMarks2",
                  "dfMarkPosHolo",
                  "bigdfOfMarks",
                  "bigdfMarkPosHolo"

                  ,"dfMarkPosHolo"
                  ,"dfAlloParentMarks"
                  ,"dfAlloParentMarksHolo"
                  ,"traspaMarks")

  dfMarkColor5S25S<-read.table(text="    markName markColor  style
        5S       black dots
       25S       white dots"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

  MStyleDataVec <- c("dfMarkColor",
                   "dfMarkColor",
                   "dfMarkColor",
                   "dfMarkColor"
                   ,"no"
                   ,"no"
                   ,"no"
                   ,"dfMarkColor5S25S")

  widFactorVec <- c("80",
                "80",
                "70",
                "80",
                "80"
                ,"80"
                ,"55"
                ,"80")

  heiFactorVec <- c("0.5",
                   "0.5",
                   "2.5",
                   "1",
                   "0.5"
                   ,"1"
                   ,"1"
                   ,"1"

                   )

  karHeightDefault <- 2
  karHeightVec <- c(5,
                    karHeightDefault,
                    2.5,
                    karHeightDefault,
                    karHeightDefault
                    ,karHeightDefault
                    ,3
                    ,karHeightDefault

  )

  karHeiSpaceDefault <- 2.5
  karHeiSpaceVec <- c(karHeiSpaceDefault,
                      karHeiSpaceDefault,
                    6,
                    4,
                    karHeiSpaceDefault
                    ,5
                    ,5
                    ,karHeiSpaceDefault

  )

  amoSeparDefault <- 10

  amoSeparVec <- c(amoSeparDefault,
                   amoSeparDefault,
                      2,
                      1,
                   amoSeparDefault
                   ,amoSeparDefault
                   ,amoSeparDefault
                   ,amoSeparDefault

  )

  karSeparDefault <- TRUE
  karSeparVec <- c(karSeparDefault,
                   karSeparDefault,
                   karSeparDefault,
                   karSeparDefault,
                   karSeparDefault
                   ,FALSE
                   ,karSeparDefault
                   ,karSeparDefault
  )

  OTUasNoteDefault <- FALSE
  OTUasNoteVec <- c(OTUasNoteDefault,
                   OTUasNoteDefault,
                   OTUasNoteDefault,
                   OTUasNoteDefault,
                   OTUasNoteDefault
                   ,TRUE
                   ,OTUasNoteDefault
                   ,OTUasNoteDefault
  )

  legendDefault <- "aside"
  legendVec <- c(legendDefault,
                   legendDefault,
                   legendDefault,
                   legendDefault,
                   legendDefault
                   ,"none"
                 ,"none"
                 ,legendDefault
  )


  chrWidthDefault <- 0.5
  chrWidthVec <- c(1.2,
                   chrWidthDefault,
                    0.35,
                   chrWidthDefault,
                   chrWidthDefault
                   ,chrWidthDefault
                   ,chrWidthDefault
                   ,1
  )

  squarenessDefault <- 4
  squarenessVec <- c(squarenessDefault,
                   squarenessDefault,
                   squarenessDefault,
                   squarenessDefault,
                   squarenessDefault
                   ,squarenessDefault
                   ,squarenessDefault
                   ,5
  )
  # not exclusive of cP
  useOneDotDefault <- FALSE
  useOneDotVec <- c(useOneDotDefault,
                     useOneDotDefault,
                     useOneDotDefault,
                     useOneDotDefault,
                     useOneDotDefault
                     ,useOneDotDefault
                     ,useOneDotDefault
                     ,useOneDotDefault
  )
  chrLabelSpacingDefault <- 0.5
  chrLabelSpacingVec <- c(chrLabelSpacingDefault,
                    chrLabelSpacingDefault,
                    chrLabelSpacingDefault,
                    chrLabelSpacingDefault,
                    chrLabelSpacingDefault
                    ,chrLabelSpacingDefault
                    ,chrLabelSpacingDefault
                    ,1
  )

  rotationDefault <- 0.5
  rotationVec <- c(rotationDefault,
                          rotationDefault,
                          rotationDefault,
                          rotationDefault,

                          rotationDefault
                          ,rotationDefault
                          ,rotationDefault
                          ,0.1
  )

  shrinkFactorDefault <- 0.9
  shrinkFactorVec <- c(shrinkFactorDefault,
                   shrinkFactorDefault,
                   shrinkFactorDefault,
                   shrinkFactorDefault,

                   shrinkFactorDefault
                   ,shrinkFactorDefault
                   ,shrinkFactorDefault
                   ,.95
  )

  radiusDefault <- 0.5
  radiusVec <- c(radiusDefault,
                     radiusDefault,
                     radiusDefault,
                     radiusDefault,

                     radiusDefault
                     ,radiusDefault
                     ,radiusDefault
                     ,5
  )

  circleCenterDefault <- 1
  circleCenterVec <- c(circleCenterDefault,
                 circleCenterDefault,
                 circleCenterDefault,
                 circleCenterDefault,
                 circleCenterDefault
                 ,circleCenterDefault
                 ,circleCenterDefault
                 ,3
  )

  OTUsrtDefault <- 0
  OTUsrtVec <- c(OTUsrtDefault,
                       OTUsrtDefault,
                       OTUsrtDefault,
                       OTUsrtDefault,
                       OTUsrtDefault
                       ,OTUsrtDefault
                       ,OTUsrtDefault
                       ,0
  )

  OTUplacingDefault <- "first"
  OTUplacingVec <- c(OTUplacingDefault,
                 OTUplacingDefault,
                 OTUplacingDefault,
                 OTUplacingDefault,

                 OTUplacingDefault
                 ,OTUplacingDefault
                 ,OTUplacingDefault
                 ,"simple"
  )

  OTUjustifDefault <- 0
  OTUjustifVec <- c(OTUjustifDefault,
                     OTUjustifDefault,
                     OTUjustifDefault,
                     OTUjustifDefault,
                     OTUjustifDefault
                     ,OTUjustifDefault
                     ,OTUjustifDefault
                     ,0
  )

  OTULabelSpacerxDefault <- 0
  OTULabelSpacerxVec <- c(OTULabelSpacerxDefault,
                    OTULabelSpacerxDefault,
                    OTULabelSpacerxDefault,
                    OTULabelSpacerxDefault,

                    OTULabelSpacerxDefault
                    ,OTULabelSpacerxDefault
                    ,OTULabelSpacerxDefault
                    ,-8
  )


  OTUlegendHeightDefault <- NA
  OTUlegendHeightVec <- c(OTUlegendHeightDefault,
                          OTUlegendHeightDefault,
                          OTUlegendHeightDefault,
                          OTUlegendHeightDefault,
                          OTUlegendHeightDefault
                          ,OTUlegendHeightDefault
                          ,OTUlegendHeightDefault
                          ,1.5
  )

  orderChrDefault<-"size"
  orderChrVec <- c(orderChrDefault,
                     orderChrDefault,
                     orderChrDefault,
                     orderChrDefault,

                     orderChrDefault
                     ,orderChrDefault
                     ,orderChrDefault
                     ,"name"
  )

  chrSpacingDefault <- "0.5"
  chrSpacingVec <- c("1",
                   chrSpacingDefault,
                   chrSpacingDefault,
                   chrSpacingDefault

                   ,chrSpacingDefault
                   ,chrSpacingDefault
                   ,chrSpacingDefault
                   ,chrSpacingDefault
  )
  # names(chrSpacingVec)[which(names(chrSpacingVec)=="chrSpacingDefault" )]<- chrSpacingDefault

  morphoDefault <- "both"
  morphoVec <- c("Guerra",
                     morphoDefault,
                     "Guerra",
                     morphoDefault,
                 morphoDefault
                 ,morphoDefault
                 ,morphoDefault
                 ,morphoDefault

  )
  # names(morphoVec)[which(names(morphoVec)=="morphoDefault" )]<- morphoDefault

  chrColorDefault <- "gray"
  chrColorVec <- c(chrColorDefault
                   ,chrColorDefault
                   ,chrColorDefault
                   ,chrColorDefault
                   ,chrColorDefault
                   ,chrColorDefault
                   ,chrColorDefault
                   ,chrColorDefault

  )

  cenColorDefault <- ""
  cenColorVec <- c(cenColorDefault
                   ,cenColorDefault
                   ,cenColorDefault
                   ,cenColorDefault

                   ,cenColorDefault
                   ,cenColorDefault
                   ,NA
                   ,"black"
  )


  chrIndexDefault <- "both"
  chrIndexVec <- c("CI",
                 chrIndexDefault,
                 "AR",
                 chrIndexDefault,
                 chrIndexDefault
                 ,chrIndexDefault
                 ,chrIndexDefault
                 ,chrIndexDefault

  )
  # names(chrIndexVec)[which(names(chrIndexVec)=="chrIndexDefault" )]<- chrIndexDefault

  chrSizeDefault <- "FALSE"

  chrSizeVec <- c( "TRUE",
                   "TRUE",
                   chrSizeDefault,
                   chrSizeDefault,
                   chrSizeDefault,
                   chrSizeDefault
                   ,chrSizeDefault
                   ,chrSizeDefault #8

  )
  # names(chrSizeVec)[which(names(chrSizeVec)=="chrSizeDefault" )]<- chrSizeDefault

  chromatidsDefault <- TRUE
  chromatidsVec <- c( chromatidsDefault,
                      chromatidsDefault,
                      chromatidsDefault,
                      chromatidsDefault,
                      FALSE
                      ,chromatidsDefault
                      ,chromatidsDefault
                      ,chromatidsDefault

  )

  circularPlotDefault <- FALSE
  circularPlotVec <- c( circularPlotDefault,
                      circularPlotDefault,
                      circularPlotDefault,
                      circularPlotDefault,
                      circularPlotDefault
                      ,circularPlotDefault
                      ,circularPlotDefault
                      ,TRUE
  )

  chrSizeMbpDefault <- "FALSE"
  chrSizeMbpVec <- c( chrSizeMbpDefault,
                      chrSizeMbpDefault,
                   chrSizeMbpDefault,
                   chrSizeMbpDefault,

                   chrSizeMbpDefault
                   ,chrSizeMbpDefault
                   ,chrSizeMbpDefault
                   ,chrSizeMbpDefault

  )
  # names(chrSizeMbpVec)[which(names(chrSizeMbpVec)=="chrSizeMbpDefault" )]<- chrSizeMbpDefault

  distTextChrDefault <- "1"
  distTextChrVec <- c( "1.2",
                      distTextChrDefault,
                      "0.8",
                      "0.5",

                      distTextChrDefault
                      ,distTextChrDefault
                      ,"0.8"
                      ,distTextChrDefault
  )
  # names(distTextChrVec)[which(names(distTextChrVec)=="distTextChrDefault" )]<- distTextChrDefault

  rulerDefault <- TRUE
  rulerVec <- c( rulerDefault,
                 rulerDefault,
                 rulerDefault,
                    rulerDefault,
                    FALSE,
                 FALSE
                 ,FALSE
                 ,   rulerDefault
  )

  rulerPosDefault <- "0"
  rulerPosVec <- c( rulerPosDefault,
                       "-0.1",
                       "-0.4",
                    rulerPosDefault,
                    rulerPosDefault
                    ,rulerPosDefault
                    ,rulerPosDefault
                    ,rulerPosDefault
  )
  # names(rulerPosVec)[which(names(rulerPosVec)=="rulerPosDefault" )]<- rulerPosDefault

  ruler.tckDefault <- "-0.02"
  ruler.tckVec <- c("-0.01",
                     "-0.02",
                    "-0.005",
                    "-0.005",
                    ruler.tckDefault
                    ,ruler.tckDefault
                    ,ruler.tckDefault
                    ,ruler.tckDefault
  )
  # names(ruler.tckVec)[which(names(ruler.tckVec)=="ruler.tckDefault" )] <- ruler.tckDefault

  rulerNumberSizeDefault <- "1"
  rulerNumberSizeVec <- c("0.8",
                    rulerNumberSizeDefault,
                    "0.4",
                    "0.9",
                    rulerNumberSizeDefault
                    ,rulerNumberSizeDefault
                    ,rulerNumberSizeDefault
                    ,rulerNumberSizeDefault
  )
  # names(rulerNumberSizeVec)[which(names(rulerNumberSizeVec)=="rulerNumberSizeDefault" )] <- rulerNumberSizeDefault

  xPosRulerTitleDefault <- "1"
  xPosRulerTitleVec <- c("3",
                          "3",
                          "3.5",
                          "2.8",
                         xPosRulerTitleDefault
                         ,xPosRulerTitleDefault
                         ,xPosRulerTitleDefault
                         ,xPosRulerTitleDefault

  )
  # names(xPosRulerTitleVec)[which(names(xPosRulerTitleVec)=="xPosRulerTitleDefault" )] <- xPosRulerTitleDefault

  legendWidthDefault <- "1.7"
  legendWidthVec <- c("1",
                      "1.2",
                      legendWidthDefault,
                      "1",
                      legendWidthDefault
                      ,legendWidthDefault
                      ,legendWidthDefault
                      ,legendWidthDefault

  )

  legendHeightDefault <- NA
  legendHeightVec <- c(legendHeightDefault,
                       legendHeightDefault,
                      legendHeightDefault,
                      legendHeightDefault,
                      legendHeightDefault
                      ,legendHeightDefault
                      ,legendHeightDefault
                      ,2.5
  )

  # names(legendWidthVec)[which(names(legendWidthVec)=="legendWidthDefault" )] <- legendWidthDefault

  fixCenBorderDefault <- "FALSE"
  fixCenBorderVec <- c("TRUE",
                       fixCenBorderDefault,
                      "TRUE",
                      fixCenBorderDefault,
                      fixCenBorderDefault
                      ,fixCenBorderDefault
                      ,fixCenBorderDefault
                      ,fixCenBorderDefault

  )

  chrBorderColorDefault <- NA
  chrBorderColorVec <- c(chrBorderColorDefault,
                       chrBorderColorDefault,
                       chrBorderColorDefault,
                       chrBorderColorDefault,

                       chrBorderColorDefault
                       ,chrBorderColorDefault
                       ,chrBorderColorDefault
                       ,chrBorderColorDefault
  )

  lwd.chrDefault <- 0.5
  lwd.chrVec <- c(lwd.chrDefault,
                         lwd.chrDefault,
                         lwd.chrDefault,
                         lwd.chrDefault,

                         lwd.chrDefault
                         ,lwd.chrDefault
                         ,lwd.chrDefault
                         ,lwd.chrDefault
  )

  xlimLeftModDefault <- "1"
  xlimLeftModVec <- c("2",
                       "2",
                       "2",
                      xlimLeftModDefault,

                      xlimLeftModDefault
                      ,xlimLeftModDefault
                      ,xlimLeftModDefault
                      ,-5
  )
  # names(xlimLeftModVec)[which(names(xlimLeftModVec)=="xlimLeftModDefault" )] <- xlimLeftModDefault

  xlimRightModDefault <- 2
  xlimRightModVec <- c(xlimRightModDefault,
                       xlimRightModDefault,
                       xlimRightModDefault,
                      xlimRightModDefault,

                      3
                      ,4
                      ,0
                      ,5
                      )

  ylimBotModDefault <- 0.2
  ylimBotModVec <- c(0,
                     -0.5,
                     0,
                     0.4,
                     ylimBotModDefault
                     ,1
                     ,ylimBotModDefault
                     ,ylimBotModDefault
  )

  ylimTopModDefault <- "0.2"
  ylimTopModVec <- c("0",
                     "0",
                     "-0.4",
                     ylimTopModDefault,
                     ylimTopModDefault
                     ,ylimTopModDefault
                     ,ylimTopModDefault
                     ,ylimTopModDefault
  )
  # names(ylimTopModVec)[which(names(ylimTopModVec)=="ylimTopModDefault" )] <- ylimTopModDefault

  mycolorsDefault <- ""
  mycolorsVec<- c(""
                  ,""
                  ,""
                  ,""

                  ,"red,dodgerblue,fdsjkfds,chartreuse3,darkgoldenrod1"
                  ,""
                  ,mycolorsDefault
                  ,mycolorsDefault
                  )

  chrNamesToSwapDefault <- ""
  chrNamesToSwapVec<- c(""
                  ,""
                  ,""
                  ,""

                  ,""
                  ,""
                  ,chrNamesToSwapDefault
                  ,"3,6,7,9,12" #8
  )

  addOTUNameDefault <- TRUE
  addOTUNameVec <- c(addOTUNameDefault
                  ,addOTUNameDefault
                  ,addOTUNameDefault
                  ,addOTUNameDefault

                  ,addOTUNameDefault
                  ,FALSE
                  ,addOTUNameDefault
                  ,addOTUNameDefault
                  )

  OTUfontDefault <- 1
  OTUfontVec <- c(OTUfontDefault
                     ,OTUfontDefault
                     ,OTUfontDefault
                     ,OTUfontDefault

                     ,OTUfontDefault
                     ,OTUfontDefault
                     ,OTUfontDefault
                     ,3
  )
  OTUfontVec<-as.character(OTUfontVec)

  moveKarHorDefault<- ""
  moveKarHorVec<- c(moveKarHorDefault
                    ,moveKarHorDefault
                    ,moveKarHorDefault
                    ,moveKarHorDefault
                    ,moveKarHorDefault
                    ,"Allopolyploid"
                    ,moveKarHorDefault
                    ,moveKarHorDefault
  )

  mkhValueDefault<- 0.5
  mkhValueVec<- c(mkhValueDefault
                    ,mkhValueDefault
                    ,mkhValueDefault
                    ,mkhValueDefault
                    ,mkhValueDefault
                    ,7
                  ,mkhValueDefault
                  ,mkhValueDefault
  )

  anchorDefault<- FALSE
  anchorVec<- c(anchorDefault
                  ,anchorDefault
                  ,anchorDefault
                  ,anchorDefault
                  ,anchorDefault
                  ,TRUE
                ,anchorDefault
                ,anchorDefault
  )

  moveAnchorVDefault<- 0
  moveAnchorVVec <- c(moveAnchorVDefault
                ,moveAnchorVDefault
                ,moveAnchorVDefault
                ,moveAnchorVDefault
                ,moveAnchorVDefault
                ,4
                ,moveAnchorVDefault
                ,moveAnchorVDefault
                )

  moveAnchorHDefault<- 0
  moveAnchorHVec<- c(moveAnchorHDefault
                     ,moveAnchorHDefault
                     ,moveAnchorHDefault
                     ,moveAnchorHDefault
                     ,moveAnchorHDefault
                     ,-1.5
                     ,moveAnchorHDefault
                     ,moveAnchorHDefault
  )

  notesTextSizeDefault <- 0.4
  notesTextSizeVec<- c(notesTextSizeDefault
                    ,notesTextSizeDefault
                    ,notesTextSizeDefault
                    ,notesTextSizeDefault
                    ,notesTextSizeDefault
                    ,1.3
                    ,notesTextSizeDefault
                    ,notesTextSizeDefault
  )

  notesPosXDefault <- 0.5
  notesPosXVec<- c(notesPosXDefault
                       ,notesPosXDefault
                       ,notesPosXDefault
                       ,notesPosXDefault

                       ,notesPosXDefault
                       ,1.5
                   ,notesPosXDefault
                   ,notesPosXDefault
  )

  }

  #
  # param presets
  #

  # observeEvent(input$exampleId, {
  observeEvent(input$exampleId, {

    updateNumericInput(session, "karHeight", value = karHeightVec[as.numeric(input$exampleId)]  )
    updateNumericInput(session, "karHeiSpace", value = karHeiSpaceVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "amoSepar", value = amoSeparVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "karSepar", value = karSeparVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "chrWidth",  value = chrWidthVec[as.numeric(input$exampleId)] )

    updateNumericInput(session, "squareness",  value = squarenessVec[as.numeric(input$exampleId)] )
    updateRadioButtons(session, "orderChr",  selected = ((orderChrVec[as.numeric(input$exampleId)] ) ) )
    updateCheckboxInput(session, "useOneDot",  value = useOneDotVec[as.numeric(input$exampleId)] )

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
    updateCheckboxInput(session, "chrSizeMbp",  value = as.logical((chrSizeMbpVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "distTextChr",  value = as.numeric((distTextChrVec[as.numeric(input$exampleId)] ) ) )

    updateCheckboxInput(session, "chromatids",  value = chromatidsVec[as.numeric(input$exampleId)] )
    updateCheckboxInput(session, "circularPlot",  value = circularPlotVec[as.numeric(input$exampleId)] )

    updateCheckboxInput(session, "ruler",  value = rulerVec[as.numeric(input$exampleId)] )
    updateNumericInput(session, "rulerPos",  value = as.numeric((rulerPosVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "ruler.tck",  value = as.numeric((ruler.tckVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "rulerNumberSize",  value = as.numeric((rulerNumberSizeVec[as.numeric(input$exampleId)] ) ) )
    updateNumericInput(session, "xPosRulerTitle",  value = as.numeric((xPosRulerTitleVec[as.numeric(input$exampleId)] ) ) )

    updateRadioButtons(session, "legend",  selected = legendVec[as.numeric(input$exampleId)] )
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

  observeEvent(input$exampleId, {
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

  observeEvent(input$exampleId, {
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

  observeEvent(input$exampleId, {
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
    if(input$colType=="character"){
      final[,input$col] <- as.character(NA)
    } else {
      final[,input$col] <- as.numeric(NA)
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
    if(nrow(final)>0){
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
      if("OTU" %in% colnames( values[["df1"]] )) {#wdf
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
    if(nrow(final)>0){
      if(input$colTypeMStyle=="character"){
        final[,input$colMStyle] <- as.character(NA)
      } else {
        final[,input$colMStyle] <- as.numeric(NA)
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

  observeEvent(input$exampleId,{
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
    downloadButton('downloadCsv2', 'Download table as .csv')
  })

  #
  #   download mark
  #

  output$buttontableMark <-  renderUI({
    validate(
      need(try(values[["df1Mark"]]), "")
    )
    downloadButton('downloadCsvMark', 'Download table as .csv')
  })

  output$buttontableMark2 <-  renderUI({
    validate(
      need(try(values[["df1Mark"]]), "")
    )
    downloadButton('downloadCsvMark2', 'Download table as .csv')
  })

  #
  #   download mark style
  #

  output$buttontableMStyle <-  renderUI({
    validate(
      need(try(values[["df1MStyle"]]), "")
    )
    downloadButton('downloadCsvMStyle', 'Download table as .csv')
  })

  output$buttontableMStyle2 <-  renderUI({
    validate(
      need(try(values[["df1MStyle"]]), "")
    )
    downloadButton('downloadCsvMStyle2', 'Download table as .csv')
  })

  #
  #   downloadHandler chr
  #

  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste0(input$chrfilename)
    },
    content = function(file) {
      write.csv(values[["df1"]], file, na="", # wdf
                row.names = FALSE, quote = TRUE)
    }
  )

  output$downloadCsv2 <- downloadHandler(
    filename = function() {
      paste0(input$chrfilename2)
    },
    content = function(file) {
      write.csv(values[["df1"]], file, na="", # wdf
                row.names = FALSE, quote = TRUE)
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
      paste0(input$markfilename)
    },
    content = function(file) {
      write.csv(values[["df1Mark"]], file, na="",
                row.names = FALSE, quote = TRUE)
    }
  )

  output$downloadCsvMark2 <- downloadHandler(
    filename = function() {
      paste0(input$markfilename2)
    },
    content = function(file) {
      write.csv(values[["df1Mark"]], file, na="",
                row.names = FALSE, quote = TRUE)
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
      paste0(input$MStylefilename)
    },
    content = function(file) {
      write.csv(values[["df1MStyle"]], file, na="",
                row.names = FALSE, quote = TRUE)
    }
  )

  output$downloadCsvMStyle2 <- downloadHandler(
    filename = function() {
      paste0(input$MStylefilename2)
    },
    content = function(file) {
      write.csv(values[["df1MStyle"]], file, na="",
                row.names = FALSE, quote = TRUE)
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
    if(invalid(values[["df1"]])) { #wdf
      return()
    } else {
      # hot <- rhandsontable(df())
      hot <- rhandsontable(values[["df1"]])
      hot
    }
  })

  #
  #   render mark table
  #

  output$outMark <- renderRHandsontable({
    if(invalid(values[["df1Mark"]])) {
      return()
    } else {
      hot <- rhandsontable(values[["df1Mark"]])
      hot
    }
  })

  #
  #   render mark style table
  #

  output$outMStyle <- renderRHandsontable({
    # if(length(invalid(values[["df1MStyle"]] ) ) > 0 ) {
    if(invalid(values[["df1MStyle"]] ) ) {
      return(data.frame())
    } else if(nrow(values[["df1MStyle"]])>0 & "style" %in% colnames(values[["df1MStyle"]] ) ) {

      Options<-c("dots","square","squareLeft", "cM","cMLeft","cenStyle", "upArrow", "downArrow")

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
                           value = "chrData.csv"
                           ,width = "50%")
                # ,actionButton("saveChrData",
                # "Save")
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
               # h2("Chr. data data.frame"),
               h4("Add/Remove Row")
               ,helpText(""
                        # , p("by pressing \"Generate table\" button.")
                        , p("Right-click inside the table to delete/insert rows.")
               )
             )
             # ,br()
             ,wellPanel(
               h4("Add/Remove Column")
               ,helpText("Use unique names")
               ,textInput("col",
                          "New Col. Name",
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

      ,column(width=8,
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
                          value = "markData.csv"
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
                           "New Col. Name",
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

      ,column(width=8,
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
                          value = "MStyleData.csv"
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
                           "New Col. Name",
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

      ,column(width=8,
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
  #   parameters ###################3
  #

  output$circParam = renderUI({
      wellPanel(
        h4("circular plot p."),
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
        h4("OTU circular plot p."),
        splitLayout(
          div(title="`OTUsrt`: numeric, for `circularPlot=TRUE`. Angle to use for OTU names. Defaults to `0`. See `OTUplacing`"

          ,numericInput("OTUsrt"
                       ,"OTU name angle"
                       ,OTUsrtDefault
                       , min = -360, max = 360, step=1
          )
          )
          ,div(title='`OTUjustif`: numeric, for `circularPlot=TRUE` and `OTUplacing="number"` or `"simple"`. Justification of OTU name. `0` = left (Default); use `0.5` for centered. See `?text` -> `adj`
'
          , numericInput("OTUjustif"
                                  ,"OTU name justif."
                                  ,OTUjustifDefault
                                  , min = 0, max = 1, step=0.5
          )
          )
        ) # sL
        ,div(title='`OTUplacing`:	character, for `circularPlot=TRUE`. location of OTU name. Defaults to `"first"`, which plots name of OTU near first chr. `"number"` places number near 1st chr. and index and name of OTU to the right or center. `"simple"` places name of OTU to the right or center without numbering. See also `OTUcentered`'
        ,radioButtons("OTUplacing","OTU name loc."
                      ,c("first","number","simple")
                      ,selected=OTUplacingDefault
                      )
        )
        ,splitLayout(
          div(title='`OTULabelSpacerx`: numeric, for `circularPlot=TRUE` and `OTUplacing="number"` or `"simple"`. Modifies x names position'
          ,numericInput("OTULabelSpacerx"
                       ,"OTU name position X-axis"
                       ,OTULabelSpacerxDefault
                       , min = -10, max = 10, step=.5
          )
          )
          ,div(title='`OTUlegendHeight`: numeric, for `circularPlot=TRUE` and `OTUplacing="number"` or `"simple"`. Modifies y names separation'
          , numericInput("OTUlegendHeight"
                         ,"OTU name vert. size"
                         ,OTUlegendHeightDefault
                         , min = 0.5, max = 10, step=0.5
          )
          )
        ) # sL
      ) #wp
    # }
  } )

  # "",

  output$parameterPanel = renderUI({
    fluidRow(
      column(width=2
             ,br()
             ,wellPanel(
               h4("Layout"),
               splitLayout(
                 div(title='`circularPlot`:	boolean, if `TRUE` chromosomes are plotted in concentric circles. Defaults to `FALSE`. See `verticalPlot`'
                 ,checkboxInput("circularPlot",
                               HTML(paste("Circular", "Plot", sep = "<br/>") )
                               ,FALSE)
               )
               )

               #
               #    circ. plot
               #
               ,uiOutput("circParam")
               ,uiOutput("circParamOTU")
             )


,wellPanel(
  h4("Anchor")
  ,splitLayout(
    div(title="`moveKarHor`: character, OTUs' names of karyotypes that should be moved horizontally. See `mkhValue`
"
    ,textInput("moveKarHor",
              "kar. to move",
              value = ""
              ,width = "100%")
    )
    ,div(title="`mkhValue`: numeric, value to move kar. hor. See `moveKarHor`
"
    ,numericInput("mkhValue", "move kar.", mkhValueDefault, min = -10, max = 10, step=0.25)
  )
  ) #sL
  ,div(title='`anchor`: boolean, when TRUE, plots a parent progeny structure in karyotypes in `moveKarHor`
'
  ,checkboxInput("anchor", "Show anchor", anchorDefault)
)
  ,splitLayout(
    div(title='`moveAnchorV`: numeric, displace anchor vertical portion to right or left. See `anchor`
'
    ,numericInput("moveAnchorV", "move anchor ver.", moveAnchorVDefault, min = -10, max = 10, step=0.25)
  )
,div(title='`moveAnchorH`: numeric, displace anchor horizontal portion to right or left. See `anchor`
'    ,numericInput("moveAnchorH", "move anchor hor.", moveAnchorHDefault, min = -10, max = 10, step=0.25)
  )
)
) #wp
      ), # col
      column(width=2
             ,br()
        ,wellPanel(
          h4("Marks")
          ,div(title="`mycolors`: optional, character vector with colors' names, which are associated automatically with marks according to their order in the data.frame of position of marks. See this ordering with `unique(dfMarkPos$markName)`. Argument example: `mycolors = c(\"red\",\"chartreuse3\",\"dodgerblue\")`. Not mandatory for plotting marks, package has default colors.
          "
          ,textInput('mycolors'
                     ,HTML(paste("Colors", '(optional, see data.frame page)', sep = "<br/>") )
                     , "red,blue")
        )
        ,div(title='`useOneDot`: boolean, use one dot instead of two in style of marks dots. Defaults to `FALSE`
'          ,checkboxInput("useOneDot",
                          "one dot only",
                          useOneDotDefault
)
        )
        )
        ,wellPanel(
          h4("chr. color")
          ,splitLayout(
            div(title='`chrColor`: (`"gray"`) Determines the color of chromosomes
'
          ,textInput('chrColor', 'chr.', "gray")
            )
          ,div(title='`cenColor`: Determines the color of centromeres. if GISH use `NULL`. Defaults to `chrColor`
'
          ,textInput('cenColor', 'cen.', "")
          )
          ) # sL
            ,h4("Borders"),
          div(title='`fixCenBorder`: boolean, when `TRUE` uses `chrColor` as centromere (and cen. mark) border color. See also `cenColor`, `chrColor`, `colorBorderMark`, `borderOfWhiteMarks`. No default value.
'
           , checkboxInput("fixCenBorder","chr. color as border color of cen.",TRUE)
          )
          ,splitLayout(
            div(title='`chrBorderColor`: character, color for border of chromosomes, defaults to `chrColor`'
            ,textInput("chrBorderColor","border color",NA)
            )
            ,div(title='`lwd.chr`: (`0.5`) width of border lines for chr. and marks when related param. absent.
'
          ,numericInput("lwd.chr","width",lwd.chrDefault,min=0,max=4,step=0.25)
          )
          ) # sL
        ) # wP
        ,wellPanel(
          h4("Legend"),
          div(title='`legend`: (`"aside"`) If you wanto to plot the names of marks near each chromosome use `legend = "inline"`, to the right of karyotypes use `legend = "aside"`, otherwise use `legend = ""` for no legend. See `markLabelSpacer`'
          ,radioButtons("legend","leg. Pos.",c("aside","inline","none") )
          )
          ,splitLayout(
            div(title='`legendWidth`: (`1.7`) numeric, factor to modify the width of the square and dots of legend. For `legend="aside"`.'
        ,numericInput("legendWidth", "width", 1, min = 0.25, max = 5, step=0.05)
            )
        ,div(title='`legendHeight`: (`NA`) numeric, factor to modify the height of the square and dots of legend. For `legend="aside"`.'
        ,numericInput("legendHeight", "height", 1, min = 0.25, max = 5, step=0.05)
        )
          ) # sL
        )
        ,wellPanel(
          h4("Info"),
          splitLayout(
            div(title='`chrSize`: boolean, when `TRUE` adds total chr size under each chr. Defautls to `FALSE`
'
            ,checkboxInput("chrSize",
                          HTML(paste("Show Chrom.", "size", sep = "<br/>") )
                          ,TRUE)
            ),
            div(title="`chrSizeMbp`: boolean, when `TRUE` adds total Mbp chr. size to each chr. provided, there is a `Mbp` column in `dfChrSize` data.frame. Defaults to `FALSE`. If data in columns `shortArmSize`, or col. `chrSize` is in millions ('Mbp'). Use `chrSize=TRUE` not this one (not column `Mbp`, you don't need this).
"
            ,checkboxInput("chrSizeMbp",
                          HTML(paste("Show Chr. Size", "in Mbp",tags$p("(column required)", style = "font-size: 80%;")
                                     , sep = "<br/>") )
                          # "Show Chr. Size in Mbp (column required)"
                          ,FALSE)
            )
          ) # sL
          ,div(title='`distTextChr`: Vertical distance from indices (text) to the chromosome.
'
          , numericInput("distTextChr", "chr. to text separation", 1.2, min = 0.2, max = 5, step=0.1)
        )
        ,div(title='`addOTUName`: (`TRUE`) If `TRUE` adds name of species (OTU) under karyotype
'
          , checkboxInput("addOTUName",
                          "Show OTU name"
                          ,FALSE)
        )
        ,div(title='`OTUfont`: numeric, `1` for normal, `2` for bold, `3` for italics, `4` for bold-italics
'
          ,radioButtons("OTUfont","OTU font type",
                        c("normal"="1","bold"="2","italics"="3","bold italics"="4"),
                        selected="1")
        )
          ) #wP
      ) # col
      ,column(
        width=8
        ,br()
        ,fluidRow(
          column(3
        ,wellPanel(
          h4("width % and height proportion"),
            numericInput("widFactor", "width %",
                         80, min = 5, max = 100, step=5)
            ,          textOutput("imageWidth")
            ,numericInput("heiFactor", "height ratio",
                         0.5, min = 0.05, max = 20, step=0.05)
            ,          textOutput("imageHeight")
        ) # wP
        ,wellPanel("Aspect",
                   div(title='`chromatids`: boolean, when `TRUE` shows separated chromatids. Defaults to `TRUE`
'
                   ,checkboxInput("chromatids",
                                 HTML(paste("Show separ.", "chromatids", sep = "<br/>") )
                                 ,TRUE)
                   ),
                   splitLayout(
                     div(title='`squareness`: (`4`) Squared or rounded vertices when marks of the "square" style (defined in data.frame passed to `dfMarkColor`). Affects chromosomes also. Smaller numbers = more rounded
'
                     ,numericInput("squareness","Vertices squareness"
                                  ,squarenessDefault
                                  , min = 1, max = 21, step=0.5)
                     )
                     ,div(title='`orderChr`: (`size`) character, use same as plot.
'
                     ,radioButtons(
                       "orderChr",
                       "chr. order by:"
                       ,c("size"="size","as in d.f."="original", "alphab."="name"
                          ,"group"="group"
                          ,"col. chrNameUp"="chrNameUp")
                       ,selected = orderChrDefault )
                     )
                   ) # sL
        ) # wP
        ) #c
        ,column(3
                ,wellPanel(
                  h4("Ruler")
                  ,div(title='`ruler`: (`TRUE`) When `TRUE` displays ruler to the left of karyotype, when `FALSE` shows no ruler
'
                  ,checkboxInput("ruler",
                                "Show ruler",TRUE)
                                )
                  ,splitLayout(
                    div(title='`rulerPos`: (`-0.5`) Absolute position of ruler, corresponds to "pos" argument of the function `axis` of R plots'
                    ,numericInput("rulerPos", "modify ruler pos.", 0, min = -5, max = 5, step=0.1) ,
                    )
                    ,div(title='`ruler.tck`: (`-0.02`) tick size of ruler, corresponds to "tck" argument of `axis` function
'
                    ,numericInput("ruler.tck", "modify ruler ticks", -0.01, min = -5, max = 5, step=0.005)
                  )
                  ),
                  splitLayout(
                    div(title="`rulerNumberSize`: (`1`) Size of number's font in ruler
"
                    ,numericInput("rulerNumberSize", "font size of rulers", 0.8, min = .1, max = 5, step=0.1) ,
                    )
                    ,div(title="
`xPosRulerTitle`: (`2.6`) Modifies the horizontal position of the title of rulers (Mb, etc). Moves to left from 1st chr. in `chrSpacing` times
"
                         ,numericInput("xPosRulerTitle", "pos. of ruler title", 3, min = -5, max = 5, step=0.1)
                  )
                  )
                ) #wp
                ,wellPanel(
                  h4("Notes")
                  ,splitLayout(
                    div(title="`OTUasNote`: (`FALSE`) See also `notes`. If `TRUE` OTU name is written to the right, as `notes`.
"
                    ,checkboxInput("OTUasNote",
                                  HTML(paste("OTU as note", "(right)", sep = "<br/>") )
                                  ,FALSE)
                    )
                    ,div(title="`notesTextSize`: (`0.4`) numeric, font size of notes, see `notes`
"
                    ,numericInput("notesTextSize", "font size of notes",
                                  notesTextSizeDefault, min = 0.1, max = 10, step=0.1)
                    )
                  ) # sL
                  ,div(title="`notesPosX`: (`0.5`) numeric, moves right notes in the x axis
"
                  ,numericInput("notesPosX", "pos. right notes hor.",
                                notesPosXDefault, min = -20, max = 20, step=0.1
                  )
                  )

                ) # wP

                )
        ,column(3
               ,wellPanel(
                  h4("Horizontal Margins"),
                  splitLayout(
                    div(title='`xlimLeftMod`: (`1`) modifies `xlim` left (first) component of the plot as in any "R-plot"
'
                    ,numericInput("xlimLeftMod", "left",
                                 2, min = -5, max = 5, step=0.5)
                    )
,div(title='`xlimRightMod`:	(`2`) `xlim` (right) modification by adding space to the right of idiograms
'
                    ,numericInput("xlimRightMod", "right",
                                 xlimRightModDefault, min = -5, max = 5, step=0.5)
                  )
) # sL
                ) # wp

        ,wellPanel(
          h4("Horizontal separ.")
          ,        splitLayout(
            div(title='`chrWidth`: (`0.5`) Determines the width of chromosomes
'
            ,numericInput("chrWidth", "Chr. width", 1.2, min = 0.1, max = 5, step=0.1)
            )
            ,div(title='`chrSpacing`: (`0.5`) Determines the horizontal spacing among chromosomes
'
            ,numericInput("chrSpacing", "horiz. spacing chr.", 1, min = 0.1, max = 5, step=0.1)
            )
          )
        ) # wP
        ,wellPanel(
          h4("Indices"),
          splitLayout(
            div(title='`morpho`: (`"both"`) character, if `"both"` (default) prints the Guerra (1986) and Levan (1964) classif. of cen. position.  , use also `"Guerra"` or `"Levan"` or `""` for none. See `?armRatioCI` also (function).
'
            ,radioButtons("morpho","Morphology",c("Guerra (1986)"="Guerra","Levan (1974)"="Levan", "both"="both","none"="")
                          )
            ),
            div(title='`chrIndex`: (`"both"`) character, add arm ratio with `"AR"` and centromeric index with `"CI"`, or `"both"` (Default), or `""` for none to each chromosome [@Levan1964]. See `armRatioCI`also.
'
            ,radioButtons("chrIndex","Add AR & CI"
                         ,c("Chr. Index"="CI","Arm Ratio"="AR", "both"="both","none"="")
                         ,selected = "CI" )
            )
          ) # sL
        ) #wp
        )
        ,column(3
                ,wellPanel(
                  h4("Vertical Margins"),
                  splitLayout(
                    div(title='`ylimBotMod`:	(`0.2`) modify `ylim` bottom component of plot adding more space
'
                    ,numericInput("ylimBotMod", "bottom",
                                 ylimBotModDefault, min = -5, max = 5, step=0.5)
                    )
                    ,div(title='`ylimTopMod`: (`0.2`) modify `ylim` top component of plot adding more space.
'
                    ,numericInput("ylimTopMod", "top"
                                  , 0, min = -5, max = 5, step=0.2)
                    )
                  ) #sL
                ) #wp
                ,wellPanel(
                  h4("Vertical separ."),
                  splitLayout(
                    div(title='`karHeight`: (`2`) Vertical size of karyotypes considering only chromosomes. for ex `karHeight = 1`
'
                    ,numericInput("karHeight", HTML(paste("Karyotype","height", sep = "<br/>") )
                                 ,5, min = 0.5, max = 50, step=0.5)
                    )
                    ,
                    div(title='`karHeiSpace`: (`2.5`) Vertical size of karyotypes including spacer. for ex `karHeiSpace = 1.2`
'
                    ,numericInput("karHeiSpace", HTML(paste("kar. vert. size","with space", sep = "<br/>") )
                                 , 2.5, min = 2, max = 150, step=0.5)
                    )
                  ) # sL
                  ,splitLayout(
                    div(title='`karSepar`: (`TRUE`) If `TRUE` reduces the space among karyotypes. `FALSE` = equally sized karyotypes or `TRUE` = equally spaced karyotypes. Incompatible with `addMissingOTUAfter`
'
                    ,checkboxInput("karSepar"
                                  ,HTML(paste("equally spaced","kar.", sep = "<br/>") )
                                  ,FALSE)
                    )
                    ,div(title='`amoSepar`: (`9`) For `karSepar = TRUE`, if zero, no space among karyotypes. Amount of separation.  if overlap, increase this and `karHeiSpace`
'
                    ,numericInput("amoSepar", "kar. vert. separ", 10, min = 0, max = 15, step=0.5)
                    )
                  ) # sL
                ) #Wp
        ) # c
        ) #fr
        ,fluidRow(
     wellPanel(style = "overflow-y:auto;text-align:center;"

        # ,actionButton("goButton", "Go!", class = "btn-success")

        ,div(style = 'overflow-y:auto;overflow-x:auto;min-width:1030px;'
        ,plotOutput("idiogramPlot"
                    ,height = "auto"
        )
      ) #d
      ) # wp
        )

     ) #c
    ) # fR
  })

  output$imageWidth<- renderText({
    px<-session$clientData$output_idiogramPlot_width*(as.numeric(input$widFactor)/100)
    paste("Width:",px,"px;",px/80,"in")
  })

  output$imageHeight<- renderText({
    px <-session$clientData$output_idiogramPlot_width*(as.numeric(input$heiFactor)*(as.numeric(input$widFactor)/100)
    )
    paste("Heigth:"
          ,px,"px;",px/80,"in")
  })

observeEvent(input$exampleId,{
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

  mc <- match.call(plotIdiograms,
                 call("plotIdiograms",
                      dfChrSize  = shQuote(input$chrfilename2)
                      ,dfMarkPos = ifelse(invalid(values[["df1Mark"]] ),shQuote(""),shQuote(input$markfilename2) )
                      ,dfMarkColor= ifelse(invalid(values[["df1MStyle"]] ),shQuote(""),shQuote(input$MStylefilename2) )

                      ,karHeight=input$karHeight,             # kar. height
                      karHeiSpace = input$karHeiSpace,        # kar. height
                      amoSepar = input$amoSepar,
                      karSepar = input$karSepar,
                      legend = shQuote(input$legend),
                      mycolors = if(length( values[["mycolors"]] )==0){shQuote("")
                        } else{values[["mycolors"]]
                          },

                      chrColor = shQuote(input$chrColor),
                      cenColor = shQuote(input$cenColor),

                      chrWidth = input$chrWidth,           # chr. width
                      chrSpacing = input$chrSpacing,           # space among chr.

                      squareness = input$squareness
                      ,orderChr = shQuote(input$orderChr)
                      ,useOneDot = input$useOneDot

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
                      chrSizeMbp = input$chrSizeMbp,        # add Mbp sizes under chr. (see above)
                      chromatids = input$chromatids,
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
  mclist[1]<-NULL
  seq<-paste(names(mclist ),mclist  , sep="=", collapse = ",\n")
  add3<-paste0('svg("dfOfChrSize.svg",width=',values[["mysvgwidth"]],", height=",values[["mysvgheight"]],')')
  block<-ifelse(input$asSvg,"","#")
  values[["strFun"]] <- paste0(add0,"\n"
                               ,add1,"\n"
                               ,block
                               ,add2,"\n"
                               ,add2b,"\n"
                               ,block
                               ,add3,"\n"
                               ,"plotIdiograms(",seq,")"
                               ,"\n"
                               ,block
                               ,"dev.off()"
                               ,"\n}")



}) # observe

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
  downloadButton('downloadR', 'Download script as .R')
})


  # output$idiogramPlot <- renderPlot( {
output$idiogramPlot <- renderImage( {
  # input$goButton

  # isolate ({

# validate(need(try(df()),"Start in previous page (left) with data.frames" ))
validate(need(try(values[["df1"]]),"Start in previous page (left) with data.frames" ))

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
      # dfChrSize  =df(),
      dfChrSize  =values[["df1"]],# data.frame of chr. size
                  dfMarkColor=values[["df1MStyle"]],  # d.f of mark style <- Optional
                  dfMarkPos  =values[["df1Mark"]],     # df of mark positions (includes cen. marks)

                  karHeight=input$karHeight,              # kar. height
                  karHeiSpace = input$karHeiSpace,              # kar. height
                  amoSepar = input$amoSepar,
                  karSepar = input$karSepar,
                  legend = as.character(input$legend),

                  mycolors = if(length( values[["mycolors"]] )==0 ){""} else { values[["mycolors"]]}
                                     ,

                  chrColor = input$chrColor,
                  cenColor = input$cenColor,

                  chrWidth = input$chrWidth,           # chr. width
                  chrSpacing = input$chrSpacing,           # space among chr.

                  squareness = input$squareness
                  ,orderChr = input$orderChr
                  ,useOneDot = input$useOneDot

                  ,chrLabelSpacing = input$chrLabelSpacing
                  ,rotation = input$rotation
                  ,shrinkFactor  = input$shrinkFactor
                  ,radius   = input$radius
                  ,circleCenter  = input$circleCenter
                  ,OTUsrt = input$OTUsrt
                  ,OTUjustif  = input$OTUjustif
                  ,OTULabelSpacerx= input$OTULabelSpacerx
                  ,OTUlegendHeight= input$OTUlegendHeight
                  ,OTUplacing  = input$OTUplacing

                  ,morpho=input$morpho,          # chr. morpho. classif. (Guerra, Levan, both, "" ) ver. >= 1.12 only
                  chrIndex=input$chrIndex,            # cen. pos. (CI, AR, both, "" ) ver. >= 1.12 only
                  chrSize = input$chrSize,           # add chr. sizes under chr.
                  chrSizeMbp = input$chrSizeMbp,        # add Mbp sizes under chr. (see above)
                  chromatids = input$chromatids,
                  circularPlot = input$circularPlot,

                  addOTUName = input$addOTUName,
                  OTUfont = as.numeric(input$OTUfont),
                  moveKarHor = as.character(input$moveKarHor),
                  mkhValue = input$mkhValue,
                  anchor = input$anchor,
                  moveAnchorV = input$moveAnchorV,
                  moveAnchorH = input$moveAnchorH,

                  OTUasNote = input$OTUasNote,
                  notesTextSize = input$notesTextSize,
                  notesPosX = input$notesPosX ,

                  ruler =input$ruler,
                  rulerPos = input$rulerPos,              # position of ruler
                  ruler.tck= input$ruler.tck,          # size and orientation of ruler ticks
                  rulerNumberSize=input$rulerNumberSize        # font size of rulers
                  ,xPosRulerTitle = input$xPosRulerTitle             # pos of ruler title

                  ,legendWidth=input$legendWidth            # width of legend items
                  ,legendHeight=input$legendHeight

                  ,fixCenBorder = input$fixCenBorder      # use chrColor as border color of cen. or cen. marks
      ,chrBorderColor = input$chrBorderColor
      ,lwd.chr = input$lwd.chr
      ,distTextChr = input$distTextChr        # chr. text separation

                  ,xlimLeftMod = input$xlimLeftMod          # xlim left param.
                  ,xlimRightMod = input$xlimRightMod          # xlim left param.

                  ,ylimBotMod = input$ylimBotMod           # modify ylim bottom argument
                  ,ylimTopMod = input$ylimTopMod           # modify ylim top argument
    ) # plot
    # )
    ,    file= (outfile <- file(values[["outfile"]],"w")),type="message"
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

  filenameR <- eventReactive(input$exampleId,{
    f<-values[["outfile"]]
  })

output$log <- renderText({
    rawText <- filenameR()
    validate(need(try(readLines(rawText) ), message = FALSE) )
    replacedText <- paste(readLines(rawText ), collapse = "\n")
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
    HTML(markdown::markdownToHTML(knit('about.Rmd', quiet = TRUE)))
  })

  output$strpanel = renderUI({ # was treechar2
    fluidRow(
      column(width=6
    ,wellPanel(
      rclipboardSetup()
      ,splitLayout(
      uiOutput("clip")
      ,uiOutput("buttonScript")
      ,checkboxInput("asSvg","to .svg (keeps size)",TRUE)
      )
      ,verbatimTextOutput("code")

    ) # end
      ),
    column(width=3
           ,wellPanel(
             h4("Save chr. data")
             ,helpText("optional")
             ,textInput("chrfilename2",
                        "File name",
                        value = "chrData.csv"
                        ,width = "50%")
             ,uiOutput("buttontable2")
           ) # end wellpanel
           ,wellPanel(
             h4("Save mark pos. data")
             ,helpText("optional")
             ,textInput("markfilename2",
                        "File name",
                        value = "markData.csv"
                        ,width = "50%")
             ,uiOutput("buttontableMark2")
           ) # end wellpanel
           ,wellPanel(
             h4("Save mark style data")
             ,helpText("optional")
             ,textInput("MStylefilename2",
                        "File name",
                        value = "MStyleData.csv"
                        ,width = "50%")
             ,uiOutput("buttontableMStyle2")
           ) # end wellpanel
           )
    ) # fR
  }) # end output

  outputOptions(output, "dfchrpanel", suspendWhenHidden = FALSE)
  outputOptions(output, "dfmarkpanel", suspendWhenHidden = FALSE)
  outputOptions(output, "dfMStylepanel", suspendWhenHidden = FALSE)
  outputOptions(output, "parameterPanel", suspendWhenHidden = FALSE)
  outputOptions(output, "logpanel", suspendWhenHidden = FALSE)
  outputOptions(output, "strpanel", suspendWhenHidden = FALSE)

}
