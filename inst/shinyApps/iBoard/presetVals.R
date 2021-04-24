
#
#   objects to load
#

emptydata.frame<-data.frame()

{
  chrDataVec <- c("dfOfChrSize",
                  "dfChrSizeHolo",
                  "bigdfOfChrSize",
                  "bigdfChrSizeHolo"

                  ,"dfChrSizeHolo"
                  ,"parentalAndHybChrSize"
                  ,"parentalAndHybHoloChrSize"
                  ,"traspadf"

                  ,"emptydata.frame"
                  ,"emptydata.frame"
  )

  markDataVec <- c("dfOfMarks2",
                   "dfMarkPosHolo",
                   "bigdfOfMarks",
                   "bigdfMarkPosHolo"

                   ,"dfMarkPosHolo"
                   ,"dfAlloParentMarks"
                   ,"dfAlloParentMarksHolo"
                   ,"traspaMarks"

                   ,"emptydata.frame"
                   ,"emptydata.frame"
  )

  MStyleDataVec <- c("dfMarkColor",
                     "dfMarkColor",
                     "dfMarkColor",
                     "dfMarkColor"

                     ,"emptydata.frame"
                     ,"emptydata.frame"
                     ,"emptydata.frame"
                     ,"dfMarkColor5S25S"

                     ,"emptydata.frame"
                     ,"emptydata.frame"
  )

  termDefault <- ""
  termVec <- c(termDefault,
                termDefault,
                termDefault,
                termDefault,

                termDefault
                ,termDefault
                ,termDefault
                ,termDefault

                ,"Bacillus cereus strain 03BB87 plasmid pBCN, complete sequence"
               ,"'Nostoc azollae' 0708, complete"
  )

  widFactorVec <- c(80,
                    80,
                    70,
                    80,

                    80
                    ,80
                    ,55
                    ,80

                    ,100
                    ,250)

  heiFactorVec <- c(0.5,
                    0.5,
                    2.5,
                    1,

                    0.5
                    ,1
                    ,1
                    ,1

                    ,1
                    ,1

  )

  pngorsvgDefault <- "svg"
  pngorsvgVec <- c(rep(pngorsvgDefault,8)
                   ,"svg"
                   ,"png"
  )

  pngorsvgDownDefault <- "svg"
  pngorsvgDownVec <- c(rep(pngorsvgDownDefault,8)
                   ,"svg"
                   ,"svg"
  )

  markNDefault<- 25
  markNVec<-c(
    rep(markNDefault,9)
    ,2
  )

  nDefault<- 50
  nVec<-c(
    rep(nDefault,9)
    ,150
  )

  useGeneNamesDefault <- TRUE
  useGeneNamesVec<-rep(useGeneNamesDefault,10)

  useRCNamesDefault <- TRUE
  useRCNamesVec<-rep(useRCNamesDefault,10)

  makeUniqueDefault <- TRUE
  makeUniqueVec<-rep(makeUniqueDefault,10)

  colorFeatureDefault <- FALSE
  colorFeatureVec<-rep(colorFeatureDefault,10)

  nucMarkStyleDefault<-"Arrows"
  nucMarkStyleVec<-rep(nucMarkStyleDefault,10)

  mirrorDefault <- FALSE
  mirrorVec<-rep(mirrorDefault,10)

  pseudoDefault<-"all"
  pseudoVec<-rep(pseudoDefault,10)

  addSTARTPosDefault<-TRUE
  addSTARTPosVec<-rep(addSTARTPosDefault,10)



# zoom
  hwModifierDefault<-1
  hwModifierVec<-c(
                   rep(hwModifierDefault,9)
                   ,0.6
                   )

  # chrId
  chrIdDefault <- "original"
  chrIdVec <- c(chrIdDefault,
                    chrIdDefault,
                    chrIdDefault,
                    chrIdDefault,

                    chrIdDefault
                    ,chrIdDefault
                    ,chrIdDefault
                    ,chrIdDefault

                    ,"none"
                ,"none"
  )

  # markLabelSize "`markLabelSize`: (`1`) Determines the size of text of the legend."

  markLabelSizeDefault <-1
  markLabelSizeVec <- c(markLabelSizeDefault,
                markLabelSizeDefault,
                markLabelSizeDefault,
                markLabelSizeDefault,

                markLabelSizeDefault
                ,markLabelSizeDefault
                ,markLabelSizeDefault
                ,markLabelSizeDefault

                ,0.7
                ,.25
  )

  patternDefault <-""
  patternVec <- c(patternDefault,
                        patternDefault,
                        patternDefault,
                        patternDefault,

                        patternDefault
                        ,patternDefault
                        ,patternDefault
                        ,patternDefault

                        ,"NT98_"
                  ,"[[:alnum:]]+_"
  )



  cMBeginCenterDefault <-FALSE
  cMBeginCenterVec <- c(cMBeginCenterDefault,
                  cMBeginCenterDefault,
                  cMBeginCenterDefault,
                  cMBeginCenterDefault,

                  cMBeginCenterDefault
                  ,cMBeginCenterDefault
                  ,cMBeginCenterDefault
                  ,cMBeginCenterDefault

                  ,TRUE
                  ,TRUE
  )

  labelSpacingDefault <-0.7
  labelSpacingVec <- c(labelSpacingDefault,
                        labelSpacingDefault,
                        labelSpacingDefault,
                        labelSpacingDefault,

                        labelSpacingDefault
                        ,labelSpacingDefault
                        ,labelSpacingDefault
                        ,labelSpacingDefault

                        ,1.7
                       ,1
  )

  labelOutwardsDefault <-FALSE
  labelOutwardsVec <- c(labelOutwardsDefault,
                       labelOutwardsDefault,
                       labelOutwardsDefault,
                       labelOutwardsDefault,

                       labelOutwardsDefault
                       ,labelOutwardsDefault
                       ,labelOutwardsDefault
                       ,labelOutwardsDefault

                       ,TRUE
                       ,TRUE

  )


  OTUTextSizeDefault <-1
  OTUTextSizeVec <- c(OTUTextSizeDefault,
                        OTUTextSizeDefault,
                        OTUTextSizeDefault,
                        OTUTextSizeDefault,

                        OTUTextSizeDefault
                        ,OTUTextSizeDefault
                        ,OTUTextSizeDefault
                        ,OTUTextSizeDefault

                        ,0.8
                      ,3
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

                    ,karHeightDefault
                    ,karHeightDefault
  )

  xModifierDefault <- 12
  xModifierVec <- c(xModifierDefault,
                    xModifierDefault,
                    xModifierDefault,
                    xModifierDefault,

                    xModifierDefault
                    ,xModifierDefault
                    ,xModifierDefault
                    ,xModifierDefault

                    ,xModifierDefault
                    ,xModifierDefault
  )

indexIdTextSizeDefault <- 1
indexIdTextSizeVec <- c(indexIdTextSizeDefault,
                    indexIdTextSizeDefault,
                    indexIdTextSizeDefault,
                    indexIdTextSizeDefault,

                    indexIdTextSizeDefault
                    ,indexIdTextSizeDefault
                    ,indexIdTextSizeDefault
                    ,indexIdTextSizeDefault

                    ,indexIdTextSizeDefault
                    ,indexIdTextSizeDefault
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

                      ,karHeiSpaceDefault
                      ,karHeiSpaceDefault
  )

  cenFormatDefault <- "rounded"
  cenFormatVec <- c(cenFormatDefault,
                      cenFormatDefault,
                    cenFormatDefault,
                    cenFormatDefault,

                      cenFormatDefault
                      ,cenFormatDefault
                      ,cenFormatDefault
                      ,cenFormatDefault

                    ,cenFormatDefault
                    ,cenFormatDefault
  )

  cenFactorDefault <- 1
  cenFactorVec <- c(cenFactorDefault,
                    cenFactorDefault,
                    cenFactorDefault,
                    cenFactorDefault,

                    cenFactorDefault
                    ,cenFactorDefault
                    ,cenFactorDefault
                    ,cenFactorDefault

                    ,cenFactorDefault
                    ,cenFactorDefault
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

                 ,"inline"
                 ,"inline"
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

                   ,chrWidthDefault
                   ,4
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

                     ,21
                     ,21
  )

  useOneDotDefault <- FALSE
  useOneDotVec <- c(useOneDotDefault,
                    useOneDotDefault,
                    useOneDotDefault,
                    useOneDotDefault,
                    useOneDotDefault
                    ,useOneDotDefault
                    ,useOneDotDefault
                    ,useOneDotDefault

                    ,useOneDotDefault
                    ,useOneDotDefault
  )

  pMarkFacDefault <- 0.25
  pMarkFacVec <- c(pMarkFacDefault,
                    pMarkFacDefault,
                    pMarkFacDefault,
                    pMarkFacDefault,

                    pMarkFacDefault
                    ,pMarkFacDefault
                    ,pMarkFacDefault
                    ,pMarkFacDefault

                   ,pMarkFacDefault
                   ,pMarkFacDefault
  )

  markTypeDefault <- c("downArrow","upArrow","cM","cMLeft")
  markTypeList <- list(markTypeDefault,
                     markTypeDefault,
                     markTypeDefault,
                     markTypeDefault,

                     markTypeDefault
                     ,markTypeDefault
                     ,markTypeDefault
                     ,markTypeDefault

                   ,"downArrow"
                   ,c("downArrow","upArrow")
  )

  fetchSelectDefault <- 1
  fetchSelectVec <- c(fetchSelectDefault,
                   fetchSelectDefault,
                   fetchSelectDefault,
                   fetchSelectDefault,

                   fetchSelectDefault
                   ,fetchSelectDefault
                   ,fetchSelectDefault
                   ,fetchSelectDefault

                   ,fetchSelectDefault
                   ,fetchSelectDefault
  )


  amountofSpacesDefault <- 13
  amountofSpacesVec <- c(amountofSpacesDefault,
                   amountofSpacesDefault,
                   amountofSpacesDefault,
                   amountofSpacesDefault,

                   amountofSpacesDefault
                   ,amountofSpacesDefault
                   ,amountofSpacesDefault
                   ,amountofSpacesDefault

                   ,10
                   ,13
                   )

  colNumberDefault <- 2
  colNumberVec <- c(colNumberDefault,
                         colNumberDefault,
                         colNumberDefault,
                         colNumberDefault,

                         colNumberDefault
                         ,colNumberDefault
                         ,colNumberDefault
                         ,colNumberDefault

                    ,colNumberDefault
                    ,6
  )

  protrudingIntDefault <- 0.5
  protrudingIntVec <- c(protrudingIntDefault,
                     protrudingIntDefault,
                     protrudingIntDefault,
                     protrudingIntDefault,

                     protrudingIntDefault
                     ,protrudingIntDefault
                     ,protrudingIntDefault
                     ,protrudingIntDefault

                     ,1.3
                     ,protrudingIntDefault
  )


  protrudingDefault <- 0.2
  protrudingVec <- c(protrudingDefault,
                   protrudingDefault,
                   protrudingDefault,
                   protrudingDefault,

                   protrudingDefault
                   ,protrudingDefault
                   ,protrudingDefault
                   ,protrudingDefault

                   ,0.5
                   ,protrudingDefault
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

                          ,chrLabelSpacingDefault
                          ,chrLabelSpacingDefault
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

                   ,0
                   ,0
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

                       ,1
                       ,1
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

                 ,2.5
                 ,8
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

                       ,circleCenterDefault
                       ,circleCenterDefault
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

                 ,OTUsrtDefault
                 ,OTUsrtDefault
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

                     ,"simple"
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
                    ,OTUjustifDefault

                    ,0.5
                    ,0.5
  )

  OTULabelSpacerxDefault <- 0
  OTULabelSpacerxVec <- c(OTULabelSpacerxDefault,
                          OTULabelSpacerxDefault,
                          OTULabelSpacerxDefault,
                          OTULabelSpacerxDefault,

                          OTULabelSpacerxDefault
                          ,OTULabelSpacerxDefault
                          ,OTULabelSpacerxDefault
                          ,-6

                          ,OTULabelSpacerxDefault
                          ,OTULabelSpacerxDefault

  )


  OTUlegendHeightDefault <- as.numeric(NA)
  OTUlegendHeightVec <- c(OTUlegendHeightDefault,
                          OTUlegendHeightDefault,
                          OTUlegendHeightDefault,
                          OTUlegendHeightDefault,
                          OTUlegendHeightDefault
                          ,OTUlegendHeightDefault
                          ,OTUlegendHeightDefault
                          ,1.5

                          ,OTUlegendHeightDefault
                          ,OTUlegendHeightDefault

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

                   ,orderChrDefault
                   ,orderChrDefault

  )

  chrSpacingDefault <- 0.5
  chrSpacingVec <- c(1,
                     chrSpacingDefault,
                     chrSpacingDefault,
                     chrSpacingDefault

                     ,chrSpacingDefault
                     ,chrSpacingDefault
                     ,chrSpacingDefault
                     ,chrSpacingDefault

                     ,chrSpacingDefault
                     ,chrSpacingDefault
  )

  morphoDefault <- "both"
  morphoVec <- c("Guerra",
                 morphoDefault,
                 "Guerra",
                 morphoDefault,
                 morphoDefault
                 ,morphoDefault
                 ,morphoDefault
                 ,morphoDefault

                 ,morphoDefault
                 ,morphoDefault
  )

  chrColorDefault <- "gray"
  chrColorVec <- c(chrColorDefault
                   ,chrColorDefault
                   ,chrColorDefault
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

                   ,cenColorDefault
                   ,cenColorDefault
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

                   ,chrIndexDefault
                   ,chrIndexDefault

  )

  karIndexDefault<-TRUE
  karIndexVec <- rep(karIndexDefault,10)

  chrSizeDefault <- FALSE

  chrSizeVec <- c( TRUE,
                   TRUE,
                   chrSizeDefault,
                   chrSizeDefault,
                   chrSizeDefault,
                   chrSizeDefault
                   ,chrSizeDefault
                   ,chrSizeDefault

                   ,chrSizeDefault
                   ,chrSizeDefault
  )

  chromatidsDefault <- TRUE
  chromatidsVec <- c( chromatidsDefault,
                      chromatidsDefault,
                      chromatidsDefault,
                      chromatidsDefault,
                      FALSE
                      ,chromatidsDefault
                      ,chromatidsDefault
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

                        ,TRUE
                        ,TRUE
  )

  chrSizeMbpDefault <- FALSE
  chrSizeMbpVec <- c( chrSizeMbpDefault,
                      chrSizeMbpDefault,
                      chrSizeMbpDefault,
                      chrSizeMbpDefault,

                      chrSizeMbpDefault
                      ,chrSizeMbpDefault
                      ,chrSizeMbpDefault
                      ,chrSizeMbpDefault

                      ,chrSizeMbpDefault
                      ,chrSizeMbpDefault
  )

  distTextChrDefault <- 1
  distTextChrVec <- c( 1.2,
                       distTextChrDefault,
                       0.8,
                       0.5,

                       distTextChrDefault
                       ,distTextChrDefault
                       ,0.8
                       ,distTextChrDefault

                       ,distTextChrDefault
                       ,distTextChrDefault
  )

  rulerDefault <- TRUE
  rulerVec <- c( rulerDefault,
                 rulerDefault,
                 rulerDefault,
                 rulerDefault,
                 FALSE,
                 FALSE
                 ,FALSE
                 ,   rulerDefault

                 ,   rulerDefault
                 ,   rulerDefault
  )

  rulerPosDefault <- 0
  rulerPosVec <- c( rulerPosDefault,
                    -0.1,
                    -0.4,
                    rulerPosDefault,
                    rulerPosDefault
                    ,rulerPosDefault
                    ,rulerPosDefault
                    ,rulerPosDefault

                    ,rulerPosDefault
                    ,rulerPosDefault
  )

  ruler.tckDefault <- -0.02
  ruler.tckVec <- c(-0.01,
                    -0.02,
                    -0.005,
                    -0.005,

                    ruler.tckDefault
                    ,ruler.tckDefault
                    ,ruler.tckDefault
                    ,ruler.tckDefault

                    ,ruler.tckDefault
                    ,ruler.tckDefault
  )

  rulerTitleSizeDefault<-1
  rulerTitleSizeVec <- rep(rulerTitleSizeDefault,10)

  rulerNumberSizeDefault <- 1
  rulerNumberSizeVec <- c(0.8,
                          rulerNumberSizeDefault,
                          0.4,
                          0.9,
                          rulerNumberSizeDefault
                          ,rulerNumberSizeDefault
                          ,rulerNumberSizeDefault
                          ,rulerNumberSizeDefault

                          ,rulerNumberSizeDefault
                          ,rulerNumberSizeDefault
  )

  xPosRulerTitleDefault <- 1
  xPosRulerTitleVec <- c(3,
                         3,
                         3.5,
                         2.8,

                         3
                         ,xPosRulerTitleDefault
                         ,xPosRulerTitleDefault
                         ,xPosRulerTitleDefault

                         ,xPosRulerTitleDefault
                         ,xPosRulerTitleDefault
  )

  legendWidthDefault <- 1.7
  legendWidthVec <- c(1,
                      1.2,
                      legendWidthDefault,
                      1,

                      legendWidthDefault
                      ,legendWidthDefault
                      ,legendWidthDefault
                      ,legendWidthDefault

                      ,legendWidthDefault
                      ,legendWidthDefault
  )

  legendHeightDefault <- as.numeric(NA)
  legendHeightVec <- c(legendHeightDefault,
                       legendHeightDefault,
                       legendHeightDefault,
                       legendHeightDefault,
                       legendHeightDefault
                       ,legendHeightDefault
                       ,legendHeightDefault
                       ,2.5

                       ,legendHeightDefault
                       ,legendHeightDefault
  )

  fixCenBorderDefault <- FALSE
  fixCenBorderVec <- c(TRUE,
                       fixCenBorderDefault,
                       TRUE,
                       fixCenBorderDefault,

                       fixCenBorderDefault
                       ,fixCenBorderDefault
                       ,fixCenBorderDefault
                       ,fixCenBorderDefault

                       ,fixCenBorderDefault
                       ,fixCenBorderDefault
  )

  chrBorderColorDefault <- as.character(NA)
  chrBorderColorVec <- c(chrBorderColorDefault,
                         chrBorderColorDefault,
                         chrBorderColorDefault,
                         chrBorderColorDefault,

                         chrBorderColorDefault
                         ,chrBorderColorDefault
                         ,chrBorderColorDefault
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

                  ,lwd.chrDefault
                  ,0.1
  )

  xlimLeftModDefault <- 1
  xlimLeftModVec <- c(2,
                      2,
                      2,
                      xlimLeftModDefault,

                      xlimLeftModDefault
                      ,xlimLeftModDefault
                      ,xlimLeftModDefault
                      ,0

                      ,2
                      ,5
  )

  xlimRightModDefault <- 2
  xlimRightModVec <- c(xlimRightModDefault,
                       xlimRightModDefault,
                       xlimRightModDefault,
                       xlimRightModDefault,

                       3
                       ,4
                       ,0
                       ,5

                       ,xlimRightModDefault
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

                     ,0
                     ,5
  )

  ylimTopModDefault <- 0.2
  ylimTopModVec <- c(0,
                     0,
                     -0.4,
                     ylimTopModDefault,

                     ylimTopModDefault
                     ,ylimTopModDefault
                     ,ylimTopModDefault
                     ,ylimTopModDefault

                     ,0
                     ,5
  )

  mycolorsDefault <- ""
  mycolorsVec<- c(mycolorsDefault
                  ,mycolorsDefault
                  ,mycolorsDefault
                  ,mycolorsDefault

                  ,"red,dodgerblue,fdsjkfds,chartreuse3,darkgoldenrod1"
                  ,mycolorsDefault
                  ,mycolorsDefault
                  ,mycolorsDefault

                  ,mycolorsDefault
                  ,mycolorsDefault
)

  mycolors2Default <- ""
  mycolors2Vec<- c(mycolors2Default
                  ,mycolors2Default
                  ,mycolors2Default
                  ,mycolors2Default

                  ,mycolors2Default
                  ,mycolors2Default
                  ,mycolors2Default
                  ,mycolors2Default

                  ,"black,forestgreen,cornflowerblue"
                  ,"black,forestgreen,cornflowerblue"
  )

  chrNamesToSwapDefault <- ""
  chrNamesToSwapVec<- c(chrNamesToSwapDefault
                        ,chrNamesToSwapDefault
                        ,chrNamesToSwapDefault
                        ,chrNamesToSwapDefault

                        ,chrNamesToSwapDefault
                        ,chrNamesToSwapDefault
                        ,chrNamesToSwapDefault
                        ,"3,6,7,9,12" #8

                        ,chrNamesToSwapDefault
                        ,chrNamesToSwapDefault
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

                  ,OTUfontDefault
                  ,OTUfontDefault
  )

  moveKarHorDefault<- ""
  moveKarHorVec<- c(moveKarHorDefault
                    ,moveKarHorDefault
                    ,moveKarHorDefault
                    ,moveKarHorDefault
                    ,moveKarHorDefault
                    ,"Allopolyploid"
                    ,moveKarHorDefault
                    ,moveKarHorDefault

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

                   ,notesPosXDefault
                   ,notesPosXDefault
  )

}

paramValues <- mget(ls(pattern = "Default$" ) )
