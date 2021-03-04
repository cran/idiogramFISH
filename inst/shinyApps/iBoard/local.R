tablist<-c("example", "dfChrTab", "dfMarkTab", "dfMSTab"
            )

tablist2<-c("paramTab", "log","funString"
)

menulist<-c("DFsMenu","parameterPlotMenu"
            ,"abouttab")
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

  xModifierDefault <- 12
  xModifierVec <- c(xModifierDefault,
                    xModifierDefault,
                    xModifierDefault,
                    xModifierDefault,

                    xModifierDefault
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

  cenFormatDefault <- "rounded"
  cenFormatVec <- c(cenFormatDefault,
                      cenFormatDefault,
                    cenFormatDefault,
                    cenFormatDefault,

                      cenFormatDefault
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

  pMarkFacDefault <- 0.25
  pMarkFacVec <- c(pMarkFacDefault,
                    pMarkFacDefault,
                    pMarkFacDefault,
                    pMarkFacDefault,

                    pMarkFacDefault
                    ,pMarkFacDefault
                    ,pMarkFacDefault
                    ,pMarkFacDefault
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
                          ,-6
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

  chrSpacingDefault <- 0.5
  chrSpacingVec <- c(1,
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

  chrSizeDefault <- FALSE

  chrSizeVec <- c( TRUE,
                   TRUE,
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

  chrSizeMbpDefault <- FALSE
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

  distTextChrDefault <- 1
  distTextChrVec <- c( 1.2,
                       distTextChrDefault,
                       0.8,
                       0.5,

                       distTextChrDefault
                       ,distTextChrDefault
                       ,0.8
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

  rulerPosDefault <- 0
  rulerPosVec <- c( rulerPosDefault,
                    -0.1,
                    -0.4,
                    rulerPosDefault,
                    rulerPosDefault
                    ,rulerPosDefault
                    ,rulerPosDefault
                    ,rulerPosDefault
  )
  # names(rulerPosVec)[which(names(rulerPosVec)=="rulerPosDefault" )]<- rulerPosDefault

  ruler.tckDefault <- -0.02
  ruler.tckVec <- c(-0.01,
                    -0.02,
                    -0.005,
                    -0.005,
                    ruler.tckDefault
                    ,ruler.tckDefault
                    ,ruler.tckDefault
                    ,ruler.tckDefault
  )
  # names(ruler.tckVec)[which(names(ruler.tckVec)=="ruler.tckDefault" )] <- ruler.tckDefault

  rulerNumberSizeDefault <- 1
  rulerNumberSizeVec <- c(0.8,
                          rulerNumberSizeDefault,
                          0.4,
                          0.9,
                          rulerNumberSizeDefault
                          ,rulerNumberSizeDefault
                          ,rulerNumberSizeDefault
                          ,rulerNumberSizeDefault
  )
  # names(rulerNumberSizeVec)[which(names(rulerNumberSizeVec)=="rulerNumberSizeDefault" )] <- rulerNumberSizeDefault

  xPosRulerTitleDefault <- 1
  xPosRulerTitleVec <- c(3,
                         3,
                         3.5,
                         2.8,
                         xPosRulerTitleDefault
                         ,xPosRulerTitleDefault
                         ,xPosRulerTitleDefault
                         ,xPosRulerTitleDefault

  )
  # names(xPosRulerTitleVec)[which(names(xPosRulerTitleVec)=="xPosRulerTitleDefault" )] <- xPosRulerTitleDefault

  legendWidthDefault <- 1.7
  legendWidthVec <- c(1,
                      1.2,
                      legendWidthDefault,
                      1,
                      legendWidthDefault
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
  )

  # names(legendWidthVec)[which(names(legendWidthVec)=="legendWidthDefault" )] <- legendWidthDefault

  fixCenBorderDefault <- FALSE
  fixCenBorderVec <- c(TRUE,
                       fixCenBorderDefault,
                       TRUE,
                       fixCenBorderDefault,
                       fixCenBorderDefault
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

  xlimLeftModDefault <- 1
  xlimLeftModVec <- c(2,
                      2,
                      2,
                      xlimLeftModDefault,

                      xlimLeftModDefault
                      ,xlimLeftModDefault
                      ,xlimLeftModDefault
                      ,0
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

  ylimTopModDefault <- 0.2
  ylimTopModVec <- c(0,
                     0,
                     -0.4,
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

paramValues <- mget(ls(pattern = "Default$" ) )
