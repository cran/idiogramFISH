#
#   param presets
#

observeEvent(
  {
    input$exampleButton
  },
  {
    updateRadioButtons(session,"nucPreset", selected = as.numeric(input$exampleId) )
    updateCheckboxInput(session,"fileInsteadNcbi", value = FALSE )

    values[["number"]] <- input$exampleId
    values[["dfList"]] <- list()
    values[["button3ab"]] <- NA
    values[["entrez_summary1"]]<-NA
    values[["entrez_titles"]]<-NA
    values[["entrez_selected"]]<-NA
    # values[["names_fetch_list"]] <- NA
    values[["fetch_listAll"]] <- NA
    values[["authors"]] <- ""

    values[["entrez_search1"]]<-NA
    values[["fetch_list"]]<-NA
    values[["titles_number"]]<-NA


  }
)

observeEvent(
  {
    input$nucPresetButton
  },
  {
    updateRadioButtons(session,"exampleId", selected = as.numeric(input$nucPreset ) )
    updateCheckboxInput(session,"fileInsteadNcbi", value = FALSE )

    values[["number"]] <- input$nucPreset
    values[["dfList"]] <- list()

    values[["button3ab"]] <- NA
    values[["entrez_summary1"]]<-NA
    values[["entrez_titles"]]<-NA
    values[["entrez_selected"]]<-NA
    # values[["names_fetch_list"]] <- NA
    values[["fetch_listAll"]] <- NA
    values[["authors"]] <- ""

    values[["entrez_search1"]]<-NA
    values[["fetch_list"]]<-NA
    values[["titles_number"]]<-NA
  }
)

observeEvent(values[["number"]]
             # input$nucPresetButton  |    input$exampleButton
             ,
             {
               if(values[["number"]] > 0) {
                 Sys.sleep(0.6)

                 updateNumericInput(session, "karHeight",   value = karHeightVec[as.numeric(values[["number"]])]  )
                 updateNumericInput(session, "karHeiSpace", value = karHeiSpaceVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "amoSepar",    value = amoSeparVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "karSepar",    value = karSeparVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "chrWidth",    value = chrWidthVec[as.numeric(values[["number"]])] )
                 updateRadioButtons(session, "chrId",       selected = chrIdVec[as.numeric(values[["number"]])] )

                 updateNumericInput(session, "squareness",  value = squarenessVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "markN",  value = markNVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "n",  value = nVec[as.numeric(values[["number"]])] )

                 updateRadioButtons(session, "orderChr",  selected = ((orderChrVec[as.numeric(values[["number"]])] ) ) )

                 updateCheckboxInput(session, "useOneDot",  value = useOneDotVec[as.numeric(values[["number"]])] )
                 updateCheckboxInput(session, "cMBeginCenter",  value = cMBeginCenterVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "pMarkFac",  value = pMarkFacVec[as.numeric(values[["number"]])] )

                 updateNumericInput(session, "protruding",  value = protrudingVec[as.numeric(values[["number"]])] )

                 #circularPlot
                 updateNumericInput(session, "chrLabelSpacing",  value = chrLabelSpacingVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "labelSpacing",  value = labelSpacingVec[as.numeric(values[["number"]])] )

                 updateNumericInput(session, "rotation",  value = rotationVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "shrinkFactor",  value = shrinkFactorVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "radius",  value = radiusVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "circleCenter",  value = circleCenterVec[as.numeric(values[["number"]])] )

                 updateNumericInput(session, "OTUsrt",  value = OTUsrtVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "OTUjustif",  value = OTUjustifVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "OTULabelSpacerx",  value = OTULabelSpacerxVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "OTUlegendHeight",  value = OTUlegendHeightVec[as.numeric(values[["number"]])] )
                 updateRadioButtons(session, "OTUplacing",  selected = ((OTUplacingVec[as.numeric(values[["number"]])] ) ) )

                 # general
                 updateNumericInput(session, "chrSpacing",  value = as.numeric((chrSpacingVec[as.numeric(values[["number"]])] ) ) )
                 updateRadioButtons(session, "morpho",  selected = ((morphoVec[as.numeric(values[["number"]])])))
                 updateTextInput(session, "chrColor",  value = chrColorVec[as.numeric(values[["number"]])] )
                 updateTextInput(session, "cenColor",  value = cenColorVec[as.numeric(values[["number"]])] )

                 updateRadioButtons(session, "chrIndex",  selected = ((chrIndexVec[as.numeric(values[["number"]])] ) ) )
                 updateCheckboxInput(session, "karIndex", value = karIndexVec[as.numeric(values[["number"]])] )
                 updateCheckboxInput(session, "chrSize",  value = as.logical(chrSizeVec[as.numeric(values[["number"]])] ) )
                 updateNumericInput(session, "indexIdTextSize",  value = as.numeric((indexIdTextSizeVec[as.numeric(values[["number"]])] ) ) )

                 updateCheckboxInput(session, "chrSizeMbp",  value = as.logical((chrSizeMbpVec[as.numeric(values[["number"]])] ) ) )
                 updateNumericInput(session, "distTextChr",  value = as.numeric((distTextChrVec[as.numeric(values[["number"]])] ) ) )
                 updateNumericInput(session, "OTUTextSize",  value = as.numeric((OTUTextSizeVec[as.numeric(values[["number"]])] ) ) )

                 updateCheckboxInput(session, "chromatids",  value = chromatidsVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "xModifier",  value = as.numeric(xModifierVec[as.numeric(values[["number"]])] ) )

                 updateCheckboxInput(session, "circularPlot",  value = circularPlotVec[as.numeric(values[["number"]])] )

                 updateCheckboxInput(session, "ruler",  value = rulerVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "rulerPos",  value = as.numeric((rulerPosVec[as.numeric(values[["number"]])] ) ) )
                 updateNumericInput(session, "ruler.tck",  value = as.numeric((ruler.tckVec[as.numeric(values[["number"]])] ) ) )
                 updateNumericInput(session, "rulerNumberSize",  value = as.numeric((rulerNumberSizeVec[as.numeric(values[["number"]])] ) ) )
                 updateNumericInput(session, "xPosRulerTitle",  value = as.numeric((xPosRulerTitleVec[as.numeric(values[["number"]])] ) ) )

                 updateRadioButtons(session, "legend",  selected = legendVec[as.numeric(values[["number"]])] )
                 updateRadioButtons(session, "cenFormat",  selected = cenFormatVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "cenFactor",  value = cenFactorVec[as.numeric(values[["number"]])] )

                 updateNumericInput(session, "legendWidth",  value = as.numeric((legendWidthVec[as.numeric(values[["number"]])] ) ) )
                 updateNumericInput(session, "legendHeight", value = as.numeric((legendHeightVec[as.numeric(values[["number"]])] ) ) )
                 updateTextInput(session, "pattern", value = patternVec[as.numeric(values[["number"]])] )

                 updateNumericInput(session, "markLabelSize", value = as.numeric((markLabelSizeVec[as.numeric(values[["number"]])] ) ) )

                 updateCheckboxInput(session, "fixCenBorder",  value = as.logical((fixCenBorderVec[as.numeric(values[["number"]])] ) ) )
                 updateCheckboxInput(session, "chrBorderColor",  value = ((chrBorderColorVec[as.numeric(values[["number"]])] ) ) )
                 updateNumericInput(session, "lwd.chr",  value = as.numeric((lwd.chrVec[as.numeric(values[["number"]])] ) ) )

                 updateNumericInput(session, "xlimLeftMod",  value = as.numeric((xlimLeftModVec[as.numeric(values[["number"]])] ) ) )
                 updateNumericInput(session, "xlimRightMod",  value = xlimRightModVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "ylimBotMod",  value = ylimBotModVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "ylimTopMod",  value = as.numeric((ylimTopModVec[as.numeric(values[["number"]])] ) ) )

                 updateSliderInput(session, "hwModifier",  value = as.numeric(hwModifierVec[as.numeric(values[["number"]])]  ) )

                 updateNumericInput(session, "widFactor",  value = as.numeric((widFactorVec[as.numeric(values[["number"]])] ) ) )
                 updateNumericInput(session, "heiFactor",  value = as.numeric((heiFactorVec[as.numeric(values[["number"]])] ) ) )

                 updateRadioButtons(session, "pngorsvg",  selected = pngorsvgVec[as.numeric(values[["number"]])]  )
                 updateRadioButtons(session, "pngorsvgDown",  selected = pngorsvgDownVec[as.numeric(values[["number"]])]  )

                 updateTextInput(session, "mycolors", value = mycolorsVec[as.numeric(values[["number"]])])

                 updateTextInput(session, "chrNamesToSwap", value = chrNamesToSwapVec[as.numeric(values[["number"]])])

                 updateCheckboxInput(session, "addOTUName",  value = addOTUNameVec[as.numeric(values[["number"]])] )
                 updateRadioButtons(session, "OTUfont",  selected = as.character(OTUfontVec[as.numeric(values[["number"]])] ) )

                 updateTextInput(session, "moveKarHor", value = moveKarHorVec[as.numeric(values[["number"]])])
                 updateNumericInput(session, "mkhValue",  value = mkhValueVec[as.numeric(values[["number"]])]  )
                 updateCheckboxInput(session, "anchor",  value = anchorVec[as.numeric(values[["number"]])] )
                 updateNumericInput(session, "moveAnchorV",  value = moveAnchorVVec[as.numeric(values[["number"]])]  )
                 updateNumericInput(session, "moveAnchorH",  value = moveAnchorHVec[as.numeric(values[["number"]])]  )

                 updateNumericInput(session, "notesTextSize",  value = notesTextSizeVec[as.numeric(values[["number"]])]  )
                 updateNumericInput(session, "notesPosX",  value = notesPosXVec[as.numeric(values[["number"]])]  )

                 updateCheckboxInput(session, "OTUasNote",  value = OTUasNoteVec[as.numeric(values[["number"]])] )
                 updateCheckboxInput(session, "labelOutwards",  value = labelOutwardsVec[as.numeric(values[["number"]])] )

                 #
                 #   nuc marks
                 #

                 updateCheckboxGroupInput(session, "markType",    selected = markTypeList[[ as.numeric(values[["number"]]) ]] )
                 updateCheckboxGroupInput(session, "fetchSelect", selected = fetchSelectVec[as.numeric(values[["number"]])] )
                 updateNumericInput(      session, "amountofSpaces",value  = amountofSpacesVec[as.numeric(values[["number"]])] )
                 updateNumericInput(      session, "colNumber",     value  = colNumberVec[as.numeric(values[["number"]])] )
                 updateNumericInput(      session, "protrudingInt", value  = protrudingIntVec[as.numeric(values[["number"]])] )
                 updateTextInput(         session, "mycolors2",     value  = mycolors2Vec[as.numeric(values[["number"]])])
                 updateTextInput(         session, "term",          value  = termVec[as.numeric(values[["number"]])] )

                 # if(as.numeric(values[["number"]] ) >8){
                 # }
               }
             }, ignoreInit = F )
