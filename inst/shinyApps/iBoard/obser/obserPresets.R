#
#   param presets
#

observeEvent(
  {
    input$exampleButton
  },
  {

    updateCheckboxInput(session,"fileInsteadNcbi", value = FALSE )

    values[["number"]] <- exampleId()$value #input$exampleId
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
    # updateRadioButtons(session,"exampleId", selected = as.numeric(input$nucPreset ) )
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

observeEvent(list(#values[["number"]],
  input$nucPresetButton,input$exampleButton)
  ,
  {
    if(values[["number"]] > 0) {
      Sys.sleep(0.6)

      updateNumericInput(session, "karHeight",   value = paramVec$karHeightVec[as.numeric(values[["number"]])]  )
      updateNumericInput(session, "karHeiSpace", value = paramVec$karHeiSpaceVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "amoSepar",    value = paramVec$amoSeparVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "karSepar",    value = paramVec$karSeparVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "chrWidth",    value = paramVec$chrWidthVec[as.numeric(values[["number"]])] )
      updateRadioButtons(session, "chrId",       selected = paramVec$chrIdVec[as.numeric(values[["number"]])] )

      updateNumericInput(session, "squareness",  value = paramVec$squarenessVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "markN",  value = paramVec$markNVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "n",  value = paramVec$nVec[as.numeric(values[["number"]])] )

      updateRadioButtons(session, "orderChr",  selected = paramVec$orderChrVec[as.numeric(values[["number"]])] )
      updateRadioButtons(session, "markDistType",  selected = paramVec$markDistTypeVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "useOneDot",  value = paramVec$useOneDotVec[as.numeric(values[["number"]])] )
      updateTextInput(session, "bannedMarkName", value = paramVec$bannedMarkNameVec[as.numeric(values[["number"]])] )
      updateTextInput(session, "specialOTUNames", value = paramVec$specialOTUNamesVec[as.numeric(values[["number"]])] )
      updateTextInput(session, "addMissingOTUAfter", value = paramVec$addMissingOTUAfterVec[as.numeric(values[["number"]])] )
      updateTextInput(session, "missOTUspacings", value = paramVec$missOTUspacingsVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "origin", value = paramVec$originVec[as.numeric(values[["number"]])] )
      updateTextInput(session, "OTUfamily", value = paramVec$OTUfamilyVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "xModMonoHoloRate",  value = paramVec$xModMonoHoloRateVec[as.numeric(values[["number"]])] )

      updateTextInput(session, "specialyTitle", value = paramVec$specialyTitleVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "specialChrWidth",  value = paramVec$specialChrWidthVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "specialChrSpacing",  value = paramVec$specialChrSpacingVec[as.numeric(values[["number"]])] )

      updateTextInput(session, "yTitle", value = paramVec$yTitleVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "nameChrIndexPos",  value = paramVec$nameChrIndexPosVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "cMBeginCenter",  value = paramVec$cMBeginCenterVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "pMarkFac",  value = paramVec$pMarkFacVec[as.numeric(values[["number"]])] )

      updateNumericInput(session, "protruding",  value = paramVec$protrudingVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "arrowhead",  value = paramVec$arrowheadVec[as.numeric(values[["number"]])] )

      #circularPlot
      updateNumericInput(session, "chrLabelSpacing",  value = paramVec$chrLabelSpacingVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "labelSpacing",  value = paramVec$labelSpacingVec[as.numeric(values[["number"]])] )

      updateNumericInput(session, "rotation",  value = paramVec$rotationVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "shrinkFactor",  value = paramVec$shrinkFactorVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "separFactor",  value = paramVec$separFactorVec[as.numeric(values[["number"]])] )

      updateNumericInput(session, "radius",  value = paramVec$radiusVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "circleCenter",  value = paramVec$circleCenterVec[as.numeric(values[["number"]])] )

      updateNumericInput(session, "OTUsrt",  value = paramVec$OTUsrtVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "OTUjustif",  value = paramVec$OTUjustifVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "OTULabelSpacerx",  value = paramVec$OTULabelSpacerxVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "OTUlegendHeight",  value = paramVec$OTUlegendHeightVec[as.numeric(values[["number"]])] )
      updateRadioButtons(session, "OTUplacing",  selected = paramVec$OTUplacingVec[as.numeric(values[["number"]])] )

      # general
      updateNumericInput(session, "chrSpacing",  value = as.numeric((paramVec$chrSpacingVec[as.numeric(values[["number"]])] ) ) )
      updateRadioButtons(session, "morpho",  selected = paramVec$morphoVec[as.numeric(values[["number"]])])
      updateTextInput(session, "chrColor",  value = paramVec$chrColorVec[as.numeric(values[["number"]])] )
      updateTextInput(session, "cenColor",  value = paramVec$cenColorVec[as.numeric(values[["number"]])] )

      updateRadioButtons(session, "chrIndex",  selected = paramVec$chrIndexVec[as.numeric(values[["number"]])] )
      updateCheckboxInput(session, "karIndex", value = paramVec$karIndexVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "karIndexPos", value = paramVec$karIndexPosVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "chrSize",  value = as.logical(paramVec$chrSizeVec[as.numeric(values[["number"]])] ) )
      updateCheckboxInput(session, "showMarkPos",  value = as.logical(paramVec$showMarkPosVec[as.numeric(values[["number"]])] ) )

      updateNumericInput(session, "indexIdTextSize",  value = as.numeric((paramVec$indexIdTextSizeVec[as.numeric(values[["number"]])] ) ) )

      updateCheckboxInput(session, "chrSizeMbp",  value = as.logical((paramVec$chrSizeMbpVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "distTextChr",  value = as.numeric((paramVec$distTextChrVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "OTUTextSize",  value = as.numeric((paramVec$OTUTextSizeVec[as.numeric(values[["number"]])] ) ) )

      updateCheckboxInput(session, "chromatids",  value = paramVec$chromatidsVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "holocenNotAsChromatids",  value = paramVec$holocenNotAsChromatidsVec[as.numeric(values[["number"]])] )

      updateNumericInput(session, "xModifier",  value = as.numeric(paramVec$xModifierVec[as.numeric(values[["number"]])] ) )

      updateCheckboxInput(session, "circularPlot",  value = paramVec$circularPlotVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "ruler",    value = paramVec$rulerVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "rulerPos",  value = as.numeric((paramVec$rulerPosVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "threshold", value = as.numeric((paramVec$thresholdVec[as.numeric(values[["number"]])] ) ) )

      updateNumericInput(session, "ceilingFactor", value = as.numeric((paramVec$ceilingFactorVec[as.numeric(values[["number"]])] ) ) )

      updateNumericInput(session, "rulerInterval",  value = as.numeric((paramVec$rulerIntervalVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "rulerIntervalMb",  value = as.numeric((paramVec$rulerIntervalMbVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "rulerIntervalcM",  value = as.numeric((paramVec$rulerIntervalcMVec[as.numeric(values[["number"]])] ) ) )

      updateNumericInput(session, "ruler.tck",  value = as.numeric((paramVec$ruler.tckVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "rulerNumberSize",  value = as.numeric((paramVec$rulerNumberSizeVec[as.numeric(values[["number"]])] ) ) )

      updateNumericInput(session, "rulerNumberPos",  value = as.numeric((paramVec$rulerNumberPosVec[as.numeric(values[["number"]])] ) ) )

      updateNumericInput(session, "rulerTitleSize",   value = paramVec$rulerTitleSizeVec[as.numeric(values[["number"]])] )

      updateNumericInput(session, "xPosRulerTitle",  value = as.numeric((paramVec$xPosRulerTitleVec[as.numeric(values[["number"]])] ) ) )

      updateRadioButtons(session, "legend",     selected = paramVec$legendVec[as.numeric(values[["number"]])] )
      updateRadioButtons(session, "cenFormat",  selected = paramVec$cenFormatVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "cenFactor",     value = paramVec$cenFactorVec[as.numeric(values[["number"]])] )

      updateNumericInput(session, "centromereSize",  value = paramVec$centromereSizeVec[as.numeric(values[["number"]])] )
      updateCheckboxInput(session, "autoCenSize",    value = as.logical((paramVec$autoCenSizeVec[as.numeric(values[["number"]])] ) ) )

      updateNumericInput(session, "legendWidth",     value = as.numeric((paramVec$legendWidthVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "legendHeight",    value = as.numeric((paramVec$legendHeightVec[as.numeric(values[["number"]])] ) ) )
      updateTextInput(session, "pattern",            value = paramVec$patternVec[as.numeric(values[["number"]])] )
      updateCheckboxInput(session, "remSimiMarkLeg", value = as.logical((paramVec$remSimiMarkLegVec[as.numeric(values[["number"]])] ) ) )

      updateNumericInput(session, "markLabelSize",   value = as.numeric((paramVec$markLabelSizeVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "markLabelSpacer", value = as.numeric((paramVec$markLabelSpacerVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "legendYcoord",    value = as.numeric((paramVec$legendYcoordVec[as.numeric(values[["number"]])] ) ) )

      updateCheckboxInput(session, "fixCenBorder",  value = as.logical((paramVec$fixCenBorderVec[as.numeric(values[["number"]])] ) ) )
      updateCheckboxInput(session, "chrBorderColor",value = paramVec$chrBorderColorVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "lwd.chr",        value = as.numeric((paramVec$lwd.chrVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "lwd.cM",         value = as.numeric((paramVec$lwd.cMVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "lwd.mimicCen",   value = as.numeric((paramVec$lwd.mimicCenVec[as.numeric(values[["number"]])] ) ) )

      updateNumericInput(session, "xlimLeftMod",  value = as.numeric((paramVec$xlimLeftModVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "xlimRightMod", value = paramVec$xlimRightModVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "ylimBotMod",   value = paramVec$ylimBotModVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "ylimTopMod",   value = as.numeric((paramVec$ylimTopModVec[as.numeric(values[["number"]])] ) ) )

      updateSliderInput(session, "hwModifier",  value = as.numeric(paramVec$hwModifierVec[as.numeric(values[["number"]])]  ) )

      updateNumericInput(session, "widFactor",  value = as.numeric((paramVec$widFactorVec[as.numeric(values[["number"]])] ) ) )
      updateNumericInput(session, "heiFactor",  value = as.numeric((paramVec$heiFactorVec[as.numeric(values[["number"]])] ) ) )

      updateRadioButtons(session, "pngorsvg",  selected = paramVec$pngorsvgVec[as.numeric(values[["number"]])]  )
      updateRadioButtons(session, "pngorsvgDown",  selected = paramVec$pngorsvgDownVec[as.numeric(values[["number"]])]  )

      updateTextInput(session, "mycolors",   value = paramVec$mycolorsVec[as.numeric(values[["number"]])])
      updateTextInput(session, "markPer",    value = paramVec$markPerVec[as.numeric(values[["number"]])])
      updateTextInput(session, "bToRemove",  value = paramVec$bToRemoveVec[as.numeric(values[["number"]])])

      updateCheckboxInput(session, "perAsFraction",  value = paramVec$perAsFractionVec[as.numeric(values[["number"]])] )

      updateTextInput(session, "chrNamesToSwap", value = paramVec$chrNamesToSwapVec[as.numeric(values[["number"]])])

      updateCheckboxInput(session, "addOTUName",  value = paramVec$addOTUNameVec[as.numeric(values[["number"]])] )
      updateRadioButtons(session, "OTUfont",   selected = as.character(paramVec$OTUfontVec[as.numeric(values[["number"]])] ) )

      updateTextInput(session, "moveKarHor",      value = paramVec$moveKarHorVec[as.numeric(values[["number"]])])
      updateNumericInput(session, "mkhValue",     value = paramVec$mkhValueVec[as.numeric(values[["number"]])]  )
      updateCheckboxInput(session, "anchor",      value = paramVec$anchorVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "moveAnchorV",  value = paramVec$moveAnchorVVec[as.numeric(values[["number"]])]  )
      updateNumericInput(session, "moveAnchorH",  value = paramVec$moveAnchorHVec[as.numeric(values[["number"]])]  )

      updateNumericInput(session, "notesTextSize",  value = paramVec$notesTextSizeVec[as.numeric(values[["number"]])]  )
      updateNumericInput(session, "notesPosX",      value = paramVec$notesPosXVec[as.numeric(values[["number"]])]  )

      updateNumericInput(session, "leftNoteFontUp", value = paramVec$leftNoteFontUpVec[as.numeric(values[["number"]])]  )
      updateNumericInput(session, "leftNotesPosX",  value = paramVec$leftNotesPosXVec[as.numeric(values[["number"]])]  )
      updateNumericInput(session, "leftNotesPosY",  value = paramVec$leftNotesPosYVec[as.numeric(values[["number"]])]  )
      updateNumericInput(session, "leftNotesUpPosY",value = paramVec$leftNotesUpPosYVec[as.numeric(values[["number"]])]  )

      updateNumericInput(session, "leftNotesUpPosX",      value = paramVec$leftNotesUpPosXVec[as.numeric(values[["number"]])]  )
      updateNumericInput(session, "notesPosY",      value = paramVec$notesPosYVec[as.numeric(values[["number"]])]  )

      updateNumericInput(session, "leftNoteFont", value = paramVec$leftNoteFontVec[as.numeric(values[["number"]])]  )
      updateNumericInput(session, "noteFont", value = paramVec$noteFontVec[as.numeric(values[["number"]])]  )

      updateNumericInput(session, "leftNotesUpTextSize",  value = paramVec$leftNotesUpTextSizeVec[as.numeric(values[["number"]])]  )
      updateNumericInput(session, "leftNotesTextSize",  value = paramVec$leftNotesTextSizeVec[as.numeric(values[["number"]])]  )

      updateCheckboxInput(session, "parseStr2lang",     value = paramVec$parseStr2langVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "moveAllKarValueHor" ,value = paramVec$moveAllKarValueHorVec[as.numeric(values[["number"]])]  )
      updateNumericInput(session, "moveAllKarValueY"   ,value = paramVec$moveAllKarValueYVec[as.numeric(values[["number"]])]  )

      updateCheckboxInput(session, "verticalPlot",     value = paramVec$verticalPlotVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "karSpaceHor",value = paramVec$karSpaceHorVec[as.numeric(values[["number"]])]  )
      updateTextInput(   session, "karAnchorLeft",   value = paramVec$karAnchorLeftVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "OTUasNote",     value = paramVec$OTUasNoteVec[as.numeric(values[["number"]])] )
      updateCheckboxInput(session, "labelOutwards", value = paramVec$labelOutwardsVec[as.numeric(values[["number"]])] )

      # updateCheckboxInput(session, "callPlot", value = paramVec$callPlotVec[as.numeric(values[["number"]])] )

      updateTextInput(   session, "colorBorderMark",   value = paramVec$colorBorderMarkVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "lwd.marks",         value = paramVec$lwd.marksVec[as.numeric(values[["number"]])]  )
      updateCheckboxInput(session, "gishCenBorder",    value = paramVec$gishCenBorderVec[as.numeric(values[["number"]])] )
      updateNumericInput(session, "hideCenLines",      value = paramVec$hideCenLinesVec[as.numeric(values[["number"]])]  )

      #
      #   nuc marks
      #
      # this one is a list #
      updateCheckboxGroupInput(session, "markType",    selected = paramVec$markTypeVec[[ as.numeric(values[["number"]]) ]] )

      updateCheckboxGroupInput(session, "fetchSelect", selected = paramVec$fetchSelectVec[as.numeric(values[["number"]])] )
      updateNumericInput(      session, "amountofSpaces",value  = paramVec$amountofSpacesVec[as.numeric(values[["number"]])] )
      updateNumericInput(      session, "colNumber",     value  = paramVec$colNumberVec[as.numeric(values[["number"]])] )
      updateNumericInput(      session, "protrudingInt", value  = paramVec$protrudingIntVec[as.numeric(values[["number"]])] )
      updateTextInput(         session, "mycolors2",     value  = paramVec$mycolors2Vec[as.numeric(values[["number"]])])
      updateTextInput(         session, "term",          value  = paramVec$termVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "useGeneNames", value = paramVec$useGeneNamesVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "useRCNames",   value = paramVec$useRCNamesVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "makeUnique",   value = paramVec$makeUniqueVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "colorFeature", value = paramVec$colorFeatureVec[as.numeric(values[["number"]])] )

      updateRadioButtons(session, "nucMarkStyle",  selected = paramVec$nucMarkStyleVec[as.numeric(values[["number"]])] )

      updateRadioButtons(session, "pseudo",        selected = paramVec$pseudoVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "mirror",       value = paramVec$mirrorVec[as.numeric(values[["number"]])] )

      updateCheckboxInput(session, "addSTARTPos",  value = paramVec$addSTARTPosVec[as.numeric(values[["number"]])] )

    }
  }, ignoreInit = F )
