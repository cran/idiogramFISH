#output panels


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
                             ,"plasmid preset (go to Nucleotides)"=9
                             ,"bac. chrom. preset (go to Nucleotides)"=10
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
              h4("Columns")
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
              ,br()
              ,div(style="margin-top:10px"
                   ,actionButton("removeFactorChr","Factors (dropdown) to character")
              )

            ) # end wellpanel
            # br()
    ) # c

    #
    ,column(8
            ,br()

            ,fluidRow(
              column(width=6
                     ,wellPanel(
                       h4("swap arms")
                       ,textInput('chrNamesToSwap', 'chr. names', "1,2")
                       ,actionButton("swapButton","Swap!",icon = icon("rotate") )
                     ))
            )
            ,fluidRow(
              column(width=6
                     ,wellPanel(
                       h2("Chr. data data.frame")
                       ,helpText(paste(values[["df1Name"]], values[["df1Origin"]] ) )
                     )
              ) # col
            ) #fR
            ,fluidRow(
              column(11
                     ,uiOutput("outUI")
              )
            )
    )
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
              h4("Columns")
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
              ,br()
              ,div(style="margin-top:10px"
                   ,actionButton("removeFactorMark","Factors (dropdown) to character")
              )
            ) # end wellpanel
    ) # c
    ,column(8
            ,br()
            ,fluidRow(
              column(width=6

                     ,wellPanel(
                       h2("Mark pos. data.frame")
                       ,helpText(paste(values[["df1MarkName"]], values[["df1MarkOrigin"]] ) )
                     )

                     ,) # col
            ) #fR
            ,fluidRow(
              column(11
                     ,uiOutput("outMarkUI")
              )
            )
    )
  ) # end fluidrow
})

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
              h4("Columns")
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
            ,uiOutput("outMStyleUI")

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
        title="`chrLabelSpacing`: numeric, for `circularPlot=TRUE`. Spacing of chr. names. Defaults to `0.5`",
        numericInput("chrLabelSpacing"
                     ,"chr. name dist."
                     ,chrLabelSpacingDefault
                     , min = 0.25, max = 10, step=0.25
        )
      )

      ,div(
        title= '`labelSpacing`: numeric, for `circularPlot=TRUE`. Spacing of mark labels. Defaults to `0.7`'
        ,numericInput("labelSpacing"
                      ,"label spacing"
                      ,labelSpacingDefault
                      , min = 0.25, max = 10, step=0.05
        )
      )
    )
    ,splitLayout(
      div(
        title="`rotation`: numeric, anti-clockwise rotation, defaults to `0.5` which rotates first chr. from top to -90 degrees. (-0.5*π = 9 o'clock)"
        ,numericInput("rotation"
                      ,"Rotation"
                      ,rotationDefault
                      , min = 0, max = 2*pi, step=0.05
        )
      )
      ,div(style= "margin-top: 0px; margin-bottom:0px"
           ,title='`labelOutwards`: boolean, inline labels projected outwards'
           ,checkboxInput("labelOutwards",
                          HTML(paste("outwards", "projected","label marks", sep = "<br/>") )
                          ,labelOutwardsDefault
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
,
div(title="`circleCenter`:	numeric, for `circularPlot=TRUE`. Affects coordinates of center of circles. Affects `legend='aside'` position."
    ,numericInput("circleCenter"
                  ,"coord. center X-axis"
                  ,circleCenterDefault
                  , min = -5, max = 15, step=0.5
    )
)
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



output$imageWidth<- renderUI({
  px<-session$clientData$output_idiogramPlot_width*(as.numeric(input$widFactor)/100)
  HTML(paste(paste(px,"px"),paste(px/80,"in"), sep ="<br/>" ) )
})

output$imageHeight<- renderUI({
  px <-session$clientData$output_idiogramPlot_width*(as.numeric(input$heiFactor)*(as.numeric(input$widFactor)/100)
  )
  HTML(paste(paste(px,"px"),paste(px/80,"in"), sep ="<br/>" ) )
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
               ,checkboxInput("asFile","to file (keeps size)",TRUE)
               ,checkboxInput("keepDefault","keep default values",FALSE)
               ,radioButtons("saveType","download format",c("csv","rds"),"csv",inline = T)
             )
             ,tags$style(type='text/css', '#code {background-color: #FFFFFF; color: black;}')
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



output$searchPanel = renderUI({
  fluidRow(
    column(width=3
           ,br()
           ,wellPanel(
             checkboxInput("showWorkflow","show workflow",value=FALSE)
           ) #wP
           ,uiOutput("workflowUI")
           ,uiOutput("presetUI")
    )
    ,column(width=3
            ,br()
            ,wellPanel(
              checkboxInput("fileInsteadNcbi","Upload file instead of search",value=FALSE)
            ) #wP
            ,uiOutput("searchUI")
            ,uiOutput("chooseFileUI")
            ,uiOutput("titleSelectUI")
            ,uiOutput("authorsUI")

    ) #c
    ,column(3
            ,br()
            ,uiOutput("fetchSelectUI")
            ,uiOutput("statsDF")
    )
    ,column(3
            ,br()
            ,uiOutput("markColumnUI")
            ,uiOutput("loadUI")
    )
  ) #fR
})


output$parameterPanel = renderUI({
  fluidRow(
    column(width=2
           ,br()
           # sq notes
           ,wellPanel(style = "padding: 0px 15px 0px 5px"
                      #style = "padding: 10px 5px 0px 5px;margin-bottom: 20px" # trbl
                      ,fluidRow(
                        column(6
                               ,h4("Legend")
                               ,div(
                                 title='`legend`: (`"aside"`) If you wanto to plot the names of marks near each chromosome use `legend = "inline"`, to the right of karyotypes use `legend = "aside"`, otherwise use `legend = ""` for no legend. See `markLabelSpacer`'
                                 ,radioButtons("legend","Pos.",c("aside","inline","none"),selected = legendDefault )
                               )
                               ,div(style = "margin-top:-12px;margin-bottom:-7px"
                                    ,title="`markLabelSize`: (`1`) Determines the size of text of the legend."
                                    ,numericInput("markLabelSize"
                                                  , "font size"
                                                  , markLabelSizeDefault
                                                  , min = 0.1, max = 5, step=0.05)
                               )
                        )
                        ,column(6
                                ,div(style = "margin-top:5px"
                                     ,title='`legendWidth`: (`1.7`) numeric, factor to modify the width of the square and dots of legend. For `legend="aside"`.'
                                     ,numericInput("legendWidth", "width", legendWidthDefault, min = 0.25, max = 5, step=0.05)
                                )
                                ,div(style = "margin-top:-12px"
                                     ,title='`legendHeight`: (`NA`) numeric, factor to modify the height of the square and dots of legend. For `legend="aside"`.'
                                     ,numericInput("legendHeight", "height"
                                                   , legendHeightDefault
                                                   , min = 0.25, max = 5, step=0.05)
                                )
                                ,div(style = "margin-top:-12px;margin-bottom:-7px"
                                     ,title= '`pattern`: (`""`) REGEX pattern to eliminate from the marks name when plotting. See human karyotype chapter for example.'
                                     ,textInput("pattern"
                                                , "remove Regex"
                                                , patternDefault
                                     )
                                )

                        )
                      ) # fR
           ) #wP


           ,wellPanel(style = "margin-top:-15px;padding: 0px 15px 0px 5px"
                      ,h4("Marks")
                      ,fluidRow(
                        column(6
                               ,div(style= "margin-top:0px;margin-bottom:-5px"
                                    ,title="`protruding` numeric, when style of mark is `cM`, fraction of chrWidth to stretch marker. Defaults to `0.2`"
                                    ,numericInput("protruding"
                                                  ,HTML(paste("protruding of"
                                                              ,"cM and arrows", sep = "<br/>") )
                                                  ,protrudingDefault,min=0.05,max=2.5,step=0.05)
                               ) # div
                               ,div(#style= "margin-top:-7px;margin-bottom:-5px"
                                 title='`useOneDot`: boolean, use one dot instead of two in style of marks dots. Defaults to `FALSE`'
                                 ,checkboxInput("useOneDot",
                                                "one dot only",
                                                useOneDotDefault
                                 )
                               ) # div

                        ),column(6
                                 ,div(style= "margin-top:0px;margin-bottom:-5px"
                                      ,title="`pMarkFac` numeric, fraction of chr. size for `exProtein` style marks. Defaults to `0.25`"
                                      ,numericInput("pMarkFac"
                                                    ,HTML(paste("exProtein"
                                                                ,"mark size", sep = "<br/>") )
                                                    ,pMarkFacDefault,min=0.05,max=1,step=0.05)
                                 )
                                 ,div(#style= "margin-bottom:15px"
                                   title= '`cMBeginCenter`: boolean, start position of `cM` and `cMLeft` marks. If `TRUE`, starts in the center (width) of chr. . Defaults to `FALSE`'
                                   ,checkboxInput("cMBeginCenter"
                                                  ,HTML(paste("cM mark"
                                                              ,"start centered", sep = "<br/>") )
                                                  ,cMBeginCenterDefault)
                                 )

                        ) # c
                      ) # fR
           ) #wP

           ,wellPanel(style="margin-top:-15px;"
                      ,fluidRow(
                        column(6
                               ,h4("OTU")
                               ,div(style="margin-top:-10px;"
                                    ,title='`addOTUName`: (`TRUE`) If `TRUE` adds name of species (OTU) under karyotype
'
                                    , checkboxInput("addOTUName"
                                                    ,HTML(paste("Show name"
                                                                , sep = "<br/>") )
                                                    ,addOTUNameDefault)
                               )
                               ,div(title='`OTUTextSize`: (`1`) font size of OTU names, except when `OTUasNote=TRUE` see `notesTextSize`'
                                    , numericInput("OTUTextSize"
                                                   ,HTML(paste("text size"
                                                               , sep = "<br/>") )
                                                   , OTUTextSizeDefault, min = 0.1, max = 5, step=0.1)
                               )
                        ) #c
                        ,column(6
                                ,div(title='`OTUfont`: numeric, `1` for normal, `2` for bold, `3` for italics, `4` for bold-italics'
                                     ,radioButtons("OTUfont","font type",
                                                   c("normal"="1","bold"="2","italics"="3","bold italics"="4"),
                                                   selected=OTUfontDefault)
                                )
                        ) #c
                      ) #fR
           ) #wP
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



    ) # col
    ,column(width=10
            ,br()
            ,fluidRow(
              # div(
              # style = "margin-bottom:-20px"

              # ,fluidRow(
              #
              # col0 test
              #
              uiOutput("colColor")
              #
              #   col 1
              #
              # KARYOTYPES

              #
              #  col2
              #
              ,column(2
                      ,wellPanel(style="padding: 0px 5px 0px 5px"
                                 ,h4("Chromosomes")
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
                                   )
                                   ,column(6
                                           ,div(#style="margin-top: -7px;"
                                                title='`chrId`: (`"original"`) If you want to rename chromosomes from 1 to n use `chrId = "simple"`. For original names use `chrId = "original"`. For no names use  `chrId = ""`'
                                                ,radioButtons("chrId"
                                                              ,HTML(paste("Chr. names") )
                                                              ,choices=c("original"="original","simple"="simple","none"="none")
                                                              ,chrIdDefault
                                                )
                                           )
                                   )
                                 ) # fR
                                 ,splitLayout(
                                   div(#style="margin-top: -15px;"
                                     title='`chrWidth`: (`0.5`) Determines the width of chromosomes
'
                                     ,numericInput("chrWidth",HTML(paste("","Width",sep="</br>") )
                                                   , chrWidthDefault, min = 0.1, max = 5, step=0.05)
                                   )
                                   ,div(#style = "margin-bottom:-10px"
                                     title='`chrSpacing`: (`0.5`) Determines the horizontal spacing among chromosomes
'
                                     ,numericInput("chrSpacing", HTML(paste("horiz.","spacing", sep="</br>") )
                                                   , chrSpacingDefault, min = 0.1, max = 5, step=0.1)
                                   )
                                   , div(#style = "margin-bottom:-10px"
                                     title='`lwd.chr`: (`0.5`) width of border lines for chr. and marks when related param. absent.'
                                     ,numericInput("lwd.chr"
                                                   ,HTML(paste("border","width", sep="</br>") )
                                                   ,lwd.chrDefault,min=0,max=4,step=0.25)
                                   )
                                 ) #sL
                      ) # wP

              ) # c3
              ,column(2

                      ,wellPanel(style = "padding: 0px 5px 0px 5px"
                                 ,splitLayout(cellWidths = c("40%","5%","55%")
                                              ,h4("Karyotypes")
                                              ,br()
                                              ,div(title='`karSepar`: (`TRUE`) If `TRUE` reduces the space among karyotypes. `FALSE` = equally sized karyotypes or `TRUE` = equally spaced karyotypes. Incompatible with `addMissingOTUAfter`'
                                                   ,checkboxInput("karSepar"
                                                                  ,HTML(paste("equally spaced","kar.", sep = "<br/>") )
                                                                  ,karSeparDefault)
                                              )
                                 )
                                 ,splitLayout(style="margin-bottom:-10px;"
                                              ,cellWidths = c("29%","2%","34%","2%","29%")
                                              ,div(#style="margin-top:-10px;"
                                                title='`karHeight`: (`2`) Vertical size of karyotypes considering only chromosomes. for ex `karHeight = 1`'
                                                ,numericInput("karHeight"
                                                              , HTML(paste("","Height", sep="</br>") )
                                                              ,karHeightDefault, min = 0.5, max = 50, step=0.5)
                                              )
                                              ,br()
                                              ,div(#style="margin-top:-12px;margin-bottom:-10px"
                                                title='`karHeiSpace`: (`2.5`) Vertical size of karyotypes including spacer. for ex `karHeiSpace = 1.2`. Use with `karSepar=FALSE`
'
                                                ,numericInput("karHeiSpace"
                                                              , HTML(paste("Height with","space", sep = "<br/>") )
                                                              , karHeiSpaceDefault, min = 2, max = 150, step=0.5)
                                              )
                                              ,br()
                                              ,div(#style= "margin-bottom:-10px"
                                                title='`amoSepar`: (`9`) For `karSepar = TRUE`, if zero, no space among karyotypes. Amount of separation.  if overlap, increase this and `karHeiSpace`'
                                                ,numericInput("amoSepar"
                                                              ,HTML(paste("Vert.","separ.", sep = "<br/>") )
                                                              , amoSeparDefault, min = 0, max = 15, step=0.5)
                                              )
                                 )
                      ) #Wp


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
              #
              # col 3
              #
              ,column(2
                      ,wellPanel(style = "padding: 0px 5px 0px 5px"
                                 ,div(style="margin-bottom:-10px;"
                                 ,h4("Ruler")
                                 )
                                 ,fluidRow(

                                   column(6

                                          ,div(
                                            title='`rulerPos`: (`-0.5`) Absolute position of ruler, corresponds to "pos" argument of the function `axis` of R plots'
                                            ,numericInput("rulerPos", "modify pos.", rulerPosDefault, min = -5, max = 5, step=0.1) ,
                                          )
                                          ,div(style = "margin-top:-15px;margin-bottom:-10px",
                                               title='`ruler.tck`: (`-0.02`) tick size of ruler, corresponds to "tck" argument of `axis` function
'
                                               ,numericInput("ruler.tck", "modify ticks", ruler.tckDefault, min = -5, max = 5, step=0.005)
                                          )
                                          ,div(style = "margin-top:-15px;margin-bottom:-10px",
                                               title="`xPosRulerTitle`: (`2.6`) Modifies the horizontal position of the title of rulers (Mb, etc). Moves to left from 1st chr. in `chrSpacing` times"
                                               ,numericInput("xPosRulerTitle"
                                                             , "pos. of title", xPosRulerTitleDefault, min = -5, max = 5, step=0.1)
                                          )
                                   ) #c
                                   ,
                                   column(6
                                          ,div(style = "margin-top:10px;margin-bottom:10px",
                                          title='`ruler`: (`TRUE`) When `TRUE` displays ruler to the left of karyotype, when `FALSE` shows no ruler
'
                                               ,checkboxInput("ruler",
                                                              "Show ruler",rulerDefault)
                                          )
                                          ,h4(tags$strong("font size") )
                                          ,div(style = "margin-top:-15px;margin-bottom:-10px",
                                               title="`rulerNumberSize`: (`1`) Size of number's font in ruler
"
                                               ,numericInput("rulerNumberSize"
                                                             , "Numbers", rulerNumberSizeDefault, min = .1, max = 5, step=0.1) ,
                                          )
                                          ,div(style = "margin-top:-15px;margin-bottom:-10px",
                                               title="`rulerTitleSize`: numeric font size of units of ruler."
                                               ,numericInput("rulerTitleSize"
                                                             , "Title", rulerTitleSizeDefault, min = .1, max = 5, step=0.1) ,
                                          )
                                   ) # c6
                                 ) # fR
                      ) #wp

                      ,wellPanel(style="margin-top:-15px;padding: 0px 15px 0px 5px"
                                 ,h4("Margins"),
                                 # fluidRow(
                                   # column(6,
                                 splitLayout(style="margin-bottom:-10px;"
                                               ,cellWidths = c("25%","25%","25%","25%")
                                          ,div(#style = "margin-right:0px;margin-bottom:-10px;padding: 0px 0px 0px 5px",
                                              title='`ylimBotMod`:	(`0.2`) modify `ylim` bottom component of plot adding more space
'
                                              ,numericInput("ylimBotMod", "bottom",
                                                            ylimBotModDefault, min = -5, max = 5, step=0.5)
                                          )
                                          ,div(#style = "margin-right:0px;margin-bottom:-10px;padding: 0px 0px 0px 5px",
                                               title='`ylimTopMod`: (`0.2`) modify `ylim` top component of plot adding more space.
'
                                               ,numericInput("ylimTopMod", "top"
                                                             , ylimTopModDefault, min = -5, max = 5, step=0.2)
                                          )
                                            ,div(#style = "margin-bottom:-10px;",
                                                title='`xlimLeftMod`: (`1`) modifies `xlim` left (first) component of the plot as in any "R-plot"
'
                                                ,numericInput("xlimLeftMod", "left",
                                                              xlimLeftModDefault, min = -5, max = 5, step=0.5)
                                            )
                                            ,div(#style = "margin-bottom:-10px;",
                                                 title='`xlimRightMod`:	(`2`) `xlim` (right) modification by adding space to the right of idiograms
'
                                                 ,numericInput("xlimRightMod", "right",
                                                               xlimRightModDefault, min = -5, max = 5, step=0.5)
                                            )
                                   ) # sL
                                 # ) # fR
                      ) #wp
              ) # c3

              #
              # col 4
              #
              ,column(2
                      ,wellPanel(style="padding: 0px 5px 0px 5px"
                                 ,
                                 splitLayout(style="margin-bottom:-10px;"
                                             ,cellWidths = c("20%","39%","41%")
                                          ,div(#style="margin-left:0px"
                                          h4("Plot")
                                          )
                                          ,div(#style="margin-bottom:-10px"
                                               numericInput("widFactor", "width %",
                                                             80, min = 5, max = 1000, step=5)
                                          )
                                          ,div(#style="margin-bottom:-10px"
                                            numericInput("heiFactor", "height ratio",
                                                         0.5, min = 0.05, max = 20, step=0.05)
                                          )
                                 )
                                 ,splitLayout(cellWidths = c("20%","39%","41%")
                                              ,br()
                                          ,uiOutput("imageWidth")
                                          ,uiOutput("imageHeight")
                                  )
                                 ,h5(strong("Format") )
                                 ,splitLayout(
                                   radioButtons("pngorsvg","Display",c("svg","png"),selected = "svg",inline = T )
                                   ,radioButtons("pngorsvgDown","Download",c("svg","png"),inline = T )
                                 )
                                 ,div(style = "margin-top:-7px;margin-bottom:7px"
                                      ,downloadButton('pngorsvgDownButton', 'Download plot')
                                 )
                      ) # wP
                      ,wellPanel(style="margin-top:-15px;padding: 5px 5px 5px 5px"
                        # ,br()
                        ,div(style = "margin-bottom:-10px"
                             ,splitLayout(cellWidths = c("30%","20%","30%","20%")
                                          ,h5(tags$strong("Zoom (X)") )
                                          ,actionButton("left", label = "<")
                                          # ,br()
                                          ,sliderInput("hwModifier","",0,10,1,step = 0.1, ticks = FALSE)
                                          # ,br()
                                          ,actionButton("right", label = ">")
                             )
                        )
                      )

              ) #c3
              #
              #   col 5
              #
              ,column(2
                      #legend marks
                      ,wellPanel(style="padding: 0px 20px 0px 5px;"
                                 ,splitLayout(
                                   span(h4("Squareness"),helpText("> 20 = squared") )
                                   ,div(#style="margin-top: -5px; margin-bottom:-5px;"
                                     title='`squareness`: (`4`) Squared or rounded vertices when marks of the "square" style (defined in data.frame passed to `dfMarkColor`). Affects chromosomes also. Smaller numbers = more rounded
'
                                     ,numericInput("squareness"
                                                   ,""
                                                   # ,HTML(paste("Vertices", "squareness", sep = "<br/>") )
                                                   , squarenessDefault
                                                   , min = 1, max = 21, step=0.5)
                                   )
                                 )
                                 ,splitLayout(style="margin-bottom:-10px;"
                                              ,cellWidths = c("50%","25%","25%")
                                   ,helpText(tags$strong("resolution:"),HTML("</br>"),"> = more vertices")
                                   ,div(#style="margin-top: -5px;margin-bottom:-5px;"
                                     title="`markN`: numeric vertices number for round corners of marks"
                                     ,numericInput("markN",
                                                   HTML(paste("marks'", sep = "<br/>") )
                                                   ,markNDefault
                                                   , min = 1, max = 200, step=1)
                                   )
                                   ,div(#style="margin-top: -5px;margin-bottom:-5px;"
                                     title="`n`: numeric vertices number for round corners of chr."
                                     ,numericInput("n",
                                                   HTML(paste("chr.", sep = "<br/>") )
                                                   ,nDefault
                                                   , min = 25, max = 400, step=1)
                                   )
                                 ) # sL

                      ) # wP

                      ,wellPanel(style="margin-top:-15px;padding: 0px 20px 0px 5px;" #style="padding: 0px 5px 0px 5px"
,splitLayout(cellWidths = c("28%","72%")
                                          ,h4("Notes")
                                          ,div(#style= "margin-top: -5px; margin-bottom:0px"
                                                 title="`OTUasNote`: (`FALSE`) See also `notes`. If `TRUE` OTU name is written to the right, as `notes`."
                                                 ,checkboxInput("OTUasNote",
                                                                HTML(paste("OTU as note (right)", sep = "<br/>") )
                                                                ,OTUasNoteDefault)
                                          )
                                   )
                                 ,fluidRow(
                                   column(6,
                                   div(style= "margin-bottom:-10px"
                                     ,title="`notesTextSize`: (`0.4`) numeric, font size of notes, see `notes`"
                                     ,numericInput("notesTextSize"
                                                   ,HTML(paste("font", "size", sep = "<br/>") )
                                                   ,notesTextSizeDefault, min = 0.1, max = 10, step=0.1)
                                   )
                                   ),column(6
                                   ,div(style= "margin-bottom:-10px"
                                     ,title="`notesPosX`: (`0.5`) numeric, moves right notes in the x axis"
                                     ,numericInput("notesPosX"
                                                   ,HTML(paste("right notes", "horiz. pos.", sep = "<br/>") )
                                                   ,notesPosXDefault, min = -20, max = 20, step=0.1
                                     )
                                   )
                                 )
                                 ) # fr
                      ) # wP
,wellPanel(style="margin-top:-15px;padding: 5px 5px 5px 5px"

           # ,div(#style = "margin-bottom:20px"
           ,splitLayout(cellWidths = c("25%","50%","25%")
                        ,actionButton("fontDecrease", label = "<")
                        ,h4("Fonts' size")
                        ,actionButton("fontIncrease", label = ">")
           )
)


              )

              # ) #fr
              # )
              # )
            ) # fR
            ,fluidRow(
              column(10,
                     wellPanel(
                       style = "margin-top:-15px;overflow-y:auto;text-align:center;" # margin-top:-12px;

                       # ,actionButton("goButton", "Go!", class = "btn-success")

                       ,div(style = 'overflow-y:auto;overflow-x:auto;min-width:1030px;'
                            ,imageOutput("idiogramPlot"
                                         ,height = "auto"
                            )
                       ) #d
                     ) # wP
              )
              ,uiOutput("lastCol")
            ) # fR
    ) # c
  )
})


output$lastCol<- renderUI({
  column(width=2
         # ,br()
         ,wellPanel(style = "margin-top:-15px;padding: 0px 15px 0px 5px"
           # ,wellPanel(style="margin-top:-15px;"
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
           ,div(title='`karIndex`: (`TRUE`) boolean. Adds karyotype indices A (intra - cen) and A2 (inter - size) [@Watanabe1999; @Zarco1986new]. Disable with `karIndex = FALSE`'
                ,checkboxInput("karIndex",tags$strong("Add kar. ind. A/A2")
                               ,karIndexDefault
                )
           )
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
                     ,div(style="margin-top:-10px; margin-bottom:-15px"
                          ,title="`chrSizeMbp`: boolean, when `TRUE` adds total Mbp chr. size to each chr. provided, there is a `Mbp` column in `dfChrSize` data.frame. Defaults to `FALSE`. If data in columns `shortArmSize`, or col. `chrSize` is in millions ('Mbp'). Use `chrSize=TRUE` not this one (not column `Mbp`, you don't need this).
"
                          ,checkboxInput("chrSizeMbp",
                                         HTML(paste("Show Chr. Size"
                                                    , "in Mbp"
                                                    ,tags$p("(column required)", style = "font-size: 80%;")
                                                    , sep = "<br/>") )
                                         ,chrSizeMbpDefault)
                     )
             ) #C
           )
         ) #WP

         ,wellPanel(style = "margin-top:-15px;padding: 0px 20px 0px 5px"
           ,splitLayout(
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
                 ,numericInput("moveAnchorV", "move ver.", moveAnchorVDefault, min = -10, max = 10, step=0.25)
             )
             ,div(title='`moveAnchorH`: numeric, displace anchor horizontal portion to right or left. See `anchor`'
                  ,numericInput("moveAnchorH", "move hor.", moveAnchorHDefault, min = -10, max = 10, step=0.25)
             )
           )
         ) #wp

  )
})

output$chrColorUI<- renderUI({
  fluidRow(
    column(6,
           div(style = "margin-top:-10px;margin-bottom:-10px"
               ,title='`chrColor`: (`"gray"`) Determines the color of chromosomes
'
               ,textInput('chrColor', 'chromosome', chrColorDefault)
           )
           ,div(title='`fixCenBorder`: boolean, when `TRUE` uses `chrColor` as centromere (and cen. mark) border color. See also `cenColor`, `chrColor`, `colorBorderMark`, `borderOfWhiteMarks`. No default value.
'
                , checkboxInput("fixCenBorder"
                                ,HTML(paste("chr. color as", "border color","of cen.", sep = "<br/>") )
                                ,fixCenBorderDefault)
           )
    ),column(6
             ,div(style = "margin-top:-10px;margin-bottom:-10px"
                  ,title='`cenColor`: Determines the color of centromeres. if GISH use `NULL`. Defaults to `chrColor`
'
                  ,textInput('cenColor', 'centromere', cenColorDefault)
             )
             ,div(title='`chrBorderColor`: character, color for border of chromosomes, defaults to `chrColor`'
                  ,textInput("chrBorderColor","border",chrBorderColorDefault)
             )
    ) # col
  )
})

output$marksUIcolor <- renderUI({

  div(#style= "margin-bottom:-5px"
      title="`mycolors`: optional, character vector with colors' names, which are associated automatically with marks according to their order in the data.frame of position of marks. See this ordering with `unique(dfMarkPos$markName)`. Argument example: `mycolors = c(\"red\",\"chartreuse3\",\"dodgerblue\")`. Not mandatory for plotting marks, package has default colors.
          "
      ,textInput('mycolors'
                 ,HTML(paste("Marks"
                             ,tags$p("(optional, see data.frame page)", style = "font-size: 80%; font-weight: normal;")
                             ,tags$p("here, only comma separated", style = "font-size: 80%;")
                 ) )
                 , mycolorsDefault)
  )
})

output$colColor<- renderUI({
  column(width=2
         # ,div(class="colorwell"
         ,wellPanel(style = "background: #F7DCDA; padding: 0px 5px 0px 5px" # margin-top:-15px;
                    #style= "min-width: 150px;max-width: 200px;overflow:auto"
                    ,h4("Colors")
                    ,uiOutput("chrColorUI")
                    ,uiOutput("marksUIcolor")
         ) # wP
         # )
  ) # col2
})
