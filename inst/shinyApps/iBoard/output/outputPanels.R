

updateRadioSubgroup <- function(session, inputId, id, inline, selected, ...) {
  value <- paste0(id, "-", selected)
  updateRadioButtons(session, inputId, label = NULL, choices = NULL, inline = inline, selected = value)
}

radioGroupContainer <- function(inputId, ...) {
  class <- "form-group shiny-input-radiogroup shiny-input-container"
  div(id = inputId, class = class, ...)
}

radioSubgroup <- function(inputId, id, label, choiceNames, choiceValues, inline = TRUE, first=FALSE) {
  choiceValues <- paste0(id, "-", choiceValues)
  choices <- setNames(choiceValues, choiceNames)
  if(first==FALSE){
    rb <- radioButtons(inputId, label, choices, selected = character(0), inline = inline)
  } else {
    rb <- radioButtons(inputId, label, choices, selected = choices[1], inline = inline)
  }
  rb$children
}

exampleId <- reactive({
  req(input$exampleId)
  parts <- unlist(strsplit(input$exampleId, "-"))
  list(id = parts[1], value = as.numeric(parts[2]) )
})


observeEvent(input$nucPreset,{

  sel <- input$nucPreset
  updateRadioSubgroup(session, "exampleId", "circular", selected = sel, inline = TRUE)
})

observeEvent(input$exampleId,{
sel <- input$exampleId
updateRadioButtons(session, "nucPreset"

                   , selected = gsub(".*-", "", sel), inline = TRUE
)
})



output$examplepanel <- renderUI({
  fluidRow(
    column(width=4,
           br()
           ,wellPanel(
             h4("Choose an example")
             ,helpText("numbered according to vignettes")
             ,helpText("this will overwrite any data you have entered in other tabs")
             ,radioGroupContainer("exampleId",
                                 fluidRow(
                                   column(12,
                                          radioSubgroup("exampleId"
                                                        , "minimal_examples"
                                                        , label = "3. Minimal Examples:"
                                                        , choiceNames = c("3.1 monocentric k.",
                                                                      "3.2 holocentric k.",
                                                                      "3.3 mono. and holo.",
                                                                      "3.4 circular m. & h. ")
                                                        , choiceValues = c(1:2,11:12)
                                                        , first = TRUE
                                                        )
                                   )
                                   ,column(12,
                                           radioSubgroup("exampleId", "plotting_chr"
                                                         , label = "4. Plotting chr.:"
                                                         , choiceNames = c("4.2 monocen."
                                                                           ,"4.3 holocen."
                                                                           ,"4.3b holocentric k. mycolors"
                                                                           ,"4.5 mono & holo in kar."
                                                                           )
                                                         , choiceValues = c(13,14,5,15)
                                           )
                                   )
                                   ,column(12,
                                          radioSubgroup("exampleId", "multiple_otus"
                                                        , label = "5. Multiple OTUs:"
                                                        , choiceNames = c("5.1 multiple mono. k.",
                                                        "5.2 multiple holo. k.")
                                                        , choiceValues = 3:4
                                                        )
                                          )
                                   ,column(12,
                                           radioSubgroup("exampleId", "changing_units"
                                                         , label = "6. Changing Units:"
                                                         , choiceNames = c("6.1 Using bases instead of micrometers"
                                                                           ,"6.2 Using threshold to fix scale"
                                                                           ,"6.3 Plot data in micrometers and bases"
                                                                           ,"6.4 Use cM as units"
                                                                           )
                                                         , choiceValues = 16:19
                                           )
                                   )

                                   ,column(12,
                                           radioSubgroup("exampleId", "gish"
                                                         , label = "7. GISH:"
                                                         , choiceNames = c("7.1 GISH"
                                                         ,"7.2 GISH holoc."
                                                         ,"7.3a GISH citrus"
                                                         ,"7.3b GISH citrus"
                                                         )
                                                         , choiceValues = c(6,7,20,21)
                                                         )
                                   )
                                   ,column(12,
                                           radioSubgroup("exampleId", "gish"
                                                         , label = "8. Groups:"
                                                         , choiceNames = c("8.1 monoc."
                                                                           ,"8.2 holoc."
                                                         )
                                                         , choiceValues = c(22,23)
                                           )
                                   )
                                   ,column(12

                                           ,radioSubgroup("exampleId", "circular"
                                                         , label = "9. Circular plots:"
                                                         , choiceNames = c("9.1 Plot 1",
                                                                           "9.2 Plot 2"
                                                                           ,"9.3 plasmid preset"
                                                                           ,"9.4 bac. chrom. preset"
                                                                           )
                                                         , choiceValues = c(24,8,9,10)



                                           )
                                           ,helpText("go to Nucleotides tab for more circular plots")
                                   )
                                   ,column(12

                                           ,radioSubgroup("exampleId", "phylog"
                                                          , label = "10. For phylog:"
                                                          , choiceNames = c("10.2 Mono"
                                                                            ,"10.3 Holo"
                                                                            ,"10.4 mono & holo"
                                                            )
                                                          , choiceValues = c(25,26,27)



                                           )
                                           ,helpText("See vignettes for the phylogeny")
                                   )

                                 )
             )

             ,actionButton("exampleButton",
                           tags$span(tags$strong("Load Example presets"))
                           ,class = "btn-info"
                           )
             ,helpText("Click button to start")
           )
    )
    ,column(width=1)
    ,column(width=4,
           br()
           ,wellPanel(
             h4("Jupyter notebooks")
             ,helpText("Based on vignettes. Work online in colab or locally after downloading")
             ,tags$table(
               tags$tr(
                 tags$th(img(src="colab-badge.svg"))
                 ,tags$th(img(src="GitHub-Mark-120px-plus.png", width=18),HTML("GitHub&emsp;&emsp;") )
                 ,tags$th("")
               )
               ,add_row("03-minimal.ipynb","3 Minimal examples")
               ,add_row("04-plotting.ipynb","4 Plotting chromosomes")
               ,add_row("05-multiple.ipynb","5 Multiple OTUs")
               ,add_row("06-units.ipynb","6 Changing units")
               ,add_row("07-gish.ipynb","7 GISH")
               ,add_row("08-groups.ipynb","8 Groups")
               ,add_row("09-circular.ipynb","9 Circular Plots")
               ,add_row("10-phylogeny.ipynb","10 Plotting alongside phylogeny")
               ,add_row("11-citrushelp.ipynb","11 Citrus")
               ,add_row("12-human.ipynb","12 Human Karyotype")
             )
           )
           ,wellPanel(
             h4("Help")
             ,helpText("Links to online vignettes and manual in the",tags$strong("About tab") )
           )
    )
  )
})

output$ARCImessageOut <- renderText({
  return(values[["ARCImessage"]])
})

output$ARCImessageUI <- renderUI({
  req(values[["ARCImessage"]]!="")
  tagList(
    br()
    ,verbatimTextOutput("ARCImessageOut")
  )
})

output$indicespanel <- renderUI({
  fluidRow(
    column(width=2
           ,br()
           ,wellPanel(
             h5("Use function armRatioCI on")
             ,
             h5("main data.frame (see data.frames page)")
             ,helpText("reads a data.frame and produces AR (arm ratio), CI (centromeric index)
                       , Guerra (1986) and Levan et al. (1964) classifications.")
             ,actionButton("ARCIbutton",
                          tags$span(tags$strong("armRatioCI") )
                          ,class = "btn-info"
                          )

           ,uiOutput("ARCImessageUI")
           ,uiOutput("downloadARCIUI")
           ,br()
           ,uiOutput("clipARCI")
           )
           ,br()
           ,wellPanel(
           h5("Use function asymmetry on")
           ,
           h5("main data.frame")
           ,helpText("calculates karyotype asymmetry A and A2.")
           ,actionButton("AA2button",
                         tags$span(tags$strong("asymmetry") )
                         ,class = "btn-info"
                         )
           ,uiOutput("downloadAA2UI")
           ,br()
           ,uiOutput("clipAA2")
           )
           )
    ,column(7
            ,br()
            ,uiOutput("outARCIUI")
            )
    ,column(2
            ,br()
            ,uiOutput("outAA2UI")
            )
    )
})

output$downloadARCIUI <- renderUI({
  req(nrow(values[["ARCI"]])>0)
  tagList(
            br()
             ,h5("Save AR and CI")
             ,textInput("ARCIfilename",
                        "File name",
                        value = "ARCIData"
                        ,width = "50%")
             ,uiOutput("ARCIbuttontable")
           )
})

output$downloadAA2UI <- renderUI({
  req(nrow(values[["AA2"]])>0)
  tagList(
    br()
    ,h5("Save A and A2")
    ,textInput("AA2filename",
               "File name",
               value = "AA2Data"
               ,width = "50%")
    ,uiOutput("AA2buttontable")
  )
})

output$markspanel <- renderUI({
  fluidRow(
    column(2
           ,br()
           ,wellPanel(
             h5("Use function posCalc on")
             ,
             h5("main data.frame (see data.frames page)")
             ,helpText("calculates position of marks in fraction of (%) chromosome units (0-1)")
             ,actionButton("posCalcbutton",
                           tags$span(tags$strong("posCalc") )
                           ,class = "btn-info"
             )
             # ,uiOutput("ARCImessageUI")
             ,uiOutput("downloadposCalcUI")
             ,br()
             ,uiOutput("clipposCalc")
           )
           ,br()
           ,wellPanel(
             h5("Use function perMark on")
             ,
             h5("main data.frame")
             ,helpText("calculates fraction (%) of chromosome for each name of mark")
             ,helpText("marks with location/chrRegion 'cen' don't have mark size")
             ,actionButton("perMarkbutton",
                           tags$span(tags$strong("perMark") )
                           ,class = "btn-info"
             )
             ,uiOutput("downloadperMarkUI")
             ,br()
             ,uiOutput("clipperMark")
           )
    )
    ,column(5
            ,br()
            ,uiOutput("outposCalcUI")
    )
    ,column(5
            ,br()
            ,uiOutput("outperMarkUI")
    )
  )
})

output$downloadposCalcUI <- renderUI({
  req(nrow(values[["posCalc"]])>0)
  tagList(
    br()
    ,h5("Save posCalc")
    ,textInput("posCalcfilename",
               "File name",
               value = "posCalcData"
               ,width = "50%")
    ,uiOutput("posCalcbuttontable")
  )
})

output$downloadperMarkUI <- renderUI({
  req(nrow(values[["perMark"]])>0)
  tagList(
    br()
    ,h5("Save perMark")
    ,textInput("perMarkfilename",
               "File name",
               value = "perMarkData"
               ,width = "50%")
    ,uiOutput("perMarkbuttontable")
  )
})


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
           )
           ,wellPanel(
             h4("Upload data")
             ,helpText("optional")
             ,fileInput("file1", "Choose CSV File",
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
             )
           )
    )
    ,column(width=2,
            br()
            ,wellPanel(
              h4("Add/Remove Row")
              ,helpText(""
                        , p("Right-click inside the table to delete/insert rows.")
              )
            )

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

            )

    )


    ,column(8
            ,br()

            ,fluidRow(
              column(width=6
                     ,wellPanel(
                       h4("swap arms")
                       ,textInput('chrNamesToSwap', 'chr. names', "1,2")
                       ,actionButton("swapButton","Swap!",icon = icon("sync") )
                     ))
            )
            ,fluidRow(
              column(width=6
                     ,wellPanel(
                       h2("Chr. data data.frame")
                       ,helpText(paste(values[["df1Name"]], values[["df1Origin"]] ) )
                     )
              )
            )
            ,fluidRow(
              column(11
                     ,uiOutput("outUI")
              )
            )
    )
  )
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


             ,uiOutput("buttontableMark")
           )
           ,wellPanel(
             h4("Upload data")
             ,helpText("optional")
             ,fileInput("file1Mark", "Choose CSV File",
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
             )
           )
    )
    ,column(width=2,
            br()
            ,wellPanel(
              h4("Add/Remove Row")
              ,helpText(""
                        , p("Right-click inside the table to delete/insert rows.")
              )
            )

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
            )
    )
    ,column(8
            ,br()
            ,fluidRow(
              column(width=6

                     ,wellPanel(
                       h2("Mark pos. data.frame")
                       ,helpText(paste(values[["df1MarkName"]], values[["df1MarkOrigin"]] ) )
                     )

                     ,)
            )
            ,fluidRow(
              column(11
                     ,uiOutput("outMarkUI")
              )
            )
    )
  )
})

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


             ,uiOutput("buttontableMStyle")
           )
           ,wellPanel(
             h4("Upload data")
             ,helpText("optional")
             ,fileInput("file1MStyle", "Choose CSV File",
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
             )
           )
    )
    ,column(width=2,
            br()
            ,wellPanel(
              h4("Add/Remove Row")
              ,helpText(""
                        , p("Right-click inside the table to delete/insert rows.")
              )
            )

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
            )

    )

    ,column(width=5,
            br()
            ,wellPanel(
              h2("Mark style data.frame")
              ,helpText(paste(values[["df1MStyleName"]], values[["df1MStyleOrigin"]] ) )
            )
            ,uiOutput("outMStyleUI")

    )
  )
})





output$dfnotespanel <- renderUI({
  tagList(
  fluidRow(
    column(width=2
           ,br()
           ,wellPanel(
             h4("Save data")
             ,helpText("optional")
             ,textInput("notesfilename",
                        "File name",
                        value = "notes"
                        ,width = "50%")


             ,uiOutput("buttontablenotes")
           )
           ,wellPanel(
             h4("Upload data")
             ,helpText("optional")
             ,fileInput("file1Notes", "Choose CSV File",
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
             )
           )
    )
    ,column(width=2,
            br()
            ,wellPanel(
              h4("Add/Remove Row")
              ,helpText(""
                        , p("Right-click inside the table to delete/insert rows.")
              )
            )
            ,wellPanel(
              h4("Columns")
              ,helpText("Use unique names")
              ,textInput("colNotes",
                         "Col. name",
                         value = "newCol"
                         ,width = "50%")
              ,radioButtons("colTypeNotes","Col. data type",c("character","numeric") )
              ,actionButton("addColumnNotes",
                            "Add")
              ,actionButton("removeColumnNotes",
                            "Remove")
              ,br()
            )
    )
    ,column(8
            ,br()
            ,fluidRow(
              column(width=6
                     ,wellPanel(
                       h2("notes data.frame")
                       ,helpText(paste(values[["notesName"]], values[["notesOrigin"]] ) )
                       ,helpText("text to be shown to the right of the karyotype")
                     )
                     )
            )
            ,fluidRow(
              column(11
                     ,uiOutput("outnotesUI")
              )
            )
    )
  )



  ,fluidRow(
    column(width=2
           ,br()
           ,wellPanel(
             h4("Save data")
             ,helpText("optional")
             ,textInput("leftNotesfilename",
                        "File name",
                        value = "leftNotes"
                        ,width = "50%")


             ,uiOutput("buttontableleftNotes")
           )
           ,wellPanel(
             h4("Upload data")
             ,helpText("optional")
             ,fileInput("file1leftNotes", "Choose CSV File",
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
             )
           )
    )
    ,column(width=2,
            br()
            ,wellPanel(
              h4("Add/Remove Row")
              ,helpText(""
                        , p("Right-click inside the table to delete/insert rows.")
              )
            )
            ,wellPanel(
              h4("Columns")
              ,helpText("Use unique names")
              ,textInput("colleftNotes",
                         "Col. name",
                         value = "newCol"
                         ,width = "50%")
              ,radioButtons("colTypeleftNotes","Col. data type",c("character","numeric") )
              ,actionButton("addColumnleftNotes",
                            "Add")
              ,actionButton("removeColumnleftNotes",
                            "Remove")
              ,br()
            )
    )
    ,column(8
            ,br()
            ,fluidRow(
              column(width=6
                     ,wellPanel(
                       h2("leftNotes data.frame")
                       ,helpText(paste(values[["leftNotesName"]], values[["leftNotesOrigin"]] ) )
                       ,helpText("text to be shown to the right of the karyotype")
                     )
              )
            )
            ,fluidRow(
              column(11
                     ,uiOutput("outleftNotesUI")
              )
            )
    )
  )


  ,fluidRow(
    column(width=2
           ,br()
           ,wellPanel(
             h4("Save data")
             ,helpText("optional")
             ,textInput("leftNotesUpfilename",
                        "File name",
                        value = "leftNotesUp"
                        ,width = "50%")


             ,uiOutput("buttontableleftNotesUp")
           )
           ,wellPanel(
             h4("Upload data")
             ,helpText("optional")
             ,fileInput("file1leftNotesUp", "Choose CSV File",
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
             )
           )
    )
    ,column(width=2,
            br()
            ,wellPanel(
              h4("Add/Remove Row")
              ,helpText(""
                        , p("Right-click inside the table to delete/insert rows.")
              )
            )
            ,wellPanel(
              h4("Columns")
              ,helpText("Use unique names")
              ,textInput("colleftNotesUp",
                         "Col. name",
                         value = "newCol"
                         ,width = "50%")
              ,radioButtons("colTypeleftNotesUp","Col. data type",c("character","numeric") )
              ,actionButton("addColumnleftNotesUp",
                            "Add")
              ,actionButton("removeColumnleftNotesUp",
                            "Remove")
              ,br()
            )
    )
    ,column(8
            ,br()
            ,fluidRow(
              column(width=6
                     ,wellPanel(
                       h2("leftNotesUp data.frame")
                       ,helpText(paste(values[["leftNotesUpName"]], values[["leftNotesUpOrigin"]] ) )
                       ,helpText("text to be shown to the right of the karyotype")
                     )
              )
            )
            ,fluidRow(
              column(11
                     ,uiOutput("outleftNotesUpUI")
              )
            )
    )
  )



  )
})

output$circParam = renderUI({
  wellPanel(
    h4("circular plot param."),
    splitLayout(
      div(
        title=chrLabelSpacingDesc
              ,numericInput("chrLabelSpacing"
                     ,"chr. name dist."
                     ,paramValues$chrLabelSpacingDefault
                     , min = 0.25, max = 10, step=0.25
        )
      )

      ,div(
        title= labelSpacingDesc
        ,numericInput("labelSpacing"
                      ,"label spacing"
                      ,paramValues$labelSpacingDefault
                      , min = 0.25, max = 10, step=0.05
        )
      )
    )
    ,splitLayout(
      div(
        title=rotationDesc
        ,numericInput("rotation"
                      ,"Rotation"
                      ,paramValues$rotationDefault
                      , min = 0, max = 2*pi, step=0.05
        )
      )
      ,div(style= "margin-top: 0px; margin-bottom:0px"
           ,title=labelOutwardsDesc
           ,checkboxInput("labelOutwards",
                          HTML(paste("outwards", "projected","label marks", sep = "<br/>") )
                          ,paramValues$labelOutwardsDefault
           )
      )
    )
    ,splitLayout(
      div(title=shrinkFactorDesc
          , numericInput("shrinkFactor"
                ,"shrink kar."
                ,paramValues$shrinkFactorDefault
                , min = 0.2, max = 1, step=0.05
             )
      )
      ,div(title=radiusDesc
     ,numericInput("radius"
                   ,"radius"
                   ,paramValues$radiusDefault
                   , min = 0.25, max = 30, step=0.05
     )
    )
  )
  ,splitLayout(
    div(title=separFactorDesc
    ,numericInput("separFactor"
                    ,"separ kar."
                    ,paramValues$separFactorDefault
                    , min = 0.5, max = 10, step=0.5
                   )
    )
    ,
    div(title=circleCenterDesc
        ,numericInput("circleCenter"
                      ,"pos. center X-axis"
                      ,paramValues$circleCenterDefault
                      , min = -5, max = 15, step=0.5
        )
    )
  )
)
})

output$circParamOTU = renderUI({
  wellPanel(
    h3("OTU name"),
    h5("(circ. plot)")
    ,fluidRow(
      column(6
             ,div(title=OTUplacingDesc
                  ,radioButtons("OTUplacing","position"
                                ,c("first","number","simple")
                                ,selected=paramValues$OTUplacingDefault
                  )
             )
      ),column(6,
               div(title=OTUsrtDesc
                   ,numericInput("OTUsrt"
                                 ,"Angle"
                                 ,paramValues$OTUsrtDefault
                                 , min = -360, max = 360, step=1
                   )
               )
               ,div(title=OTUjustifDesc
                    , numericInput("OTUjustif"
                                   ,"Justif."
                                   ,paramValues$OTUjustifDefault
                                   , min = 0, max = 1, step=0.5
                    )
               )
      )
    )
    ,splitLayout(
      div(title=OTULabelSpacerxDesc
          ,numericInput("OTULabelSpacerx"
                        ,HTML(paste("position X-axis") )
                        ,paramValues$OTULabelSpacerxDefault
                        , min = -10, max = 10, step=.5
          )
      )
      ,div(title=OTUlegendHeightDesc
           , numericInput("OTUlegendHeight"
                          ,HTML(paste("vert. size") )
                          ,paramValues$OTUlegendHeightDefault
                          , min = 0.5, max = 10, step=0.5
           )
      )
    )
  )
})

output$imageWidth<- renderUI({
  px<-session$clientData$output_idiogramPlot_width*(as.numeric(input$widFactor)/100)
  HTML(paste(paste(px,"px"),paste(px/80,"in"), sep ="<br/>" ) )
})

output$imageHeight<- renderUI({
  px <-session$clientData$output_idiogramPlot_width*(as.numeric(input$heiFactor)*(as.numeric(input$widFactor)/100)
  )
  HTML(paste(paste(px,"px"),paste(px/80,"in"), sep ="<br/>" ) )
})

output$strpanel = renderUI({
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

           )
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
           )
           ,uiOutput("saveMarkPosUI")
           ,uiOutput("saveMarkStyleUI")
           )
  ,column(2
           ,uiOutput("saveNotesUI")
           ,uiOutput("saveleftNotesUI")
           ,uiOutput("saveleftNotesUpUI")
    )
  )
})

output$saveMarkPosUI <- renderUI({
  if(!invalid( values[["df1Mark"]] ) ){
  wellPanel(
  h4("Save mark pos. data")
  ,helpText("")
  ,textInput("markfilename2",
             "File name",
             value = "markData"
             ,width = "50%")
  ,uiOutput("buttontableMark2")
)
  }
})

output$saveMarkStyleUI<- renderUI({
  if(!invalid( values[["df1MStyle"]]  ) ){
  wellPanel(
  h4("Save mark style data")
  ,helpText("")
  ,textInput("MStylefilename2",
             "File name",
             value = "MStyleData"
             ,width = "50%")
  ,uiOutput("buttontableMStyle2")
  )
  }
})

output$saveNotesUI<- renderUI({
  if(!invalid( values[["notes"]]  ) ){
wellPanel(
  h4("Save notes data.frame")
  ,helpText("")
  ,textInput("notesfilename2",
             "File name",
             value = "notes"
             ,width = "50%")
  ,uiOutput("buttontablenotes2")
)
  }
})

output$saveleftNotesUI<-renderUI({
  if(!invalid( values[["leftNotes"]]  ) ){
    wellPanel(
  h4("Save leftNotes data.frame")
  ,helpText("")
  ,textInput("leftNotesfilename2",
             "File name",
             value = "leftNotes"
             ,width = "50%")
  ,uiOutput("buttontableleftNotes2")
)
  }
  })

output$saveleftNotesUpUI<- renderUI({
  if(!invalid( values[["leftNotesUp"]]  ) ){
wellPanel(
  h4("Save leftNotesUp data.frame")
  ,helpText("")
  ,textInput("leftNotesUpfilename2",
             "File name",
             value = "leftNotesUp"
             ,width = "50%")
  ,uiOutput("buttontableleftNotesUp2")
)
  }
})


output$searchPanel = renderUI({
  fluidRow(
    column(width=3
           ,br()
           ,wellPanel(
             checkboxInput("showWorkflow","show workflow",value=FALSE)
           )
           ,uiOutput("workflowUI")
           ,uiOutput("presetUI")
    )
    ,column(width=3
            ,br()
            ,wellPanel(
              checkboxInput("fileInsteadNcbi","Upload file instead of search",value=FALSE)
            )
            ,uiOutput("searchUI")
            ,uiOutput("chooseFileUI")
            ,uiOutput("titleSelectUI")
            ,uiOutput("authorsUI")

    )
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
  )
})

output$parameterPanel = renderUI({
  fluidRow(
    column(width=2
           ,br()

           ,wellPanel(style = "padding: 0px 15px 0px 5px"

                      ,fluidRow(
                        column(4
                               ,h4("Legend")
                               ,div(
                                 title=legendDesc
                                 ,radioButtons("legend","Pos.",c("aside","inline","none")
                                               ,selected = paramValues$legendDefault )
                               )
                               ,div(style = "margin-top:-12px;margin-bottom:-7px"
                                    ,title= remSimiMarkLegDesc
                                    ,checkboxInput("remSimiMarkLeg"
                                                   ,HTML(paste("remove"
                                                               ,"duplicated", sep = "<br/>") )
                                                   , paramValues$remSimiMarkLegDefault
                                    )
                               )

                        )
                        ,column(4
                                ,div(
                                     title=legendWidthDesc
                                     ,numericInput("legendWidth", "width"
                                                   , paramValues$legendWidthDefault, min = 0.25, max = 5, step=0.05)
                                )
                                ,div(
                                     title= patternDesc
                                     ,textInput("pattern"
                                                , "remove Regex"
                                                , paramValues$patternDefault
                                     )
                                )
                        )
                        ,column(4
                                ,div(
                                     title=legendHeightDesc
                                     ,numericInput("legendHeight", "height"
                                                   , paramValues$legendHeightDefault
                                                   , min = 0.25, max = 5, step=0.05)
                                )

                                ,div(
                                  title=bannedMarkNameDesc
                                  ,textInput("bannedMarkName",
                                             HTML(paste("remove"
                                                        ,"this labels", sep = "<br/>")
                                             )
                                             ,paramValues$bannedMarkNameDefault
                                  )
                               )
                           )
                      )
                      ,splitLayout(
                        cellWidths = c("30%","2%","30%","2%","30%")
                        ,div(
                             title=markLabelSizeDesc
                             ,numericInput("markLabelSize"

                                           ,tagList(
                                             div(style="height:15px;",
                                                 tags$span("font")
                                             ),
                                             div(style="height:15px;",
                                                 tags$span("size")
                                             )
                                           )
                                           , paramValues$markLabelSizeDefault
                                           , min = 0.1, max = 5, step=0.05)
                        )
                        ,br()
                        ,div(
                             title=markLabelSpacerDesc
                             ,numericInput("markLabelSpacer"

                                           ,tagList(
                                             div(style="height:15px;",
                                                 tags$span("separ.")
                                             ),
                                             div(style="height:15px;",
                                                 tags$span("from kar.")
                                             )
                                           )
                                           , paramValues$markLabelSpacerDefault
                                           , min = 0.25, max = 15, step=0.25)
                        )
                        ,br()

                        ,div(
                             title=legendYcoordDesc
                             ,numericInput("legendYcoord"

                                           ,tagList(
                                             div(style="height:15px;",
                                                 tags$span("vertical")
                                             ),
                                             div(style="height:15px;",
                                                 tags$span("pos.")
                                             )
                                           )
                                           , paramValues$legendYcoordDefault
                                           , min = 0.0, max = 30, step=0.2)
                        )
                      )
           )
           ,wellPanel(style="margin-top:-15px;padding: 0px 20px 0px 5px;"
                      ,splitLayout(cellWidths = c("28%","72%")
                                   ,h4("Notes")
                                   ,div(
                                     title=OTUasNoteDesc
                                     ,checkboxInput("OTUasNote",
                                                    HTML(paste("OTU as note (right)", sep = "<br/>") )
                                                    ,paramValues$OTUasNoteDefault)
                                   )
                      )
                      ,div(style="margin-top:-10px;"
                           ,title=parseStr2langDesc
                           , checkboxInput("parseStr2lang"
                                           ,HTML(paste("use str2lang"
                                                       , sep = "<br/>") )
                                           ,paramValues$parseStr2langDefault)
                      )
                      ,splitLayout(
                        cellWidths = c("15%","21%","21%","21%","21%")
                      ,tags$span()
                      ,tags$span(tags$strong("font") )
                      ,tags$span()
                      ,tags$span(tags$strong("position") )
                      )

                      ,splitLayout(
                        cellWidths = c("15%","21%","21%","21%","21%")




                        ,tagList(tags$span(tags$strong("right") )
                                 ,br()
                                 ,tags$span(tags$strong("notes"))
                        )

                               ,div(
                                   title=notesTextSizeDesc
                                   ,numericInput("notesTextSize"
                                                 ,HTML(paste("size", sep = "<br/>") )
                                                 ,paramValues$notesTextSizeDefault, min = 0.1, max = 10, step=0.1)
                               )
                        ,div(
                          title=noteFontDesc
                          ,numericInput("noteFont",
                                        HTML(paste("style", sep = "<br/>")
                                        )
                                        ,paramValues$noteFontDefault
                                        ,min=1,max=4,step=1
                          )
                        )
                               ,div(
                                 title=notesPosXDesc
                                 ,numericInput("notesPosX"
                                               ,HTML(paste("X-axis", sep = "<br/>") )
                                               ,paramValues$notesPosXDefault, min = -20, max = 20, step=0.1
                                 )
                               )
                        ,div(
                          title=notesPosYDesc
                          ,numericInput("notesPosY"
                                        ,HTML(paste("Y-axis", sep = "<br/>") )
                                        ,paramValues$notesPosYDefault, min = -20, max = 20, step=0.1
                          )
                        )
                        )

                      ,splitLayout(
                        cellWidths = c("15%","21%","21%","21%","21%")
                        ,tagList(tags$span(tags$strong("left") )
                                 ,br()
                                 ,tags$span(tags$strong("notes"))
                        )
                        ,div(
                          title=leftNotesTextSizeDesc
                          ,numericInput("leftNotesTextSize"
                                        ,HTML(paste("size", sep = "<br/>") )
                                        ,paramValues$leftNotesTextSizeDefault, min = 0.1, max = 10, step=0.1)
                        )
                        ,div(
                          title=leftNoteFontDesc
                          ,numericInput("leftNoteFont",
                                        HTML(paste("style", sep = "<br/>")
                                        )
                                        ,paramValues$leftNoteFontDefault
                                        ,min=1,max=4,step=1
                          )
                        )
                        ,div(
                            title=leftNotesPosXDesc
                                     ,numericInput("leftNotesPosX",
                                                   HTML(paste("X-axis", sep = "<br/>")
                                                   )
                                                   ,paramValues$leftNotesPosXDefault
                                                   ,min=-5,max=5,step=0.1
                                     )
                                   )


                                   ,div(
                                     title=leftNotesPosYDesc
                                     ,numericInput("leftNotesPosY",
                                                   HTML(paste("Y-axis", sep = "<br/>")
                                                   )
                                                   ,paramValues$leftNotesPosYDefault
                                                   ,min=-5,max=5,step=0.1
                                     )
                                   )
                          )

                      ,splitLayout(
                        cellWidths = c("15%","21%","21%","21%","21%")
                        ,tagList(tags$span(tags$strong("up left"), style = "font-size: 80%;")
                                 ,br()
                                 ,tags$span(tags$strong("notes"))
                        )
                        ,div(
                          title=leftNotesUpTextSizeDesc
                          ,numericInput("leftNotesUpTextSize"
                                        ,HTML(paste("size", sep = "<br/>") )
                                        ,paramValues$leftNotesUpTextSizeDefault, min = 0.1, max = 10, step=0.1)
                        )
                        ,div(
                          title=leftNoteFontUpDesc
                          ,numericInput("leftNoteFontUp",
                                        HTML(paste("style", sep = "<br/>")
                                        )
                                        ,paramValues$leftNoteFontUpDefault
                                        ,min=1,max=4,step=1
                          )
                        )
                        ,div(
                             title=leftNotesUpPosXDesc
                             ,numericInput("leftNotesUpPosX",
                                           HTML(paste("X-axis", sep = "<br/>")
                                           )
                                           ,paramValues$leftNotesUpPosXDefault
                                           ,min=-5,max=5,step=0.1
                             )
                        )
                         ,div(
                               title=leftNotesUpPosYDesc
                                     ,numericInput("leftNotesUpPosY",
                                                   HTML(paste("Y-axis", sep = "<br/>")
                                                   )
                                                   ,paramValues$leftNotesUpPosYDefault
                                                   ,min=-5,max=5,step=0.1
                                     )
                                   )
                      )
           )
           ,wellPanel(style = "margin-top:-15px;padding: 0px 15px 0px 5px"
                      ,h4("Marks")
                      ,fluidRow(
                        column(6
                               ,div(style= "margin-top:0px;margin-bottom:-5px"
                                    ,title=protrudingDesc
                                    ,numericInput("protruding"
                                                  ,HTML(paste("protruding of"
                                                              ,"cM and arrows", sep = "<br/>") )
                                                  ,paramValues$protrudingDefault,min=0.05,max=2.5,step=0.05)
                               )
                               ,div(style= "margin-top:0px;margin-bottom:-5px"
                                    ,title=arrowheadDesc
                                    ,numericInput("arrowhead"
                                                  ,HTML(paste("proportion of"
                                                              ,"arrowhead", sep = "<br/>") )
                                                  ,paramValues$arrowheadDefault,min=0.05,max=1,step=0.05)
                               )
                               ,div(
                                 title=useOneDotDesc
                                 ,checkboxInput("useOneDot",
                                                "one dot only",
                                                paramValues$useOneDotDefault
                                 )
                               )
                               ,div(
                                 title= cMBeginCenterDesc
                                 ,checkboxInput("cMBeginCenter"
                                                ,HTML(paste("cM mark"
                                                            ,"start centered", sep = "<br/>") )
                                                ,paramValues$cMBeginCenterDefault)
                               )

                        )
                        ,column(6
                                 ,div(style= "margin-top:0px;margin-bottom:-5px"
                                      ,title=pMarkFacDesc
                                      ,numericInput("pMarkFac"
                                                    ,HTML(paste("exProtein"
                                                                ,"mark size", sep = "<br/>") )
                                                    ,paramValues$pMarkFacDefault,min=0.05,max=1,step=0.05)
                                 )
                                 ,div(
                                   title=markDistTypeDesc
                                   ,radioButtons("markDistType","coord."
                                                 ,c("beginning"="beg"
                                                    ,"center"="cen"
                                                 )
                                                 ,selected = paramValues$markDistTypeDefault
                                   )
                                 )
                                ,div(
                                  title=originDesc
                                  ,radioButtons("origin","coord.(hol)"
                                                ,c("bottom"="b"
                                                   ,"top"="t"
                                                )
                                                ,selected = paramValues$originDefault
                                  )
                                )

                                ,div(title=hideCenLinesDesc
                                     ,numericInput("hideCenLines"
                                                   ,"hide factor"
                                                   ,paramValues$hideCenLinesDefault
                                     )
                                )
                        )
                      )
                      ,div(style="text-align:center;"
                      ,tags$span(tags$strong("thickness") )
                      )
                        ,splitLayout(
                          div(
                            title=lwd.marksDesc
                            ,numericInput("lwd.marks"
                                          ,HTML(paste("general"
                                                      , sep = "<br/>") )
                                          ,paramValues$lwd.marksDefault,min=0.25,max=5,step=0.25)
                          )
                        ,div(
                             title=lwd.mimicCenDesc
                             ,numericInput("lwd.mimicCen"
                                           ,HTML(paste("cenStyle"
                                                       , sep = "<br/>") )
                                           ,paramValues$lwd.mimicCenDefault,min=0.25,max=5,step=0.25)
                        )

                        ,div(
                             title=lwd.cMDesc
                             ,numericInput("lwd.cM"
                                           ,HTML(paste("cM Marks"
                                                       , sep = "<br/>") )
                                           ,paramValues$lwd.cMDefault
                                           ,min=0.1,max=5,step=0.1)
                        )
                        )
           )
           ,wellPanel(
             h4("Add empty kars.")
             ,fluidRow(

             column(8
             ,div(
               title=addMissingOTUAfterDesc
               ,textInput("addMissingOTUAfter",
                          HTML(paste("after OTUs:", sep = "<br/>")
                          )
                          ,paramValues$addMissingOTUAfterDefault
               )
             )
             )
             ,column(4
             ,div(
               title=missOTUspacingsDesc
               ,textInput("missOTUspacings",
                             HTML(paste("spaces", sep = "<br/>")
                             )
                             ,paramValues$missOTUspacingsDefault
               )
             )
           )
             )
           )
           ,wellPanel(
             h4("Special scale")
             ,div(
               title=specialOTUNamesDesc
               ,textInput("specialOTUNames",
                          HTML(paste("OTUs", sep = "<br/>")
                          )
                          ,paramValues$specialOTUNamesDefault
               )
             )
             ,splitLayout(style="margin-bottom:-10px;"
                            ,cellWidths = c("33%","33%","33%")
               ,div(
                 title=specialyTitleDesc
                 ,textInput("specialyTitle",
                            HTML(paste("ruler","title", sep = "<br/>")
                            )
                            ,paramValues$specialyTitleDefault
                 )
               )
               ,div(
                 title=specialChrWidthDesc
                 ,numericInput("specialChrWidth",
                            HTML(paste("chr.","width", sep = "<br/>")
                            )
                            ,paramValues$specialChrWidthDefault
                            ,min=0.1,max=5,step=0.1
                 )
               )
               ,div(
                 title=specialChrSpacingDesc
                 ,numericInput("specialChrSpacing",
                            HTML(paste("chr","spacing", sep = "<br/>")
                            )
                            ,paramValues$specialChrSpacingDefault
                            ,min=0.1,max=5,step=0.1
                 )
               )
             )
           )

           ,wellPanel(style="margin-top:-15px;"
                      ,fluidRow(
                        column(6
                               ,h4("OTU")
                               ,div(style="margin-top:-10px;"
                                    ,title=OTUfamilyDesc
                                    , textInput("OTUfamily"
                                                    ,HTML(paste("Font family"
                                                                , sep = "<br/>") )
                                                    ,paramValues$OTUfamilyDefault)
                               )
                               ,div(style="margin-top:-10px;"
                                    ,title=addOTUNameDesc
                                    , checkboxInput("addOTUName"
                                                    ,HTML(paste("Show name"
                                                                , sep = "<br/>") )
                                                    ,paramValues$addOTUNameDefault)
                               )
                               ,div(title=OTUTextSizeDesc
                                    , numericInput("OTUTextSize"
                                                   ,HTML(paste("text size"
                                                               , sep = "<br/>") )
                                                   , paramValues$OTUTextSizeDefault, min = 0.1, max = 5, step=0.1)
                               )
                        )
                        ,column(6
                                ,div(title=OTUfontDesc
                                     ,radioButtons("OTUfont","font type",
                                                   c("normal"="1","bold"="2","italics"="3","bold italics"="4"),
                                                   selected=paramValues$OTUfontDefault)
                                )
                        )
                      )
           )
           ,wellPanel(style= "margin-top: -10px"
                      ,splitLayout(
                        h4("Layout"),
                        div(title=circularPlotDesc
                            ,checkboxInput("circularPlot",
                                           HTML(paste("Circular", "Plot", sep = "<br/>") )
                                           ,paramValues$circularPlotDefault)
                        )
                      )
                      ,uiOutput("circParam")
                      ,uiOutput("circParamOTU")
           )
    )
    ,column(width=10
            ,br()
            ,fluidRow(
              uiOutput("colColor")
              ,column(2
                      ,wellPanel(style="padding: 0px 5px 0px 5px"
                                   ,h4("Chromosomes")
                                 ,fluidRow(
                                   column(3
                                          ,div(style="margin-bottom:-15px",
                                               tags$span(tags$strong("order") )
                                               ,tags$span(tags$strong("by:") )
                                          )
                                   )
                                   ,column(9
                                   ,div(style="margin-top:-20px",
                                               title=orderChrDesc
                                               ,radioButtons(
                                                 "orderChr",""
                                                 ,c("size"="size"
                                                    ,"as in d.f."="original"
                                                    ,"alphab."   ="name"
                                                    ,"group"     ="group"
                                                    ,"col. chrNameUp"="chrNameUp"
                                                    )
                                                 ,selected = paramValues$orderChrDefault
                                                 ,inline = T
                                              )
                                          )
                                   )
                                 )
                                 ,fluidRow(
                                 column(3
                                 ,div(style="margin-bottom:-15px",
                                      tags$span(tags$strong("Chr.") )
                                      ,tags$span(tags$strong("names:") )
                                 )
                                 )
                                 ,column(9
                                   ,div(style="margin-top: -17px;",
                                               title=chrIdDesc
                                               ,radioButtons("chrId"

                                                              ,""
                                                              ,choices=c("original"="original","simple"="simple"
                                                                         ,"none"="none"
                                                                         )
                                                              ,paramValues$chrIdDefault
                                                              ,inline = T
                                                )
                                           )
                                   )
                                 )
                                 ,splitLayout(
                                   div(
                                     title=chrWidthDesc
                                     ,numericInput("chrWidth",HTML(paste("","Width",sep="</br>") )
                                                   , paramValues$chrWidthDefault, min = 0.1, max = 5, step=0.05)
                                   )
                                   ,div(
                                     title=chrSpacingDesc
                                     ,numericInput("chrSpacing", HTML(paste("horiz.","spacing", sep="</br>") )
                                                   , paramValues$chrSpacingDefault, min = 0.1, max = 5, step=0.1)
                                   )
                                   , div(
                                     title=lwd.chrDesc
                                     ,numericInput("lwd.chr"
                                                   ,HTML(paste("border","width", sep="</br>") )
                                                   ,paramValues$lwd.chrDefault,min=0,max=4,step=0.25)
                                   )
                                 )
                      )
              )
              ,column(2
                      ,wellPanel(style = "padding: 0px 5px 0px 5px"
                                 ,splitLayout(cellWidths = c("40%","30%","30%")
                                              ,h4("Karyotypes")
                                              ,div(title=karSeparDesc
                                                   ,checkboxInput("karSepar"
                                                                  ,HTML(paste(
                                                                    tags$span("equally", style = "font-size: 80%;")
                                                                    ,tags$span("spaced", style = "font-size: 80%;")
                                                                    , sep="<br/>") )
                                                                  ,paramValues$karSeparDefault)
                                              )
                                              ,div(title=verticalPlotDesc
                                                   ,checkboxInput("verticalPlot"
                                                                  ,HTML(paste(
                                                                  tags$span("vertic.", style = "font-size: 80%;")
                                                                  ,tags$span("plot", style = "font-size: 80%;")
                                                                  , sep="<br/>")
                                                                  )
                                                                  ,paramValues$verticalPlotDefault)
                                              )
                                 )

                                 ,splitLayout(style="margin-bottom:-12px;"
                                              ,cellWidths = c("23%","1%","23%","1%","23%","1%","23%")
                                              ,div(
                                                title=karHeightDesc
                                                ,numericInput("karHeight"
                                                              ,tagList(
                                                                div(style="height:15px;",
                                                                      tags$span("")
                                                                ),
                                                                div(style="height:15px;",
                                                                      tags$span("")
                                                                ),
                                                                div(style="height:15px;",
                                                                      tags$span("Height")
                                                                )
                                                              )
                                                              ,paramValues$karHeightDefault
                                                              , min = 0.5, max = 50, step=0.5
                                                              )
                                              )
                                              ,br()
                                              ,div(
                                                title=karHeiSpaceDesc
                                                ,numericInput("karHeiSpace"
                                                              ,tagList(
                                                              div(style="height:15px;",
                                                                   tags$span("Height", style = "font-size: 70%; ")
                                                                ),
                                                              div(style="height:15px;",
                                                                tags$span("with",  style = "font-size: 70%; ")
                                                              ),
                                                              div(style="height:15px;",
                                                                tags$span("space", style = "font-size: 70%; ")
                                                              )
                                                              )
                                                              , paramValues$karHeiSpaceDefault
                                                              , min = 2, max = 150, step=0.5
                                                              )
                                              )
                                              ,br()
                                              ,div(
                                                title=amoSeparDesc
                                                ,numericInput("amoSepar"
                                                            ,tagList(
                                                              div(style="height:15px;",
                                                                  tags$span("")
                                                              ),
                                                              div(style="height:15px;",
                                                                  tags$span("Vert.")
                                                              ),
                                                              div(style="height:15px;",
                                                                  tags$span("separ.")
                                                              )
                                                            )
                                                              , paramValues$amoSeparDefault, min = 0, max = 15, step=0.5)
                                              )
                                              ,br()
                                              ,div(
                                                title=karSpaceHorDesc
                                                ,numericInput("karSpaceHor"
                                                              ,tagList(
                                                                div(style="height:15px;",
                                                                    tags$span("")
                                                                ),
                                                                div(style="height:15px;",
                                                                    tags$span("Horiz.")
                                                                ),
                                                                div(style="height:15px;",
                                                                    tags$span("separ.")
                                                                )
                                                              )
                                                              , paramValues$karSpaceHorDefault
                                                              , min = 0, max = 15, step=0.5)
                                              )
                                 )


                      )
                               ,wellPanel(style="margin-top:-15px;padding: 0px 5px 0px 5px"
                                          ,fluidRow(
                                            column(7
                                             ,h4("Chromatids")
                                             ,splitLayout(
                                              cellWidths = c("50%","50%")
                                          ,div(
                                            title=chromatidsDesc
                                               ,checkboxInput("chromatids"
                                                              ,tagList(
                                                                div(style="height:15px;",
                                                                    tags$span("Show",style = "font-size: 80%;")
                                                                ),
                                                                div(style="height:15px;",
                                                                    tags$span("separ.",style = "font-size: 80%;")
                                                                )
                                                              )
                                                              ,paramValues$chromatidsDefault)
                                          )
                                          ,div(
                                               title=holocenNotAsChromatidsDesc
                                               ,checkboxInput("holocenNotAsChromatids"
                                                              ,tagList(
                                                                div(style="height:15px;",
                                                                    tags$span("hol.",style = "font-size: 80%;")
                                                                ),
                                                                div(style="height:15px;",
                                                                    tags$span("forbid.",style = "font-size: 80%;")
                                                                )
                                                              )
                                                              ,paramValues$holocenNotAsChromatidsDefault
                                               )
                                          )
                                          )
                                          )
                                          ,column(5
                                          ,div(
                                               title=xModifierDesc
                                               ,numericInput("xModifier"
                                                             ,tags$span("Separation",style = "font-size: 80%;")
                                                             ,paramValues$xModifierDefault
                                                             ,min = 2, max = 25, step=0.5)
                                          )
                                          ,div(
                                            title=xModMonoHoloRateDesc
                                            ,numericInput("xModMonoHoloRate"
                                                          ,tags$span("hol. sep. prop.",style = "font-size: 80%;")
                                                          ,paramValues$xModMonoHoloRateDefault
                                                          ,min = 1, max = 25, step=1)
                                          )

                                      )
                                          )
                               )

              )



              ,column(2
                      ,wellPanel(style = "padding: 0px 5px 0px 5px"
                                 ,splitLayout(style="margin-bottom:-10px;"
                                             ,cellWidths = c("50%","50%")
                                             ,div(
                                               h4("Ruler")
                                             )
                                             ,div(
                                               title=rulerDesc
                                               ,checkboxInput("ruler",
                                                              "Show ruler"
                                                              ,paramValues$rulerDefault)
                                             )
                                 )


                                 ,splitLayout(style="margin-bottom:-10px;"
                                              ,cellWidths = c("40%","60%")
                                              ,br()
                                              ,h4(tags$strong("position") )
                                 )
                                 ,splitLayout(style="margin-bottom:-10px;"
                                              ,cellWidths = c("22%","3%","23%","23%","23%")
                                              ,div(
                                                title=ceilingFactorDesc
                                                ,numericInput("ceilingFactor"
                                                              , "approx."
                                                              , paramValues$ceilingFactorDefault, min = -2, max = 5, step=1) ,
                                              )
                                              ,br()
                                          ,div(
                                            title=rulerPosDesc
                                            ,numericInput("rulerPos", "ruler"
                                                          , paramValues$rulerPosDefault
                                                          , min = -5, max = 5, step=0.1) ,
                                          )
                                          ,div(
                                               title=xPosRulerTitleDesc
                                               ,numericInput("xPosRulerTitle"
                                                          , "title"
                                                          , paramValues$xPosRulerTitleDefault
                                                          , min = -5, max = 5, step=0.1)
                                          )
                                          ,div(
                                            title=rulerNumberPosDesc
                                            ,numericInput("rulerNumberPos"
                                                          ,tags$span("Numbers", style = "font-size: 80%;")
                                                          , paramValues$rulerNumberPosDefault
                                                          , min = .1, max = 5, step=0.1)
                                          )
                                   )
                                 ,splitLayout(style="margin-bottom:-10px;"
                                              ,cellWidths = c("45%","50%")
                                              ,br()
                                              ,h4(tags$strong("Interval") )
                                 )
                                 ,splitLayout(
                                   cellWidths = c("22%","3%","23%","23%","23%")
                                   ,div(
                                     title=yTitleDesc
                                     ,textInput("yTitle",
                                                HTML(paste("title", sep = "<br/>")
                                                )
                                                ,paramValues$yTitleDefault
                                     )
                                   )
                                   ,br()
                                   ,div(
                                     title=rulerIntervalDesc
                                     ,numericInput("rulerInterval"
                                                   , "for \u00B5m"
                                                   , paramValues$rulerIntervalDefault
                                                   , min = .1, max = 20, step=0.1) ,
                                   )

                                 ,div(
                                      title=rulerIntervalcMDesc
                                      ,numericInput("rulerIntervalcM"
                                                    , "for cM"
                                                    , paramValues$rulerIntervalcMDefault
                                                    , min = .1, max = 20, step=0.1) ,
                                 )
                                 ,div(
                                       title=rulerIntervalMbDesc
                                       ,numericInput("rulerIntervalMb"
                                                     , "for Mb"
                                                     , paramValues$rulerIntervalMbDefault
                                                     , min = 0.1, max = 10000, step=0.1) ,
                                 )
                                 )

                                 ,splitLayout(style="margin-bottom:-10px;"
                                              ,cellWidths = c("54%","44%")
                                              ,br()
                                 ,h4(tags$strong("font size") )
                                 )
                                 ,splitLayout(style="margin-bottom:-10px;"
                                              ,cellWidths = c("26%","24%","4%","21%","21%")
                                          ,div(
                                            title=ruler.tckDesc
                                            ,numericInput("ruler.tck", "ticks"
                                                          , paramValues$ruler.tckDefault, min = -5, max = 5, step=0.005)
                                          )
                                          ,div(
                                            title=thresholdDesc
                                            ,numericInput("threshold"

                                                          ,tags$span("thresh.", style = "font-size: 80%;")
                                                          , paramValues$thresholdDefault, min = 5, max = 95, step=1)
                                          )

                                          ,br()

                                          ,div(
                                               title=rulerNumberSizeDesc
                                               ,numericInput("rulerNumberSize"
                                                             ,tags$span("Numbers", style = "font-size: 80%;")
                                                             , paramValues$rulerNumberSizeDefault, min = .1, max = 5, step=0.1) ,
                                          )
                                          ,div(
                                               title=rulerTitleSizeDesc
                                               ,numericInput("rulerTitleSize"
                                                             , "Title"
                                                             , paramValues$rulerTitleSizeDefault, min = .1, max = 5, step=0.1) ,
                                          )
                                   )
                      )
              )
              ,column(2
                      ,wellPanel(style="padding: 0px 5px 0px 5px"
                                  ,splitLayout(
                                    cellWidths = c("50%","25%","24%")
                                    ,h4("Centromere")
                                    ,h5("Size")
                                    ,div(
                                      title=autoCenSizeDesc
                                      ,checkboxInput("autoCenSize", "auto"
                                                     , paramValues$autoCenSizeDefault
                                      )
                                    )
                                  )
                                  ,splitLayout(
                                    cellWidths = c("50%","25%","24%")
                                    ,div(style = "margin-top:-25px"
                                         ,title=cenFormatDesc
                                         ,radioButtons("cenFormat","",c("rounded","triangle","inProtein")
                                                       ,selected=paramValues$cenFormatDefault)
                                    )
                                    ,div(
                                      title=cenFactorDesc
                                      ,numericInput("cenFactor"
                                                    ,tags$span("modify",style = "font-size: 80%;")
                                                    , paramValues$cenFactorDefault, min = 0, max = 10, step=0.25)
                                    )
                                    ,div(
                                      title=centromereSizeDesc
                                      ,numericInput("centromereSize"
                                                    , tags$span("absolute",style = "font-size: 80%;")
                                                    , paramValues$centromereSizeDefault
                                                    , min = 0, max = 100000000, step=1)
                                    )
                                  )
                      )
                      ,wellPanel(style="margin-top:-15px;padding: 0px 20px 0px 5px;"
                                 ,splitLayout(
                                   span(h4("Squareness"),helpText("> 20 = squared") )
                                   ,div(
                                     title=squarenessDesc
                                     ,numericInput("squareness"
                                                   ,""

                                                   , paramValues$squarenessDefault
                                                   , min = 1, max = 21, step=0.5)
                                   )
                                 )
                                 ,splitLayout(style="margin-bottom:-10px;"
                                              ,cellWidths = c("50%","25%","25%")
                                   ,helpText(tags$strong("resolution:"),HTML("</br>"),"> = more vertices")
                                   ,div(
                                     title=markNDesc
                                     ,numericInput("markN",
                                                   HTML(paste("marks'", sep = "<br/>") )
                                                   ,paramValues$markNDefault
                                                   , min = 1, max = 200, step=1)
                                   )
                                   ,div(
                                     title=nDesc
                                     ,numericInput("n",
                                                   HTML(paste("chr.", sep = "<br/>") )
                                                   ,paramValues$nDefault
                                                   , min = 25, max = 400, step=1)
                                   )
                                 )
                      )

                      ,wellPanel(style="margin-top:-15px;padding: 5px 5px 5px 5px"


           ,splitLayout(cellWidths = c("25%","50%","25%")
                        ,actionButton("fontDecrease", label = "<")
                        ,h4("Fonts' size")
                        ,actionButton("fontIncrease", label = ">")
           )
     )
)

,column(2
        ,wellPanel(style="padding: 0px 5px 0px 5px"

                   ,splitLayout(style="margin-bottom:-10px;"
                                ,cellWidths = c("20%","38%","38%")
                                ,div(
                                  h4("Plot")
                                )
                                ,div(
                                  numericInput("widFactor"
                                               , "width %",
                                               80, min = 5, max = 1000, step=5)
                                )
                                ,div(
                                  numericInput("heiFactor"
                                               ,"height ratio",
                                               0.5, min = 0.05, max = 20, step=0.05)
                                )
                   )
                   ,splitLayout(cellWidths = c("25%","37%","38%")






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
        )
        ,wellPanel(style="margin-top:-15px;padding: 5px 5px 5px 5px"

                   ,div(style = "margin-bottom:-10px"
                        ,splitLayout(cellWidths = c("30%","20%","30%","20%")
                                     ,h5(tags$strong("Zoom (X)") )
                                     ,actionButton("left", label = "<")

                                     ,sliderInput("hwModifier","",0,10,1, step = 0.1, ticks = FALSE)

                                     ,actionButton("right", label = ">")
                        )
                   )
        )

)
            )
            ,fluidRow(
              column(10,
                     wellPanel(
                       style = "margin-top:-15px;overflow-y:auto;text-align:center;"



                       ,div(style = 'overflow-y:auto;overflow-x:auto;min-width:1030px;'
                            ,imageOutput("idiogramPlot"
                                         ,height = "auto"
                            )
                       )
                     )
              )
              ,uiOutput("lastCol")
            )
    )
  )
})


output$lastCol<- renderUI({
  column(width=2

         ,wellPanel(style="margin-top:-15px;padding: 0px 15px 0px 5px",
           h4("Margins"),
           splitLayout(style="margin-bottom:-10px;"
                       ,cellWidths = c("25%","25%","25%","25%")
                       ,div(
                         title=ylimBotModDesc
                         ,numericInput("ylimBotMod", "bottom",
                                       paramValues$ylimBotModDefault
                                       , min = -5, max = 5, step=0.5)
                       )
                       ,div(
                         title=ylimTopModDesc
                         ,numericInput("ylimTopMod", "top"
                                       , paramValues$ylimTopModDefault
                                       , min = -5, max = 5, step=0.5)
                       )
                       ,div(
                         title=xlimLeftModDesc
                         ,numericInput("xlimLeftMod", "left",
                                       paramValues$xlimLeftModDefault
                                       , min = -5, max = 5, step=0.5)
                       )
                       ,div(
                         title=xlimRightModDesc
                         ,numericInput("xlimRightMod", "right",
                                       paramValues$xlimRightModDefault
                                       , min = -6, max = 6, step=0.4)
                       )
           )
       )
         ,wellPanel(style = "margin-top:-15px;padding: 0px 15px 0px 5px"
                    ,h5("Move all karyotypes")
                    ,splitLayout(
                      div(title=moveAllKarValueHorDesc
                          ,numericInput("moveAllKarValueHor",
                                        HTML(paste("horiz.",sep = "<br/>") )
                                        ,paramValues$moveAllKarValueHorDefault
                                        , min = 0.05, max = 5, step=0.05)
                      )
                      ,div(title=moveAllKarValueYDesc
                           ,numericInput("moveAllKarValueY",
                                         HTML(paste("vertical",sep = "<br/>") )
                                         ,paramValues$moveAllKarValueYDefault
                                         , min = 0.05, max = 5, step=0.05)
                      )
                    )
                    )
         ,wellPanel(style = "margin-top:-15px;padding: 0px 15px 0px 5px"

           ,h4("Indices & Info.")
           ,splitLayout(
             div(title=indexIdTextSizeDesc
                 ,numericInput("indexIdTextSize",
                               HTML(paste("font","size",sep = "<br/>") )
                               ,paramValues$indexIdTextSizeDefault
                               , min = 0.05, max = 5, step=0.05)
             )
             ,div(title=distTextChrDesc
                  , numericInput("distTextChr"
                                 ,HTML(paste("chr. to text", "separation"
                                             , sep = "<br/>") )
                                 , paramValues$distTextChrDefault, min = 0.2, max = 5, step=0.1)
             )
           )
           ,splitLayout(
             div(title=morphoDesc
                 ,radioButtons("morpho","Morphology"
                               ,c("Guerra (1986)"="Guerra"
                                  ,"Levan (1974)"="Levan"
                                  ,"both"="both"
                                  ,"none"="")
                               ,selected = paramValues$morphoDefault
                 )
             ),
             div(title=chrIndexDesc
                 ,radioButtons("chrIndex","Add AR & CI"
                               ,c("Chr. Index"="CI"
                                  ,"Arm Ratio"="AR"
                                  ,"both"="both"
                                  ,"none"="")
                               ,selected = paramValues$chrIndexDefault )
             )
           )
           ,splitLayout(style="margin-bottom:-10px;"
                        ,cellWidths = c("33%","67%")
                        ,br()
                        ,h4(tags$strong("Show") )
           )
           ,fluidRow(
           column(6
             ,div(
                  title=chrSizeDesc
                  ,checkboxInput("chrSize",
                                 HTML(paste("Chrom.", "size", sep = "<br/>") )
                                 ,paramValues$chrSizeDefault)
             )
             ,div(title=karIndexDesc
                 ,checkboxInput("karIndex",
                                HTML(paste("Add kar.","ind. A/A2",sep="<br/>"
                                )
                                )
                                ,paramValues$karIndexDefault
                 )
             )
           )
           ,column(6
              ,div(
                  title=chrSizeMbpDesc
                  ,checkboxInput("chrSizeMbp"
                                 ,tagList(
                                   div(style="height:15px;",
                                       tags$span("Chr.")
                                   ),
                                   div(style="height:15px;",
                                       tags$span("size")
                                   )
                                   ,div(style="height:15px;",
                                        tags$span("in Mbp")
                                   )
                                   ,div(style="height:15px;",
                                        tags$p("(column", style = "font-size: 80%;")
                                   )
                                   ,div(style="height:15px;",
                                        tags$p("required)", style = "font-size: 80%;")
                                   )
                                 )
                                 ,paramValues$chrSizeMbpDefault)
             )
           )
           )
           ,splitLayout(

           div(title=karIndexPosDesc
                ,numericInput("karIndexPos",tags$strong("Kar. index pos.")
                               ,paramValues$karIndexPosDefault
                )
           )
           ,div(title=nameChrIndexPosDesc
                ,numericInput("nameChrIndexPos"
                              ,tags$strong("Chr. index pos.")
                              ,paramValues$nameChrIndexPosDefault
                )
           )
           )
           ,splitLayout(
             div(
               title=perAsFractionDesc
               ,checkboxInput("perAsFraction",
                              HTML(paste("% as","fraction", sep = "<br/>") )
                              ,paramValues$perAsFractionDefault)
             )
             ,div(
                 title=markPerDesc
                 ,textInput("markPer",
                        HTML(paste("% of this","marks:", sep = "<br/>") )
                        ,paramValues$markPerDefault)
                    )

           )
           ,splitLayout(
             div(
               title=showMarkPosDesc
               ,checkboxInput("showMarkPos",
                              HTML(paste("mark's", "pos.", sep = "<br/>") )
                              ,paramValues$showMarkPosDefault)
             )
              ,div(
                      title=bToRemoveDesc
                      ,textInput('bToRemove'
                                 ,HTML(paste("hide pos. of","this marks", sep="<br/>"
                                 ) )
                                 , paramValues$bToRemoveDefault)
                    )
           )

         )
         ,wellPanel(style = "margin-top:-15px;padding: 0px 20px 0px 5px"
           ,splitLayout(
             h4("Anchor")
             ,  div(title=anchorDesc
                    ,checkboxInput("anchor", "Show anchor", paramValues$anchorDefault)
             )
           )
           ,splitLayout(
             div(title=moveKarHorDesc
                 ,textInput("moveKarHor",
                            "kar. to move",
                            value  = paramValues$moveKarHorDefault
                            ,width = "100%")
             )
             ,div(title=mkhValueDesc
                  ,numericInput("mkhValue", "move kar.", paramValues$mkhValueDefault, min = -10, max = 10, step=0.25)
             )
           )
           ,div(title=karAnchorLeftDesc
                             ,textInput("karAnchorLeft",
                                        "kar. to anchor",
                                        value  = paramValues$karAnchorLeftDefault
                                        ,width = "100%")
           )
           ,splitLayout(
             div(title=moveAnchorVDesc
                 ,numericInput("moveAnchorV", "move ver.", paramValues$moveAnchorVDefault, min = -10, max = 10, step=0.25)
             )
             ,div(title=moveAnchorHDesc
                  ,numericInput("moveAnchorH", "move hor.", paramValues$moveAnchorHDefault, min = -10, max = 10, step=0.25)
             )
           )
         )

  )
})

output$chrColorUI<- renderUI({
  tagList(
    splitLayout(
      cellWidths = c("50%","50%")
      ,h4("Colors")
      ,h5("border")
    )
    ,splitLayout(style = "margin-top:-10px;margin-bottom:-10px"
      ,cellWidths = c("24%","24%","24%","24%")
      ,div(
           title=cenColorDesc
           ,textInput('cenColor', 'centr.', paramValues$cenColorDefault)
      )
      ,div(
          title=chrColorDesc
          ,textInput('chrColor', 'chr.', paramValues$chrColorDefault)
      )
      ,div(
        title=colorBorderMarkDesc
        ,textInput('colorBorderMark'
                   ,HTML(paste("Marks"

                   ) )
                   , paramValues$colorBorderMarkDefault)
      )
      ,div(title=chrBorderColorDesc
           ,textInput("chrBorderColor","chr.",paramValues$chrBorderColorDefault)
      )
    )
  ,splitLayout(
           div(title=fixCenBorderDesc
                , checkboxInput("fixCenBorder"
                                ,HTML(paste("chr. color as", "border color","of cen.", sep = "<br/>") )
                                ,paramValues$fixCenBorderDefault)
           )
           ,div(title=gishCenBorderDesc
                , checkboxInput("gishCenBorder"
                                ,HTML(paste("mark color", "to border", sep = "<br/>") )
                                ,paramValues$gishCenBorderDefault)
           )
    )
  )
})

output$marksUIcolor <- renderUI({

  div(
      title=mycolorsDesc
      ,textInput('mycolors'
                 ,HTML(paste("Marks"
                             ,tags$p("(optional, see data.frame page)", style = "font-size: 80%; font-weight: normal;")
                             ,tags$p("here, only comma separated", style = "font-size: 80%;")
                 ) )
                 , paramValues$mycolorsDefault)
  )
})

output$colColor<- renderUI({
  column(width=2
         ,wellPanel(style = "background: #F7DCDA; padding: 0px 5px 0px 5px"
                    ,uiOutput("chrColorUI")
                    ,uiOutput("marksUIcolor")
         )
  )
})
