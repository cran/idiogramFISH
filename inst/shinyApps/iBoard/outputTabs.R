
output$myTabs<-renderUI({
  tabItems(

    #
    # examples
    #

    tabItem(tabName = "examplesMenu",
            fluidRow(
              actionButton('jumpToPrevMenu0',strong('Previous page'),  icon("arrow-up") , style="padding:10px 5px 10px 5px"
              ),
              actionButton('jumpToNextMenu0',strong('Next page'),  icon("arrow-down") , style="padding:10px 5px 10px 5px"
              )
            )
            ,fluidRow(
              uiOutput("tabsetpanel0UI")
            ) #Row
    ) # ,  tabitem


    #
    #   data.frames
    #

    ,tabItem(tabName = "DFsMenu",
             fluidRow(
               actionButton('jumpToPrevMenu',strong('Previous page'),  icon("arrow-up") , style="padding:10px 5px 10px 5px"
               ),
               actionButton('jumpToNextMenu',strong('Next page'),  icon("arrow-down") , style="padding:10px 5px 10px 5px"
               )
               ,
               actionButton('jumpToNext',strong('Next tab'),  icon("arrow-right") , style="padding:10px 5px 10px 5px"
                             ,class="centerAlign")
               ,actionButton('jumpToPrev',strong('Previous tab'),  icon("arrow-left") , style="padding:10px 5px 10px 5px"
                             ,class="centerAlign")
             )
             ,fluidRow(
               uiOutput("tabsetpanel1UI")
             ) #Row
    ) # ,  tabitem

    #
    #   parameters
    #

    ,      tabItem(tabName = "parameterPlotMenu",
                   fluidRow(

                     # ,
                     actionButton('jumpToPrevMenu2',strong('Previous page'),  icon("arrow-up") , style="padding:10px 5px 10px 5px"
                     ),
                     actionButton('jumpToNextMenu2',strong('Next page'),  icon("arrow-down") , style="padding:10px 5px 10px 5px"
                     )

                     ,actionButton('jumpToNext2',strong('Next tab'),  icon("arrow-right") , style="padding:10px 5px 10px 5px"
                                   ,class="centerAlign")
                     ,actionButton('jumpToPrev2',strong('Previous tab'),  icon("arrow-left") , style="padding:10px 5px 10px 5px"
                                   ,class="centerAlign")
                   ),
                   fluidRow(
                     uiOutput("tabsetpanel2UI")
                   )#fluidrow
    )#tabitem

    # tab nuccore
    ,tabItem(tabName = "nuccoreMenu",
                   fluidRow(
                     actionButton('jumpToPrevMenu4',strong('Previous page'),  icon("arrow-up") , style="padding:10px 5px 10px 5px"
                     ),
                     actionButton('jumpToNextMenu4',strong('Next page'),  icon("arrow-down") , style="padding:10px 5px 10px 5px"
                     )

                     # ,actionButton('jumpToNext4',strong('Next tab'),  icon("arrow-right") , style="padding:10px 5px 10px 5px"
                     #               ,class="centerAlign")
                     # ,actionButton('jumpToPrev4',strong('Previous tab'),  icon("arrow-left") , style="padding:10px 5px 10px 5px"
                     #               ,class="centerAlign")
                   ),
                   fluidRow(
                     uiOutput("tabsetpanel4UI")
                   ) #fluidrow
    ) #tabitem

    ,tabItem(tabName = "aboutMenu",

             fluidRow(
               actionButton('jumpToPrevMenu3',strong('Previous page'),  icon("arrow-up") , style="padding:10px 5px 10px 5px"
               ),
               actionButton('jumpToNextMenu3',strong('Next page'),  icon("arrow-down") , style="padding:10px 5px 10px 5px"
               )

             )
             ,fluidRow(
               column(width=2),
               column(width=8
                      ,tags$head(tags$style(HTML('

                   ')))
                      ,uiOutput("markdown")
               ) # column
             ) #fluidrow close
    )#tabitem
  )
})

#
#   about file
#

output$markdown <- renderUI({
  htmltools::tags$iframe(src = "README2.html", width = '100%',  height = 1000,  style = "border:none;")
})
