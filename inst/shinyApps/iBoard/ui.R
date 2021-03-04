library(shiny)
library(shinydashboard)
library(idiogramFISH)
library(rhandsontable)
library(gtools)
library(knitr)
library(rclipboard)
library(bib2df)

rmarkdown::render("www/README2.Rmd")#, output_dir = "www")

sidebar <- dashboardSidebar(
  # width = 88,
  sidebarMenu(
    id = "tabs",
    sidebarMenuOutput("menu1")
  )# menu
) # sidebar
ui <- dashboardPage(title="idiogramFISH"
                ,dashboardHeader(title = p("idiogramFISH shiny app") )
                ,sidebar
  ,dashboardBody(
    tabItems(
      #
      #   data.frames
      #
      tabItem(tabName = "DFsMenu",
              fluidRow(
                tags$head(tags$style("
    h3, h4 {
    margin-top: 4px;
    }
    .rightAlign{float:right;}
    .leftAlign{float:left;}
                    .centerAlign {
                      float: right;

                      position: relative;
                      left: -50%; /* or right 50% */
                        text-align: left;
                    }
                    .centerAlign > .child {
                      position: relative;
                      left: 50%;
                    }
     .well {
        padding: 10px;
     }
     .tab-pane {
          padding-left: 10px;
     }'
     ")
                )
                ,actionButton('jumpToPrevMenu',strong('Previous page'),  icon("arrow-up") , style="padding:10px 5px 10px 5px"
                ),
                actionButton('jumpToNextMenu',strong('Next page'),  icon("arrow-down") , style="padding:10px 5px 10px 5px"
                )
                # fluidRow(
                ,tags$style(HTML("
                          .tabs-above > .nav > li > a {
                          background-color: #cdebff;
                          }
                          .tabs-above > .nav > li[class=active] > a {
                          background-color: #81ccff;
                          " ) ) # tag
                ,actionButton('jumpToNext',strong('Next tab'),  icon("arrow-right") , style="padding:10px 5px 10px 5px"
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

                       tags$head(tags$style("
    h3, h4 {
    margin-top: 4px;
    }
    .rightAlign{float:right;}
    .leftAlign{float:left;}
                    .centerAlign {
                      float: right;

                      position: relative;
                      left: -50%; /* or right 50% */
                        text-align: left;
                    }
                    .centerAlign > .child {
                      position: relative;
                      left: 50%;
                    }
     .well {
        padding: 10px;
     }
     .tab-pane {
          padding-left: 10px;
     }'
     ")
                       )
                       ,actionButton('jumpToPrevMenu2',strong('Previous page'),  icon("arrow-up") , style="padding:10px 5px 10px 5px"
                       ),
                       actionButton('jumpToNextMenu2',strong('Next page'),  icon("arrow-down") , style="padding:10px 5px 10px 5px"
                       )

                       ,tags$style(HTML("
                          .tabs-above > .nav > li > a {
                          background-color: #cdebff;
                          }
                          .tabs-above > .nav > li[class=active] > a {
                          background-color: #81ccff;
                          " ) ) # tag
                       ,actionButton('jumpToNext2',strong('Next tab'),  icon("arrow-right") , style="padding:10px 5px 10px 5px"
                                     ,class="centerAlign")
                       ,actionButton('jumpToPrev2',strong('Previous tab'),  icon("arrow-left") , style="padding:10px 5px 10px 5px"
                                     ,class="centerAlign")
                     ),
                     fluidRow(
                       tags$style(HTML("
                          .tabs-above > .nav > li > a {
                          background-color: #cdebff;
                          }
                          .tabs-above > .nav > li[class=active] > a {
                          background-color: #81ccff;
                          " )), # tag
                          uiOutput("tabsetpanel2UI")
                     )#fluidrow
      )#tabitem
      ,tabItem(tabName = "abouttab",

               fluidRow(

                 tags$head(tags$style("
    h3, h4 {
    margin-top: 4px;
    }
    .rightAlign{float:right;}
    .leftAlign{float:left;}
                    .centerAlign {
                      float: right;

                      position: relative;
                      left: -50%; /* or right 50% */
                        text-align: left;
                    }
                    .centerAlign > .child {
                      position: relative;
                      left: 50%;
                    }
     .well {
        padding: 10px;
     }
     .tab-pane {
          padding-left: 10px;
     }'
     ")
                 )
                 ,actionButton('jumpToPrevMenu3',strong('Previous page'),  icon("arrow-up") , style="padding:10px 5px 10px 5px"
                 ),
                 actionButton('jumpToNextMenu3',strong('Next page'),  icon("arrow-down") , style="padding:10px 5px 10px 5px"
                 )

               )



              ,fluidRow(
                column(width=2),
                column(width=8
                       ,tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }
                                                        ')))

                       # ,img(src='logo.png', align = "right", width="20%")

                       ,uiOutput("markdown")
                ) # column
                # htmlOutput("abouttext")
              ) #fluidrow close
      )#tabitem
    )
  ) # dashboardbody
)
