# options(shiny.error = NA)
library(shiny)
library(shinydashboard)
library(idiogramFISH)
library(rhandsontable)
library(gtools)
library(knitr)
library(rclipboard)
library(clipr)

devel <- FALSE

if(system.file(package = "rmarkdown") != '') {
  if(rmarkdown::pandoc_available()) {
    if(rmarkdown::pandoc_version()>2.11 & devel==F) {
      rmarkdown::render("www/README2.Rmd")
    }
  }
}

ui <- tagList(
  tags$style(type='text/css', '#ARCImessageOut {white-space: pre-wrap; word-break: keep-all;}'),
  tags$style("html,body{background-color: white;}
                .container{
                    width: 100%;
                    margin: 0 auto;
                    padding: 0;
                }
               @media screen and (min-width: 700px){
                .container{
                    min-width: 1850px;
                    max-width: 1920px;
                }
               }
             .form-control {
  padding-top: 0px;
  padding-right: 0px;
  padding-bottom: 0px;
  padding-left: 5px;
}
"),
  tags$div(class="container",
           dashboardPage( title="idiogramFISH"
                          ,dashboardHeader(title = p("idiogramFISH shiny app") )
                          ,dashboardSidebar(
                            uiOutput("mysidebar")
                          )
                          ,dashboardBody(
                            tags$head(
                              tags$style(HTML(".fa{font-size: 12px;}") )
                              ,tags$style(HTML(".innerI .fa {font-size: 18px;}") )
                              ,tags$style("#markdown{height:100vh !important;}")
                              ,tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css" )
                              ,tags$style(
                                "
body{
    height: auto;
    margin: auto;
    overflow-x: auto;
}"
                              )
                            ) # head
                            ,uiOutput("myTabs")
                          ) # dashboardbody
           )
  )
)
