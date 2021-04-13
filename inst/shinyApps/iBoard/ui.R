library(shiny)
library(shinydashboard)
library(idiogramFISH)
library(rhandsontable)
library(gtools)
library(knitr)
library(rclipboard)

rmarkdown::render("www/README2.Rmd")#, output_dir = "www")

ui <- dashboardPage( title="idiogramFISH"
                ,dashboardHeader(title = p("idiogramFISH shiny app") )
                ,dashboardSidebar(
                  uiOutput("mysidebar")
                )
                ,dashboardBody(
                   tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css" )
                   ) # head
                   ,uiOutput("myTabs")
                ) # dashboardbody
)
