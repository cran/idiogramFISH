
output$tabsetpanel0UI<-renderUI({
  tabsetPanel(id="tabsetpanel0"
              ,tabPanel("1. Load example"
                        ,tags$head(tags$style( HTML(' wpanel .tab-content {margin-left:50px;}')) )
                        ,value="exampleTab"
                        ,div(class = "wpanel",
                             uiOutput("examplepanel")
                        )
              ) # tP
  ) # tabsetPanel
})

output$tabsetpanel1UI<-renderUI({
  tabsetPanel(id="tabsetpanel1"
              ,tabPanel("2. Chr. data data.frame", value="dfChrTab",
                        uiOutput("dfchrpanel") )
              ,tabPanel("3. Mark Pos. data d.f.",  value="dfMarkTab",
                        uiOutput("dfmarkpanel") )
              ,tabPanel("4. Mark's Style",         value="dfMSTab",
                        uiOutput("dfMStylepanel") )
  ) # tabsetPanel
})

output$tabsetpanel2UI<-renderUI({
  tabsetPanel(id="tabsetpanel2",
              tabPanel("1. Parameters", value="paramTab"
                       # ,style= "min-width:1366px;max-width:1920px;overflow:auto"
                       ,uiOutput("parameterPanel") )
              ,tabPanel("2. Log", value="logTab",
                        uiOutput("logpanel") )
              ,tabPanel("3. code", value="codeTab",
                        uiOutput("strpanel") )
  )# tabsetpanel
})

output$tabsetpanel4UI<-renderUI({
  tabsetPanel(id="tabsetpanel4",
              tabPanel("1. Search", value="searchTab",
                       uiOutput("searchPanel") )
              # ,tabPanel("2. Log", value="logTab",
              # uiOutput("logpanel") )
  ) # tabsetpanel
})

