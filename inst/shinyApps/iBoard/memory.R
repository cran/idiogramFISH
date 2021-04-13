
# output$memorybox<-renderUI({
#   wellPanel(id ="memconf",
#       h4("Memory configuration"),
#       # selectInput("memmega", "Define threshold for Memory in Megas", c(30:memmax),memthreshold)
#       numericInput("memmega", "Define threshold for Memory in Megabytes:"
#                    , memthreshold
#                    , min = memthreshold
#                    , max = memmax
#                    )
#       # ,br()
#       ,span("Increasing memory threshold might cause shinyapp to crash, use F5 to reload")
#       ,br()
#       ,htmlOutput("memhandler")
#       ,br()
#       ,htmlOutput("currentmemory")
#       ,br()
#       ,uiOutput("memmessage")
#   ) # end box
# })

# output$memhandler <-  renderUI({
#   HTML('<div id="texthtml333"  style="width: 90%;padding: 5px 0px 0px 10px" >',
#        paste("Decrease memory threshold if shinyapp crashes or closes" ),
#        '</div>'
#   )
# })
#
# output$currentmemory <-  renderUI({
#   HTML('<div id="texthtml333"  style="width: 90%;padding: 5px 0px 0px 10px" >',
#        ifelse(invalid(values$mem),"",paste(values$mem) ),
#        '</div>'  )
# })
#
# output$memmessage <-  renderText({
#   # if(as.integer(pryr::mem_used() )>input$memmega*1000000){
#   t<-values$memoryhandler
# })
