# values[["number"]]  updates reactives
# file1               updates reactives
#
#   update chr. data.frame
#



observeEvent(input$loadDFbutton, ignoreInit = T, {
  values[["df1MStyleName"]]   <- values[["df1MarkName"]]   <- values[["df1Name"]]   <- "Nucleotide tab"
  values[["df1MStyleOrigin"]] <- values[["df1MarkOrigin"]] <-values[["df1Origin"]]  <- "data.frame was loaded"
}
)

observeEvent(values[["number"]], ignoreInit = T, {
  values[["df1"]] <- data.frame()
  Sys.sleep(0.1)
  df <- chrDataVec[as.numeric(values[["number"]]) ]
  values[["df1"]]       <- get(df)
  values[["df1Name"]]   <- ifelse(df=="emptydata.frame","no",df )
  values[["df1MStyleOrigin"]]<- values[["df1MarkOrigin"]] <- values[["df1Origin"]] <- "data.frame was loaded"
}
)

observeEvent(input$file1, {
  inFile <- input[["file1"]]
  values[["df1"]] <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
  values[["df1Name"]] <- inFile$name
  values[["df1Origin"]] <- "file was loaded"
})

#
#   update mark pos data.frame
#

observeEvent(values[["number"]], ignoreInit = T, {
  values[["df1Mark"]] <- data.frame()
  Sys.sleep(0.1)
  df <- markDataVec[as.numeric(values[["number"]])]
  values[["df1Mark"]] <- get( df )
  values[["df1MarkName"]] <- ifelse(df=="emptydata.frame","no",df )
})

observeEvent(input$file1Mark, {
  inFile <- input[["file1Mark"]]
  values[["df1Mark"]] <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
  values[["df1MarkName"]] <- inFile$name
  values[["df1MarkOrigin"]] <- "file was loaded"
})

#
#   update mark style data.frame
#

observeEvent(values[["number"]], ignoreInit = T, {
  values[["df1MStyle"]] <- data.frame()
  df <- MStyleDataVec[as.numeric(values[["number"]])]
  values[["df1MStyle"]] <- get( df )
  values[["df1MStyleName"]] <- ifelse(df=="emptydata.frame","no",df )
})



observeEvent(input$file1MStyle, {
  inFile <- input[["file1MStyle"]]
  values[["df1MStyle"]] <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE )
  values[["df1MStyleName"]] <- inFile$name
  values[["df1MStyleOrigin"]] <- "file was loaded"
})


#
#   keep inputs chr
#

observeEvent(input$out, {
  if (!is.null(input$out)) {
    tryCatch(df <- hot_to_r(input$out), error = function(e) {data.frame() } )
    if (!is.function(df)) {
      values[["df1"]] <- df
    }
  }
} )

#
#   keep inputs mark
#

observeEvent(input$outMark,{
  if (!is.null(input$outMark)) {
    tryCatch(df <- hot_to_r(input$outMark), error = function(e) {NA; NA } )
    if (!is.function(df)) {
      values[["df1Mark"]] <- df
    }
  }
})


#
#   keep inputs mark style
#

observeEvent(input$outMStyle,{
  # if (!is.null(input$outMStyle) ) {
  tryCatch(df <- hot_to_r(input$outMStyle), error = function(e) {data.frame() } )
  if (!is.function(df)) {
    values[["df1MStyle"]] <- df
  }
  # }
})



observeEvent(input$chrfilename,
             {
               updateTextInput(session = session,
                               inputId = "chrfilename2",
                               value = isolate(input$chrfilename)
               )
             }
)

observeEvent(input[["chrfilename2"]],
             {
               updateTextInput(session = session,
                               inputId = "chrfilename",
                               value = isolate(input$chrfilename2)
               )
             })



observeEvent(input$markfilename,
             {
               updateTextInput(session = session,
                               inputId = "markfilename2",
                               value = isolate(input$markfilename)
               )
             }
)

observeEvent(input[["markfilename2"]],
             {
               updateTextInput(session = session,
                               inputId = "markfilename",
                               value = isolate(input$markfilename2)
               )
             })





observeEvent(input$MStylefilename,
             {
               updateTextInput(session = session,
                               inputId = "MStylefilename2",
                               value = isolate(input$MStylefilename)
               )
             }
)

observeEvent(input[["MStylefilename2"]],
             {
               updateTextInput(session = session,
                               inputId = "MStylefilename",
                               value = isolate(input$MStylefilename2)
               )
             })

