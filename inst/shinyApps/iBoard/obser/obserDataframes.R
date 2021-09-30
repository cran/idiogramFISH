#
#   loadDFbutton is a Nuccore button
#

observeEvent(input$loadDFbutton, ignoreInit = T, {
  values[["df1MStyleName"]]   <- values[["df1MarkName"]]   <- values[["df1Name"]]   <- "Nucleotide tab"
  values[["df1MStyleOrigin"]] <- values[["df1MarkOrigin"]] <- values[["df1Origin"]]  <- "data.frame was loaded"
}
)

# update all origins

observeEvent(#values[["number"]]
             list(input$nucPresetButton,input$exampleButton)
             , ignoreInit = T, {
               if(values[["number"]]>0){
  values[["df1MStyleOrigin"]]   <- values[["df1MarkOrigin"]] <- values[["df1Origin"]] <- "data.frame was loaded"
  values[["notesOrigin"]]       <- "data.frame was loaded"
  values[["leftNotesOrigin"]]   <- "data.frame was loaded"
  values[["leftNotesUpOrigin"]] <- "data.frame was loaded"
               }
})

#
#   update chr. data.frame
#

observeEvent(#values[["number"]]
             list(input$nucPresetButton,input$exampleButton)
             , ignoreInit = T, {
               if(values[["number"]]>0){
  values[["df1"]] <- data.frame()
  Sys.sleep(0.1)
  df <- paramVec$dfChrSizeVec[as.numeric(values[["number"]]) ]
  tryCatch(load(paste0("www/rda/",df,".rda") ), error=function(e){""}, warning = function(w){""} )
  values[["df1"]]       <- get(df)
  values[["df1Name"]]   <- ifelse(df=="emptydata.frame","no",df )
               }
})

observeEvent(input$file1, {
  inFile <- input[["file1"]]
  values[["df1"]] <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
  values[["df1Name"]] <- inFile$name
  values[["df1Origin"]] <- "file was loaded"
})

#
#   update notes data.frame
#

observeEvent(#values[["number"]]
             list(input$nucPresetButton,input$exampleButton)
             , ignoreInit = T, {
               if(values[["number"]]>0){
  values[["notes"]] <- data.frame()
  Sys.sleep(0.1)
  df <- paramVec$notesVec[as.numeric(values[["number"]]) ]
  tryCatch(load(paste0("www/rda/",df,".rda") ), error=function(e){""}, warning = function(w){""} )
  values[["notes"]]       <- get(df)
  values[["notesName"]]   <- ifelse(df=="emptydata.frame","no",df )
               }
})

observeEvent(input$file1Notes, {
  inFile <- input[["file1Notes"]]
  values[["notes"]] <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
  values[["notesName"]] <- inFile$name
  values[["notesOrigin"]] <- "file was loaded"
})

#
#   update leftNotes data.frame
#

observeEvent(#values[["number"]]
             list(input$nucPresetButton,input$exampleButton)
             , ignoreInit = T, {
               if(values[["number"]]>0){
  values[["leftNotes"]] <- data.frame()
  Sys.sleep(0.1)
  df <- paramVec$leftNotesVec[as.numeric(values[["number"]]) ]
  tryCatch(load(paste0("www/rda/",df,".rda") ), error=function(e){""}, warning = function(w){""} )
  values[["leftNotes"]]       <- get(df)
  values[["leftNotesName"]]   <- ifelse(df=="emptydata.frame","no",df )
               }
})

observeEvent(input$file1leftNotes, {
  inFile <- input[["file1leftNotes"]]
  values[["leftNotes"]] <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
  values[["leftNotesName"]] <- inFile$name
  values[["leftNotesOrigin"]] <- "file was loaded"
})

#
#   update leftNotesUp data.frame
#

observeEvent(#values[["number"]]
             list(input$nucPresetButton,input$exampleButton)
             , ignoreInit = T, {
               if(values[["number"]]>0){
  values[["leftNotesUp"]] <- data.frame()
  Sys.sleep(0.1)
  df <- paramVec$leftNotesUpVec[as.numeric(values[["number"]]) ]
  tryCatch(load(paste0("www/rda/",df,".rda") ), error=function(e){""}, warning = function(w){""} )
  values[["leftNotesUp"]]       <- get(df)
  values[["leftNotesUpName"]]   <- ifelse(df=="emptydata.frame","no",df )
               }
})

observeEvent(input$file1leftNotesUp, {
  inFile <- input[["file1leftNotesUp"]]
  values[["leftNotesUp"]] <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
  values[["leftNotesUpName"]] <- inFile$name
  values[["leftNotesUpOrigin"]] <- "file was loaded"
})


#
#   update mark pos data.frame
#

observeEvent(#values[["number"]]
             list(input$nucPresetButton,input$exampleButton)
             , ignoreInit = T, {
               if(values[["number"]]>0){
  values[["df1Mark"]] <- data.frame()
  Sys.sleep(0.1)
  df <- paramVec$dfMarkPosVec[as.numeric(values[["number"]])]
  tryCatch(load(paste0("www/rda/",df,".rda") ), error=function(e){""}, warning = function(w){""} )
  values[["df1Mark"]] <- get( df )
  values[["df1MarkName"]] <- ifelse(df=="emptydata.frame","no",df )
               }
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

observeEvent(#values[["number"]]
             list(input$nucPresetButton,input$exampleButton)
             , ignoreInit = T, {
               if(values[["number"]]>0){
  values[["df1MStyle"]] <- data.frame()
  df <- paramVec$dfMarkColorVec[as.numeric(values[["number"]])]
  tryCatch(load(paste0("www/rda/",df,".rda") ), error=function(e){""}, warning = function(w){""} )
  values[["df1MStyle"]] <- get( df )
  values[["df1MStyleName"]] <- ifelse(df=="emptydata.frame","no",df )
               }
})

observeEvent(input$file1MStyle, {
  inFile <- input[["file1MStyle"]]
  values[["df1MStyle"]] <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE )
  values[["df1MStyleName"]] <- inFile$name
  values[["df1MStyleOrigin"]] <- "file was loaded"
})

#
#   keep inputs notes
#

observeEvent(input$outNotes, {
  if (!is.null(input$outNotes)) {
    tryCatch(df <- hot_to_r(input$outNotes), error = function(e) {data.frame() } )
    if (!is.function(df)) {
      values[["notes"]] <- df
    }
  }
} )

#
#   keep inputs leftNotes
#

observeEvent(input$outleftNotes, {
  if (!is.null(input$outleftNotes)) {
    tryCatch(df <- hot_to_r(input$outleftNotes), error = function(e) {data.frame() } )
    if (!is.function(df)) {
      values[["leftNotes"]] <- df
    }
  }
})

#
#   keep inputs leftNotesUp
#

observeEvent(input$outleftNotesUp, {
  if (!is.null(input$outleftNotesUp)) {
    tryCatch(df <- hot_to_r(input$outleftNotesUp), error = function(e) {data.frame() } )
    if (!is.function(df)) {
      values[["leftNotesUp"]] <- df
    }
  }
} )

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
  tryCatch(df <- hot_to_r(input$outMStyle), error = function(e) {data.frame() } )
  if (!is.function(df)) {
    values[["df1MStyle"]] <- df
  }
  # }
})

#
#   update filenames among analogous inputs
#

observeEvent(input$notesfilename,
             {
               updateTextInput(session = session,
                               inputId = "notesfilename2",
                               value = isolate(input$notesfilename)
               )
             })

observeEvent(input[["notesfilename2"]],
             {
               updateTextInput(session = session,
                               inputId = "notesfilename",
                               value = isolate(input$notesfilename2)
               )
             })

observeEvent(input$chrfilename,
             {
               updateTextInput(session = session,
                               inputId = "chrfilename2",
                               value = isolate(input$chrfilename)
               )
             })

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

