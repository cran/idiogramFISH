# output download dataframes

#
#   download chr.
#

output$buttontable <-  renderUI({
  validate(
    need(try(values[["df1"]]), "") #was df
  )
  downloadButton('downloadCsv', 'Download table as .csv')
})

output$buttontable2 <-  renderUI({
  validate(
    need(try(values[["df1"]]), "") #wdf
  )
  if(input$saveType=="csv"){
    downloadButton('downloadCsv2', 'Download table as .csv')
  } else {
    downloadButton('downloadRds2', 'Download table as .rds')
  }
})

#
#   download mark
#

output$buttontableMark <-  renderUI({
  validate(
    need(try(values[["df1Mark"]]), "")
  )
  if (nrow(values[["df1Mark"]]) > 0 ) {
    downloadButton('downloadCsvMark', 'Download table as .csv')
  }
})

output$buttontableMark2 <-  renderUI({
  validate(
    need(try(values[["df1Mark"]]), "")
  )
  if (nrow(values[["df1Mark"]]) > 0 ) {
    if(input$saveType=="csv"){
      downloadButton('downloadCsvMark2', 'Download table as .csv')
    } else {
      downloadButton('downloadRdsMark2', 'Download table as .rds')
    }
  }
})

#
#   download mark style
#

output$buttontableMStyle <-  renderUI({
  validate(
    need(try(values[["df1MStyle"]]), "")
  )
  if (nrow(values[["df1MStyle"]]) > 0 ) {
    downloadButton('downloadCsvMStyle', 'Download table as .csv')
  }
})

output$buttontableMStyle2 <-  renderUI({
  validate(
    need(try(values[["df1MStyle"]]), "")
  )
  if (nrow(values[["df1MStyle"]]) > 0 ) {
    if(input$saveType=="csv"){
      downloadButton('downloadCsvMStyle2', 'Download table as .csv')
    } else {
      downloadButton('downloadRdsMStyle2', 'Download table as .rds')
    }
  }
})

#
#   downloadHandler chr
#

output$downloadCsv <- downloadHandler(
  filename = function() {
    # paste0(input$chrfilename)
    paste0(input$chrfilename,".csv")
  },
  content = function(file) {
    write.csv(values[["df1"]], file, na="", # wdf
              row.names = FALSE, quote = TRUE)
  }
)

output$downloadCsv2 <- downloadHandler(
  filename = function() {
    paste0(input$chrfilename2,".csv")
  },
  content = function(file) {
    write.csv(values[["df1"]], file, na="", # wdf
              row.names = FALSE, quote = TRUE)
  }
)

output$downloadRds2 <- downloadHandler(
  filename = function() {
    paste0(input$chrfilename2,".rds")
  },
  content = function(file) {
    saveRDS(values[["df1"]], file,
    )
  }
)

#
#   downloadHandler Mark
#

output$downloadCsvMark <- downloadHandler(
  filename = function() {
    # paste0(input$markfilename)
    paste0(input$markfilename,".csv")
  },
  content = function(file) {
    write.csv(values[["df1Mark"]], file, na="",
              row.names = FALSE, quote = TRUE)
  }
)

output$downloadCsvMark2 <- downloadHandler(
  filename = function() {
    paste0(input$markfilename2,".csv")
  },
  content = function(file) {
    write.csv(values[["df1Mark"]], file, na="",
              row.names = FALSE, quote = TRUE)
  }
)

output$downloadRdsMark2 <- downloadHandler(
  filename = function() {
    paste0(input$markfilename2,".rds")
  },
  content = function(file) {
    saveRDS(values[["df1Mark"]], file,
    )
  }
)


#
#   downloadHandler MStyle style
#

output$downloadCsvMStyle <- downloadHandler(
  filename = function() {
    # paste0(input$MStylefilename)
    paste0(input$MStylefilename,".csv")
  },
  content = function(file) {
    write.csv(values[["df1MStyle"]], file, na="",
              row.names = FALSE, quote = TRUE)
  }
)

output$downloadCsvMStyle2 <- downloadHandler(
  filename = function() {
    paste0(input$MStylefilename2,".csv")
  },
  content = function(file) {
    write.csv(values[["df1MStyle"]], file, na="",
              row.names = FALSE, quote = TRUE)
  }
)

output$downloadRdsMStyle2 <- downloadHandler(
  filename = function() {
    paste0(input$MStylefilename2,".rds")
  },
  content = function(file) {
    saveRDS(values[["df1MStyle"]], file,
    )
  }
)

#
#   render chr table
#

output$out <- renderRHandsontable({
  req(CurrentM$menu=="DFsMenu",Current$Tab=="dfChrTab")
  rhandsontable(values[["df1"]] ) %>%#, width = 700)
  hot_cols(columnSorting = TRUE)
})

#
#   render mark table
#
output$outMark <- renderRHandsontable({
  req(CurrentM$menu=="DFsMenu",Current$Tab=="dfMarkTab")
  rhandsontable(values[["df1Mark"]]) %>% #, width = 700)
    hot_cols(columnSorting = TRUE)
})

#
#   render mark style table
#
output$outMStyle <- renderRHandsontable({
  req(CurrentM$menu=="DFsMenu",Current$Tab=="dfMSTab")
  validate(need(try(values[["df1MStyle"]] )
                ,"Hint: Colors can be added in next page under 'Color' (optional)"
  ))
  if(invalid(values[["df1MStyle"]] ) ) {
    rhandsontable(values[["df1MStyle"]])
  } else if( nrow(values[["df1MStyle"]]) > 0 & "style" %in% colnames(values[["df1MStyle"]] ) ) {

    Options<-c("dots","square","squareLeft", "cM","cMLeft","cenStyle", "upArrow", "downArrow"
               ,"exProtein","inProtein")

    tmpExampleTable <- rhandsontable(values[["df1MStyle"]],
                                     rowHeaders = NULL,
                                     stretchH = "all",
                                     selectCallback = TRUE
                                     , width = 400, height = 400
    ) %>%
      hot_col("style", allowInvalid = FALSE, type = "dropdown"
              , readOnly = TRUE)
    if(!is.null(input$outMStyle_select$select$r)) {
      tmpExampleTable <- hot_col(tmpExampleTable,
                                 col = "style",
                                 allowInvalid = FALSE,
                                 type = "dropdown",
                                 source = Options) %>%
        hot_cell(row = input$outMStyle_select$select$r
                 , col = "style",
                 readOnly = FALSE)
    }
    tmpExampleTable %>% hot_cols(columnSorting = TRUE)
  }
})


output$outUI<-renderUI({
  req(CurrentM$menu=="DFsMenu",Current$Tab=="dfChrTab")
  rHandsontableOutput("out")
})

output$outMarkUI<-renderUI({
  req(CurrentM$menu=="DFsMenu",Current$Tab=="dfMarkTab")
  rHandsontableOutput("outMark")
})

output$outMStyleUI<-renderUI({
  req(CurrentM$menu=="DFsMenu",Current$Tab=="dfMSTab")
  rHandsontableOutput("outMStyle")
})
