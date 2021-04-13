# removeFactorMark  update reactivs

observeEvent(input$removeFactorMark,{
  req(inherits(values[["df1Mark"]],"data.frame") )
  req(nrow(values[["df1Mark"]])>0 )
  values[["df1Mark"]] <- tryCatch(idiogramFISH:::makeNumCols(values[["df1Mark"]])
                                  ,error=function(e){data.frame() })
})

observeEvent(input$removeFactorChr,{
  req(inherits(values[["df1"]],"data.frame") )
  req(nrow(values[["df1"]])>0 )
  values[["df1"]] <- tryCatch(idiogramFISH:::makeNumCols(values[["df1"]])
                              ,error=function(e){data.frame() })
})


#
#   add or remove columns chr
#

observeEvent(input$addColumn,{

  final <- values[["df1"]]
  if(inherits(final,"data.frame")) {
    if(nrow(final)>0) {
      if(input$colType=="character"){
        final[,input$col] <- as.character(NA)
      } else {
        final[,input$col] <- as.numeric(NA)
      }
    } else {
      final <- data.frame(chrName="chrName"
                          ,shortArmSize="shortArmSize"
                          ,longArmSize="longArmSize"
                          ,chrSize="chrSize"
                          ,OTU="1"
      )
    }
  } else {

    final <- data.frame(chrName="chrName"
                        ,shortArmSize="shortArmSize"
                        ,longArmSize="longArmSize"
                        ,chrSize="chrSize"
                        ,OTU=1
    )

  }
  values[["df1"]] <- final
})

observeEvent(input$removeColumn,{
  final <- values[["df1"]]
  final[,input$col] <- NULL
  values[["df1"]] <- final
})

observeEvent(input$addColumnMark,{

  final <- values[["df1Mark"]]
  if(inherits(final,"data.frame")){
    if(nrow(final)>0) {
      if(input$colTypeMark=="character"){
        final[,input$colMark] <- as.character(NA)
      } else {
        final[,input$colMark] <- as.numeric(NA)
      }
    } else { # if no rows

      final <- data.frame(chrName="chrName"
                          ,markName="markName"
                          ,chrRegion="chrRegion"
                          ,markPos="pos."
                          ,markSize="markSize")

      if("OTU" %in% colnames( values[["df1"]] )) {
        final$OTU <-as.character(NA)
      }
    }

  } else {

    final <- data.frame(chrName="chrName"
                        ,markName="markName"
                        ,chrRegion="chrRegion"
                        ,markPos="pos."
                        ,markSize="markSize")

    if("OTU" %in% colnames( values[["df1"]] )) {
      final$OTU <-as.character(NA)
    }
  }
  values[["df1Mark"]] <- final
})


observeEvent(input$removeColumnMark,{

  final <- values[["df1Mark"]]
  final[,input$colMark] <- NULL
  values[["df1Mark"]] <- final
})

#
#   add or remove columns mark style
#

observeEvent(input$addColumnMStyle,{

  final <- values[["df1MStyle"]]
  if(inherits(final,"data.frame")){
    if(nrow(final)>0){
      if(input$colTypeMStyle=="character"){
        final[,input$colMStyle] <- as.character(NA)
      } else {
        final[,input$colMStyle] <- as.numeric(NA)
      }
    } else {
      final <- data.frame(markName="markName",markColor="colorName",style="dots")
    }
  } else {
    final <- data.frame(markName="markName",markColor="colorName",style="dots")
  }
  values[["df1MStyle"]] <- final
})

observeEvent(input$removeColumnMStyle,{

  final <- values[["df1MStyle"]]
  final[,input$colMStyle] <- NULL
  values[["df1MStyle"]] <- final
})
