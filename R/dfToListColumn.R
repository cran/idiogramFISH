#' dfToListColumn
#' This is an internal function that makes list of d.fs
#'
#' @keywords internal
#'
#' @param dfLookColumn df with or without Column to list
#' @param columnToSplit character, column name to split
#'
#' @return list

dfToListColumn<-function(dfLookColumn,columnToSplit="OTU"){

  if(columnToSplit %in% colnames(dfLookColumn)){
    listOfdfLookColumn <- base::split(dfLookColumn, factor(dfLookColumn[,columnToSplit],levels = unique(dfLookColumn[,columnToSplit])  ) )
    names(listOfdfLookColumn) <- unique(dfLookColumn[,columnToSplit])
  } else {
    listOfdfLookColumn <- list(dfLookColumn)
    names(listOfdfLookColumn)<-1
  }

return(listOfdfLookColumn)

}
