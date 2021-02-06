# makeNumCols and makeCharCols
#' These are internal functions that eliminate factors
#'
#' It returns a data.frame
#'
#' @keywords internal
#'
#' @param df data.frame
#'
#' @return data.frame
#'
makeNumCols<-function(df){
  df<-as.data.frame(df)
  df[] <- lapply(df, as.character)
  cond <- apply(df, 2, function(x) {
    x <- x[!is.na(x)]
    all(suppressWarnings(!is.na(as.numeric(x))))
  })
  numeric_cols <- names(df)[cond]
  df[,numeric_cols] <- sapply(df[,numeric_cols], as.numeric)
  # if("chrName" %in% colnames(df)) {
    # df$chrName<-as.character(df$chrName)
  # }
  return(df)
}

makeCharCols<-function(df){
  df<-as.data.frame(df)
  df[] <- lapply(df, as.character)
  return(df)
}
