#' Function to automatically make a data.frame of Marks' characterisitcs
#'
#' @description This function reads character vector with marks names, a 
#' character vector of 
#' prohibited colors and a custom list of colors
#' to be associated with the mark names
#' @description It returns a data.frame with color and style for each mark
#'
#' @param markNames names of marks
#' @param colorstoremove character vector of colors to remove from mycolors
#' @param mycolors character vector of names of colors
#' @param defaultStyleMark character vector with default style \code{"square"} 
#' \code{"cM"} 
#' \code{"dots"}
#'
#' @keywords mark
#' @keywords internal
#' @return data.frame
#' @importFrom grDevices col2rgb

makedfMarkColorMycolors<- function(markNames, mycolors, colorstoremove=NULL, defaultStyleMark="square"){
  dfMarkColor<-idiogramFISH::dfMarkColor
  message(crayon::green(paste("By default 5S are plotted as dots, to change this behavior make your own dfMarkColor data.frame") )
          ) # m
  dfMarkColor[which(dfMarkColor$style %in% "square"),]$style<-defaultStyleMark

  mycolors<-mycolors[!mycolors %in% colorstoremove]

  mycolors<-mycolors[sapply(mycolors, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) {message(crayon::red(paste("Color",X,"invalid, removed from mycolors") ) ); return(FALSE) })
  } )]

  dfMarkColorNew<-data.frame(markName=markNames)
  dfMarkColorNew$markColor<-NA

  lenmandf<-nrow(dfMarkColorNew)
  if(length(mycolors)<lenmandf){
    message(crayon::red(paste("Not enough colors in mycolor parameter, will be recycled") ) )
    repF<-ceiling(lenmandf/length(mycolors) )
    mycolors<-rep(mycolors,repF)
  }
  dfMarkColorNew$markColor<-mycolors[1:lenmandf]
  dfMarkColorNew$style<-dfMarkColor$style[match(toupper(dfMarkColorNew$markName),toupper(dfMarkColor$markName) )]
  dfMarkColorNew$style[which(is.na(dfMarkColorNew$style))]<-defaultStyleMark
  return(dfMarkColorNew)
}



