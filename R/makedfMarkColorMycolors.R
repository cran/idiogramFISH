#' @name makedfMarkColorMycolors
#' @title FUNCTION to make a data.frame of Marks' characterisitcs
#'
#' @description This function reads character vector with marks names, a
#' character vector of
#' prohibited colors and a custom list of colors
#' to be associated with the mark names
#' @description It returns a data.frame with color and style for each mark
#'
#' @param markNames names of marks
#' @param colorstoremove character vector of colors to remove from mycolors. Default \code{NULL}
#' @param mycolors character vector of names of colors
#' @param defaultStyleMark character vector with default style \code{"square"}. Other options:  \code{"squareLeft"}, \code{"cM"}
#' \code{"cMLeft"}, \code{"dots"}, \code{"upArrow"}, \code{"downArrow"}, \code{"cenStyle"}
#'
#' @keywords mark create
#' @importFrom grDevices col2rgb
#' @export
#' @return data.frame
#'
makedfMarkColorMycolors<- function(markNames, mycolors, colorstoremove=NULL, defaultStyleMark="square"){

  dfMarkColor<-idiogramFISH::dfMarkColor

  message(crayon::green(paste("By default 5S are plotted as dots, to change this behavior make your own dfMarkColor data.frame") )
          ) # m

  tryCatch(dfMarkColor[which(dfMarkColor$style %in% "square"),]$style<-defaultStyleMark, error=function(e) "")

  mycolors<-mycolors[!mycolors %in% colorstoremove]

  mycolors<-mycolors[sapply(mycolors, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) {message(crayon::red(paste("Color",X,"invalid, removed from mycolors") ) ); return(FALSE) })
  } )]

  dfMarkColorNew<-data.frame(markName=markNames)
  dfMarkColorNew$markNameNP <- dfMarkColorNew$markName
  dfMarkColorNew$markNameNP <- gsub("^inProtein","",dfMarkColorNew$markNameNP)
  dfMarkColorNew$markColor <- NA

  vecNP<-unique(dfMarkColorNew$markNameNP)
  lenmandf<-length(vecNP)

  if(length(mycolors)<lenmandf){
    message(crayon::red(paste("Not enough colors in mycolor parameter, will be recycled") ) )
    repF<-ceiling(lenmandf/length(mycolors) )
    mycolors<-rep(mycolors,repF)
  }

  mycolors <- mycolors[1:lenmandf]
  names(mycolors)<- vecNP

  dfMarkColorNew$markColor <- mycolors[ match( dfMarkColorNew$markNameNP,names(mycolors)  ) ]

  dfMarkColorNew$style<-dfMarkColor$style[match(toupper(dfMarkColorNew$markNameNP),toupper(dfMarkColor$markName) )]

  dfMarkColorNew$style[which(is.na(dfMarkColorNew$style))]<-defaultStyleMark

  tryCatch(dfMarkColorNew[which(dfMarkColorNew$markName %in%
                         grep("inProtein",dfMarkColorNew$markName,value=TRUE) ),]$style<-"inProtein"
           ,error=function(e){""} )
  return(dfMarkColorNew)
}



