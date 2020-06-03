#' Function to automatically make a data.frame of Marks' characterisitcs
#'
#' @description This function reads character vector with marks names
#' @description It returns a data.frame with color and style for each mark
#'
#' @param markNames names of marks
#' @param dfMarkColor d.f of marks to use
#' @param colorstoremove remove this colors from the result
#' @param defaultStyleMark character vector with default style \code{"square"} \code{"cM"} \code{"dots"}
#'
#' @keywords mark
#' @keywords internal
#' @return data.frame
#' @importFrom grDevices col2rgb

makedfMarkColor<- function(dfMarkColor,markNames,colorstoremove=NA,defaultStyleMark="square"){

  message(crayon::green(paste("By default 5S are plotted as dots, to change this behavior make your own dfMarkColor data.frame") ) )

  dfMarkColor[which(dfMarkColor$style %in% "square"),]$style<-defaultStyleMark

  manualcolors<-c('black','forestgreen', 'orange', 'cornflowerblue',
                    'magenta', 'darkolivegreen4',
                    'indianred1', 'tan4', 'darkblue', 'red2',
                    'mediumorchid1','firebrick4',  'yellowgreen', 'lightsalmon', 'tan3',
                    "tan1",'darkgray', 'wheat4', '#DDAD4B', 'chartreuse', 'seagreen1',
                    'moccasin', 'mediumvioletred', 'seagreen','cadetblue1',
                    "darkolivegreen1" ,"tan2" ,   "tomato3" , "#7CE3D8","gainsboro")

  manualcolors<-manualcolors[!manualcolors %in% colorstoremove]

  dfMarkColorNew<-data.frame(markName=markNames)

  dfMarkColorNew$markColor<-NA

  # lenmandf <- length( dfMarkColor$markColor[which(dfMarkColor$markColor %in% colorstoremove)] )
  # dfMarkColor$markColor[which(dfMarkColor$markColor %in% colorstoremove)]<-manualcolors[1:lenmandf]

  manualcolors <- manualcolors[!manualcolors %in% c(dfMarkColor$markColor)]

  dfMarkColorNew$markColor<-dfMarkColor$markColor[match(toupper(dfMarkColorNew$markName),toupper(dfMarkColor$markName) )]

  lenman <- length(dfMarkColorNew$markColor[which(is.na(dfMarkColorNew$markColor))])

  if(length(manualcolors) < lenman){
    message(crayon::red(paste("Not enough colors, will be recycled - you can pass more colors with mycolors parameter") ) )
    repF<-ceiling(lenman/length(manualcolors) )
    manualcolors<-rep(manualcolors,repF)
  }

  dfMarkColorNew$markColor[which(is.na(dfMarkColorNew$markColor))] <- manualcolors[1:lenman]

  dfMarkColorNew$style <- dfMarkColor$style[match(toupper(dfMarkColorNew$markName),toupper(dfMarkColor$markName) )]

  dfMarkColorNew$style[which(is.na(dfMarkColorNew$style))]<-defaultStyleMark

  return(dfMarkColorNew)
}



