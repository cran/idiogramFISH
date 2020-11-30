#' @name mapGGChr
#' @title FUNCTIONS mapGGChr and mapGGChrMark (for ggplot)
#' @aliases mapGGChrMark
#' @description Currently works for holocentrics when only one OTU. See vignette.
#' @description mapGGChr: reads a data.frame and produces coordinates for ggplot of chr.
#' @description mapGGChrMark: reads data.frames and produces data.frames for ggplot of chr. and marks
#'
#' @param dfChrSize character, name of data.frame with columns: chrSize
#' @param chrSpacing numeric, \code{1 - chrSpacing} will be the width of chr.
#' @param dfMarkPos (\code{mapGGChrMark}) name of data.frame of marks
#' @param squareness numeric, squareness
#' @param n numeric, vertices number for rounded portions
#'
#' @keywords data.frame size arm
#'
#' @return list
#' @rdname mapGGChr
#' @export
#'
mapGGChr <- function(dfChrSize,chrSpacing=0.5,squareness=4, n= 50) {

  starts_with <- NULL

if(chrSpacing > 0.95){
  message(crayon::green("Use < 0.95 in chrSpacing") )
  chrSpacing <- 0.95
}
  # chrSpacing <-.5
chrWidth   <- 1 - chrSpacing

  if(requireNamespace("tidyr")==FALSE){
    message(crayon::red("You need to install tidyr"))
    return(NULL)
  } else {
    requireNamespace("tidyr")
  }

chromosome_ns  <- nrow(dfChrSize)
chromRepVector <- rep(1,chromosome_ns)

croytop <- list()

for (i in 1:length(chromRepVector)) {
  croytop[i] <- list(c(   ( (dfChrSize[,"chrSize"])[i]   ),
                rep(0,2),
                ( (dfChrSize[,"chrSize"])[i]  )
  )
  )
}

xm <- matrix(rep(NA,length(chromRepVector)*4),ncol=4, nrow=length(chromRepVector) )

for (i in 1:length(chromRepVector)) {
  xm[i,] <- c(
    rep( ( (chrSpacing*i+( i*chrWidth))+chrWidth),2),
    rep( ( (chrSpacing*i+( i*chrWidth))     ),2)
  ) - chrWidth/2
}

x  <- base::split(xm, row(xm))

ym <- t(sapply ( croytop, function (x) {length (x) ; return (x)}) )

y  <- base::split(ym, row(ym))

  xbind <- as.data.frame(do.call(rbind, x ) )

  xbind$Chr <- seq_along(xbind$V1)
  xDat <- tidyr::pivot_longer(xbind, names_to="variable.x", values_to="x", cols=starts_with("V")  )

  ybind <- as.data.frame(do.call(rbind, croytop ) )
  ybind$id.y <- seq_along(ybind$V1)
  yDat <- tidyr::pivot_longer(ybind, names_to="variable.y", values_to="y", cols=starts_with("V")  )

  dataChr <- cbind(xDat, yDat)
  dataChr <- dataChr[,c("Chr","x","y")]

mapChrL <- list()
mapChrL$dataChr<-dataChr
mapChrL$xm  <- xm
mapChrL$ym  <- ym
mapChrL$chrWidth  <- chrWidth

  if(squareness < 20 ) {

    r2 <- chrWidth / (squareness * 2)

    yfactor <-  max(dfChrSize$chrSize) /  chromosome_ns

    xyCoords <- makeRoundCoordXY(r2, yfactor, x, y, 1, length(y), n )

    xbind <- as.data.frame(do.call(rbind, xyCoords$roundedX ) )

    xbind$Chr <- seq_along(xbind$V1)

    ybind <- as.data.frame(do.call(rbind, xyCoords$roundedY ) )

    ybind$id.y <- seq_along(ybind$V1)

    xDat <- tidyr::pivot_longer(xbind, names_to="variable.x", values_to="x", cols = starts_with("V")  )

    yDat <- tidyr::pivot_longer(ybind, names_to="variable.y", values_to="y", cols = starts_with("V")  )

    dataChr <- cbind(xDat, yDat)

    dataChr <- dataChr[,c("Chr","x","y") ]

    mapChrL$dataChr <- dataChr
    mapChrL$rounded <- TRUE

    return(mapChrL)
  } else {
    return(mapChrL)
  }
}
#'
#' @rdname mapGGChr
#' @return list
#' @export
#'

mapGGChrMark <- function(dfChrSize,dfMarkPos,chrSpacing=0.5, squareness=4, n=50) {

  starts_with <- NULL

  mapChrL <- mapGGChr(dfChrSize, chrSpacing,squareness,n)

  if(requireNamespace("tidyr")==FALSE){
    message(crayon::red("You need to install tidyr"))
    return(NULL)
  } else {
    requireNamespace("tidyr")
  }

yMark1<-xMark1<-xMark2<-NULL

markNames<-character()

for (m in 1:nrow(dfMarkPos)) {

  rowIndex <- dfMarkPos[,"chrName"][m]

  markName <- dfMarkPos[,"markName"][m]

  chrStart <- mapChrL$ym[rowIndex ,2]

  yinf <- chrStart + dfMarkPos[m,"markPos"]

  ysup <- chrStart + sum(dfMarkPos[m,"markPos"],dfMarkPos[m,"markSize"],na.rm=TRUE )

  yMark1[[m]] <- c(ysup,yinf,yinf,ysup)

  xMark1[[m]] <- mapChrL$xm[dfMarkPos[,"chrName"][m],]

  xMark2[[m]] <- c(xMark1[[m]],markName)

  markNames <- c(markNames,markName)
}

xMarkSqDF <- as.data.frame(do.call(rbind, xMark2 ) )

colnames(xMarkSqDF)[5] <- "markName"

xMarkSqDF$id <- seq_along(xMarkSqDF$V1)

# xDatMark <- reshape2::melt(xMarkSqDF, measure.vars = c("V1","V2","V3","V4" ) ) # melt longer cast wider
# xDatMark <- tidyr::gather(xMarkSqDF, key="variable", value="x", "V1","V2","V3","V4"  )
xDatMark <- tidyr::pivot_longer(xMarkSqDF, names_to= "variable", values_to="x", cols=starts_with("V")  )

xDatMark$x<-as.numeric(xDatMark$x)

yMark1DF <- as.data.frame(do.call(rbind, yMark1 ) )
yMark1DF$id.y <- seq_along(yMark1DF$V1)
yDatMark <- tidyr::pivot_longer(yMark1DF, names_to="variable.y", values_to="y", cols=starts_with("V")  )
dataMark <- cbind(xDatMark,yDatMark)
dataMark <- dataMark[,c("id","markName","x","y")]

mapChrL$dataMark <- dataMark

  if(squareness < 20 ) {

    chromosome_ns  <- nrow(dfChrSize)

    r2 <- mapChrL$chrWidth  / (squareness * 2)

    yfactor <-  max(dfChrSize$chrSize) /  chromosome_ns

    xyCoords <- makeRoundCoordXY(r2, yfactor, xMark1, yMark1, 1, length(yMark1), n )

    xMarkSqDF <- as.data.frame(do.call(rbind, xyCoords$roundedX ) )
    xMarkSqDF$markName <- markNames

    xMarkSqDF$id <- seq_along(xMarkSqDF$V1)

    xDatMark <- tidyr::pivot_longer(xMarkSqDF, names_to= "variable", values_to="x", cols=starts_with("V")  )

    yMark1DF <- as.data.frame(do.call(rbind, xyCoords$roundedY ) )

    yMark1DF$id.y <- seq_along(yMark1DF$V1)
    yDatMark <- tidyr::pivot_longer(yMark1DF, names_to="variable.y", values_to="y", cols=starts_with("V")  )
    dataMark <- cbind(xDatMark,yDatMark)
    dataMark<-dataMark[,c("id","markName","x","y")]
    mapChrL$dataMark <- dataMark
    return(mapChrL)
  } else {
    return(mapChrL)
  }
}

# names(mapChrL)

