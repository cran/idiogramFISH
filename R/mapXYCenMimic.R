#' mapXYCenMimic
#' This is an internal function that creates coords for chr. or arms or marks
#'
#' @keywords internal
#'
#' @param start 1st index
#' @param end last index
#' @param y y coords
#' @param yfactor y x factor
#' @param r2 r2ius
#' @param pts points of squareness
#' @param cenFormat boolean
#'
#' @return list

mapXYCenMimic <- function(start,end,y,x,yfactor,r2,pts,cenFormat="triangle"){

  x2_1<-list()
  x2_2<-list()

  xy_1<-list()
  xy_2<-list()

  xy_3<-list() # NEW
  xy_4<-list() # NEW

  roundedX<-list()
  roundedY<-list()

  for (counter in start: end ) {
    r2backup<-r2
    diffx<-max(x[[counter]]) - min(x[[counter]])
    diffy<-max(y[[counter]]) - min(y[[counter]])
    ratexy<-diffx/diffy
    ifelse( (diffx/r2) * 2 < ratexy*4 ,  r2 <- diffx/(ratexy*2) ,r2 )

    ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

      x2_1 <- min(x[[counter]])+r2
      x2_2 <- max(x[[counter]])-r2

      minY<-min(y[[counter]])
      maxY<-max(y[[counter]])

      xy_1 <- cbind( x2_1 + r2 * sin(ptsl[[1]]), minY + r2 * cos(ptsl[[1]]) )
      xy_2 <- cbind( x2_2 + r2 * sin(ptsl[[2]]), minY + r2 * cos(ptsl[[2]] ) *yfactor )
      xy_3 <- cbind( x2_2 + r2 * sin(ptsl[[3]]), maxY + r2 * cos(ptsl[[3]]))
      xy_4 <- cbind( x2_1 + r2 * sin(ptsl[[4]]), maxY + r2 * cos(ptsl[[4]]))

    if (cenFormat=="triangle"){

      roundedX[[counter]]<-c(rev(xy_3[,1]),rev(xy_2[,1]), xy_4[,1],xy_1[,1])
      roundedY[[counter]]<-c(rev(xy_3[,2]),rev(xy_2[,2]), xy_4[,2],xy_1[,2])

    } else {

      maxXmod<-max(x[[counter]])-r2
      minXmod<-min(x[[counter]])+r2

      maxYmod<-max(y[[counter]])-r2
      minYmod<-min(y[[counter]])+r2

      diffxMod<-maxXmod-minXmod
      diffyMod<-maxYmod-minYmod

      halfmaxXMod <- diffxMod/2
      halfmaxYMod <- diffyMod/2

      xy_1b <- cbind( minXmod+halfmaxXMod + halfmaxXMod * sin(ptsl[[1]]), minYmod + halfmaxYMod * cos(ptsl[[1]]))

      xy_2b <- cbind( minXmod+halfmaxXMod + halfmaxXMod * sin(ptsl[[2]]), minYmod + halfmaxYMod * cos(ptsl[[2]]))

      xy_3b <- cbind( minXmod+halfmaxXMod + halfmaxXMod * sin(ptsl[[3]]), maxYmod + halfmaxYMod * cos(ptsl[[3]]))

      xy_4b <- cbind( minXmod+halfmaxXMod + halfmaxXMod * sin(ptsl[[4]]), maxYmod + halfmaxYMod * cos(ptsl[[4]] ) )

      roundedX[[counter]]<-c(xy_1[,1],minXmod,xy_1b[,1], (xy_2b[,1]),xy_2[,1],(xy_3[,1] ),  (xy_3b[,1] ),minXmod+halfmaxXMod,xy_4b[,1],xy_4[,1]) # opposite of cen.
      roundedY[[counter]]<-c(xy_1[,2],minYmod,xy_1b[,2], (xy_2b[,2]),xy_2[,2],(xy_3[,2] ),  (xy_3b[,2] ),minYmod+halfmaxYMod,xy_4b[,2],xy_4[,2])

    }

    attr(roundedY[[counter]],"rowIndex")<-attr(y[[counter]],"rowIndex")
    attr(roundedX[[counter]],"rowIndex")<-attr(x[[counter]],"rowIndex")
    attr(roundedY[[counter]],"chrName1")<-attr(y[[counter]],"chrName1")
    attr(roundedX[[counter]],"chrName1")<-attr(x[[counter]],"chrName1")

  r2<-r2backup

  } # for counter

  roundXroundY<-list()

  roundXroundY$roundedX<-roundedX
  roundXroundY$roundedY<-roundedY

  return(roundXroundY)
}


mapXYCenMimicInside <- function(start,end,y,x,yfactor,r2,pts
                                ,cenFormat="triangle"
                                ) {

  x2_1<-list()
  x2_2<-list()

  xy_1<-list()
  xy_2<-list()

  xy_3<-list() # NEW
  xy_4<-list() # NEW

  roundedX<-list()
  roundedY<-list()

  for (counter in start: end ) {
    r2backup<-r2
    diffx<-max(x[[counter]]) - min(x[[counter]])
    diffy<-max(y[[counter]]) - min(y[[counter]])
    ratexy<-diffx/diffy
    ifelse( (diffx/r2) * 2 < ratexy*4 ,  r2 <- diffx/(ratexy*2) ,r2 )

    ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

    x2_1 <- min(x[[counter]])+r2
    x2_2 <- max(x[[counter]])-r2

    minY<-min(y[[counter]])
    maxY<-max(y[[counter]])
    maxX<-max(x[[counter]])

    xy_1 <- cbind( x2_1 + r2 * sin(ptsl[[1]]), minY + r2 * cos(ptsl[[1]]) )
    xy_2 <- cbind( x2_2 + r2 * sin(ptsl[[2]]), minY + r2 * cos(ptsl[[2]]) )
    xy_3 <- cbind( x2_2 + r2 * sin(ptsl[[3]]), maxY + r2 * cos(ptsl[[3]]) )
    xy_4 <- cbind( x2_1 + r2 * sin(ptsl[[4]]), maxY + r2 * cos(ptsl[[4]]) )

    if (cenFormat=="triangle") {

      roundedX[[counter]] <- c(rev(xy_3[,1]) # up right
                               ,maxX
                  ,rev(xy_4[,1])# up left
                  , xy_2[,1]# down
                  ,maxX
                  ,xy_1[,1]
      )
      roundedY[[counter]] <- c(rev(xy_3[,2]) #
                               ,maxY
                  ,rev(xy_4[,2])#
                  , xy_2[,2]#
                  ,minY
                  ,xy_1[,2]
      )

      } else {

        maxXmod<-max(x[[counter]])-r2
        minXmod<-min(x[[counter]])+r2

        maxYmod<-max(y[[counter]])-r2
        minYmod<-min(y[[counter]])+r2

        diffxMod<-maxXmod-minXmod
        diffyMod<-maxYmod-minYmod

        halfmaxXMod <- diffxMod/2
        halfmaxYMod <- diffyMod/2

        xy_1b <- cbind( minXmod+halfmaxXMod + halfmaxXMod * sin(ptsl[[1]]), minYmod + halfmaxYMod * cos(ptsl[[1]]))

        xy_2b <- cbind( minXmod+halfmaxXMod + halfmaxXMod * sin(ptsl[[2]]), minYmod + halfmaxYMod * cos(ptsl[[2]]))

        xy_3b <- cbind( minXmod+halfmaxXMod + halfmaxXMod * sin(ptsl[[3]]), maxYmod + halfmaxYMod * cos(ptsl[[3]]))

        xy_4b <- cbind( minXmod+halfmaxXMod + halfmaxXMod * sin(ptsl[[4]]), maxYmod + halfmaxYMod * cos(ptsl[[4]] ) )

        roundedX[[counter]]<-c(maxX, (xy_3[,1] )  ,  (xy_3b[,1] )
                    ,rev(xy_1b[,1]) #,minXmod
                    ,rev(xy_1[,1]) # down left
                    ,maxX
                    ,rev(xy_2[,1]),rev(xy_2b[,1]) # down right
                    ,xy_4b[,1],xy_4[,1] #top left
        ) # opposite of cen.
        roundedY[[counter]] <-c(maxY,(xy_3[,2] )  ,  (xy_3b[,2] )
                    ,rev(xy_1b[,2]) # ,minYmod
                    ,rev(xy_1[,2])
                    ,minY
                    ,rev(xy_2[,2]),rev(xy_2b[,2]) # down right
                    ,xy_4b[,2],xy_4[,2]
        )
    } # else rC

    attr(roundedY[[counter]],"rowIndex")<-attr(y[[counter]],"rowIndex")
    attr(roundedX[[counter]],"rowIndex")<-attr(x[[counter]],"rowIndex")
    attr(roundedY[[counter]],"chrName1")<-attr(y[[counter]],"chrName1")
    attr(roundedX[[counter]],"chrName1")<-attr(x[[counter]],"chrName1")

    r2<-r2backup

  } # for counter

  roundXroundY<-list()

  roundXroundY$roundedX<-roundedX
  roundXroundY$roundedY<-roundedY

  return(roundXroundY)
}

