#' mapXYCen
#' This is an internal function that creates coords for chr. or arms or marks
#'
#' @keywords internal
#'
#' @param start 1st index
#' @param end last index
#' @param y y coords
#' @param yMod y coords modified because of roundness
#' @param yfactor y x factor
#' @param r2 radius
#' @param pts points of roundness
#'
#' @return list

mapXYCen <- function(start,end,y,yMod,x,yfactor,r2,pts){
  topBotline_x <- list()

  x2_1<-list()
  x2_2<-list()

  topline_y<-list()

  y2_1<-list() # NEW
  y2_2<-list() # NEW

  xy_1<-list()
  xy_2<-list()

  xy_3<-list() # NEW
  xy_4<-list() # NEW

  newLongx<-list()
  newLongy<-list()

  bottomline_y<-list()

  for (counter in start: end ) {
    r2backup<-r2
    diffx<-max(x[[counter]]) - min(x[[counter]])
    diffy<-max(y[[counter]]) - min(y[[counter]])
    ratexy<-diffx/diffy
    # max<-max(c(diffx,diffy))
    # rad <- max(x)/number*2
    ifelse( (diffx/r2) * 2 < ratexy*4 ,  r2 <- diffx/(ratexy*2) ,r2 )

    topBotline_x[[counter]]<-c(min(x[[counter]])+r2,
                              max(x[[counter]])-r2 )
    x2_1[[counter]]<-min(x[[counter]])+r2
    x2_2[[counter]]<-max(x[[counter]])-r2

    bottomline_y[[counter]]<-rep(min(yMod[[counter]]),2)

    topline_y[[counter]]<-rep(max(yMod[[counter]]),2)

    y2_1[[counter]]<-max(y[[counter]])-r2*yfactor
    y2_2[[counter]]<-min(y[[counter]])+r2*yfactor

    ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

    # xy_1[[counter]] <- cbind(x2_1[[counter]] + r2 * sin(pts_1)*1  , y2_1[[counter]] + (r2 * cos(pts_1) *yfactor) )
    # was upper left now, down left
    xy_1[[counter]] <- cbind(x2_1[[counter]] + r2 * sin(ptsl[[1]]), min(y[[counter]]) + r2 * cos(ptsl[[1]]) )

    # xy_2[[counter]] <- cbind(x2_2[[counter]] + r2 * sin(pts_2)*1, y2_1[[counter]] + (r2 * cos(pts_2) *yfactor) )
    #was upper right, now down right
    xy_2[[counter]] <- cbind(x2_2[[counter]] + r2 * sin(ptsl[[2]]), min(y[[counter]]) + r2 * cos(ptsl[[2]] ) *yfactor )

    # xy_3[[counter]] <- cbind(x2_1[[counter]] + r2 * sin(pts_3), y2_2[[counter]] + (r2 * cos(pts_3) *yfactor) ) # new
    # was down right, now, up right
    # xy_3 <- cbind( (max(x)-rad) + rad * sin(ptsl[[3]]), (min(y)+rad) + rad * cos(ptsl[[3]]))
    xy_3[[counter]] <- cbind( x2_2[[counter]] + r2 * sin(ptsl[[3]]), (max(y[[counter]])) + r2 * cos(ptsl[[3]]))


    # xy_4[[counter]] <- cbind(x2_2[[counter]] + r2 * sin(pts_4), y2_2[[counter]] + (r2 * cos(pts_4) *yfactor) ) # new
    #  was down left, now, up left
    # xy_4 <- cbind( (min(x)+rad) + rad * sin(ptsl[[4]]), (min(y)+rad) + rad * cos(ptsl[[4]]))
    xy_4[[counter]] <- cbind( x2_1[[counter]] + r2 * sin(ptsl[[4]]), (max(y[[counter]])) + r2 * cos(ptsl[[4]]))

    # was down right, now, up right
    # xy_3 <- cbind( (max(x)-rad) + rad * sin(ptsl[[3]]), (min(y)+rad) + rad * cos(ptsl[[3]]))
    # xy_3b <- cbind( (max(x)-rad) + rad * sin(ptsl[[3]]), (max(y)) + rad * cos(ptsl[[3]]))

     # was down left, now, up left
    # xy_4 <- cbind( (min(x)+rad) + rad * sin(ptsl[[4]]), (min(y)+rad) + rad * cos(ptsl[[4]]))
    # xy_4b <- cbind( (min(x)+rad) + rad * sin(ptsl[[4]]), (max(y)) + rad * cos(ptsl[[4]]))
#
#     yMod[[counter]][which(yMod[[counter]]==max(yMod[[counter]]))]<-yMod[[counter]][which(yMod[[counter]]==max(yMod[[counter]]))]-r2*yfactor
#     yMod[[counter]][which(yMod[[counter]]==min(yMod[[counter]]))]<-yMod[[counter]][which(yMod[[counter]]==min(yMod[[counter]]))]+r2*yfactor
#
#     newLongx[[counter]]<-c(x[[counter]][1:2],xy_4[[counter]][,1],topBotline_x[[counter]],xy_3[[counter]][,1],
#                           x[[counter]][3:4],xy_1[[counter]][,1],topBotline_x[[counter]],xy_2[[counter]][,1])
#
#     newLongy[[counter]]<-c(yMod[[counter]][1:2],xy_4[[counter]][,2],bottomline_y[[counter]],xy_3[[counter]][,2],
#                           yMod[[counter]][3:4],xy_1[[counter]][,2],topline_y[[counter]]   ,xy_2[[counter]][,2])
#

    newLongx[[counter]]<-c(rev(xy_3[[counter]][,1]),rev(xy_2[[counter]][,1]), xy_4[[counter]][,1],xy_1[[counter]][,1])
    newLongy[[counter]]<-c(rev(xy_3[[counter]][,2]),rev(xy_2[[counter]][,2]), xy_4[[counter]][,2],xy_1[[counter]][,2])


    attr(newLongy[[counter]],"rowIndex")<-attr(y[[counter]],"rowIndex")
    attr(newLongx[[counter]],"rowIndex")<-attr(x[[counter]],"rowIndex")
    attr(newLongy[[counter]],"chrName1")<-attr(y[[counter]],"chrName1")
    attr(newLongx[[counter]],"chrName1")<-attr(x[[counter]],"chrName1")

  r2<-r2backup
  } # for counter
  longxlongy<-list()

  longxlongy$newLongx<-newLongx
  longxlongy$newLongy<-newLongy


  return(longxlongy)
}
