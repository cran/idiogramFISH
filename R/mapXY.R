#' mapXY
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
#' @param pts_1 points of roundness
#' @param pts_2 points of roundness
#' @param pts_3 points of roundness
#' @param pts_4 points of roundness
#'
#' @return list

mapXY <- function(start,end,y,yMod,x,yfactor,r2,pts_1,pts_2,pts_3,pts_4 ){
  topBotline_x <- list()

  x2_1<-x2_2<-list()

  bottomline_y<-topline_y<-list()

  y2_1<-y2_2<-list() #

  xy_1<-xy_2<-list()
  xy_3<-xy_4<-list() #

  newLongx<-newLongy<-list()

  for (counter in start: end ) {
    r2backup<-r2
    diffx<-max(x[[counter]]) - min(x[[counter]])
    diffy<-max(y[[counter]]) - min(y[[counter]])
    ratexy<-diffx/diffy
    ifelse( (diffx/r2) * 2 < ratexy*4 ,  r2 <- diffx/(ratexy*2) ,r2 )

    topBotline_x[[counter]]<-c(min(x[[counter]])+r2,
                              max(x[[counter]])-r2 )
    x2_1[[counter]]<-min(x[[counter]])+r2
    x2_2[[counter]]<-max(x[[counter]])-r2

    bottomline_y[[counter]]<-rep(min(yMod[[counter]]),2)

    topline_y[[counter]]<-rep(max(yMod[[counter]]),2)

    y2_1[[counter]]<-max(y[[counter]])-r2*yfactor
    y2_2[[counter]]<-min(y[[counter]])+r2*yfactor

    xy_1[[counter]] <- cbind(x2_1[[counter]] + r2 * sin(pts_1)*1, y2_1[[counter]] + (r2 * cos(pts_1) *yfactor) )
    xy_2[[counter]] <- cbind(x2_2[[counter]] + r2 * sin(pts_2)*1, y2_1[[counter]] + (r2 * cos(pts_2) *yfactor) )

    xy_3[[counter]] <- cbind(x2_1[[counter]] + r2 * sin(pts_3), y2_2[[counter]] + (r2 * cos(pts_3) *yfactor) ) # new
    xy_4[[counter]] <- cbind(x2_2[[counter]] + r2 * sin(pts_4), y2_2[[counter]] + (r2 * cos(pts_4) *yfactor) ) # new

    yMod[[counter]][which(yMod[[counter]]==max(yMod[[counter]]))]<-yMod[[counter]][which(yMod[[counter]]==max(yMod[[counter]]))]-r2*yfactor
    yMod[[counter]][which(yMod[[counter]]==min(yMod[[counter]]))]<-yMod[[counter]][which(yMod[[counter]]==min(yMod[[counter]]))]+r2*yfactor

    newLongx[[counter]]<-c(x[[counter]][1:2],xy_4[[counter]][,1],topBotline_x[[counter]],xy_3[[counter]][,1],
                          x[[counter]][3:4],xy_1[[counter]][,1],topBotline_x[[counter]],xy_2[[counter]][,1])

    newLongy[[counter]]<-c(yMod[[counter]][1:2],xy_4[[counter]][,2],bottomline_y[[counter]],xy_3[[counter]][,2],
                          yMod[[counter]][3:4],xy_1[[counter]][,2],topline_y[[counter]]   ,xy_2[[counter]][,2])


    attr(newLongy[[counter]],"rowIndex")<-attr(y[[counter]],"rowIndex")
    attr(newLongx[[counter]],"rowIndex")<-attr(x[[counter]],"rowIndex")
    attr(newLongy[[counter]],"chrName1")<-attr(y[[counter]],"chrName1")
    attr(newLongx[[counter]],"chrName1")<-attr(x[[counter]],"chrName1")

  r2<-r2backup
  } # for
  longxlongy<-list()
  longxlongy$newLongx<-newLongx
  longxlongy$newLongy<-newLongy
  return(longxlongy)
}
