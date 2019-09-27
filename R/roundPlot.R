# roundPlot
#' This is an internal function that eliminates factors
#'
#' It returns a dataframes
#'
#' @keywords internal
#'
#' @param roundness round
#' @param x x
#' @param y y
#' @param col col
#' @param border border
#' @param chrWidth width of
#' @param yfactor y distortion
#' @param n2 data.frame
#' @param lwd.chr thick of border
#'
#' @return plot
#' @importFrom graphics polygon text
#'

roundPlot<-function(roundness, x, y, col, border, chrWidth, yfactor,n2, lwd.chr){
  if(roundness<1){
    roundness<-1
  }
  # if((min(dfMarkPosInternal$markSize)*roundness)<2 ){
  #   roundness<-2/min(dfMarkPosInternal$markSize)
  # }
  if(roundness>15){
    lapply(1:length(x), function(w) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                                 col=col,
                                                                                 lwd=lwd.chr,
                                                                                 border=border
                                                                                 ), # pol
                                               x=x[[w]],
                                               y=y[[w]]
                                               # ,z=listOfdfMarkPosSq[[w]]$markName
                                           ) # mapply
    ) # lapp
  } else {

    r2 <- chrWidth/(roundness*2)
    pts_1 <- seq(-pi/2, 0, length.out = n2)
    pts_2 <- seq( 0, pi/2, length.out = n2)
    pts_3 <- seq(pi, pi*1.5, length.out = n2)
    pts_4 <- seq(pi/2, pi, length.out = n2)

    yMod<-y # Y
    topline_y<-list()
    bottomline_y<-list()
    topBotline_x<-list()
    x2_1<-list()
    x2_2<-list()
    y2_1<-list()
    y2_2<-list()
    xy_1<-list()
    xy_2<-list()
    xy_3<-list()
    xy_4<-list()
    newLongx<-list()
    newLongy<-list()

    for (s in 1:length(yMod) ) {
      topline_y[[s]]<-list()
      bottomline_y[[s]]<-list()
      topBotline_x[[s]]<-list()
      x2_1[[s]]<-list()
      x2_2[[s]]<-list()
      y2_1[[s]]<-list()
      y2_2[[s]]<-list()
      xy_1[[s]]<-list()
      xy_2[[s]]<-list()
      xy_3[[s]]<-list()
      xy_4[[s]]<-list()
      newLongx[[s]]<-list()
      newLongy[[s]]<-list()

      for (m in 1: length(yMod[[s]]) ) { # mark
        # topline_y<-rep(max(y),2)
        topline_y[[s]][[m]]   <-rep(max(yMod[[s]][[m]]),2)
        # bottomline_y<-rep(min(y),2)
        bottomline_y[[s]][[m]]<-rep(min(yMod[[s]][[m]]),2)
        # topBotline_x<-   c(min(x)+r2, max(x)-r2)
        topBotline_x[[s]][[m]]<-c(min(x[[s]][[m]])+r2,max(x[[s]][[m]])-r2 )
        # yMod[which(yMod==max(yMod))]<-yMod[which(yMod==max(yMod))]-r2
        yMod[[s]][[m]][which(yMod[[s]][[m]]==max(yMod[[s]][[m]]))]<-yMod[[s]][[m]][which(yMod[[s]][[m]]==max(yMod[[s]][[m]]))]-r2*yfactor
        # yMod[which(yMod==min(yMod))]<-yMod[which(yMod==min(yMod))]+r2
        yMod[[s]][[m]][which(yMod[[s]][[m]]==min(yMod[[s]][[m]]))]<-yMod[[s]][[m]][which(yMod[[s]][[m]]==min(yMod[[s]][[m]]))]+r2*yfactor

        # x2_1<-min(x)+r2
        # x2_2<-max(x)-r2
        x2_1[[s]][[m]]<-min(x[[s]][[m]])+r2
        x2_2[[s]][[m]]<-max(x[[s]][[m]])-r2
        # y2_1<-max(y)-r2
        # y2_2<-min(y)+r2
        y2_1[[s]][[m]]<-max(y[[s]][[m]])-r2*yfactor
        y2_2[[s]][[m]]<-min(y[[s]][[m]])+r2*yfactor
        # xy_1 <- cbind(x2_1 + r2 * sin(pts_1), y2_1 + r2 * cos(pts_1))
        xy_1[[s]][[m]] <- cbind(x2_1[[s]][[m]] + r2 * sin(pts_1), y2_1[[s]][[m]] + (r2 * cos(pts_1) *yfactor) )
        # xy_2 <- cbind(x2_2 + r2 * sin(pts_2), y2_1 + r2 * cos(pts_2))
        xy_2[[s]][[m]] <- cbind(x2_2[[s]][[m]] + r2 * sin(pts_2), y2_1[[s]][[m]] + (r2 * cos(pts_2) *yfactor) )
        # xy_3 <- cbind(x2_1 + r2 * sin(pts_3), y2_2 + r2 * cos(pts_3))
        xy_3[[s]][[m]] <- cbind(x2_1[[s]][[m]] + r2 * sin(pts_3), y2_2[[s]][[m]] + (r2 * cos(pts_3) *yfactor) )
        # xy_4 <- cbind(x2_2 + r2 * sin(pts_4), y2_2 + r2 * cos(pts_4))
        xy_4[[s]][[m]] <- cbind(x2_2[[s]][[m]] + r2 * sin(pts_4), y2_2[[s]][[m]] + (r2 * cos(pts_4) *yfactor) )
        # newLongx<-c(x[1:2],xy_4[,1],topBotline_x,xy_3[,1],
        # x[3:4],xy_1[,1],topBotline_x,xy_2[,1])
        newLongx[[s]][[m]]<-c(x[[s]][[m]][1:2],xy_4[[s]][[m]][,1],topBotline_x[[s]][[m]],xy_3[[s]][[m]][,1],
                              x[[s]][[m]][3:4],xy_1[[s]][[m]][,1],topBotline_x[[s]][[m]],xy_2[[s]][[m]][,1])
        # newLongy<-c(yMod[1:2],xy_4[,2],bottomline_y,xy_3[,2],
        # yMod[3:4], xy_1[,2],topline_y,xy_2[,2] )
        newLongy[[s]][[m]]<-c(yMod[[s]][[m]][1:2],xy_4[[s]][[m]][,2],bottomline_y[[s]][[m]],xy_3[[s]][[m]][,2],
                              yMod[[s]][[m]][3:4],xy_1[[s]][[m]][,2],topline_y[[s]][[m]],xy_2[[s]][[m]][,2])
      } # for
    } # for

    newLongy<-newLongy[!is.na(newLongy)]
    newLongx<-newLongx[!is.na(newLongx)]

    lapply(1:length(newLongx), function(w) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                                 col=col,
                                                                                 lwd=.5,
                                                                                 border=border
                                                                           ), # pol
                                               x=newLongx[[w]],
                                               y=newLongy[[w]] #
                                               # ,z=listOfdfMarkPosSq[[s]]$markName
                                              ) # mapply
    ) # l
  } # else
}
