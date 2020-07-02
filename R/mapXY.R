#' mapXY
#' This is an internal function that creates coords for chr. or arms or marks
#'
#' @keywords internal
#'
#' @param start 1st index
#' @param end last index
#' @param y y coords
#' @param yMod y coords modified because of squareness
#' @param yfactor y x factor
#' @param r2 radius
#' @param pts_1 points of squareness
#' @param pts_2 points of squareness
#' @param pts_3 points of squareness
#' @param pts_4 points of squareness
#'
#' @return list

mapXY <- function(start,end,y,yMod,x,yfactor,r2,pts_1,pts_2,pts_3,pts_4 ){
  topBotline_x <- list()

  x2_1<-x2_2<-list()

  bottomline_y<-topline_y<-list()

  y2_1<-y2_2<-list() #

  xy_1<-xy_2<-list()
  xy_3<-xy_4<-list() #

  roundedX<-roundedY<-list()

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

    roundedX[[counter]]<-c(x[[counter]][1:2],xy_4[[counter]][,1],topBotline_x[[counter]],xy_3[[counter]][,1],
                          x[[counter]][3:4],xy_1[[counter]][,1],topBotline_x[[counter]],xy_2[[counter]][,1])

    roundedY[[counter]]<-c(yMod[[counter]][1:2],xy_4[[counter]][,2],bottomline_y[[counter]],xy_3[[counter]][,2],
                          yMod[[counter]][3:4],xy_1[[counter]][,2],topline_y[[counter]]   ,xy_2[[counter]][,2])


    attr(roundedY[[counter]],"rowIndex")<-attr(y[[counter]],"rowIndex")
    attr(roundedX[[counter]],"rowIndex")<-attr(x[[counter]],"rowIndex")
    attr(roundedY[[counter]],"chrName1")<-attr(y[[counter]],"chrName1")
    attr(roundedX[[counter]],"chrName1")<-attr(x[[counter]],"chrName1")

  r2<-r2backup
  } # for

  roundXroundY<-list()
  roundXroundY$roundedX<-roundedX
  roundXroundY$roundedY<-roundedY
  return(roundXroundY)
}

mapXYCen <- function(start,end,ycoordCentsS,xcoordCentsS,pts_1,pts_2,pts_3,pts_4,mimic=FALSE ){

  xy_1<-xy_2<-list()
  xy_3<-xy_4<-list() #

  roundedX<-roundedY<-list()

  for (counter in start: end ) {

    diffx<-max(xcoordCentsS[[counter]]) - min(xcoordCentsS[[counter]])
    diffy<-max(ycoordCentsS[[counter]]) - min(ycoordCentsS[[counter]])

    halfmaxX <- diffx/2
    halfmaxY <- diffy/2

    minX<-min(xcoordCentsS[[counter]])
    maxX<-max(xcoordCentsS[[counter]])
    minY<-min(ycoordCentsS[[counter]])
    maxY<-max(ycoordCentsS[[counter]])


    xy_1[[counter]] <- cbind( min(xcoordCentsS[[counter]])+halfmaxX + halfmaxX * sin(pts_1), min(ycoordCentsS[[counter]]) + halfmaxY * cos(pts_1) )

    xy_2[[counter]] <- cbind( min(xcoordCentsS[[counter]])+halfmaxX + halfmaxX * sin(pts_2), min(ycoordCentsS[[counter]]) + halfmaxY * cos(pts_2))

    xy_3[[counter]] <- cbind( min(xcoordCentsS[[counter]])+halfmaxX + halfmaxX * sin(pts_3), max(ycoordCentsS[[counter]]) + halfmaxY * cos(pts_3))

    xy_4[[counter]] <- cbind( min(xcoordCentsS[[counter]])+halfmaxX + halfmaxX * sin(pts_4), max(ycoordCentsS[[counter]]) + halfmaxY * cos(pts_4 ) )

    if(mimic==FALSE){
    roundedX[[counter]] <-c(minX+halfmaxX, xy_4[[counter]][,1],minX  ,maxX , (xy_3[[counter]][,1]),
                            minX+halfmaxX,xy_2[[counter]][,1],maxX , minX, xy_1[[counter]][,1])
    roundedY[[counter]] <-c(minY+halfmaxY, xy_4[[counter]][,2],maxY,  maxY , (xy_3[[counter]][,2]),
                            minY+halfmaxY,xy_2[[counter]][,2],minY ,minY, xy_1[[counter]][,2])
    } else {

    roundedX[[counter]]<-c(minX,xy_1[[counter]][,1],minX+halfmaxX,(xy_2[[counter]][,1]),
                           maxX, (xy_3[[counter]][,1] ),minX+halfmaxX,xy_4[[counter]][,1]) # opposite of cen.

    roundedY[[counter]]<-c(minY,xy_1[[counter]][,2],minY+halfmaxY,(xy_2[[counter]][,2]),
                           maxY, (xy_3[[counter]][,2] ),minY+halfmaxY,xy_4[[counter]][,2])
    }
  } # for

  roundXroundY<-list()
  roundXroundY$roundedX<-roundedX
  roundXroundY$roundedY<-roundedY
  return(roundXroundY)
}

mapXYchromatidLA <- function(start,end,y,x,xModifier=.1 ){

  longArmChrtx<-longArmChrty<-list()

  for (counter in start: end ) {
    maxX<-minX<-halfX<-halfXModMinus<-halfXModPlus<-NULL

    maxX <- max(x[[counter]])
    minX <- min(x[[counter]])

    maxY <- max(y[[counter]])
    minY <- min(y[[counter]])

    halfX <- (maxX+minX)/2
    halfXModPlus  <- halfX + xModifier
    halfXModMinus <- halfX - xModifier

    longArmChrtx[[counter]]<-c(maxX,maxX,halfXModPlus,halfXModPlus,halfXModMinus,halfXModMinus,minX,minX)

    longArmChrty[[counter]]<-c(maxY,minY,minY,        maxY,        maxY,         minY,         minY,maxY)

  } # for

  chrtXchrtYLA<-list()
  chrtXchrtYLA$longArmChrtx<-longArmChrtx
  chrtXchrtYLA$longArmChrty<-longArmChrty
  return(chrtXchrtYLA)
}

mapXYchromatidSA <- function(start,end,y,x,xModifier=.1 ){

  shortArmChrtx<-shortArmChrty<-list()

  for (counter in start: end ) {
    maxX<-minX<-halfX<-halfXModMinus<-halfXModPlus<-NULL

    maxX <- max(x[[counter]])
    minX <- min(x[[counter]])

    maxY <- max(y[[counter]])
    minY <- min(y[[counter]])

    halfX <- (maxX+minX)/2
    halfXModPlus  <- halfX + xModifier
    halfXModMinus <- halfX - xModifier

    shortArmChrtx[[counter]] <-c(maxX,maxX,minX,minX,halfXModMinus,halfXModMinus,halfXModPlus,halfXModPlus)
    shortArmChrty[[counter]] <-c(maxY,minY,minY,maxY,         maxY,         minY,        minY,        maxY)

  } # for

  chrtXchrtYSA<-list()
  chrtXchrtYSA$shortArmChrtx<-shortArmChrtx
  chrtXchrtYSA$shortArmChrty<-shortArmChrty
  return(chrtXchrtYSA)
}

mapXYchromatidHolo <- function(start,end,y,x,xModifier=.1 ){

  xCT1<-yCT1<-xCT2<-yCT2<-list()

  for (counter in start: end ) {
    maxX<-minX<-halfX<-halfXModMinus<-halfXModPlus<-NULL

    maxX <- max(x[[counter]])
    minX <- min(x[[counter]])

    maxY <- max(y[[counter]])
    minY <- min(y[[counter]])

    halfX <- (maxX+minX)/2
    halfXModPlus  <- halfX + xModifier
    halfXModMinus <- halfX - xModifier

    xCT1[[counter]]<-c(maxX,maxX,halfXModPlus,halfXModPlus)
    yCT1[[counter]]<-c(maxY,minY,minY,        maxY)

    #left chrt holocen sq
    xCT2[[counter]]<-c(halfXModMinus,halfXModMinus,minX,minX)
    yCT2[[counter]]<-c(maxY,         minY,         minY,maxY)
    # attr(yMarkPer[[s]][[m]],"rowIndex")<-name
  } # for

  chrtXchrtYHolo<-list()
  chrtXchrtYHolo$xCT1<-xCT1
  chrtXchrtYHolo$xCT2<-xCT2
  chrtXchrtYHolo$yCT1<-yCT1
  chrtXchrtYHolo$yCT2<-yCT2
  return(chrtXchrtYHolo)
}

mapXYchromatidSARo <- function(start,end,y,x,r2,xModifier,pts){

  RoundedSAChrtx<-RoundedSAChrty<-list()

  for (counter in start: end ) {

    maxX<-minX<-halfX<-halfXModMinus<-halfXModPlus<-NULL

    maxX <- max(x[[counter]])
    minX <- min(x[[counter]])

    maxY <- max(y[[counter]])
    minY <- min(y[[counter]])

    halfX <- (maxX+minX)/2
    halfXModPlus  <- halfX + xModifier
    halfXModMinus <- halfX - xModifier

    r2backup<-r2

    diffx<-maxX - minX
    diffy<-maxY - minY
    ratexy<-diffx/diffy
    ifelse( (diffx/r2) * 2 < ratexy*4 ,  r2 <- diffx/(ratexy*2) ,r2 )

    yMod<-y[[counter]]

    yMod[which(yMod==max(yMod))] <- yMod[which(yMod==max(yMod))]-r2
    yMod[which(yMod==min(yMod))] <- yMod[which(yMod==min(yMod))]+r2

    topBotline_x<-c(minX+r2, maxX-r2)

    topBotline_x2 <- c(halfXModPlus + r2, maxX-r2, halfXModMinus-r2,minX+r2)

    bottomline_y <-rep(minY,2)

    # pts<- seq(-pi/2, pi*1.5, length.out = ver*4)
    ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

    xy_1 <- cbind( (minX+r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))
    xy_2 <- cbind( (maxX-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
    xy_3 <- cbind( (maxX-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
    xy_4 <- cbind( (minX+r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))

    # xy_5 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))
    # xy_6 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
    xy_7 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
    xy_8 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))

    # xy_9  <- cbind( (halfXModPlus - xModifier) + xModifier * sin(ptsl[[2]]), (maxY-(xModifier*2)) + xModifier * cos(ptsl[[2]]))
    # xy_10 <- cbind( (halfXModMinus+ xModifier) + xModifier * sin(ptsl[[1]]), (maxY-(xModifier*2)) + xModifier * cos(ptsl[[1]]))
    xy_11 <- cbind( (halfXModMinus+ xModifier) + xModifier * sin(ptsl[[4]]), (minY+(xModifier*2)) + xModifier * cos(ptsl[[4]]))
    xy_12 <- cbind( (halfXModPlus - xModifier) + xModifier * sin(ptsl[[3]]), (minY+(xModifier*2)) + xModifier * cos(ptsl[[3]]))

    RoundedSAChrtx[[counter]] <-  c(rep(maxX,2),xy_3[,1] , topBotline_x[2:1],xy_4[,1],rep(minX,2),xy_1[,1],topBotline_x2[4:3] # 3 4 1
                               ,xy_7[,1], halfXModMinus,halfXModMinus # 7
                               ,rev(xy_11[,1]),rev(xy_12[,1]) # 11 12
                               , rep(halfXModPlus,2)
                               ,xy_8[,1], topBotline_x2[2:1],xy_2[,1] # 8 2
    )

    RoundedSAChrty[[counter]] <-  c(yMod[1:2],  xy_3[,2] , bottomline_y,     xy_4[,2],yMod[2:1],xy_1[,2],rep(maxY,2)  # 3 4 1
                              ,xy_7[,2], yMod[1], minY+(xModifier*2) # 7
                              ,rev(xy_11[,2]),rev(xy_12[,2]) # 11 12
                              ,c(minY+(xModifier*2),yMod[1])
                              ,xy_8[,2], rep(maxY,2),xy_2[,2] # 8 2
                              #5 6 9 10
    )

    r2<-r2backup
  } # for

  chrtXchrtYSARo<-list()
  chrtXchrtYSARo$RoundedSAChrtx<-RoundedSAChrtx
  chrtXchrtYSARo$RoundedSAChrty<-RoundedSAChrty
  return(chrtXchrtYSARo)
}

mapXYchromatidLARo <- function(start,end,y,x,r2,xModifier,pts){

  RoundedLAChrtx<-RoundedLAChrty<-list()

  for (counter in start: end ) {

    maxX<-minX<-halfX<-halfXModMinus<-halfXModPlus <- NULL

    maxX <- max(x[[counter]])
    minX <- min(x[[counter]])

    maxY <- max(y[[counter]])
    minY <- min(y[[counter]])

    halfX <- (maxX+minX)/2
    halfXModPlus  <- halfX + xModifier
    halfXModMinus <- halfX - xModifier

    r2backup<-r2

    diffx<-maxX - minX
    diffy<-maxY - minY
    ratexy<-diffx/diffy
    ifelse( (diffx/r2) * 2 < ratexy*4 ,  r2 <- diffx/(ratexy*2) ,r2 )

    yMod<-y[[counter]]

    yMod[which(yMod==max(yMod))] <- yMod[which(yMod==max(yMod))]-r2
    yMod[which(yMod==min(yMod))] <- yMod[which(yMod==min(yMod))]+r2

    topBotline_x <- c(minX+r2, maxX-r2)

    topBotline_x2 <- c(halfXModPlus + r2, maxX-r2, halfXModMinus-r2,minX+r2)

    bottomline_y <-rep(minY,2)

    # pts<- seq(-pi/2, pi*1.5, length.out = ver*4)
    ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

    xy_1 <- cbind( (minX+r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))
    xy_2 <- cbind( (maxX-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
    xy_3 <- cbind( (maxX-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
    xy_4 <- cbind( (minX+r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))

    xy_5 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))
    xy_6 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
    # xy_7 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
    # xy_8 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))

    xy_9  <- cbind( (halfXModPlus - xModifier) + xModifier * sin(ptsl[[2]]), (maxY-(xModifier*2)) + xModifier * cos(ptsl[[2]]))
    xy_10 <- cbind( (halfXModMinus+ xModifier) + xModifier * sin(ptsl[[1]]), (maxY-(xModifier*2)) + xModifier * cos(ptsl[[1]]))
    # xy_11 <- cbind( (halfXModMinus+ xModifier) + xModifier * sin(ptsl[[4]]), (minY+(xModifier*2)) + xModifier * cos(ptsl[[4]]))
    # xy_12 <- cbind( (halfXModPlus - xModifier) + xModifier * sin(ptsl[[3]]), (minY+(xModifier*2)) + xModifier * cos(ptsl[[3]]))

    RoundedLAChrtx[[counter]] <- c(rep(maxX,2),xy_3[,1] , topBotline_x2[1:2],xy_5[,1],halfXModPlus, # 2 5
                                                           rev(xy_9[,1]),rev(xy_10[,1]) # 9 10
                                                           ,halfXModMinus,         halfXModMinus
                                                           ,xy_6[,1], topBotline_x2[3:4],xy_4[,1],rep(minX,2),xy_1[,1],topBotline_x,xy_2[,1] # 6 4 1 2
    )

    RoundedLAChrty[[counter]] <-  c(yMod[1:2],  xy_3[,2] , bottomline_y[1:2],xy_5[,2],maxY-(xModifier*2), # 3 5
                                    rev(xy_9[,2]),rev(xy_10[,2]) # 9 10
                                    ,maxY-(xModifier*2),          yMod[2]
                                    ,xy_6[,2], rep(minY,2),       xy_4[,2], yMod[2:1], xy_1[,2],rep(maxY,2)  ,xy_2[,2] # 6 4 1 2
    )
    # 7 8 11 12

    r2<-r2backup
  } # for

  chrtXchrtYLARo<-list()
  chrtXchrtYLARo$RoundedLAChrtx<-RoundedLAChrtx
  chrtXchrtYLARo$RoundedLAChrty<-RoundedLAChrty
  return(chrtXchrtYLARo)
}



mapXYchromatidHoloRo <- function(start,end,y,x,r2, xModifier,pts ){

  holoRightx<-holoLeftx<-holoRighty<-holoLefty<-list()

  for (counter in start: end ) {
    maxX<-minX<-halfX<-halfXModMinus<-halfXModPlus<-NULL

    maxX <- max(x[[counter]])
    minX <- min(x[[counter]])

    maxY <- max(y[[counter]])
    minY <- min(y[[counter]])

    diffx<-maxX - minX
    diffy<-maxY - minY
    ratexy<-diffx/diffy
    ifelse( (diffx/r2) * 2 < ratexy*4 ,  r2 <- diffx/(ratexy*2) ,r2 )

    yMod<-y[[counter]]

    yMod[which(yMod==max(yMod))] <- yMod[which(yMod==max(yMod))]-r2
    yMod[which(yMod==min(yMod))] <- yMod[which(yMod==min(yMod))]+r2

    halfX <- (maxX+minX)/2
    halfXModPlus  <- halfX + xModifier
    halfXModMinus <- halfX - xModifier

    topBotline_x2 <- c(halfXModPlus + r2, maxX-r2, halfXModMinus-r2,minX+r2)
    bottomline_y <-rep(minY,2)

    ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

    xy_1 <- cbind( (minX+r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))
    xy_2 <- cbind( (maxX-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
    xy_3 <- cbind( (maxX-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
    xy_4 <- cbind( (minX+r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))

    xy_5 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))
    xy_6 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
    xy_7 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
    xy_8 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))

    holoRightx[[counter]]<-c(rep(maxX,2),xy_3[,1],topBotline_x2[1:2],xy_5[,1] # 3 5
                  ,halfXModPlus,halfXModPlus
                  ,xy_8[,1], topBotline_x2[2:1],xy_2[,1] # 8 2
    )
    holoRighty[[counter]]<-c(yMod[1:2],xy_3[,2],bottomline_y,      xy_5[,2] # 3 5
                  ,yMod[3:4]
                  ,xy_8[,2], rep(maxY,2),xy_2[,2] # 8 2
    )
    holoLeftx[[counter]]<-c(rep(halfXModMinus,2),xy_6[,1],topBotline_x2[3:4], # 6
                            xy_4[,1],rep(minX,2), # 4
                 xy_1[,1],topBotline_x2[4:3],xy_7[,1] # 1 7
    )
    holoLefty[[counter]]<-c(yMod[1:2],           xy_6[,2],bottomline_y, # 6
                            xy_4[,2],yMod[3:4], # 4
                 xy_1[,2],rep(maxY,2),       xy_7[,2]# 1 7
    )
  } # for

  chrtXchrtYHoloRo<-list()
  chrtXchrtYHoloRo$holoRightx<-holoRightx
  chrtXchrtYHoloRo$holoLeftx<-holoLeftx
  chrtXchrtYHoloRo$holoRighty<-holoRighty
  chrtXchrtYHoloRo$holoLefty<-holoLefty
  return(chrtXchrtYHoloRo)
}



mapXYmarksRo <- function(start,end,y,x,r2, xModifier,pts ) {

  markRightx<-markLeftx<-markRighty<-markLefty<-list()

  for (counter in start: end ) {

    if( attr(y[[counter]],"wholeArm")=='false' ) {

    maxX<-minX<-halfX<-halfXModMinus<-halfXModPlus<-NULL

    maxX <- max(x[[counter]])
    minX <- min(x[[counter]])

    maxY <- max(y[[counter]])
    minY <- min(y[[counter]])

    r2backup<-r2

    diffx<-maxX - minX
    diffy<-maxY - minY
    ratexy<-diffx/diffy

    ifelse( (diffx/r2) * 2 < ratexy*4 ,  r2 <- diffx/(ratexy*2) ,r2 )

    yMod<-y[[counter]]

    yMod[which(yMod==max(yMod))] <- yMod[which(yMod==max(yMod))]-r2
    yMod[which(yMod==min(yMod))] <- yMod[which(yMod==min(yMod))]+r2

    halfX <- (maxX+minX)/2
    halfXModPlus  <- halfX + xModifier
    halfXModMinus <- halfX - xModifier

    topBotline_x2 <- c(halfXModPlus + r2, maxX-r2, halfXModMinus-r2,minX+r2)
    bottomline_y <-rep(minY,2)

    ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

    xy_1 <- cbind( (minX+r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))
    xy_2 <- cbind( (maxX-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
    xy_3 <- cbind( (maxX-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
    xy_4 <- cbind( (minX+r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))

    xy_5 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))
    xy_6 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
    xy_7 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
    xy_8 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))

    markRightx[[counter]]<-c(rep(maxX,2),xy_3[,1],topBotline_x2[1:2],xy_5[,1] # 3 5
                             ,halfXModPlus,halfXModPlus
                             ,xy_8[,1], topBotline_x2[2:1],xy_2[,1] # 8 2
    )
    markRighty[[counter]]<-c(yMod[1:2],xy_3[,2],bottomline_y,      xy_5[,2] # 3 5
                             ,yMod[3:4]
                             ,xy_8[,2], rep(maxY,2),xy_2[,2] # 8 2
    )
    markLeftx[[counter]]<-c(rep(halfXModMinus,2),xy_6[,1],topBotline_x2[3:4], # 6
                            xy_4[,1],rep(minX,2), # 4
                            xy_1[,1],topBotline_x2[4:3],xy_7[,1] # 1 7
    )
    markLefty[[counter]]<-c(yMod[1:2],           xy_6[,2],bottomline_y, # 6
                            xy_4[,2],yMod[3:4], # 4
                            xy_1[,2],rep(maxY,2),       xy_7[,2]# 1 7
    )

    r2<-r2backup

    } else { # whole arm False True

      if( attr(y[[counter]],"whichArm")=='short' ) {

        maxX<-minX<-halfX<-halfXModMinus<-halfXModPlus<-NULL

        maxX <- max(x[[counter]])
        minX <- min(x[[counter]])

        maxY <- max(y[[counter]])
        minY <- min(y[[counter]])

        halfX <- (maxX+minX)/2
        halfXModPlus  <- halfX + xModifier
        halfXModMinus <- halfX - xModifier

        r2backup<-r2

        diffx<-maxX - minX
        diffy<-maxY - minY

        ratexy<-diffx/diffy

        ifelse( (diffx/r2) * 2 < ratexy*4 ,  r2 <- diffx/(ratexy*2) ,r2 )

        yMod<-y[[counter]]

        yMod[which(yMod==max(yMod))] <- yMod[which(yMod==max(yMod))]-r2
        yMod[which(yMod==min(yMod))] <- yMod[which(yMod==min(yMod))]+r2

        topBotline_x<-c(minX+r2, maxX-r2)

        topBotline_x2 <- c(halfXModPlus + r2, maxX-r2, halfXModMinus-r2,minX+r2)

        bottomline_y <-rep(minY,2)

        # pts<- seq(-pi/2, pi*1.5, length.out = ver*4)
        ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

        xy_1 <- cbind( (minX+r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))
        xy_2 <- cbind( (maxX-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
        xy_3 <- cbind( (maxX-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
        xy_4 <- cbind( (minX+r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))

        # xy_5 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))
        # xy_6 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
        xy_7 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
        xy_8 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))

        # xy_9  <- cbind( (halfXModPlus - xModifier) + xModifier * sin(ptsl[[2]]), (maxY-(xModifier*2)) + xModifier * cos(ptsl[[2]]))
        # xy_10 <- cbind( (halfXModMinus+ xModifier) + xModifier * sin(ptsl[[1]]), (maxY-(xModifier*2)) + xModifier * cos(ptsl[[1]]))
        xy_11 <- cbind( (halfXModMinus+ xModifier) + xModifier * sin(ptsl[[4]]), (minY+(xModifier*2)) + xModifier * cos(ptsl[[4]]))
        xy_12 <- cbind( (halfXModPlus - xModifier) + xModifier * sin(ptsl[[3]]), (minY+(xModifier*2)) + xModifier * cos(ptsl[[3]]))

        # this is not only right but both chrtids

        markRightx[[counter]] <-  c(rep(maxX,2),xy_3[,1] , topBotline_x[2:1],xy_4[,1],rep(minX,2),xy_1[,1],topBotline_x2[4:3] # 3 4 1
                                        ,xy_7[,1], halfXModMinus,halfXModMinus # 7
                                        ,rev(xy_11[,1]),rev(xy_12[,1]) # 11 12
                                        , rep(halfXModPlus,2)
                                        ,xy_8[,1], topBotline_x2[2:1],xy_2[,1] # 8 2
        )

        # this is not only right but both chrts

        markRighty[[counter]] <-  c(yMod[1:2],  xy_3[,2] , bottomline_y,     xy_4[,2],yMod[2:1],xy_1[,2],rep(maxY,2)  # 3 4 1
                                        ,xy_7[,2], yMod[1], minY+(xModifier*2) # 7
                                        ,rev(xy_11[,2]),rev(xy_12[,2]) # 11 12
                                        ,c(minY+(xModifier*2),yMod[1])
                                        ,xy_8[,2], rep(maxY,2),xy_2[,2] # 8 2
                                        #5 6 9 10
        )

        markLeftx[[counter]]<-NA
        markLefty[[counter]]<-NA

        r2<-r2backup

      } else { # whichArm short else long

        maxX<-minX<-halfX<-halfXModMinus<-halfXModPlus <- NULL

        maxX <- max(x[[counter]])
        minX <- min(x[[counter]])

        maxY <- max(y[[counter]])
        minY <- min(y[[counter]])

        halfX <- (maxX+minX)/2
        halfXModPlus  <- halfX + xModifier
        halfXModMinus <- halfX - xModifier

        r2backup<-r2

        diffx<-maxX - minX
        diffy<-maxY - minY
        ratexy<-diffx/diffy
        ifelse( (diffx/r2) * 2 < ratexy*4 ,  r2 <- diffx/(ratexy*2) ,r2 )

        yMod<-y[[counter]]

        yMod[which(yMod==max(yMod))] <- yMod[which(yMod==max(yMod))]-r2
        yMod[which(yMod==min(yMod))] <- yMod[which(yMod==min(yMod))]+r2

        topBotline_x <- c(minX+r2, maxX-r2)

        topBotline_x2 <- c(halfXModPlus + r2, maxX-r2, halfXModMinus-r2,minX+r2)

        bottomline_y <- rep(minY,2)

        # pts<- seq(-pi/2, pi*1.5, length.out = ver*4)
        ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

        xy_1 <- cbind( (minX+r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))
        xy_2 <- cbind( (maxX-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
        xy_3 <- cbind( (maxX-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
        xy_4 <- cbind( (minX+r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))

        xy_5 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[4]]), (minY+r2) + r2 * cos(ptsl[[4]]))
        xy_6 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[3]]), (minY+r2) + r2 * cos(ptsl[[3]]))
        # xy_7 <- cbind( (halfXModMinus-r2) + r2 * sin(ptsl[[2]]), (maxY-r2) + r2 * cos(ptsl[[2]]))
        # xy_8 <- cbind( (halfXModPlus +r2) + r2 * sin(ptsl[[1]]), (maxY-r2) + r2 * cos(ptsl[[1]]))

        xy_9  <- cbind( (halfXModPlus - xModifier) + xModifier * sin(ptsl[[2]]), (maxY-(xModifier*2)) + xModifier * cos(ptsl[[2]]))
        xy_10 <- cbind( (halfXModMinus+ xModifier) + xModifier * sin(ptsl[[1]]), (maxY-(xModifier*2)) + xModifier * cos(ptsl[[1]]))
        # xy_11 <- cbind( (halfXModMinus+ xModifier) + xModifier * sin(ptsl[[4]]), (minY+(xModifier*2)) + xModifier * cos(ptsl[[4]]))
        # xy_12 <- cbind( (halfXModPlus - xModifier) + xModifier * sin(ptsl[[3]]), (minY+(xModifier*2)) + xModifier * cos(ptsl[[3]]))


        # this is not only right but both chrtids

        markRightx[[counter]] <- c(rep(maxX,2),xy_3[,1] , topBotline_x2[1:2],xy_5[,1],halfXModPlus, # 2 5
                                       rev(xy_9[,1]),rev(xy_10[,1]) # 9 10
                                       ,halfXModMinus,         halfXModMinus
                                       ,xy_6[,1], topBotline_x2[3:4],xy_4[,1],rep(minX,2),xy_1[,1],topBotline_x,xy_2[,1] # 6 4 1 2
        )

        # both:

        markRighty[[counter]] <-  c(yMod[1:2],  xy_3[,2] , bottomline_y[1:2],xy_5[,2],maxY-(xModifier*2), # 3 5
                                        rev(xy_9[,2]),rev(xy_10[,2]) # 9 10
                                        ,maxY-(xModifier*2),          yMod[2]
                                        ,xy_6[,2], rep(minY,2),       xy_4[,2], yMod[2:1], xy_1[,2],rep(maxY,2)  ,xy_2[,2] # 6 4 1 2
        )
        # 7 8 11 12

        markLeftx[[counter]]<-NA
        markLefty[[counter]]<-NA

        r2<-r2backup

      }
    }
  } # for

  chrtXchrtYmarkRo<-list()

  chrtXchrtYmarkRo$markRightx<-markRightx
  chrtXchrtYmarkRo$markRighty<-markRighty

  chrtXchrtYmarkRo$markLeftx <-markLeftx
  chrtXchrtYmarkRo$markLefty <-markLefty

  return(chrtXchrtYmarkRo)
#  chrtXchrtYmarkRo665<<-chrtXchrtYmarkRo
}

