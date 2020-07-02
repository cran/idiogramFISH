#' FUNCTION to produce a Robertsonian translocation
#'
#' @description This function reads a data.frame with chr. sizes 
#' \code{\link{chrbasicdatamono}} 
#' and another with marks' positions,
#' \code{\link{markposDFs}} and gets as arguments two chr. names and two arms, 
#' respectively.
#'
#' @description It returns a list with two data.frames. One with the chr. size 
#' of the resulting 
#' translocation
#' and another with the marks' positions for the derivative chr.
#'
#' @param dfChrSize name of data.frame of chr. sizes
#' @param dfMarkPos name of data.frame of chr marks' positions
#' @param chr1 name of chr.
#' @param chr2 name of chr.
#' @param arm1 arm of \code{chr1} to be included
#' @param arm2 arm of \code{chr2} to be included
#'
#' @keywords translocation
#' @export
#' @examples
#' data(humChr)
#' data(humMarkPos)
#' chrt13q14q<-robert(humChr,humMarkPos,13,14,"q","q")
#'
#' @references Robertson, W. R. B. (1916). Chromosome studies. I. Taxonomic 
#' relationships shown in 
#' the chromosomes of Tettigidae and Acrididae: V-shaped chromosomes and their 
#' significance in 
#' Acrididae, Locustidae, and Gryllidae: chromosomes and variation. Journal of 
#' Morphology, 27(2), 
#' 179-331.
#' @return list
#'

robert<-function(dfChrSize,dfMarkPos,chr1,chr2,arm1,arm2){
# extract chr1 data
dfChrSizechr1<-dfChrSize[which(dfChrSize$chrName %in% chr1),]
dfMarkPoschr1<-dfMarkPos[which(dfMarkPos$chrName %in% chr1),]
# extract chr2 data
dfChrSizechr2<-dfChrSize[which(dfChrSize$chrName %in% chr2),]
dfMarkPoschr2<-dfMarkPos[which(dfMarkPos$chrName %in% chr2),]

# Making derivative data.frame of Marks
# leave one arm for chr2
dfMarkPoschr2Der<-dfMarkPoschr2[dfMarkPoschr2$chrRegion==arm2,]
# leave one arm for chr2
dfMarkPoschr1Der<-dfMarkPoschr1[dfMarkPoschr1$chrRegion==arm1,]
# merge fragments of Marks
dfMarkPosDer<-rbind(dfMarkPoschr2Der,dfMarkPoschr1Der)

# Making derivative data.frame of chr. size

# remove not used arms for chr1
if(arm1=="p"){
  dfChrSizechr1$longArmSize<-NULL
  dfChrSizechr1$ArmSize<-dfChrSizechr1$shortArmSize
} else {
  dfChrSizechr1$shortArmSize<-NULL
  dfChrSizechr1$ArmSize<-dfChrSizechr1$longArmSize
}

# remove not used arms for chr2
if(arm2=="p"){
  dfChrSizechr2$longArmSize<-NULL
  dfChrSizechr2$ArmSize<-dfChrSizechr2$shortArmSize
} else {
  dfChrSizechr2$shortArmSize<-NULL
  dfChrSizechr2$ArmSize<-dfChrSizechr2$longArmSize
}

# reposition arms based on length
if(dfChrSizechr2$ArmSize>dfChrSizechr1$ArmSize){
  dfChrSizechr2$longArmSize <-dfChrSizechr2$ArmSize
  dfChrSizechr2$shortArmSize <- NULL
  dfChrSizechr1$shortArmSize<-dfChrSizechr1$ArmSize
  dfChrSizechr1$longArmSize <- NULL
} else {
  dfChrSizechr1$longArmSize <-dfChrSizechr1$ArmSize
  dfChrSizechr1$shortArmSize <- NULL
  dfChrSizechr2$shortArmSize<-dfChrSizechr2$ArmSize
  dfChrSizechr2$longArmSize <- NULL
}

# establish new arm of marks
if( "longArmSize" %in% colnames(dfChrSizechr1) ){
  dfChrSizeDer<-dfChrSizechr1
  dfChrSizeDer$shortArmSize<-dfChrSizechr2$shortArmSize
  dfMarkPosDer[which(dfMarkPosDer$chrName %in% chr1),]$chrRegion<-"q"
  dfMarkPosDer[which(dfMarkPosDer$chrName %in% chr2),]$chrRegion<-"p"

} else if("longArmSize" %in% colnames(dfChrSizechr2)  ){
  dfChrSizeDer<-dfChrSizechr2
  dfChrSizeDer$shortArmSize<-dfChrSizechr1$shortArmSize
  dfMarkPosDer[which(dfMarkPosDer$chrName %in% chr2),]$chrRegion<-"q"
  dfMarkPosDer[which(dfMarkPosDer$chrName %in% chr1),]$chrRegion<-"p"
}

# rename Der. chr.
dfChrSizeDer$chrName<-paste0("t(",chr1,";",chr2,")(",arm1,"10:",arm2,"10)")

derList<-list()
# put both df. in list
derList$dfChrSizeDer<-dfChrSizeDer
dfMarkPosDer$chrName<-paste0("t(",chr1,";",chr2,")(",arm1,"10:",arm2,"10)")
derList$dfMarkPosDer<-dfMarkPosDer
return(derList)
}
