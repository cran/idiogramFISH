#' @name citrusSize
#' @aliases citrusMarkPos, markOverCMA
#' @title FUNCTIONS: citrusSize, citrusMarkPos, markOverCMA
#' @description Helper function to create data.frames with
#' chr. size and mark size data for Citrus
#' based on categories in Carvalho et al. (2005)
#' @description Special behaviour while plotting:
#' normally you will get chr. names as: B_1, B_2, etc.
#' to remove _*, use \code{chrIdPatternRem='_.*'} in
#' \code{plotIdiograms}. However, for FL+ and FL0,
#' this conversion is automatic. So, in plot you will
#' never see FL0_1, FL0_2, for example.
#'
#' @param A number of A to calculate (citrusSize)
#' @param B number of B to calculate (citrusSize)
#' @param C number of C to calculate (citrusSize)
#' @param D number of D to calculate (citrusSize)
#' @param E number of E to calculate (citrusSize)
#' @param F number of F to calculate (citrusSize)
#' @param G number of G to calculate (citrusSize)
#' @param FL number of FL+ to calculate (citrusSize)
#' @param FL0 number of FL0 to calculate (citrusSize)
#' @param shortArm for A to G (not FL) (citrusSize)
#' @param longArm for A to G (not FL) (citrusSize)
#' @param shortArmFL for FL (citrusSize)
#' @param longArmFL for FL (citrusSize)
#' @param OTU name of species (citrusSize)
#' @param chrSizeDf data.frame created with \code{citrusSize} function (citrusMarkPos)
#' @param mSizePter numeric, default size for P(short) ter (terminal) bands. \code{0.25} (default) (citrusMarkPos)
#' @param mSizeQter numeric, default size for Q(long) ter (terminal) bands. \code{0.35} (default) (citrusMarkPos)
#' @param mSizePprox numeric, default size for P prox (proximal) bands. \code{0.35} (default) (citrusMarkPos)
#' @param mOther numeric, default size for other bands. \code{0.25} (default) (citrusMarkPos)
#' @param markName character, default name of mark \code{"CMA"}, or \code{"45S"}, respectively. (citrusMarkPos,markOverCMA)
#' @param citrusMarkPosDF data.frame, with CMA marks (markOverCMA)
#' @param chrType character, defaults to "B", chr. type to duplicate mark (markOverCMA)
#' @param chrName character, defaults to "B", chr. name to duplicate mark (markOverCMA)
#' @param chrRegion character, arm, defaults to "p". for mark duplication (markOverCMA)
#'
#' @keywords size arm
#' @examples
#' citrusSizeDF <- citrusSize(B=1,D=11,F=4,FL0=2,OTU="C. jambhiri")
#' suppressMessages(
#' plotIdiograms(citrusSizeDF,
#'               indexIdTextSize=.4,# font size
#'               rulerNumberSize=.4,# font size
#'               rulerTitleSize=.4, # font size
#'               rulerPos =-.5,     # ruler pos.
#'               xPosRulerTitle =1.5,     # ruler title pos.
#'               orderChr="original"# order of chr. as in d.f.
#'               )
#'               )
#' citrusSizeDF2 <- citrusSize(B=2,D=10,F=4,FL0=1,
#' FL=1,         # equivalent to FL+
#' OTU="C. limettioides")
#'
#'
#'suppressMessages(
#'  plotIdiograms(citrusSizeDF2,     # FL^NA error corrected in 1.15.4
#'                indexIdTextSize=.4,# font size
#'                rulerNumberSize=.4,# font size
#'                rulerTitleSize=.4, # font size
#'                rulerPos =-.5,     # ruler pos.
#'                xPosRulerTitle =1.5,     # ruler title pos.
#'                orderChr="original"# order of chr. as in d.f.
#'  )
#')
#'
#' @references Carvalho, R., Soares Filho, W. S., Brasileiro-Vidal, A. C., & Guerra, M. (2005). The relationships among lemons, limes and citron: A chromosomal comparison. Cytogenetic and Genome Research, 109(1–3), 276–282. https://doi.org/10.1159/000082410
#'
#' @return data.frame
#' @rdname citrusSize
#' @export
#'
#' @importFrom dplyr bind_rows
#'

citrusSize <- function(A=0,B=0,C=0,D=0,E=0,F=0,FL=0,FL0=0,G=0,
                       shortArm=1.2,longArm=1.7,
                       shortArmFL=1.3, longArmFL=1.8,
                       OTU="OTU 1") {

nonFLsum<- sum(A,B,C,D,E,F,G)
FLsum   <- sum(FL,FL0)

if(nonFLsum>0){
  chrNames <- c(rep("A",A),rep("B",B),rep("C",C),rep("D",D),rep("E",E),rep("F",F),rep("G",G) )
  chrNames <- make.uniqueIF(chrNames)
chrSizeCitrusNonFL<- data.frame(chrName=chrNames,shortArmSize=shortArm,longArmSize=longArm)
chrSizeCitrusNonFL$OTU<-OTU
# chrSizeCitrusNonFL <- chrSizeCitrusNonFL[order(chrSizeCitrusNonFL$chrName),]
}
if(FLsum>0){
  chrNamesFL <- c(rep("FL+",FL),rep("FL0",FL0))
  chrNamesFL <- make.uniqueIF(chrNamesFL)
chrSizeCitrusFL   <- data.frame(chrName=chrNamesFL,shortArmSize=shortArmFL,longArmSize=longArmFL)
chrSizeCitrusFL$OTU<-OTU
}

chrSizeCitrus <- dplyr::bind_rows(
  rev(as.list(environment() ))[which(names(rev(as.list(environment()) ) ) %in%
                                  grep("chrSizeCitrus",names(rev(as.list(environment() ) ) )
                                       , value=TRUE)
                                )]
)

return(chrSizeCitrus)
} # fun

#'
#' @rdname citrusSize
#' @return data.frame
#' @examples
#' citrusMarkPosDF <- citrusMarkPos(citrusSizeDF)
#' suppressMessages(
#' plotIdiograms(dfChrSize= citrusSizeDF,      # chr. size data.frame
#'               dfMarkPos= citrusMarkPosDF,# mark position data.frame (inc. cen.)
#'               ruler=FALSE,               # remove
#'               chrIndex=FALSE,            # remove
#'               morpho=FALSE,              # remove
#'               karIndex=FALSE,            # remove
#'               indexIdTextSize=.4,        # font size
#'               xlimRightMod=4,            # xlim mod.
#'               orderChr="original",       # order chr. as in d.f.
#'               chrColor="blue",           # chr. color
#'               legendHeight=3             # legend item height
#'               )
#'               )
#' @export
#'
citrusMarkPos<-function(chrSizeDf,mSizePter=.25,mSizeQter=.35,mSizePprox=.35,mOther=.25,markName="CMA"){
  # making A marks
  getMarkPosA<-chrSizeDf[which(chrSizeDf$chrName %in% grep("A",chrSizeDf$chrName, value=T )),]

  numberOfA<-nrow(getMarkPosA)

  if(numberOfA>0){
  markPosAThree<-do.call("rbind", replicate(3, getMarkPosA, simplify = FALSE))
  markPosAThree$markDistCen <- c(getMarkPosA$shortArmSize-mSizePter,
                                 rep(0,numberOfA),
                                 getMarkPosA$longArmSize-mSizeQter )
  markPosAThree<-markPosAThree[order(markPosAThree$chrName),]

  markPosAThree$chrRegion <- rep(c("p","p","q"),numberOfA)
  markPosAThree$markSize <- rep(c(mSizePter,mSizePprox,mSizeQter),numberOfA)
  }

  getMarkPosB<-chrSizeDf[which(chrSizeDf$chrName %in% grep("B",chrSizeDf$chrName, value=T )),]

  numberOfB<-nrow(getMarkPosB)
  if(numberOfB>0){
    markPosBtwo<-do.call("rbind", replicate(2, getMarkPosB, simplify = FALSE))

    markPosBtwo$markDistCen <- c(rep(0,numberOfB),getMarkPosB$longArmSize-mSizeQter )
    markPosBtwo<-markPosBtwo[order(markPosBtwo$chrName),]

    markPosBtwo$chrRegion <- rep(c("p","q"),numberOfB)
    markPosBtwo$markSize <- rep(c(mSizePprox,mSizeQter),numberOfB)
    # markPosBtwo$markDistCen <- rep(c(0,getMarkPosB$longArmSize-mSizeQter ),numberOfB)

  }

  getMarkPosC<-chrSizeDf[which(chrSizeDf$chrName %in% grep("C",chrSizeDf$chrName, value=T )),]

  numberOfC<-nrow(getMarkPosC)

  if(numberOfC>0){
    markPosCtwo<-do.call("rbind", replicate(2, getMarkPosC, simplify = FALSE))

    markPosCtwo$markDistCen <- c(getMarkPosC$shortArmSize-mSizePter,
                                 getMarkPosC$longArmSize-mSizeQter )

    markPosCtwo<-markPosCtwo[order(markPosCtwo$chrName),]

    markPosCtwo$chrRegion <- rep(c("p","q"),numberOfC)
    markPosCtwo$markSize <- rep(c(mSizePter,mSizeQter),numberOfC)

  }
  getMarkPosD<-chrSizeDf[which(chrSizeDf$chrName %in% grep("D",chrSizeDf$chrName, value=T )),]

  numberOfD<-nrow(getMarkPosD)

  if(numberOfD>0){
    markPosDtwo<-do.call("rbind", replicate(1, getMarkPosD, simplify = FALSE))
    markPosDtwo$markDistCen <- getMarkPosD$longArmSize-mSizeQter
    markPosDtwo[order(markPosDtwo$chrName),]

    markPosDtwo$chrRegion <- rep(c("q"),numberOfD)
    markPosDtwo$markSize <- rep(c(mSizeQter),numberOfD)
    # markPosDtwo$markDistCen <- rep(c(getMarkPosD$longArmSize-mSizeQter ),numberOfD)
  }

  getMarkPosE<-chrSizeDf[which(chrSizeDf$chrName %in% grep("E",chrSizeDf$chrName, value=T )),]

  numberOfE<-nrow(getMarkPosE)

  if(numberOfE>0){
    markPosEtwo<-do.call("rbind", replicate(1, getMarkPosE, simplify = FALSE))
    markPosEtwo$markDistCen <- getMarkPosE$longArmSize-2*mOther
    markPosEtwo<-markPosEtwo[order(markPosEtwo$chrName),]

    markPosEtwo$chrRegion <- rep(c("q"),numberOfE)
    markPosEtwo$markSize <- rep(c(mOther),numberOfE)
  }

  getMarkPosFL<-chrSizeDf[which(chrSizeDf$chrName %in% grep("^FL\\+",chrSizeDf$chrName, value=T )),]

  numberOfFL<-nrow(getMarkPosFL)

  if(numberOfFL>0){
    markPosFLtwo<-do.call("rbind", replicate(1, getMarkPosFL, simplify = FALSE))
    markPosFLtwo$markDistCen <- getMarkPosFL$longArmSize-mOther
    markPosFLtwo<-markPosFLtwo[order(markPosFLtwo$chrName),]

    markPosFLtwo$chrRegion <- rep(c("q"),numberOfFL)
    markPosFLtwo$markSize <- rep(c(mOther),numberOfFL)
  }

  getMarkPosG<-chrSizeDf[which(chrSizeDf$chrName %in% grep("G",chrSizeDf$chrName, value=T )),]

  numberOfG<-nrow(getMarkPosG)

  if(numberOfG>0){
    markPosGtwo<-do.call("rbind", replicate(2, getMarkPosG, simplify = FALSE))
    markPosGtwo$markDistCen <- c(getMarkPosG$longArmSize-mOther,
                                 getMarkPosG$longArmSize-3*mOther )
    markPosGtwo<-markPosGtwo[order(markPosGtwo$chrName),]

    markPosGtwo$chrRegion <- rep(c("q","q"),numberOfG)
    markPosGtwo$markSize <- rep(c(mOther,mOther),numberOfG)
    # markPosGtwo$markDistCen <- rep(c(getMarkPosG$longArmSize-mOther,getMarkPosG$longArmSize-3*mOther ),numberOfG)

  }
  # print(grep("markPos.+",names(as.list(environment()) ), value=T))
  marksDF<-dplyr::bind_rows(
    as.list(environment() )[which(names(as.list(environment()) ) %in% grep("markPos.+",names(as.list(environment()) ), value=T))]
  )
  marksDF<-marksDF[order(marksDF$chrName),]
  row.names(marksDF)<-1:nrow(marksDF)
  marksDF$markName<-markName
  marksDF <- marksDF[,c("chrName","chrRegion","markName","markDistCen","markSize")]
  tryCatch(marksDF$OTU<-unique(chrSizeDf$OTU), error=function(e){"no OTU name"})
  return(marksDF)
}
#'
#' @rdname citrusSize
#' @return data.frame
#' @examples
#' citrusMarkPosDF45S<-markOverCMA(citrusMarkPosDF, chrType="B", chrRegion="p", markName="45S")
#' suppressMessages(
#' plotIdiograms(dfChrSize= citrusSizeDF, # chr. size data.frame
#'               dfMarkPos= citrusMarkPosDF45S,# mark position data.frame (inc. cen.)
#'               ruler=FALSE,             # remove ruler
#'               chrIndex=FALSE,          # remove index
#'               morpho=FALSE,            # remove morphol.
#'               karIndex=FALSE,          # remove
#'               indexIdTextSize=.4,      # font size chr.
#'               xlimRightMod=4,          # modify xlim
#'               orderChr="original",     # as in d.f.
#'               chrColor="blue",
#'               legendHeight=5,          # height of legend item
#'               colorBorderMark="black", # mark border color
#'               OTUfont=3                # italics
#'               )
#'               )
#' @export
#'
markOverCMA <- function(citrusMarkPosDF,chrType="B", chrName, chrRegion="p", markName="45S"){

if(!missing(chrName)){
  if(chrName=="A"){
    minMDC<-min(citrusMarkPosDF[which(citrusMarkPosDF$chrName %in% grep(paste0("^",chrName,"$"), citrusMarkPosDF$chrName, value=TRUE) &
                                citrusMarkPosDF$chrRegion %in% chrRegion),]$markDistCen)
    smallDF<-citrusMarkPosDF[which(citrusMarkPosDF$chrName %in% grep(paste0("^",chrName,"$"), citrusMarkPosDF$chrName, value=TRUE) &
                                 citrusMarkPosDF$chrRegion %in% chrRegion &
                                   citrusMarkPosDF$markDistCen %in% minMDC ),]
  } else {
    smallDF<-citrusMarkPosDF[which(citrusMarkPosDF$chrName %in% grep(paste0("^",chrName,"$"), citrusMarkPosDF$chrName, value=TRUE) &
                                     citrusMarkPosDF$chrRegion %in% chrRegion),]
  }
} else {
  if(chrType=="A"){
    Anames <- grep(chrType, citrusMarkPosDF$chrName, value=TRUE)
    minMDC<-numeric()
    for (i in 1:length(Anames)){
      minMDC[i] <- min(citrusMarkPosDF[which(citrusMarkPosDF$chrName %in% Anames[i] &
                                        citrusMarkPosDF$chrRegion %in% chrRegion),]$markDistCen)
    }
    smallDF<-citrusMarkPosDF[which(citrusMarkPosDF$chrName %in% grep(chrType, citrusMarkPosDF$chrName, value=TRUE) &
                                     citrusMarkPosDF$chrRegion %in% chrRegion &
                                     citrusMarkPosDF$markDistCen %in% minMDC ),]
  } else {
    smallDF<-citrusMarkPosDF[which(citrusMarkPosDF$chrName %in% grep(chrType, citrusMarkPosDF$chrName, value=TRUE) &
                                     citrusMarkPosDF$chrRegion %in% chrRegion),]
  }
}
  if(nrow(smallDF)==0){
    return(message("no bands found"))
  }
smallDF$markName<-markName
mS<-smallDF$markSize
smallDF$markSize<-mS/2
smallDF$markDistCen<-smallDF$markDistCen+mS/4
citrusMarkPosDF45S<-dplyr::bind_rows(citrusMarkPosDF, smallDF)
return(citrusMarkPosDF45S)
}


