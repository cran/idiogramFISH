#' FUNCTION to plot idiograms of karyotypes with and without centromere
#' @description This function reads a data.frame passed to \code{dfChrSize} with columns: \code{chrName} (mono/holo) and
#' \code{shortArmSize} and \code{longArmSize} for monocentrics or a column \code{chrSize} for holocentrics and produces a plot of idiograms. If more
#' than one species, a column named \code{OTU} is needed.
#'
#' @description Optionally, it reads another data.frame passed to \code{dfMarkPos} with the position of
#' marks (sites). Examples: \code{\link{markposDFs}}. Another data.frame for mark characteristics
#' can be used \code{\link{dfMarkColor}} or a character vector passed to \code{mycolors}
#'
#' @param dfChrSize mandatory data.frame, with columns: \code{OTU} (optional), \code{chrName} (mandatory),
#'   \code{shortArmSize}, \code{longArmSize} for monocen. or \code{chrSize} for holocen.
#' @param dfMarkPos data.frame of marks (sites): columns: \code{OTU} (opt), \code{chrName},
#'   \code{markName} (name of site), \code{chrRegion} (for monocen. and opt for whole arm (w) in holocen.), \code{markDistCen} (for monocen.),
#'   \code{markPos} (for holocen.), \code{markSize}; column \code{chrRegion}:
#'   use \code{p} for short arm, \code{q} for long arm, \code{cen} for centromeric mark and \code{w} for whole chr. mark; column
#'   \code{markDistCen}: use distance from
#'   centromere to mark, not necessary for cen. marks (cen), w, p, q (when whole arm). See also param. \code{markDistType}
#' @param dfCenMarks data.frame, specific for centromeric marks. columns: \code{chrName}
#'   and \code{markName}. See also \code{dfMarkPos} for another option to pass cen. marks
#' @param dfMarkColor data.frame, optional, specifying colors and style for marks (sites);
#'   columns: \code{markName}, \code{markColor}, \code{style}. \code{style} accepts: \code{square} or \code{dots} or \code{cM}.
#'   (if column \code{style} missing all (except 5S) are plotted as in param. \code{defaultStyleMark}). \code{cM} introduced in 1.13
#' @param mycolors character vector, optional, i.e. \code{c("blue","red","green")} for specifying color of marks in order of appearance. if diverges with number of marks will be recycled if \code{dfMarkColor} present, mycolors will be ignored. To know the
#' order of your marks use something like: \code{unique(c(dfMarkPos$markName,dfCenMarks$markName) ) }
#' @param addMissingOTUAfter character, when you want to add space (ghost OTUs) after one or several OTUs, pass the names of OTUs preceding the desired space in a character vector i.e. \code{c("species one","species five")}
#' @param missOTUspacings numeric, when you use \code{addMissingOTUAfter} this numeric vector should have the same length and corresponds to the number of free spaces (ghost OTUs) to add after each OTU respectively
#' @param orderBySize logical value, when \code{TRUE}, sorts chromosomes by total
#'   length from the largest to the smallest
#' @param centromereSize numeric, optional, this establishes the apparent size of cen. in the plot in \eqn{\mu}m. Automatic when \code{NA}. Default: \code{NA}
#' @param origin, For non-monocentric chr. (for holocentrics only) Use \code{"b"} (default) if distance to mark in (\code{"markPos"} column in \code{"dfMarkPos"}) data.frame measured from bottom of chromosome, use \code{"t"} for distance to mark from top of chr.
#' @param cMBeginCenter, boolean, start position of \code{cM} and \code{cMLeft} marks. If \code{TRUE}, starts in the center (width) of chr. . Defaults to \code{FALSE}
#' @param arrowhead numeric, proportion of head of arrow (mark styles: \code{upArrow,downArrow}). Defaults to \code{0.3}
#' @param shrinkArrow numeric, proportion, shrinks body of arrow. Defaults to \code{0.3333}
#' @param arrowheadWidthShrink numeric, proportion, shrinks head of arrow. Defaults to \code{0.1}
#' @param arrowsToSide boolean, when \code{FALSE} use a centered arrow, instead of an arrow next to chr. margins. Defaults to \code{TRUE}
#' @param markDistType character, if \code{"cen"} = the distance you provided in data.frame (\code{dfMarkPos}) column \code{markDistCen}
#' or \code{markPos}  is to the center of the mark, if \code{"beg"} = the distance you provided is to the
#'   beginning of the mark (Default)
#' @param chrWidth numeric, relative chromosome width. Defaults to \code{0.5}
#' @param specialChrWidth numeric, relative chromosome width. Defaults to \code{0.5} for OTUs in \code{specialOTUNames}
#' @param chrSpacing numeric, horizontal spacing among chromosomes, see also  \code{chrWidth}. Defaults to \code{0.5}
#' @param specialChrSpacing numeric, horizontal spacing among chromosomes for OTUs in \code{specialOTUNames}, see also  \code{chrWidth}. Defaults to \code{0.5}
#' @param chrColor character, main color for chromosomes. Defaults to \code{"gray"}
#' @param chrBorderColor character, color for border of chromosomes, defaults to \code{chrColor}
#' @param cenColor character, color for centromeres, if GISH use \code{NULL} or \code{NA}. Defaults to \code{chrColor}
#' @param roundness numeric, shape of vertices of chromosomes and square marks,
#'   higher values more squared. Defaults to \code{4}
#' @param karHeight numeric, vertical size of karyotypes. See also  \code{karHeiSpace}. Defaults to \code{2}
#' @param karHeiSpace numeric, vertical size of karyotypes including spacing. Proportional to \code{karHeight}, if overlap, increase. Defautl value \code{2.5}
#' @param karSepar boolean, reduce distance among karyotypes \code{FALSE} = equally
#'   sized karyotypes or \code{TRUE} = equally spaced karyotypes. Incompatible with \code{addMissingOTUAfter}
#' @param amoSepar numeric, depends on \code{karSepar=TRUE}, if zero your
#'   karyotypes will have no distance among them, if overlap,
#'   increase this and \code{karHeiSpace}
#' @param chrId character, print name of chromosome, \code{"original"} uses the original
#'   name in OTU column of dfChrSize, \code{"simple"} (just 1 to ...) or \code{""} (none).
#' @param distTextChr numeric, distance from name of chromosome to chromosome,
#'   also affects vertical separation of indices. Defaults to \code{1}
#' @param groupUp boolean, when \code{TRUE} when groups present, they appear over the chr. name. Defaults to \code{FALSE}
#' @param indexIdTextSize numeric, font size of chr. and kar. indices and
#'   chromosome name. Defaults to \code{1}
#' @param OTUTextSize numeric, font size of OTU name (species). Defaults to \code{1}. When \code{OTUasNote} is \code{TRUE}, use  \code{notesTextSize} instead
#' @param legend character, \code{""} for no legend; \code{"inline"} prints labels near
#'   chromosomes; \code{"aside"} prints legend to the right of karyotypes (default). See \code{markLabelSpacer}
#' @param legendWidth numeric, factor to increase width of squares and of legend. Defaults to \code{1.7}
#' @param legendHeight numeric, factor to increase height of squares and dots of legend. Automatic.
#' @param defaultStyleMark character, default style of mark, only used when \code{style} column of \code{dfMarkColor} data.frame is missing or in absence of this data.frame. Use \code{"square"} (default) \code{"dots"} or \code{"cM"}. Introduced in 1.13
#' @param colorBorderMark character, without default, pass a name of a color to use as border of marks. See \code{borderOfWhiteMarks}
#' @param borderOfWhiteMarks boolean, if \code{TRUE} (Default) uses black border for white marks. See \code{dfMarkColor}. Does not apply to marks with style \code{cenStyle}
#' @param protruding numeric, when style of mark is \code{"cM"}, fraction of chrWidth to stretch marker. Defaults to \code{0.2}. Introduced in 1.13
#' @param markLabelSize numeric, only if legend != (not) "", size of the text of
#'   labels of marks (legend). Defaults to \code{1}
#' @param markLabelSpacer numeric, only if \code{legend="aside"}, space from the
#'   rightmost chr. to legend. Defaults to \code{1}
#' @param legendYcoord numeric, modify Y position of legend when \code{legend="aside"}
#' @param pattern REGEX pattern to remove from names of marks
#' @param chrIndex character, add arm ratio with \code{"AR"} and centromeric index with \code{"CI"}, or \code{"both"} (Default), or \code{""} for none
#' @param nameChrIndexPos numeric, modify position of name of chr. indices
#' @param karIndex logical, add karyotype indices A (intrachromosomal -
#'   centromere pos.) and A2 (interchromosomal asymmetry, variation among
#'   chromosome sizes)
#' @param karIndexPos numeric, move karyotype index
#' @param notesPos numeric, move notes to the right
#' @param morpho character, if \code{"both"} (default) prints the Guerra and Levan classif of cen. position, use also \code{"Guerra"} or  \code{"Levan"} or \code{""} for none. See also \code{?armRatioCI}.
#' @param addOTUName boolean, if \code{TRUE} adds OTU (species) name to karyotype
#' @param OTUfont numeric, \code{1} for normal,  \code{2} for bold,  \code{3} for italics,  \code{4} for bold-italics
#' @param OTUfamily character, font family for OTU name.
#' @param OTUasNote boolean, if \code{TRUE} adds OTU (species) name to the right, see \code{notes}
#' @param revOTUs boolean, The order of species is the one in the main
#'   data.frame, use \code{TRUE} to reverse
#' @param ruler boolean, display ruler to the left of karyotype, when \code{FALSE} no ruler
#' @param rulerPos numeric, absolute position of ruler, corresponds to \code{pos}
#'   argument of \code{axis} R plot
#' @param rulerPosMod numeric, modify position of ruler, corresponds to \code{line}
#'   argument of \code{axis} R plot
#' @param ruler.tck numeric, tick size of ruler, corresponds to \code{tck} argument of
#'   \code{axis} R plot. Defaults to \code{-0.02}
#' @param rulerNumberPos numeric, modify position of numbers of ruler. Defaults to \code{0.5}
#' @param rulerNumberSize numeric, size of number's font in ruler. Defaults to \code{1}
#' @param ceilingFactor numeric, affects number of decimals for ceiling. Affects max. value of ruler. Defaults to \code{0}. When \code{threshold} is greater than \code{35} this may have to be negative. Introduced in 1.13
#' @param rulerInterval numeric, intervals in ruler. No default, automatic. Introduced in 1.13
#' @param rulerIntervalcM numeric, intervals in ruler of OTU in \code{specialOTUNames}. No default. Introduced in 1.13
#' @param rulerIntervalMb numeric, intervals in ruler of OTU with data in Mb (>\code{MbThreshold}) and absent from \code{specialOTUNames}. No default. Introduced in 1.13
#' @param yTitle character, units for common title. Defaults to \eqn{\mu m}
#' @param specialOTUNames character vector, normally title of ruler is micrometer or Mb (big numbers). Use this param. to be able to put a different unit in ruler title. See \code{"specialyTitle"}
#' @param specialyTitle, character, title of ruler if OTU is in \code{specialOTUNames}. Will not apply if \code{MbThreshold} met. In that case use \code{MbUnit}
#' @param xlimLeftMod numeric, modifies \code{xlim} left argument of plot
#' @param xlimRightMod numeric, \code{xlim} right side modification by adding space to the right
#'   of idiograms. Defaults to \code{2}
#' @param ylimBotMod numeric, modify \code{ylim} bottom argument of plot
#' @param ylimTopMod numeric, modify \code{ylim} top argument of plot
#' @param lwd.cM thickness of cM marks. Defaults to \code{lwd.chr}
#' @param lwd.chr thickness of border of chr., marks; ruler. Also thick of cM marks if \code{lwd.cM} absent Defaults to \code{0.5}
#' @param MbThreshold, numeric, if greater than this number (defaults to \code{10000}), \code{MbUnit} will apply and \code{specialyTitle} will not.
#' @param threshold, this is the max. value allowed for the main two significative digits, otherwise scale will shrink. For example, after 35 \eqn{\mu m} (Default), apparent size will be 3.5 and scale interval will change. See also \code{ceilingFactor}, you may have to use \code{-1}. Introduced in 1.13
#' @param MbUnit, character, text of units of title when \code{MbThreshold} met and OTU not in \code{specialOTUNames}. See \code{specialyTitle}
#' Defaults to \code{"Mb"}, but anything can be used. Introduced in 1.13. See \code{specialyTitle}
#' @param ylabline, numeric, modify position of ruler title. See \code{yTitle, specialyTitle, MbUnit}
#' @param rulerTitleSize, numeric font size of units of ruler. See also \code{ylabline}
#' @param n, numeric vertices number for round corners
#' @param notes, data.frame, optional, with columns \code{OTU} and \code{note} for adding notes to each OTU, they appear to the right of chromosomes
#' @param notesTextSize numeric, font size of notes, see \code{notes}
#' @param fixCenBorder boolean, when \code{TRUE} uses \code{chrColor} as centromere (and cen. mark) border color. See also \code{cenColor},
#' \code{chrColor}, \code{colorBorderMark}, \code{borderOfWhiteMarks}. No default value.
#' @param propWidth, boolean, defaults to \code{FALSE}. Diminishes chr. width with increasing number of OTUs
#' @param asp, numeric, y x aspect of plot. Defautls to \code{1}
#' @param defaultFontFamily character. use this as the font family. No default value.
#' @param circularPlot boolean, if \code{TRUE} chromosomes are plotted in concentric circles. Defaults to \code{FALSE}
#' @param shrinkFactor numeric, for \code{circularPlot=TRUE} percentage of usage of circle. Defaults to \code{0.9}
#' @param separFactor numeric, for \code{circularPlot=TRUE} modify separation of concentric karyotypes. Defaults to \code{1.5}
#' @param labelSpacing numeric, for \code{circularPlot=TRUE}. Spacing of mark labels. Defaults to \code{0.7}
#' @param labelOutwards boolean, inline labels projected outwards
#' @param chrLabelSpacing numeric, for \code{circularPlot=TRUE}. Spacing of chr. labels. Defaults to \code{0.5}
#' @param OTUlabelSpacing numeric, for \code{circularPlot=TRUE}. Spacing for OTU names. Defaults to \code{0.3}
#' @param radius numeric, for \code{circularPlot=TRUE}. Affects radius of karyotypes. Defaults to \code{0.5}
#' @param OTUsrt numeric, for \code{circularPlot=TRUE}. Angle to use for OTU names. Defaults to \code{0}
#' @param OTUplacing character, for \code{circularPlot=TRUE}. location of OTU name. Defaults to \code{"first"} plots name near
#' first chr. \code{"number"} places number near 1st chr. and index and name to the right or center.
#' \code{"simple"} place name to the right or center without numbering. See also \code{OTUcentered}
#' @param useOneDot boolean, for \code{circularPlot=TRUE}. use one dot instead of two in style of marks \code{dots}. Defaults to \code{TRUE}
#' @param circleCenter numeric, for \code{circularPlot=TRUE}. Coordinate X of center of circles. Affects \code{legend="aside"} position. Defaults to \code{1}
#' @param circleCenterY numeric, for \code{circularPlot=TRUE}. Coordinate Y of center of circles. Affects \code{legend="aside"} position. Defaults to \code{1}
#' @param OTULabelSpacerx numeric, for \code{circularPlot=TRUE} and \code{OTUplacing="number" or "simple"}. Modifies x names position
#' @param OTULabelSpacery numeric, for \code{circularPlot=TRUE} and \code{OTUplacing="number" or "simple"}. Modifies y names position
#' @param OTUcentered boolean, for \code{circularPlot=TRUE} and \code{OTUplacing="number" or "simple"}. OTU name in center of circle
#' when \code{TRUE}, otherwise, to the right.
#' @param OTUjustif numeric, for \code{circularPlot=TRUE} and \code{OTUplacing="number" or "simple"}. Justification of OTU name. \code{0} = left
#' (Default); use \code{0.5} for centered. See \code{?text} -> \code{adj}
#' @param OTUlegendHeight numeric, for \code{circularPlot=TRUE} and \code{OTUplacing="number" or "simple"}. Modifies y names separation
#' @param callPlot boolean, create new plot in your device. Defaults to \code{TRUE}
#' @param rotation numeric, anti-clockwise rotation, defaults to \code{0.5} which rotates chr. from top to -90 degrees. (-0.5*\eqn{\pi} )
#' @param ... accepts other arguments for the plot, see, \code{?plot}
#'
#' @keywords data.frame chromosome
#'
#' @importFrom graphics par plot segments mtext
#' @importFrom dplyr bind_rows
#' @importFrom grDevices col2rgb
#'
#' @export
#'
#' @examples
#' data(dfOfChrSize)
#' plotIdiograms(dfOfChrSize, ylimBotMod = .75, rulerPos=-.5)
#' plotIdiograms(dfOfChrSize, circularPlot = TRUE, chrLabelSpacing = 1)
#' plotIdiograms(dfChrSizeHolo, rulerPos=-.5)
#' @seealso \code{\link{asymmetry}}
#' @seealso \code{\link{armRatioCI}}
#' @seealso \code{\link{chrbasicdatamono}}
#' @seealso \code{\link{chrbasicdataHolo}}
#' @seealso \code{\link{markposDFs}}
#' @seealso \code{\link{markdataholo}}
#' @seealso \code{\link{dfMarkColor}}
#' @return plot
plotIdiograms <- function(dfChrSize, # karyotype

  defaultFontFamily,
  revOTUs=FALSE,
  karHeight=2,karHeiSpace=2.5,
  karSepar=TRUE,amoSepar=9,
  addMissingOTUAfter=NA,missOTUspacings=0,
  n=50,
  notes, notesTextSize=.4, notesPos=.5,
  propWidth=FALSE,

  # Units
  MbThreshold=10000,
  threshold=35,
  # divisor,
  MbUnit="Mb",
  yTitle="\u00B5m",
  specialyTitle="cM",
  specialOTUNames="",

  #OTU names
  addOTUName=TRUE,
  OTUTextSize=1,
  OTUfont,
  OTUfamily,
  # OTUasNote=TRUE,
  OTUasNote=FALSE,

  # chromosomes
  orderBySize=TRUE,
  # orderBySize=FALSE,
  chrId="original",
  # chrId="simple",
  indexIdTextSize=1,
  distTextChr=1,
  groupUp=FALSE,
  chrWidth=0.5,
  chrSpacing=0.5,
  specialChrWidth=0.3, specialChrSpacing=0.7,

  chrColor="gray",
  chrBorderColor,
  centromereSize=NA,
  cenColor,
  fixCenBorder,
  roundness=4,
  lwd.chr=0.5,
  lwd.cM,

  #marks
  dfMarkPos, dfCenMarks,
  defaultStyleMark="square",
  markDistType="beg",
  protruding=0.2,
  # markDistType="cen",
  origin="b",
  # origin="t",
  cMBeginCenter=FALSE,
  arrowhead = .3,
  shrinkArrow = .3333,
  arrowheadWidthShrink = .1,
  arrowsToSide=TRUE,

  dfMarkColor,
  mycolors,
  borderOfWhiteMarks=TRUE,
  colorBorderMark,
  pattern="",

  # mark labels
  legend="aside",
  # legend="inline",
  legendWidth=1.7, legendHeight=NA,
  markLabelSize=1,
  markLabelSpacer=1,
  legendYcoord=0,

  #indices
  chrIndex="both",
  morpho="both",
  nameChrIndexPos=2,
  karIndex=TRUE,
  # karIndex=FALSE,
  karIndexPos=.5,

  # rulers
  ruler=TRUE,
  rulerPos=0,
  rulerPosMod=0,
  ruler.tck=-0.02,
  rulerNumberPos=0.5,
  rulerNumberSize=1,
  rulerInterval,
  rulerIntervalcM,
  rulerIntervalMb,
  ceilingFactor=0,
  ylabline=0,
  rulerTitleSize=1,

  # margins
  xlimLeftMod=1, xlimRightMod=2,
  ylimBotMod=.2, ylimTopMod=.2,
  callPlot=TRUE,
  asp=1,

  # c plot
  circularPlot=FALSE,
  # circularPlot=TRUE,

  shrinkFactor=.9,
  separFactor=1.5,
  labelSpacing=.7,
  labelOutwards=FALSE,
  chrLabelSpacing=.5,
  # radius=1,
  radius=.5,

  rotation=0.5,
  useOneDot=TRUE,
  circleCenter=1,
  circleCenterY=1,

  # c. p OTU
  OTUlabelSpacing=.3,
  OTUsrt=0,
  OTUplacing="first",
  # useOneDot=FALSE,
  OTULabelSpacerx=0,
  OTULabelSpacery=0,
  OTUcentered=TRUE,
  OTUjustif=0,
  OTUlegendHeight=NA,
  ...) {

  xfactor <- yfactor <- 1

  chrWFactor<-specialChrWidth/chrWidth

  if(circularPlot){
    chrWidth<-chrWidth*4
    legendHeight<-legendHeight*4
    # legendWidth<-legendWidth*2
    specialChrWidth<-specialChrWidth*4
  }

  if(!missing(defaultFontFamily)){
    defaultFontFamily2<-defaultFontFamily
  } else {
    defaultFontFamily2<-"sans"
  }

  if(!missing(lwd.cM)){
    lwd.cM2 <- lwd.cM
  } else {
    lwd.cM2 <- lwd.chr
  }

  OTUfont2<-ifelse( !missing(OTUfont),   OTUfont,   1)
  OTUfamily2<-ifelse( !missing(OTUfamily), OTUfamily, defaultFontFamily2)

  if(!missing(dfChrSize)){
    dfChrSizeInternal<-makeNumCols(dfChrSize)
  } else {
    message(crayon::red("Missing mandatory dfChrSize data.frame"))
    return(NA)
  }

  if(!missing(dfMarkPos) ) {

    #
    #   rename column markArm if necessary
    #

    if("markArm" %in% colnames(dfMarkPos)  ){
      message(crayon::red(paste(c("Column markArm in d.f. of marks renamed to chrRegion")))
      ) # mess
      colnames(dfMarkPos)[which(names(dfMarkPos)=="markArm")]<-"chrRegion"
    }

    dfMarkPos[dfMarkPos==""]<-NA
    dfMarkPosInternal1 <- dfMarkPosInternal <- makeNumCols(dfMarkPos)

    if(is.null(dfMarkPosInternal1$markPos)){
      dfMarkPosInternal1$markPos<-NA
    }
    if(is.null(dfMarkPosInternal1$markSize)){
      dfMarkPosInternal1$markSize<-NA
    }
    if(is.null(dfMarkPosInternal1$markDistCen)){
      dfMarkPosInternal1$markDistCen<-NA
    }

    dfCenMarksInternal <- dfMarkPosInternal1[which(dfMarkPosInternal1$chrRegion=="cen"),]

    if(nrow(dfCenMarksInternal)==0 ){
      remove(dfCenMarksInternal)
    }
    dfpGISHInternal <- dfMarkPosInternal1[which(dfMarkPosInternal1$chrRegion %in% "p" &
                                                  is.na(dfMarkPosInternal1$markSize) &
                                                  is.na(dfMarkPosInternal1$markDistCen)
                                                ),]
    if(nrow(dfpGISHInternal)==0 ){
      remove(dfpGISHInternal)
    }

    dfqGISHInternal <- dfMarkPosInternal1[which(dfMarkPosInternal1$chrRegion %in% "q" &
                                                  is.na(dfMarkPosInternal1$markSize) &
                                                  is.na(dfMarkPosInternal1$markDistCen)
    ),]
    if(nrow(dfqGISHInternal)==0 ){
      remove(dfqGISHInternal)
    }

    dfwholeGISHInternal <- dfMarkPosInternal1[which(dfMarkPosInternal1$chrRegion %in% "w" &
                                                  is.na(dfMarkPosInternal1$markSize) &
                                                  (is.na(dfMarkPosInternal1$markDistCen) |
                                                   is.na(dfMarkPosInternal1$markPos) )
    ),]

    if(nrow(dfwholeGISHInternal)==0 ){
      remove(dfwholeGISHInternal)
    }

} # df mark pos

  if (exists("dfMarkPosInternal")) {

    listOfdfMarkPosInternal<-dfToListColumn(dfMarkPosInternal)

    # listOfdfMarkPosInternal <- lapply(listOfdfMarkPosInternal, function(x) makeCharCols(x) )

    dfMarkPosInternal <- dplyr::bind_rows(listOfdfMarkPosInternal, .id = "OTU")
    # dfMarkPosInternal <- makeNumCols(dfMarkPosInternal )

    # listOfdfMarkPosInternal <- lapply(listOfdfMarkPosInternal, function(x) makeNumCols(x))

  } # df of marks

  if(!missing(dfCenMarks)  ) {
    dfCenMarksInternal2<-makeNumCols(dfCenMarks)
  }
  if(exists("dfCenMarksInternal") & exists("dfCenMarksInternal2") ) {
    dfCenMarksInternal<-dplyr::bind_rows(dfCenMarksInternal,dfCenMarksInternal2)
  }
  if(!exists("dfCenMarksInternal") & exists("dfCenMarksInternal2") ) {
    dfCenMarksInternal<-dfCenMarksInternal2
  }

  if (exists("dfCenMarksInternal")){

    listOfdfDataCen<-dfToListColumn(dfCenMarksInternal)

    dfCenMarksInternal <- dplyr::bind_rows(listOfdfDataCen, .id = "OTU")
  } # cen internal

  #
  #   mark style
  #

  if(!missing(dfMarkColor)){
    dfMarkColorInternal<-makeNumCols(dfMarkColor)
  }

  message(crayon::black(paste("Making checks\n")) )
  message(crayon::black(paste("In case of error see messages and the help ?functionName\n")) )

  #
  #   dfChrSizeInternal
  #

  #################################################################### LISTS

  #
  # transform dfs to list of df dfChrSizeInternal
  #

  if (!"OTU" %in% colnames(dfChrSizeInternal) ) {
    addOTUName<-FALSE
    OTUasNote <-FALSE
  }

listOfdfChromSize<-dfToListColumn(dfChrSizeInternal)

  #
  #   reconstitute dfChrSizeInternal OTU
  #

# dfChrSizeInternal <- dplyr::bind_rows(listOfdfChromSize, .id = "OTU")

  #
  #   Classify data.frames from list as monocen or holocen Add attribute cenType
  #

listOfdfChromSize <- addAttributesDfChrSize(listOfdfChromSize,threshold,specialOTUNames,centromereSize,MbThreshold)

  dfChrSizeInternal <- dplyr::bind_rows(listOfdfChromSize, .id = "OTU")
  dfChrSizeInternal <- makeNumCols(dfChrSizeInternal)

  # important must be after bind_rows
  listOfdfChromSize <- lapply(listOfdfChromSize, function(x) makeNumCols(x))



  #
  #     calculate armRatioCI only for chr. with centromere attr cenType =  holocen.
  #

  #
  #    generate Chromosome indexes for Monocen
  #

  if(chrIndex=="both" | chrIndex=="AR"| chrIndex=="CI" | morpho=="both" | morpho=="Guerra" | morpho == "Levan") {
    for (i in 1:length(listOfdfChromSize)) {
      if(attr(listOfdfChromSize[[i]], "cenType")=="monocen"){ # only for monocen

        listOfdfChromSize[[i]]<-armRatioCI(listOfdfChromSize[[i]])

        if(attr(listOfdfChromSize[[i]], "indexStatus")=="failure"){

          if("OTU" %in% colnames(listOfdfChromSize[[i]])){
            message(crayon::red(paste("in",unique(listOfdfChromSize[[i]]$OTU) ) ) )
            } # otu
          message(crayon::red("\nFix measures or use chrIndex=\"\", and morpho=\"\" ")
                  ) #m
      } # if failure
    } # monocen
    } # for
  } # if chrIndex

  ##############################################################################
  #
  #   names of holo and mono
  #

{
  monocenNames<-makeVectorNames(listOfdfChromSize,"cenType","monocen")

  holocenNames<-makeVectorNames(listOfdfChromSize,"cenType","holocen")
}

  ##########################################3
  #
  #     gish  p
  #
  #########################################

# message(crayon::green("GISH data loading"))

  if (exists("dfpGISHInternal")) {

    listOfdfpGISHInternal<-dfToListColumn(dfpGISHInternal)

    # monocen

    listOfdfpGISHInternalMonocen<-listOfdfpGISHInternal[which(names(listOfdfpGISHInternal) %in% monocenNames)]

    # names(listOfdfpGISHInternalMonocen)
    if(length(listOfdfpGISHInternalMonocen)==0){
      remove(listOfdfpGISHInternalMonocen)
    } else {
      listOfdfpGISHInternalMonocen<-Filter(function(x) {nrow(x) >= 1}, listOfdfpGISHInternalMonocen)
      dfpGISHInternalMonocen <- dplyr::bind_rows(listOfdfpGISHInternalMonocen, .id = "OTU")
      # dfpGISHInternalMonocen$chrRegion<-"p"
    } # else

    # P marks of Holocen MUST NOt exist

    checkArmHolocenError(listOfdfpGISHInternal,holocenNames)

  } #   if (exists("dfpGISHInternal")){

  ##########################################3
  #
  #     gish  q
  #
  #########################################

  if (exists("dfqGISHInternal")){

    listOfdfqGISHInternal<-dfToListColumn(dfqGISHInternal)

    # monocen

    listOfdfqGISHInternalMonocen<-listOfdfqGISHInternal[which(names(listOfdfqGISHInternal) %in% monocenNames)]

    if(length(listOfdfqGISHInternalMonocen)==0){
      remove(listOfdfqGISHInternalMonocen)
    } else {
      listOfdfqGISHInternalMonocen<-Filter(function(x) {nrow(x) >= 1}, listOfdfqGISHInternalMonocen)
      dfqGISHInternalMonocen <- dplyr::bind_rows(listOfdfqGISHInternalMonocen, .id = "OTU")
    } # else

    # q marks of Holocen MUST NOt exist

    checkArmHolocenError(listOfdfqGISHInternal,holocenNames)

  } #   if (exists("dfpGISHInternal")){

  ##########################################3
  #
  #     gish whole
  #
  #########################################

  if(exists("dfwholeGISHInternal")){

    listOfdfwholeGISHInternal<-dfToListColumn(dfwholeGISHInternal)

    ###########################################################################################################################3
    #
    # MONOCEN GISH TO P Q CEN
    #

    listOfdfwholeGISHMonocen<-listOfdfwholeGISHInternal[which(names(listOfdfwholeGISHInternal) %in% monocenNames)]

    if(length(listOfdfwholeGISHMonocen)==0){
      remove(listOfdfwholeGISHMonocen)
    } else {
      listOfdfwholeGISHMonocen<-Filter(function(x) {nrow(x) >= 1}, listOfdfwholeGISHMonocen)

      #
      #   p part
      #

      listOfdfpGISHInternalMonocen2 <- listOfdfwholeGISHMonocen

      dfpGISHInternalMonocen2 <- dplyr::bind_rows(listOfdfpGISHInternalMonocen2, .id = "OTU")
      dfpGISHInternalMonocen2$chrRegion<-"p"

      #
      #   q part
      #

      listOfdfqGISHInternalMonocen2 <- listOfdfwholeGISHMonocen

      dfqGISHInternalMonocen2 <- dplyr::bind_rows(listOfdfqGISHInternalMonocen2, .id = "OTU")
      dfqGISHInternalMonocen2$chrRegion<-"q"

      #
      # cen part
      #

      listOfdfCenMarksInternal2 <- listOfdfwholeGISHMonocen
      dfCenMarksInternal2 <- dplyr::bind_rows(listOfdfCenMarksInternal2, .id = "OTU")
      dfCenMarksInternal2$chrRegion<-"cen"

      if(exists("dfCenMarksInternal") & exists("dfCenMarksInternal2") ) {
        dfCenMarksInternal <- dplyr::bind_rows(dfCenMarksInternal,dfCenMarksInternal2)
      }
      if(!exists("dfCenMarksInternal") & exists("dfCenMarksInternal2") ) {
        dfCenMarksInternal <- dfCenMarksInternal2
      }
    } # else

    #
    # HOLOCEN
    #
    ###########################################################################################################

    listOfdfwholeGISHHolocen<-listOfdfwholeGISHInternal[which(names(listOfdfwholeGISHInternal) %in% holocenNames)]

    if(length(listOfdfwholeGISHHolocen)==0){
      remove(listOfdfwholeGISHHolocen)
    } else {

    dfwholeGISHHolocen <- dplyr::bind_rows(listOfdfwholeGISHHolocen, .id = "OTU")
    #
    # remake chrom sizes df
    #

    # dfChrSizeInternal <- dplyr::bind_rows(listOfdfChromSize, .id = "OTU")

    dfwholeGISHHolocen$markSize<-dfChrSizeInternal[match(interaction(dfwholeGISHHolocen[c("OTU","chrName")] ),
                                                                 interaction(dfChrSizeInternal[c("OTU","chrName") ] )
                                                                 ),]$chrSize
    dfwholeGISHHolocen$markPos<-0
    if(markDistType=="cen") { # center
      dfwholeGISHHolocen$markPos <- dfChrSizeInternal[match(interaction(dfwholeGISHHolocen[c("OTU","chrName")] ),
                                                           interaction(dfChrSizeInternal[c("OTU","chrName") ] )
      ),]$chrSize/2
    }

    #
    #   merge dfMarkPosInternal and dfwholeGISHHolocen
    #

    if(exists("dfMarkPosInternal") & exists("dfwholeGISHHolocen") ) {
      dfMarkPosInternal <- dplyr::bind_rows(dfMarkPosInternal,dfwholeGISHHolocen)
    }
    if(!exists("dfMarkPosInternal") & exists("dfwholeGISHHolocen") ) {
      dfMarkPosInternal<-dfwholeGISHHolocen
    }
    } #     if(length(listOfdfwholeGISHHolocen)==0){

  }  #  end   if(exists("dfwholeGISHInternal")){

  #################################################################################################################3
  #
  #   merge p
  #

  if(exists("dfpGISHInternalMonocen") & exists("dfpGISHInternalMonocen2") ) {
    dfpGISHInternalMonocen <- dplyr::bind_rows(dfpGISHInternalMonocen,dfpGISHInternalMonocen2)
  }

  if(!exists("dfpGISHInternalMonocen") & exists("dfpGISHInternalMonocen2") ) {
    dfpGISHInternalMonocen<-dfpGISHInternalMonocen2
  }

  if(exists("dfpGISHInternalMonocen") ) {

    dfpGISHInternalMonocen<-markDistCenGISHfix(dfpGISHInternalMonocen,dfChrSizeInternal,"shortArmSize",markDistType)
  } # p gish

  ############################################################################################
  # q

  if(exists("dfqGISHInternalMonocen") & exists("dfqGISHInternalMonocen2") ) {
    dfqGISHInternalMonocen<-dplyr::bind_rows(dfqGISHInternalMonocen,dfqGISHInternalMonocen2)
  }
  if(!exists("dfqGISHInternalMonocen") & exists("dfqGISHInternalMonocen2") ) {
    dfqGISHInternalMonocen<-dfqGISHInternalMonocen2
  }

  if(exists("dfqGISHInternalMonocen") ) {
    dfqGISHInternalMonocen<-markDistCenGISHfix(dfqGISHInternalMonocen,dfChrSizeInternal,"longArmSize",markDistType)
    #                                  error=function(e) NA)
  } # q gish

  ##################################################################################################
  #
  #       merging p and q
  #
  ##################################################################################################

  if(exists("dfqGISHInternalMonocen") & exists("dfpGISHInternalMonocen") ) {
    dfMarkPosInternal2 <- dplyr::bind_rows(dfqGISHInternalMonocen,dfpGISHInternalMonocen)
  }
  if(!exists("dfqGISHInternalMonocen") & exists("dfpGISHInternalMonocen") ) {
    dfMarkPosInternal2 <- dfpGISHInternalMonocen
  }
  if(exists("dfqGISHInternalMonocen") & !exists("dfpGISHInternalMonocen") ) {
    dfMarkPosInternal2 <- dfqGISHInternalMonocen
  }

  #
  #    merge dfMarkPosInternal2 dfMarkPosInternal
  #

  if(exists("dfMarkPosInternal") & exists("dfMarkPosInternal2") ) {
    dfMarkPosInternal <- dplyr::bind_rows(dfMarkPosInternal,dfMarkPosInternal2)
  }
  if(!exists("dfMarkPosInternal") & exists("dfMarkPosInternal2") ) {
    dfMarkPosInternal<-dfMarkPosInternal2
  }

  #
  #     DF OF marks to list
  #

  if (exists("dfMarkPosInternal")) {

    dfMarkPosInternal <- unique(dfMarkPosInternal)

    listOfdfMarkPosInternal<-dfToListColumn(dfMarkPosInternal)

    #
    #              monocen marks list
    #

    listOfdfMarkPosMonocen<-listOfdfMarkPosInternal[which(names(listOfdfMarkPosInternal) %in% monocenNames)]
#    listOfdfMarkPosMonocen908<<-listOfdfMarkPosMonocen

    # names(listOfdfMarkPosMonocen)
    if(length(listOfdfMarkPosMonocen)==0){
      remove(listOfdfMarkPosMonocen)
    } else {
      for (i in 1:length(listOfdfMarkPosMonocen)){
      listOfdfMarkPosMonocen[[i]]  <- listOfdfMarkPosMonocen[[i]][which(listOfdfMarkPosMonocen[[i]]$chrRegion!="cen"),]
      } # for
      listOfdfMarkPosMonocen<-Filter(function(x) {nrow(x) >= 1}, listOfdfMarkPosMonocen)
    }# else

    #
    #                holocen marks list
    #

    listOfdfMarkPosHolocen<-listOfdfMarkPosInternal[which(names(listOfdfMarkPosInternal) %in% holocenNames)]
    if(length(listOfdfMarkPosHolocen)==0){
      remove(listOfdfMarkPosHolocen)
    }

} # end missing dfMarkPosInternal

  #
  #   df of cen marks to list
  #

  if (exists("dfCenMarksInternal")){

    listOfdfDataCen<-dfToListColumn(dfCenMarksInternal)

    listOfdfDataCen<-listOfdfDataCen[which(names(listOfdfDataCen) %in% monocenNames)]
    if(length(listOfdfDataCen)==0){
      remove(listOfdfDataCen)
    } else {
      #
      #   remove columns without info.
      #
      for (i in 1:length(listOfdfDataCen)){
        listOfdfDataCen[[i]][listOfdfDataCen[[i]]==""]<-NA
        listOfdfDataCen[[i]]<-  listOfdfDataCen[[i]][, !apply(is.na(listOfdfDataCen[[i]]), 2, all)]
      } # for
    } # else
  } # end missing dfCenMarksInternal

  #
  #   for each d.f. of dfmarkpos check columns
  #

  ############################################################################################################################
  #
  #   Monocen check marks
  #

  if(exists("listOfdfMarkPosMonocen")){
    message(crayon::black("\nChecking mandatory columns from dfMarkPos: chrName, markName, chrRegion,markDistCen\n (column OTU  is necessary if more than one species)\nmarkSize can be absent when cM style"
    ) )# cat

    for (i in 1:length(listOfdfMarkPosMonocen ) ) {

      listOfdfMarkPosMonocen[[i]][listOfdfMarkPosMonocen[[i]]==""] <- NA
      listOfdfMarkPosMonocen[[i]] <- listOfdfMarkPosMonocen[[i]][, !apply(is.na(listOfdfMarkPosMonocen[[i]]), 2, all)]

      #
      #   rename column markpos if necessary
      #

      if(!"markDistCen" %in% colnames(listOfdfMarkPosMonocen[[i]]) & "markPos" %in% colnames(listOfdfMarkPosMonocen[[i]])  ){
        message(crayon::red(paste(c("Columns markPos in d.f. of marks of OTU",names(listOfdfMarkPosMonocen)[[i]] ,"renamed to markDistCen")))
        ) # mess
        colnames(listOfdfMarkPosMonocen[[i]])[which(names(listOfdfMarkPosMonocen[[i]])=="markPos")]<-"markDistCen"
      }

      #
      #   REMOVE GISH DATA incomplete duplicated data
      #

      listOfdfMarkPosMonocen[[i]] <- listOfdfMarkPosMonocen[[i]][setdiff(1:length(listOfdfMarkPosMonocen[[i]]$chrRegion),
                                                                         which(listOfdfMarkPosMonocen[[i]]$chrRegion %in% "p" &
is.na(listOfdfMarkPosMonocen[[i]]$markSize) &
is.na(listOfdfMarkPosMonocen[[i]]$markDistCen)
      ) ) ,]

      listOfdfMarkPosMonocen[[i]] <- listOfdfMarkPosMonocen[[i]][setdiff(1:length(listOfdfMarkPosMonocen[[i]]$chrRegion), which(listOfdfMarkPosMonocen[[i]]$chrRegion %in% "q" &
                                                                                                                                  is.na(listOfdfMarkPosMonocen[[i]]$markSize) &
                                                                                                                                  is.na(listOfdfMarkPosMonocen[[i]]$markDistCen)
      ) ) ,]

      listOfdfMarkPosMonocen[[i]] <- listOfdfMarkPosMonocen[[i]][setdiff(1:length(listOfdfMarkPosMonocen[[i]]$chrRegion), which(listOfdfMarkPosMonocen[[i]]$chrRegion %in% "w"
      ) ) ,]

      #
      #   column error check
      #

      # if(length (setdiff(c("chrName", "markName", "chrRegion","markDistCen","markSize"),
      if(length (setdiff(c("chrName", "markName", "chrRegion","markDistCen"),
                         colnames(listOfdfMarkPosMonocen[[i]]) ) )>0 ) {
        message(crayon::red(paste(c("ERROR Missing columns in d.f. of marks of OTU",names(listOfdfMarkPosMonocen)[[i]] ,":",
                                setdiff(c("chrName", "markName", "chrRegion","markDistCen"), # rem markSize
                                        colnames(listOfdfMarkPosMonocen[[i]]) ) ) , sep="\n", collapse = " " )
        )
        ) # cat
        message(crayon::red(paste("\nERRORS PRESENT, see above, dfMarksPos of OTU", names(listOfdfMarkPosMonocen)[[i]] ,"REMOVED\n")
        ) )#m
        listOfdfMarkPosMonocen[[i]]<-NA
      } # fi setdiff
      #
      #   column without error
      #
      else { # if no error

        corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosMonocen)[[i]] )
        divisor2<-as.numeric(attr(listOfdfChromSize[[corr_index]],"divisor"))
          listOfdfMarkPosMonocen[[i]]$markDistCen<-listOfdfMarkPosMonocen[[i]]$markDistCen/divisor2
            if("markSize" %in% colnames(listOfdfMarkPosMonocen[[i]])){
              listOfdfMarkPosMonocen[[i]]$markSize<-listOfdfMarkPosMonocen[[i]]$markSize/divisor2
            }

        message(crayon::black(paste("\nOK marks of OTU",names(listOfdfMarkPosMonocen)[[i]],"checked \n")
                ) ) #m
        if(markDistType=="cen") { # this is from center
          #
          #   fix bug when markDistType is cen (center) but cM style of marks have NA in markSize column
          #
          # halfMarkSize<-ifelse(is.na(listOfdfMarkPosMonocen[[i]]$markSize/2),0,(listOfdfMarkPosMonocen[[i]]$markSize/2) )
          if("markSize" %in% colnames(listOfdfMarkPosMonocen[[i]])){
          listOfdfMarkPosMonocen[[i]]$markDistCen <- psum(listOfdfMarkPosMonocen[[i]]$markDistCen,
                                                          ( - listOfdfMarkPosMonocen[[i]]$markSize/2),
                                                          na.rm=TRUE)
          }
        } # if
      } # else No Error
    } # for each data.frame of Marks of Monocen

  listOfdfMarkPosMonocen<-listOfdfMarkPosMonocen[!is.na(listOfdfMarkPosMonocen)]
  # do as before with holo 27/09

} # fi listOfdfMarkPosMonocen

  ##################################################################################################################
  #
  #   holocen check mark
  #

  if(exists("listOfdfMarkPosHolocen")){
    message(crayon::black("\nChecking mandatory columns from dfMarkPos (without cen.): chrName, markName, markPos\n (column OTU  is necessary if more than one species)\nmarkSize column is not necessary for style of mark cM"
    ) )# mess

    for (i in 1:length(listOfdfMarkPosHolocen ) ) {

      listOfdfMarkPosHolocen[[i]][listOfdfMarkPosHolocen[[i]]==""]<-NA
      listOfdfMarkPosHolocen[[i]]<-  listOfdfMarkPosHolocen[[i]][, !apply(is.na(listOfdfMarkPosHolocen[[i]]), 2, all)]

      #
      #   REMOVE GISH DATA incomplete duplicated data
      #

      listOfdfMarkPosHolocen[[i]] <- listOfdfMarkPosHolocen[[i]][setdiff(1:length(listOfdfMarkPosHolocen[[i]]$chrName), which(listOfdfMarkPosHolocen[[i]]$chrRegion %in% "w" & is.na(listOfdfMarkPosHolocen[[i]]$markSize )
      ) ) ,]

      #
      #   rename column markdistcen if necessary
      #

      if(!"markPos" %in% colnames(listOfdfMarkPosHolocen[[i]]) & "markDistCen" %in% colnames(listOfdfMarkPosHolocen[[i]])  ){
        message(crayon::red(paste(c("Columns markDistCen in d.f. of marks of OTU",names(listOfdfMarkPosHolocen)[[i]] ,"renamed to markPos")))
        ) # mess
        colnames(listOfdfMarkPosHolocen[[i]])[which(names(listOfdfMarkPosHolocen[[i]])=="markDistCen")]<-"markPos"
      }

      #
      #   column error
      #

      # if(length (setdiff(c("chrName", "markName", "markPos","markSize"),
      if(length (setdiff(c("chrName", "markName", "markPos"),
                           colnames(listOfdfMarkPosHolocen[[i]]) ) )>0 ){
          message(crayon::red(paste(c("ERROR Missing columns:",
                                      # setdiff(c("chrName", "markName", "markPos","markSize"),
                                              setdiff(c("chrName", "markName", "markPos"),
                                              colnames(listOfdfMarkPosHolocen[[i]]) ) ) , sep="\n", collapse = " " )
          )
          ) # cat
          message(crayon::red(paste("\nERRORS PRESENT, see above, dfMarksPos of OTU", names(listOfdfMarkPosHolocen)[[i]] ,"REMOVED\n")
          ) ) #m
          # listOfdfMarkPosHolocen<-listOfdfMarkPosHolocen[-i]
          listOfdfMarkPosHolocen[[i]]<-NA
        } # fi
      #
      #   column without error
      #

      else { # if no error
        message(crayon::black(paste("\nOK marks of OTU",names(listOfdfMarkPosHolocen)[[i]],"checked \n")
        ) ) #m

        if(origin=="t"){
          listOfdfMarkPosHolocen[[i]]$markPos2<-listOfdfMarkPosHolocen[[i]]$markPos
          listOfdfMarkPosHolocen[[i]]$chrSize<-
            #
            dfChrSizeInternal[match(interaction( listOfdfMarkPosHolocen[[i]][c("OTU", "chrName")]),
                                    interaction( dfChrSizeInternal[c("OTU", "chrName")] )
                                    ),]$chrSize

          if(markDistType=="beg"){
            if("markSize" %in% colnames(listOfdfMarkPosHolocen[[i]])){
              # markSize2<-ifelse(is.na( listOfdfMarkPosHolocen[[i]]$markSize  ) ,0, ( listOfdfMarkPosHolocen[[i]]$markSize ) )
              listOfdfMarkPosHolocen[[i]]$markPos<- psum(listOfdfMarkPosHolocen[[i]]$chrSize,
                                                         - listOfdfMarkPosHolocen[[i]]$markPos2,
                                                         - listOfdfMarkPosHolocen[[i]]$markSize,
                                                         na.rm=TRUE)
            } # markSize column exist

          } else if(markDistType=="cen"){
              if("markSize" %in% colnames(listOfdfMarkPosHolocen[[i]])){
                # halfMarkSize<-ifelse(is.na(listOfdfMarkPosHolocen[[i]]$markSize/2) ,0, ( (listOfdfMarkPosHolocen[[i]]$markSize/2) ) )
                listOfdfMarkPosHolocen[[i]]$markPos<-psum( listOfdfMarkPosHolocen[[i]]$chrSize,
                                                           - listOfdfMarkPosHolocen[[i]]$markPos2,
                                                           (- listOfdfMarkPosHolocen[[i]]$markSize/2),
                                                           na.rm=TRUE)
              } # col markSize exists
          } # cen

        } else if (origin=="b") { # if t else b

          if(markDistType=="cen") { # center
            if("markSize" %in% colnames(listOfdfMarkPosHolocen[[i]])){

            listOfdfMarkPosHolocen[[i]]$markPos <- psum(listOfdfMarkPosHolocen[[i]]$markPos,
                                                        (- listOfdfMarkPosHolocen[[i]]$markSize/2),
                                                        na.rm=TRUE)
            } # if col markSize exist
          } # cen
        } # origin b
      } # else No Error
    } # for each data.frame of Marks of Monocen

  listOfdfMarkPosHolocen<-listOfdfMarkPosHolocen[!is.na(listOfdfMarkPosHolocen)]

  for (i in 1:length(listOfdfMarkPosHolocen)) {

    corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosHolocen)[[i]] )
    divisor2<-as.numeric(attr(listOfdfChromSize[[corr_index]],"divisor"))

    # if(attr(listOfdfMarkPosHolocen[i],"name") %in% MbNames ){
      listOfdfMarkPosHolocen[[i]]$markPos <-listOfdfMarkPosHolocen[[i]]$markPos/divisor2
      if("markSize" %in% colnames(listOfdfMarkPosHolocen[[i]])){
        listOfdfMarkPosHolocen[[i]]$markSize<-listOfdfMarkPosHolocen[[i]]$markSize/divisor2
      }
    # } # if
  } # for
} # fi holocen exists

  ################################################################################################################################
  #
  #   cen Mark check
  #

  if(exists("listOfdfDataCen")){
    message(crayon::black("\nChecking mandatory columns from dfCenMarks: chrName, markName\n (column OTU  is necessary if more than one species)\n")
    ) # mess

    for (i in 1:length(listOfdfDataCen)){
      #
      #   columns with error
      #

    if(length(setdiff(c("chrName", "markName"),
                      colnames(listOfdfDataCen[[i]]) ) )>0 ){
      message(crayon::red(paste(c("ERROR Missing columns:",
                              setdiff(c("chrName", "markName"),
                                      colnames(listOfdfDataCen[[i]]) ),"in OTU", names(listOfdfDataCen)[[i]]   ), sep="\n", collapse = " " )
      )
      ) # cat
      message(crayon::red(paste("\nERRORS PRESENT, see above, dfCenMarks of OTU", names(listOfdfDataCen)[[i]] ,"REMOVED\n")
      ) ) #m
      listOfdfDataCen[[i]]<-NA

    } # fi

      #
      #   columns without error
      #

    else { # if no error
      message(crayon::black(paste("\nOK cen. marks of OTU",names(listOfdfDataCen)[[i]],"checked \n")
      ))# mess
    } # else
    } # for

  listOfdfDataCen<-listOfdfDataCen[!is.na(listOfdfDataCen)]

  } # fi   if(exists("listOfdfDataCen"))

  ##############################################################################################################
  #
  #   OTU cross check of d.fs
  #

  if(exists("listOfdfMarkPosMonocen")){

    listOfdfMarkPosMonocen<-filterExtraOTU(listOfdfChromSize,listOfdfMarkPosMonocen)

  } # exists

  if(exists("listOfdfMarkPosHolocen")){
    # message(crayon::black("\n####\ndfMarkPos exists, if error will be removed\n") )

    listOfdfMarkPosHolocen<-filterExtraOTU(listOfdfChromSize,listOfdfMarkPosHolocen)

  } # exists

  #
  #     check chromosomes names  from d.f. marks to chr. size. d.f.
  #

  if(exists("listOfdfMarkPosMonocen") ) {
    listOfChecksChr<-checkNameChrDfMarks(listOfdfChromSize,listOfdfMarkPosMonocen)
    listOfdfChromSize<-listOfChecksChr[[1]]
    listOfdfMarkPosMonocen<-listOfChecksChr[[2]]

    if(length(listOfdfMarkPosMonocen)==0){
      remove(listOfdfMarkPosMonocen)
    } else {

      allMarkNames<-unique(listOfChecksChr[[3]])

      if(length(listOfChecksChr[[4]])>0){
        allMarkMaxSize<-max(listOfChecksChr[[4]], na.rm=TRUE)
      }
    }
  } # listOfdfMarkPosMonocen


  if(exists("listOfdfMarkPosHolocen") ) {

#    # listOfdfMarkPosHolocen<<-listOfdfMarkPosHolocen
#    # listOfdfChromSize<<-listOfdfChromSize

    listOfChecksChr<-checkNameChrDfMarks(listOfdfChromSize,listOfdfMarkPosHolocen)
    listOfdfChromSize<-listOfChecksChr[[1]]

    listOfdfMarkPosHolocen<-listOfChecksChr[[2]]

    if(length(listOfdfMarkPosHolocen)==0){
      remove(listOfdfMarkPosHolocen)
    } else {

      if(exists("allMarkNames")) {
#        # allMarkNames1<<-allMarkNames
        allMarkNames<-unique(c(allMarkNames,listOfChecksChr[[3]] ) )
      } else {
        allMarkNames<-unique(listOfChecksChr[[3]] )
#        # allMarkNames2<<-allMarkNames
      }

      if(length(listOfChecksChr[[4]])>0){
        if (exists("allMarkMaxSize")){
          allMarkMaxSize<-max(c(allMarkMaxSize,max(listOfChecksChr[[4]], na.rm=TRUE) ), na.rm=TRUE)
        } else {
          allMarkMaxSize<-max(listOfChecksChr[[4]], na.rm=TRUE)
        }
      }
    }
  } # holocen

    if(exists("listOfdfDataCen") ) {
      listOfChecksChr<-checkNameChrDfMarks(listOfdfChromSize,listOfdfDataCen)
      listOfdfChromSize<-listOfChecksChr[[1]]
      listOfdfDataCen<-listOfChecksChr[[2]]

      if(length(listOfdfDataCen)==0){
        remove(listOfdfDataCen)
      } else {

      if(exists("allMarkNames")){
        allMarkNames<-unique(c(allMarkNames,listOfChecksChr[[3]] ) )
      } else {
        allMarkNames<-unique(listOfChecksChr[[3]])
      }
        cenMarkNames<-unique(listOfChecksChr[[3]])

      } # listOfdfDataCen 0
    } # listOfdfDataCen

  #############################################################################################################
  #
  #   check compatibility of columns dfMarkColor
  #

  if(!missing(cenColor)){
    cenColor2<-cenColor

    if( is.null(cenColor)  ){
      cenColor2<-NULL
    }

  } else {
    cenColor2<-chrColor
  }


  {
  if(exists("dfMarkColorInternal") ) {

#    dfMarkColorInternal1311<<-dfMarkColorInternal

    message(crayon::black("\n####\nChecking mandatory columns from dfMarkColor: markName, markColor\n"
    ) )#cat

    #
    #   create style column when missing
    #

    if(!"style" %in% colnames(dfMarkColorInternal) ) {
      message(crayon::red(paste0("\ndfMarkColor style column missing, created with string: ",defaultStyleMark,"\n")
                          ) ) # m
      dfMarkColorInternal$style <- defaultStyleMark    # "square" or user default
    }

    if (length( setdiff(c("markName", "markColor","style"),
                        colnames(dfMarkColorInternal) ) )>0 ){
      message(crayon::red(paste(c("ERROR Missing column:",
                              setdiff(c("markName", "markColor","style"),
                                      colnames(dfMarkColorInternal) ) ) , sep="\n", collapse = " ")
      )
      )# cat
      remove(dfMarkColorInternal)
      message(crayon::red("\nError in dfMarkColor, REMOVED\n") )
    } #fi
    else {  # column names ok
      if(exists("allMarkNames")){
        dfMarkColorInternal <- dfMarkColorInternal[which(dfMarkColorInternal$markName %in% allMarkNames) ,]
        if (nrow(dfMarkColorInternal)==0){
          message(crayon::red("\nError in dfMarkColor markNames respect to Marks pos. data.frames, dfMarkColor REMOVED\n")
          )#cat
          remove(dfMarkColorInternal)
        } else if ( length(setdiff(allMarkNames,unique(dfMarkColorInternal$markName) ) )>0) { # nrow not 0
          message(crayon::black("\nColors provided in to dfMarkColor are not enough, internal colors will be used.\n") )
          dfMarkColorInternal<-makedfMarkColor(dfMarkColorInternal,allMarkNames,c(chrColor,cenColor2) )
        } else { # nrow not 0
          message(crayon::black("\nCheck OK\n") )
        }
      } # fi # end allmarknames exist
      else { # all Mark Names does not exist
        message(crayon::red("\nError in dfMarkColor Names respect to Marks data.frames, dfMarkColor REMOVED\n")
        )
        remove(dfMarkColorInternal)
      } # else
    } # else column names ok
  } #fi exists dfMarkColor ... continues
  else if (missing(mycolors) ) { # if dfMarkColor not exist and missing mycolors
      if(exists("allMarkNames")){
        dfMarkColorInternal<-makedfMarkColor(idiogramFISH::dfMarkColor,allMarkNames,c(chrColor,cenColor2),defaultStyleMark )
        } # allmarknames
    } else if (!missing(mycolors) ) { # if dfMarkColor not exist , mycolors exist
      if(exists("allMarkNames")){
        dfMarkColorInternal<-makedfMarkColorMycolors(allMarkNames, mycolors, c(chrColor,cenColor2),defaultStyleMark )
      }
  } # elif
  }

  if(exists("dfMarkColorInternal")){
    dfMarkColorInternal$markBorderColor<-dfMarkColorInternal$markColor

    if(!missing(colorBorderMark)){

      colorBorderMarkFiltered<-colorBorderMark[sapply(colorBorderMark, function(X) {
        tryCatch(is.matrix(col2rgb(X)),
                 error = function(e) {message(crayon::red(paste("Color",X,"invalid, removed from colorBorderMark") ) ); return(FALSE) })
      } )]
      colorBorderMarkFiltered<-colorBorderMarkFiltered[!is.na(colorBorderMarkFiltered)]

      if(length(colorBorderMarkFiltered)>0 ) {
        dfMarkColorInternal$markBorderColor<-colorBorderMarkFiltered
      }

    } # colorBorderMark

    if(borderOfWhiteMarks){
      tryCatch(dfMarkColorInternal[which(dfMarkColorInternal$markColor %in% "white" &
                                           !dfMarkColorInternal$style %in% "cenStyle"),]$markBorderColor<-"black",
               error= function (e) NA)
    }
  }

  if (exists("dfMarkColorInternal") & legend=="inline"){
    dfMarkColorInternalCopy<-dfMarkColorInternal
    dfMarkColorInternalArrowsLabels<-dfMarkColorInternalCopy[which(dfMarkColorInternalCopy$style %in% c("downArrow","upArrow") ),]
    if(nrow(dfMarkColorInternalArrowsLabels)>0){
      tryCatch(dfMarkColorInternalArrowsLabels[which(dfMarkColorInternalArrowsLabels$style %in% "downArrow"),]$style<-"cMLeft",error = function(e) {""})
      tryCatch(dfMarkColorInternalArrowsLabels[which(dfMarkColorInternalArrowsLabels$style %in% "upArrow"),]$style<-"cM",error = function(e) {""})
      tryCatch(dfMarkColorInternalArrowsLabels$markName <- paste0(dfMarkColorInternalArrowsLabels$markName," "),error = function(e) {""} )
      dfMarkColorInternal <- dplyr::bind_rows(dfMarkColorInternal,dfMarkColorInternalArrowsLabels)
#      # dfMarkColorInternal1 <<-dfMarkColorInternal
#      # dfMarkColorInternalArrowsLabels1<<-dfMarkColorInternalArrowsLabels
    }
  }

# dfMarkColorInternal1402<<-dfMarkColorInternal
  #
  #   CREATION OF CHILD DATAFRAMES MARKS
  #

  ###################
  # REQUIRES STYLE
  ###################

  # substituted by:

  if(exists("listOfdfMarkPosMonocen") & exists("dfMarkColorInternal")  ){
    listOfdfMarkPosMonocenArrowsLabels<-list()

    for (i in 1:length(listOfdfMarkPosMonocen)){
      if(class(listOfdfMarkPosMonocen[[i]])=="data.frame" ) {

        listOfdfMarkPosMonocen[[i]]$style <- dfMarkColorInternal$style[match(listOfdfMarkPosMonocen[[i]]$markName, dfMarkColorInternal$markName)]

        if (legend=="inline") {
          listOfdfMarkPosMonocenArrowsLabels[[i]]<-listOfdfMarkPosMonocen[[i]]
          listOfdfMarkPosMonocenArrowsLabels[[i]]<-listOfdfMarkPosMonocenArrowsLabels[[i]][which(listOfdfMarkPosMonocenArrowsLabels[[i]]$style %in% c("downArrow","upArrow") ),]
          if(nrow(listOfdfMarkPosMonocenArrowsLabels[[i]])>0) {
            tryCatch(listOfdfMarkPosMonocenArrowsLabels[[i]][which(listOfdfMarkPosMonocenArrowsLabels[[i]]$style %in% "downArrow"),]$style<-"cMLeft",error = function(e) {""})

            tryCatch(listOfdfMarkPosMonocenArrowsLabels[[i]][which(listOfdfMarkPosMonocenArrowsLabels[[i]]$style %in% "upArrow"),]$style<-"cM",error = function(e) {""})

            tryCatch({
              downleftp <- which(listOfdfMarkPosMonocenArrowsLabels[[i]]$style %in% "cMLeft" &
                                 listOfdfMarkPosMonocenArrowsLabels[[i]]$chrRegion %in% "p" )
              listOfdfMarkPosMonocenArrowsLabels[[i]][downleftp,]$markDistCen<-
                listOfdfMarkPosMonocenArrowsLabels[[i]][downleftp,]$markDistCen+
                listOfdfMarkPosMonocenArrowsLabels[[i]][downleftp,]$markSize
                },    error = function(e) {""} )
            tryCatch({
              uprightq <- which(listOfdfMarkPosMonocenArrowsLabels[[i]]$style %in% "cM" &
                                   listOfdfMarkPosMonocenArrowsLabels[[i]]$chrRegion %in% "q" )
              listOfdfMarkPosMonocenArrowsLabels[[i]][uprightq,]$markDistCen<-
                listOfdfMarkPosMonocenArrowsLabels[[i]][uprightq,]$markDistCen+
                listOfdfMarkPosMonocenArrowsLabels[[i]][uprightq,]$markSize
            },    error = function(e) {""} )

            tryCatch(listOfdfMarkPosMonocenArrowsLabels[[i]]$markName <- paste0(listOfdfMarkPosMonocenArrowsLabels[[i]]$markName," ")
                     ,error = function(e) {""} )

            listOfdfMarkPosMonocen[[i]] <- dplyr::bind_rows(listOfdfMarkPosMonocen[[i]],listOfdfMarkPosMonocenArrowsLabels[[i]])
          } # arrows present
        } # inline

        tryCatch(listOfdfMarkPosMonocen[[i]]$protruding <- dfMarkColorInternal$protruding[match(listOfdfMarkPosMonocen[[i]]$markName,
                                                                                                dfMarkColorInternal$markName)]
                 ,error = function(e) {""}
                 )

      if(length(listOfdfMarkPosMonocen[[i]][which(listOfdfMarkPosMonocen[[i]]$style %in% "cenStyle"),]$markSize)>0){

        for (m in 1:nrow(listOfdfMarkPosMonocen[[i]][which(listOfdfMarkPosMonocen[[i]]$style %in% "cenStyle"),] ) ) {
          if( is.na(listOfdfMarkPosMonocen[[i]][which(listOfdfMarkPosMonocen[[i]]$style %in% "cenStyle"),]$markSize[m]) ) {
            listOfdfMarkPosMonocen[[i]][which(listOfdfMarkPosMonocen[[i]]$style %in% "cenStyle"),]$markSize[m]<-

              dfChrSizeInternal[match(interaction( listOfdfMarkPosMonocen[[i]][which(
                listOfdfMarkPosMonocen[[i]]$style %in% "cenStyle"),][m,c("OTU", "chrName")]),
                                      interaction( dfChrSizeInternal[c("OTU", "chrName")] )
              ),]$centromereSize

            } # if
          } # for
        } # if
      } # if data.frame

    } # for each monocen

#    listOfdfMarkPosMonocenArrowsLabels1 <<- listOfdfMarkPosMonocenArrowsLabels
#    listOfdfMarkPosMonocen1 <<- listOfdfMarkPosMonocen


  } # fi exists monocen

  if(exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal")  ) {

    listOfdfMarkPosHolocenArrowsLabels<-list()

    for (i in 1:length(listOfdfMarkPosHolocen)){
      if(class(listOfdfMarkPosHolocen[[i]])=="data.frame" ) {

        listOfdfMarkPosHolocen[[i]]$style<-dfMarkColorInternal$style[match(listOfdfMarkPosHolocen[[i]]$markName, dfMarkColorInternal$markName)]

        if (legend=="inline"){
          listOfdfMarkPosHolocenArrowsLabels[[i]]<-listOfdfMarkPosHolocen[[i]]
          listOfdfMarkPosHolocenArrowsLabels[[i]]<-listOfdfMarkPosHolocenArrowsLabels[[i]][which(listOfdfMarkPosHolocenArrowsLabels[[i]]$style %in% c("downArrow","upArrow") ),]
          if(nrow(listOfdfMarkPosHolocenArrowsLabels[[i]])>0){
            tryCatch(listOfdfMarkPosHolocenArrowsLabels[[i]][which(listOfdfMarkPosHolocenArrowsLabels[[i]]$style %in% "downArrow"),]$style<-"cMLeft",error = function(e) {""})
            tryCatch(listOfdfMarkPosHolocenArrowsLabels[[i]][which(listOfdfMarkPosHolocenArrowsLabels[[i]]$style %in% "upArrow"),]$style<-"cM",error = function(e) {""})

            tryCatch({
              downleft <- which(listOfdfMarkPosHolocenArrowsLabels[[i]]$style %in% "cMLeft" )

              listOfdfMarkPosHolocenArrowsLabels[[i]][downleft,]$markPos <-
                listOfdfMarkPosHolocenArrowsLabels[[i]][downleft,]$markPos+
                listOfdfMarkPosHolocenArrowsLabels[[i]][downleft,]$markSize
            },    error = function(e) {""} )

            tryCatch(listOfdfMarkPosHolocenArrowsLabels[[i]]$markName <- paste0(listOfdfMarkPosHolocenArrowsLabels[[i]]$markName," "),error = function(e) {""} )
            listOfdfMarkPosHolocen[[i]] <- dplyr::bind_rows(listOfdfMarkPosHolocen[[i]],listOfdfMarkPosHolocenArrowsLabels[[i]])
          }
        } # inline

        tryCatch(listOfdfMarkPosHolocen[[i]]$protruding <- dfMarkColorInternal$protruding[match(listOfdfMarkPosHolocen[[i]]$markName,
                                                                                                dfMarkColorInternal$markName)]
                 ,error = function(e) {""}
        )


      if(length(listOfdfMarkPosHolocen[[i]][which(listOfdfMarkPosHolocen[[i]]$style %in% "cenStyle"),]$markSize)>0){
        for (m in 1:nrow(listOfdfMarkPosHolocen[[i]][which(listOfdfMarkPosHolocen[[i]]$style %in% "cenStyle"),] ) ) {
          if( is.na(listOfdfMarkPosHolocen[[i]][which(listOfdfMarkPosHolocen[[i]]$style %in% "cenStyle"),]$markSize[m]) ) {
            listOfdfMarkPosHolocen[[i]][which(listOfdfMarkPosHolocen[[i]]$style %in% "cenStyle"),]$markSize[m]<-

              dfChrSizeInternal[match(interaction( listOfdfMarkPosHolocen[[i]][which(
                listOfdfMarkPosHolocen[[i]]$style %in% "cenStyle"),][m,c("OTU", "chrName")]),
                interaction( dfChrSizeInternal[c("OTU", "chrName")] )
              ),]$centromereSize

          } # if
        } # for
      } # if

      } # if data.frame
    } # for
#    # listOfdfMarkPosHolocen1<<-listOfdfMarkPosHolocen
  } # fi


  #################################################################################
  #
  #       MISSING OTUs
  #

  if(!is.na(addMissingOTUAfter[1]) ){
    if (length(missOTUspacings) != length(addMissingOTUAfter) ){
      missOTUspacings<-rep(missOTUspacings, abs(length(addMissingOTUAfter)/length(missOTUspacings)) ) [1:length(addMissingOTUAfter)]
    }
    for (i in 1:length(addMissingOTUAfter)){
    listOfdfChromSize <- append(listOfdfChromSize,
                                rep(NA,missOTUspacings[[i]]),
                                (which(names(listOfdfChromSize)==addMissingOTUAfter[[i]]) )
                                ) # append
    }
  } # fi

  #
  #     reverse
  #

  listOfdfChromSize<-rev(listOfdfChromSize)

  if(revOTUs){
    listOfdfChromSize<-rev(listOfdfChromSize)
  }

  #
  #	change of size based on number of sps
  #

  if(propWidth){
    chrWidth  <- chrWidth/length(listOfdfChromSize)
    chrSpacing<- chrSpacing/length(listOfdfChromSize)
  }  # dfMarkPosSq to listOfdfMarkPosInternalSq


  #######################################################
  #
  #   If Marks missing, rename duplicates of chrNames
  #
  #######################################################

  for (s in 1:length(listOfdfChromSize) ){
    OTUname<-names(listOfdfChromSize[s])
    if (exists("listOfdfMarkPosHolocen") ){
      OTUlistOfdfMarkPosHolocen<-listOfdfMarkPosHolocen[which(names(listOfdfMarkPosHolocen) %in% OTUname)]
      if(length(OTUlistOfdfMarkPosHolocen)==0){
        remove(OTUlistOfdfMarkPosHolocen)
      }
    }
    if (exists("listOfdfMarkPosMonocen") ){
      OTUlistOfdfMarkPosMonocen<-listOfdfMarkPosMonocen[which(names(listOfdfMarkPosMonocen) %in% OTUname)]
      if(length(OTUlistOfdfMarkPosMonocen)==0){
        remove(OTUlistOfdfMarkPosMonocen)
      }
    }
    if (exists("listOfdfDataCen") ){
      OTUlistOfdfDataCen<-listOfdfDataCen[which(names(listOfdfDataCen) %in% OTUname)]
      if(length(OTUlistOfdfDataCen)==0){
        remove(OTUlistOfdfDataCen)
      }
    }
    mybooleanChrName <- !exists("OTUlistOfdfMarkPosHolocen") & !exists("OTUlistOfdfMarkPosMonocen") & !exists("OTUlistOfdfDataCen")
    dfChromSize<-fixChrNameDupDF(listOfdfChromSize[s], mybooleanChrName)
    listOfdfChromSize[[s]]<-dfChromSize[[1]]
  }

  #####################
  #   total size of chr
  #####################

  #
  #   add column total to data.frames
  #

  listOfdfChromSize <- addChrSizeColumn(listOfdfChromSize)

  {
    totalLength<-lapply(listOfdfChromSize, function(x) tryCatch(x$chrSize, error=function(e) NA)  )
    ifelse(
      inherits(totalLength, "matrix"),
      totalLength <- base::split(totalLength, col(totalLength) )
      ,NA)
    normalizeToOne<-karHeight/max(unlist(totalLength) , na.rm=TRUE)
  }

  ##############################################################################
  # order by size
  ##############################################################################
  #{ ord. chunk
    if(orderBySize==TRUE) {
        orderlist<-lapply(totalLength, function(x) order(x, decreasing = TRUE) )
    } else { # if not want to order by size, set order by name of chro
        orderlist<-lapply(listOfdfChromSize, function(x) tryCatch(order(x$chrName), error=function(e) NA ) )
    } #else

    ##################################################
    #
    #   add column of new chro index to data.frames
    #
    ##################################################

  listOfdfChromSize <- addNeworderColumn(listOfdfChromSize,orderlist)

    ###################################################
    #     groups
    ###################################################

    if("group" %in% colnames(dfChrSizeInternal)){
      message(crayon::blue("group column present - remove column if not using") )
      grouporderlist<-lapply(listOfdfChromSize, function(x) tryCatch(order(x$group), error=function(e) NA ) )

      for (s in 1:length(listOfdfChromSize)){
        if(class(listOfdfChromSize[[s]])=="data.frame") {
        listOfdfChromSize[[s]]<-listOfdfChromSize[[s]][grouporderlist[[s]], ]
        listOfdfChromSize[[s]]$neworder<-1:nrow(listOfdfChromSize[[s]])
        } #fi
      } # end for
      # )
      orderlist<-grouporderlist
    } # if group

    ##
    # order by group and chromosome
    ##

    ##################################################
    #
    #   important - add new indexer to df of marks - adds neworder column to listOfdfChromSizeCenType
    #
    ##################################################

    if(exists("listOfdfMarkPosHolocen") ){

      listOfdfMarkPosHolocen<-newOrderColumn(listOfdfChromSize,listOfdfMarkPosHolocen)

    } # end if presence of listOfdfMarkPosHolocen order

    if(exists("listOfdfMarkPosMonocen") ) {

      listOfdfMarkPosMonocen<-newOrderColumn(listOfdfChromSize,listOfdfMarkPosMonocen)

    } # end if presence of listOfdfMarkPosMonocen

    ######################################################
    #
    #   important - add new indexer to d.f DataCen
    #
    ######################################################

    if (exists("listOfdfDataCen")){

      listOfdfDataCen<-newOrderColumn(listOfdfChromSize,listOfdfDataCen)

    }  # end presence of dfCenMarksInternal
  #} ordering chunk

  #######################
  #
  #      main plot
  #
  #######################

  {
    # Monocen
    num_species<-length(listOfdfChromSize)
  }

 # processing for main plot

    # Monocen
{
    rownumber<-2
    chromosome_ns<-sapply(listOfdfChromSize, function(x) nrow(x) )

    # listOfdfChromSize ->     chromosome_ns ->     arms_number ->     armRepVector

    arms_number<-sapply(chromosome_ns, function(x) x*2)

    armRepVector<-lapply(arms_number, function(x) return(tryCatch( c(rep(rownumber,ceiling(x/rownumber) ) ), error=function(e) NA ) ))

    rownumber<-1
    chromRepVector<-lapply(chromosome_ns, function(x) return(tryCatch( c(rep(rownumber,ceiling(x/rownumber) ) ), error=function(e) NA ) ))

    #
    #   creating x y main coords fro chr.
    #

    ym<-xm<-y<-x<-list()

}
########################################################################################################################################
for (s in 1:num_species) {
      if(class(listOfdfChromSize[[s]])=="data.frame"){
        #######################################################################################################

      if(attr(listOfdfChromSize[[s]], "cenType")=="monocen") {       # monocen

        if(attr(listOfdfChromSize[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
          chrSpacing2<-specialChrSpacing
        } else {
          chrWidth2<-chrWidth
          chrSpacing2<-chrSpacing
        }

        croxright<-  croxleft<-  croytop<-croybot <- list()

      for (i in 1:length(armRepVector[[s]] )){

        centromereSize2<-as.numeric(attr(listOfdfChromSize[[s]],"centromere"))

        croybot[i]<-tryCatch(list(c(karHeight,
                           rep((karHeight-(listOfdfChromSize[[s]][,"longArmSize"]*normalizeToOne)[i]),2),
                           karHeight
                          )#c
                        ), error=function(e) NA ) # list

        croytop[i]<-tryCatch( list(c((karHeight+(centromereSize2*normalizeToOne)+(listOfdfChromSize[[s]][,"shortArmSize"]*normalizeToOne)[i] ),
                           rep((karHeight+(centromereSize2*normalizeToOne)),2),
                           (karHeight+(centromereSize2*normalizeToOne)+(listOfdfChromSize[[s]][,"shortArmSize"]*normalizeToOne)[i] )
        ) # c
        ), error=function(e) NA) # list
      } # for

      crox<-matrix(rep(NA,length(armRepVector[[s]])*4),ncol=4, nrow=length(armRepVector[[s]]) )

      for (i in 1:length(armRepVector[[s]])){
        crox[i,] <- c(
          rep( ( (chrSpacing2 * (i-0) + ( (i-0) * chrWidth2) ) + chrWidth2),2),
          rep( ( (chrSpacing2 * (i-0) + ( (i-0) * chrWidth2) )     ),2)
        ) # c rox
      } # for

      xm[[s]] <- rbind(crox,crox)
      x[[s]]  <- base::split(xm[[s]], row(xm[[s]]))

      ifelse(any(is.na(x[[s]]) ), x[[s]]<-NA,"") # ifelseinloop

      ym[[s]] <- t(sapply (c(croybot,croytop ), function (x) {length (x) ; return (x)}) ) + (karHeiSpace*(s-1)  )

      } # fi is monocen
        ########################################################################################### HOLOCEN

      if (attr(listOfdfChromSize[[s]], "cenType")=="holocen" ) {
        if(attr(listOfdfChromSize[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
          chrSpacing2<-specialChrSpacing
        } else {
          chrWidth2<-chrWidth
          chrSpacing2<-chrSpacing
        }

      croxright <-  croxleft <- croytop <- list()

      for (i in 1:length(chromRepVector[[s]])){

        croytop[i]<-tryCatch( list(c(   (karHeight/2+(listOfdfChromSize[[s]][,"chrSize"]*normalizeToOne)[i]   ), # 0+
                                        rep(karHeight/2,2),                                                              # 0,2
                                        (karHeight/2+(listOfdfChromSize[[s]][,"chrSize"]*normalizeToOne)[i]  )   # 0+
        ) # c
        ),error=function(e) NA)  # list
      } # for

      crox<-matrix(rep(NA,length(chromRepVector[[s]])*4),ncol=4, nrow=length(chromRepVector[[s]]) )

      for (i in 1:length(chromRepVector[[s]])){
        crox[i,] <- c(
                      rep( ( (chrSpacing2*(i-0)+((i-0)*chrWidth2))+chrWidth2),2),
                      rep( ( (chrSpacing2*(i-0)+((i-0)*chrWidth2))     ),2)
                    ) # c rox
      } # for

      xm[[s]] <- crox
      x[[s]] <- base::split(xm[[s]], row(xm[[s]]))
      ifelse(any(is.na(x[[s]]) ),x[[s]]<-NA,"") # ifelseinloop

      # listOfdfChromSize[[s]]<-x[[s]]

      ym[[s]] <- t(sapply ( croytop, function (x) {length (x) ; return (x)}) ) + (karHeiSpace*(s-1)  )

    } # holocen if

  } # data.frame
  # message(crayon::green(paste0("main plot calc section end" ) ) )

} # for species
    ###################################################################################################

   if (length(ym)==1){
      karSepar=FALSE
    }

    #
    #	reducing distance among OTUs
    #

  names(ym)<-names(listOfdfChromSize) # important here


    ymCopyC<-ymCopy2<-ymCopy<-ym # important must stay here before modifying ym

    if(is.na(addMissingOTUAfter[1])){
      if(karSepar){
        for (s in 1:(length(ym)-1)) {
          diffnext<-abs( min(ym[[s+1]] ) - max(ym[[s]]) )
          ym[[s+1]]=ym[[s+1]]-diffnext
          ifelse(amoSepar>diffnext,amoSepar2<-diffnext,amoSepar2<-amoSepar)
          # message(crayon::red(paste0("ym",amoSepar2 ) ) )
          ym[[s+1]]=ym[[s+1]]+amoSepar2

        }
      }
    }

for (s in 1:length(ym)) {
  if(all(is.null(ym[[s]])) ){
    # y[[s]] <-NA
    xm[[s]]<-ym[[s]]<-x[[s]] <-NA
    listOfdfChromSize[[s]]<-NA
  } # if
  attr(listOfdfChromSize[[s]],"groupPresence") <- 0
} # for

{
  areNA<-which(is.na(ym))

  listOfdfChromSizenoNA <- removeNAFromList(listOfdfChromSize,areNA)

  xmnoNA <- removeNAFromList(xm,areNA)

    for (i in 1:length(xmnoNA)){
      attr(xmnoNA[[i]],"cenType") <- attr(listOfdfChromSizenoNA[[i]],"cenType")
    }

  ymnoNA <- removeNAFromList(ym,areNA)
  #######################################################
  #
  #  modif x, creation y
  #
  #######################################################

  x<-removeNAFromList(x,areNA)

  y<-lapply(1:length(ymnoNA), function(s) base::split(ymnoNA[[s]], row(ymnoNA[[s]] )) )

  names(xm)<-names(listOfdfChromSize)

  names(x)<-names(listOfdfChromSizenoNA)
  names(y)<-names(listOfdfChromSizenoNA)

#  yInternalbefore<<-y
#  listOfdfChromSizeI1<<-listOfdfChromSize

for (s in 1:length(y) ){
    # lenYS<-length(y[[s]] )
   # if (names(listOfdfChromSizenoNA[s]) %in% c(monocenNames,holocenNames)){

    lenCS<-length(listOfdfChromSizenoNA[[s]][,"chrName"] )

    for (c in 1:lenCS )  {
      attr(y[[s]][[c]], "chrName1") <- listOfdfChromSizenoNA[[s]][,"chrName"][c]
    }
    if (length(y[[s]])>lenCS ){
      for (c in 1:lenCS )  {
      attr(y[[s]][[c+lenCS]], "chrName1") <- listOfdfChromSizenoNA[[s]][,"chrName"][c]
      }
    }
   # } # mono holo check
  }
} # chunk




    {
      # monocenVector2<-integer()
      for(i in 1:length(listOfdfChromSizenoNA)){
        if(class(listOfdfChromSizenoNA[[i]])=="data.frame" ){
          if(attr(listOfdfChromSizenoNA[[i]],"cenType")=="monocen"){
            # monocenVector2<-c(monocenVector2,i)
            attr(listOfdfChromSizenoNA[[i]],"positionnoNA") <- i
          }
        }
      }

      # holocenVector2<-integer()
      for(i in 1:length(listOfdfChromSizenoNA)){
        if(class(listOfdfChromSizenoNA[[i]])=="data.frame" ){
          if(attr(listOfdfChromSizenoNA[[i]],"cenType")=="holocen"){
            # holocenVector2<-c(holocenVector2,i)
            attr(listOfdfChromSizenoNA[[i]],"positionnoNA") <- i
          }
        }
      }

    }

    for (s in 1:length(listOfdfChromSizenoNA) ) {
      attr(y[[s]], "positionnoNA")<- attr(listOfdfChromSizenoNA[[s]],"positionnoNA")
    }

#    yInternal<<-y
#    xInternal<<-x
#    ymInternal<<-ym
#    xmInternal<<-xm


    {
      monocenVector2<-integer()
      for(i in 1:length(listOfdfChromSize)){
        if(class(listOfdfChromSize[[i]])=="data.frame" ){
          if(attr(listOfdfChromSize[[i]],"cenType")=="monocen"){
            monocenVector2<-c(monocenVector2,i)
            attr(listOfdfChromSize[[i]],"position") <- i
          }
        }
      }

      monocenNames2<-names(listOfdfChromSize)[monocenVector2]
      listOfdfChromSizeMonocen<-listOfdfChromSize[monocenVector2]
      if(length(listOfdfChromSizeMonocen)==0){
        remove(listOfdfChromSizeMonocen)
      }

      holocenVector2<-integer()
      for(i in 1:length(listOfdfChromSize)){
        if(class(listOfdfChromSize[[i]])=="data.frame" ){
          if(attr(listOfdfChromSize[[i]],"cenType")=="holocen"){
            holocenVector2<-c(holocenVector2,i)
            attr(listOfdfChromSize[[i]],"position") <- i
          }
        }
      }
      holocenNames2<-names(listOfdfChromSize)[holocenVector2]
      listOfdfChromSizeHolocen<-listOfdfChromSize[holocenVector2]

      if(length(listOfdfChromSizeHolocen)==0){
        remove(listOfdfChromSizeHolocen)
      }
    }

    if(!missing(chrBorderColor)) {
      chrBorderColor2<-chrBorderColor
    } else {
      chrBorderColor2<-chrColor
    }

#{ # plot types
    xsizeplot<-(max(unlist(x), na.rm=TRUE)+xlimRightMod )- ( (min(unlist(x), na.rm=TRUE)-(xlimLeftMod)) )
    ysizeplot<- max(unlist(y), na.rm=TRUE)

    if(roundness < 1) {
      roundness<- 1
    }
    # plot types

      yInter<-intercalate(y,monocenNames)
      names(yInter)<-names(y)

      ylistNewChrSimple<-yVertoHor(yInter,monocenNames)
      names(ylistNewChrSimple)<-names(y)

      ylistTransChrSimple<-transYList(ylistNewChrSimple,shrinkFactor,monocenNames)
      names(ylistTransChrSimple)<-names(ylistNewChrSimple)

      if(roundness > 20) {  ########################################  roundness > 20 #################### > 20

        if(circularPlot==FALSE){

        #####################################################################################################################
          if(callPlot){
            graphics::plot("",xlim=c( (min(unlist(x), na.rm=TRUE)-xlimLeftMod),(max(unlist(x), na.rm=TRUE)+xlimRightMod ) ),
                       ylim = c( ylimBotMod*-1 , ( (max(unlist(y), na.rm = TRUE) )+ylimTopMod) ) ,
                       ylab = "", xaxt='n',
                       xlab="", yaxt='n',main = NULL, frame.plot = FALSE, asp=asp, ...)
        # xlab="", yaxt='n',main = NULL, frame.plot = FALSE)
          }
        ######################################################################################################################

          lapply(1:length(y), function(s) mapply(function(x,y) graphics::polygon(x=x,
                                                                                 y=y,
                                                                                 col=chrColor,
                                                                                 lwd=lwd.chr,
                                                                                 border=chrColor),
                                                 x=x[[s]],
                                                 y=y[[s]]
                                          )#m
          )#l
        } # circular false

        else { # circular TRUE

          #
          #   x horizontal to vertical
          #

          xlistNewChr<-xHortoVer(x)

          circleMaps <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransChrSimple,xlistNewChr,n,0,
                                       chrWidth,rotation=rotation)


#          # ylistTransChrSimpleI<<-ylistTransChrSimple
#          # listYChrCenterI<<-listYChrCenter
#          #          chrNamesI<<-chrNames

#          circleMapsOTUname1851<<-circleMapsOTUname
          if(callPlot){

          graphics::plot("",xlim=c( (min(unlist(circleMaps), na.rm=TRUE)-xlimLeftMod),
                                    (max(unlist(circleMaps), na.rm=TRUE)+xlimRightMod )
          ),
          ylim = c( min (unlist(circleMaps), na.rm = TRUE) +ylimBotMod*-1 ,
                    ( (max(unlist(circleMaps), na.rm = TRUE) )+ylimTopMod)
          ) ,
          ylab = "",
          xaxt='n',
          xlab="",
          yaxt='n',
          main = NULL, frame.plot = FALSE,
          asp=asp,
          ...
          ) # plot
          }

          #
          #   plot chr
          #

          drawPlot(circleMaps,chrColor,lwd.chr)

          #
          #   add OTU names
          #
          if(addOTUName){

            firstXchrEach <- lapply(xlistNewChr, `[[`, 1)
            firstYchrEach <- lapply(ylistTransChrSimple, `[[`, 1)

          circleMapsOTUname <- mapOTUnames(firstYchrEach , firstXchrEach, ylistNewChrSimple, n, radius, circleCenter, circleCenterY,separFactor,
                                           OTUlabelSpacing, chrWidth,rotation=rotation)
          addOTUnames(circleMapsOTUname,OTUTextSize,OTUsrt,OTUplacing,OTUfont2,OTUfamily2,
                      circleCenter,OTULabelSpacerx,circleCenterY,OTULabelSpacery,
                      OTUlegendHeight*normalizeToOne,radius,chrWidth,normalizeToOne,OTUcentered,OTUjustif,separFactor,labelSpacing)
          }

          #
          # Plot chr names
          #

          if(chrId!=""){
            listXChrCenter <- mapChrCenter(xlistNewChr)
            listYChrCenter <- mapChrCenter(ylistTransChrSimple)
            names(listYChrCenter)<-names(ylistTransChrSimple)
            chrNames <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,
                                       listYChrCenter,listXChrCenter,n,-chrLabelSpacing,chrWidth,
                                       specialOTUNames=specialOTUNames,chrWFactor=chrWFactor,rotation=rotation)
            plotChrNames(chrNames, indexIdTextSize, chrId,monocenNames,chrColor)
          }

        } # cP true

      } else { # if roundness ##################                                       ######## roundness < 20
        # message(crayon::green(paste0("main plot roundness section start" ) ) )
        pts_1 <- seq(-pi/2, 0, length.out = n)
        pts_2 <- seq( 0, pi/2, length.out = n)
        pts_3 <- seq(pi, pi*1.5, length.out = n)
        pts_4 <- seq(pi/2, pi, length.out = n)

        yMod<-y

        newLongy<-newLongx<-list()

        for (s in 1:length(yMod) ) {

          if(class(listOfdfChromSizenoNA[[s]])=="data.frame") {
            ########################################################

          if(attr(listOfdfChromSizenoNA[[s]], "cenType")=="monocen" ) {                 ############# monocen ###############
            if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
              chrWidth2  <-specialChrWidth
            } else {
              chrWidth2 <- chrWidth
            }
            r2<- chrWidth2/(roundness*2)

                  shortxyCoords<-mapXY(( (length(yMod[[s]])/2)+1 ) , length(yMod[[s]]),
                                           y[[s]],yMod[[s]],x[[s]],
                                           yfactor,r2,pts_1,pts_2,pts_3,pts_4)
#                  shortxyCoordsIPREV<<-shortxyCoords

                   longxyCoords<-mapXY( 1 , (length(yMod[[s]])/2 ) ,
                                          y[[s]], yMod[[s]] ,x[[s]],
                                          yfactor,r2,pts_1,pts_2,pts_3,pts_4)
#                   longxyCoordsI<<-longxyCoords

                  shortxyCoords$newLongx[sapply(shortxyCoords$newLongx, is.null)] <-
                    longxyCoords$newLongx[!sapply(longxyCoords$newLongx, is.null)]

                  shortxyCoords$newLongy[sapply(shortxyCoords$newLongy, is.null)] <-
                    longxyCoords$newLongy[!sapply(longxyCoords$newLongy, is.null)]

#                  shortxyCoordsINEW<<-shortxyCoords

                  newLongx[[s]]<-shortxyCoords$newLongx
                  newLongy[[s]]<-shortxyCoords$newLongy

    } # if monocen
            ###########################################################                                   holocen

          else if(attr(listOfdfChromSizenoNA[[s]], "cenType")=="holocen" ) {
            if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
              chrWidth2  <-specialChrWidth
            } else {
              chrWidth2<-chrWidth
            }

            r2<- chrWidth2/(roundness*2)

            xyCoords<-mapXY(1 , (length(yMod[[s]]) ) ,
                                y[[s]], yMod[[s]] ,
                                x[[s]],
                                yfactor,r2,
                                pts_1,pts_2,pts_3,pts_4)

            newLongx[[s]]<-xyCoords$newLongx
            newLongy[[s]]<-xyCoords$newLongy

          } # holocen ################################################################################# END HOLOCEN
      } # d.f.
   } # for species

  newLongy<-newLongy[!is.na(newLongy)]
  newLongx<-newLongx[!is.na(newLongx)]

  for(s in 1: length(newLongy)){
    attr(newLongy[[s]], "positionnoNA")<- attr(listOfdfChromSizenoNA[[s]],"positionnoNA")
    for (a in 1: length(newLongy[[s]])){
      # attr(newLongy[[s]][[a]],"chrName1")<- attr(y[[s]][[a]],"chrName1")
      names(newLongy[[s]])[a]<- names(y[[s]][a])
      names(newLongx[[s]])[a]<- names(y[[s]][a])
    }
  }

  names(newLongy)<-names(y)
  names(newLongx)<-names(y)

#  newLongyInternal<<-newLongy
#  newLongxInternal<<-newLongx

  if(circularPlot==FALSE){

    ##################################################################################################################### <20
    if(callPlot){
    graphics::plot("",xlim=c( (min(unlist(x), na.rm=TRUE)-xlimLeftMod),(max(unlist(x), na.rm=TRUE)+xlimRightMod ) ),
                   ylim = c( ylimBotMod*-1 ,( (max(unlist(y), na.rm = TRUE) )+ylimTopMod) ) , ylab = "", xaxt='n',
                   xlab="", yaxt='n',main = NULL, frame.plot = FALSE, asp=asp, ...)
    # xlab="", yaxt='n',main = NULL, frame.plot = FALSE)
}
    ######################################################################################################################

      lapply(1:length(y), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                               col=chrColor,
                                                                               lwd=lwd.chr,
                                                                               border=chrBorderColor2),
                                               x=newLongx[[s]],
                                               y=newLongy[[s]]
                                        )#m
      ) #l
   } # cP FALSE

  else { # cP TRUE         ####################################################         circular - roundness < 20

    yInterLong<-intercalate(newLongy,monocenNames)
    names(yInterLong)<-names(y)

    ylistNewChrLong<-yVertoHor(yInterLong,monocenNames )
    names(ylistNewChrLong)<-names(y)

    #
    #   x horizontal to vertical
    #

    xlistNewChr<-xHortoVer(newLongx)

    ylistTransChr<-transYList(ylistNewChrLong,shrinkFactor,monocenNames)
    names(ylistTransChr)<-names(ylistNewChrLong)
#    ylistTransChrI<<-ylistTransChr
    #
      circleMaps<-applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransChr,xlistNewChr,n,0,chrWidth,rotation=rotation)

      if(callPlot){
      graphics::plot("",xlim=c( (min(unlist(circleMaps), na.rm=TRUE)-xlimLeftMod),
                                    (max(unlist(circleMaps), na.rm=TRUE)+xlimRightMod )
          ),
          ylim = c( min (unlist(circleMaps), na.rm = TRUE) +ylimBotMod*-1 ,
                    ( (max(unlist(circleMaps), na.rm = TRUE) )+ylimTopMod)
                    ) ,
          ylab = "",
          xaxt='n',
          xlab="",
          yaxt='n',
          main = NULL,
          frame.plot = FALSE,
          asp=asp,
          ...
      ) # plot
      }

      #
      #   plot chr
      #

      drawPlot(circleMaps,chrColor,lwd.chr)

      #
      #   add OTU names
      #

      if(addOTUName){

      firstXchrEach <- lapply(xlistNewChr, `[[`, 1)

      firstYchrEach <- lapply(ylistTransChr, `[[`, 1)
      #      chrNamesI<<-chrNames

      #
      #  OTU name place
      #

      circleMapsOTUname <- mapOTUnames(firstYchrEach , firstXchrEach, ylistNewChrLong, n, radius, circleCenter,
                                       circleCenterY,separFactor, OTUlabelSpacing, chrWidth,rotation=rotation)

      addOTUnames(circleMapsOTUname,OTUTextSize,OTUsrt,OTUplacing,OTUfont2,OTUfamily2,
                  circleCenter,OTULabelSpacerx,circleCenterY,OTULabelSpacery,
                  OTUlegendHeight*normalizeToOne,radius,chrWidth,normalizeToOne,OTUcentered,OTUjustif,separFactor,labelSpacing)
      }

      #
      # Plot chr. names
      #

      if(chrId!=""){
        listYChrCenter <- mapChrCenter(ylistTransChr)
        names(listYChrCenter)<-names(ylistTransChr)

        #      listYChrCenterI<<-listYChrCenter

        listXChrCenter <- mapChrCenter(xlistNewChr)

       chrNames <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,
                                  listYChrCenter,listXChrCenter,n,
                                  -chrLabelSpacing, # inner
                                  chrWidth,
                                  specialOTUNames=specialOTUNames,chrWFactor=chrWFactor,rotation=rotation)

       plotChrNames(chrNames, indexIdTextSize, chrId,monocenNames ,chrColor)

       } # if
} # circularplot ####################################################################################################

}# roundness if roundness <20

 #} # plot types

  ####################################################################################
  #
  #                                                ruler calculate
  #
  ####################################################################################
    # listOfdfChromSize<-rev(listOfdfChromSize)


# message(crayon::red(paste("back2089",rulerIntervalback)))
if(circularPlot==FALSE){
  if (ruler) {
    if (length( which(names(listOfdfChromSize) %in% monocenNames) )>0 ) {
    # for (x in monocenVector2){

    maxShortRound<-lapply(1:length(listOfdfChromSizeMonocen),
                            function(x) {
                            return(tryCatch(customCeiling(max(listOfdfChromSizeMonocen[[x]]$shortArmSize),
                                         ceilingFactor), error=function(e) NA ) )
                            }
                          )
    names(maxShortRound) <- names(listOfdfChromSizeMonocen)

    maxLongRound <-lapply(1:length(listOfdfChromSizeMonocen),
                          function(x)
                            tryCatch(customCeiling(max(listOfdfChromSizeMonocen[[x]]$longArmSize ),
                                  ceilingFactor), error=function(e) NA ) )


    fromZerotoMaxLong<-fromZerotoMaxShort<-list()

    for (i in 1:length(maxShortRound)) {

      corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfChromSizeMonocen)[[i]] )
      divisor2<-as.numeric(attr(listOfdfChromSize[[corr_index]],"divisor"))

      if ( attr(listOfdfChromSizeMonocen[[i]], "ytitle" )=="cM" ) {
        if (!missing(rulerIntervalcM)) {
          if (rulerIntervalcM > divisor2/11 ) {
            rulerInterval2<-rulerIntervalcM/divisor2
          } else {
            message(crayon::red("rulerIntervalCM too small, using default"))
            rulerInterval2<-1
          }
        } else {
          rulerInterval2<-1
        }
      } else if( attr(listOfdfChromSizeMonocen[[i]], "ytitle" )=="Mb" ) {
        if (!missing(rulerIntervalMb)) {
          if (rulerIntervalMb > divisor2/11 ) {
            rulerInterval2<-rulerIntervalMb/(divisor2)
          } else {
            message(crayon::red("rulerIntervalMb too small, using default"))
            rulerInterval2<-1
          }
        } else {
          rulerInterval2<-1
        }
      } else if(attr(listOfdfChromSizeMonocen[[i]], "ytitle" )=="notMb" ) {
        # rulerInterval2<-rulerInterval2/divisor2
        rulerInterval2 <- ifelse(missing(rulerInterval),
                                 1,
                                 rulerInterval/divisor2
        )
      }

      if(rulerInterval2 > maxShortRound[[i]]){
        message(crayon::red(paste0("rulerInterval too big. Use smaller rulerInterval; rulerIntervalMb or rulerIntervalcM
                                   if you have Mb data or specialOTUNames, respectively" ) )
                )
      }

      # listOfdfChromSizeMonocen  - >  maxShortRound  ->  fromZerotoMaxShort

      fromZerotoMaxShort[[i]]<- tryCatch(seq(
        from = 0,
        to = (maxShortRound[[i]] +
        (rulerInterval2 * ifelse(maxShortRound[[i]] %% rulerInterval2>0,1,0) ) # ifelseinloop
        - maxShortRound[[i]] %% rulerInterval2),
        by = rulerInterval2), error=function(e) NA
        ) # try

      fromZerotoMaxLong[[i]]<- tryCatch(seq(
        from = 0,
          to = (maxLongRound[[i]] + (rulerInterval2 * ifelse(maxLongRound[[i]] %% rulerInterval2>0,1,0) ) # ifelseinloop
          - maxLongRound[[i]] %% rulerInterval2),
          by = rulerInterval2),
        error=function(e) NA
      ) # try

      if(!is.na(centromereSize)){
        centromereSize2<-centromereSize
      } else {
        centromereSize2<-divisor2
      }

      attr(fromZerotoMaxShort[[i]],"centromere") <- centromereSize2/divisor2

      remove(rulerInterval2)

    } # for maxshortround

    names(fromZerotoMaxShort)<-names(maxShortRound)

    ycoordLongRound <-lapply(1:length(fromZerotoMaxLong), function(x) {
      pos<-as.numeric(attr(listOfdfChromSizeMonocen[[x]],"position") )
      unlist(
        lapply(1:length(fromZerotoMaxLong[[x]]), function(y)
          (karHeight-(fromZerotoMaxLong[[x]][y]*normalizeToOne) ) + (1*karHeiSpace*(pos-1))
        ) #l
      ) #u
    } # ycoordLongRound
    ) # l

    names(ycoordLongRound)<-(monocenNames2)

    ycoordShortRound  <-lapply(1:length(fromZerotoMaxShort), function(x){
      pos<-as.numeric(attr(listOfdfChromSizeMonocen[[x]],"position") )
      unlist(
        lapply(1:length(fromZerotoMaxShort[[x]]), function(y)
          # (karHeight+(centromereSize*normalizeToOne)+(fromZerotoMaxShort[[x]][y]*normalizeToOne))+(1*karHeiSpace*(pos-1))
          (karHeight+(as.numeric(attr(fromZerotoMaxShort[[x]],"centromere"))*normalizeToOne)+(fromZerotoMaxShort[[x]][y]*normalizeToOne))+(1*karHeiSpace*(pos-1))

        ) # l
      ) # u
    }
    ) #l

    names(ycoordShortRound)<-(monocenNames2)

    if(is.na(addMissingOTUAfter[1])){
      if(karSepar){
        for (s in 1:(length(ymCopy)-1) ) {

          diffnext<-abs(min(ymCopy[[s+1]] ) - max(ymCopy[[s]]) )
          ymCopy[[s+1]] <- ymCopy[[s+1]]-diffnext
          ifelse(amoSepar>diffnext,amoSepar2<-diffnext,amoSepar2<-amoSepar)
          ymCopy[[s+1]] <- ymCopy[[s+1]]+amoSepar2

          nameYm<-names(ymCopy)[[s+1]]

          if(length(which(names(ycoordLongRound)==nameYm))>0 ){
          ycoordLongRound[[which(names(ycoordLongRound)==nameYm)]] <- ycoordLongRound[[which(names(ycoordLongRound)==nameYm)]] -diffnext
          ycoordLongRound[[which(names(ycoordLongRound)==nameYm)]] <- ycoordLongRound[[which(names(ycoordLongRound)==nameYm)]] + amoSepar2

          ycoordShortRound[[which(names(ycoordShortRound)==nameYm)]]<- ycoordShortRound[[which(names(ycoordShortRound)==nameYm)]]-diffnext
          ycoordShortRound[[which(names(ycoordShortRound)==nameYm)]]<- ycoordShortRound[[which(names(ycoordShortRound)==nameYm)]]+ amoSepar2

          }
        }
      }
    }

    ycoordLongRound  <-ycoordLongRound[!is.na(ycoordLongRound)]
    ycoordShortRound <-ycoordShortRound[!is.na(ycoordShortRound)]

    ########################################################################################3
    #
    #   add rulers
    #
    ####################

    ###############
    # short arm ruler labels
    ###############
    opar<-graphics::par(no.readonly = TRUE)
    on.exit(suppressWarnings(par(opar) ))
    graphics::par(mgp=c(3,rulerNumberPos,0))

    rulerPlot(ycoordShortRound,listOfdfChromSize,listOfdfChromSizeMonocen,fromZerotoMaxShort,rulerNumberSize,rulerPosMod,rulerPos,ruler.tck,
              lwd.chr)

    ################

    # long arm ruler labels

    ################

    rulerPlot(ycoordLongRound,listOfdfChromSize,listOfdfChromSizeMonocen,fromZerotoMaxLong,rulerNumberSize,rulerPosMod,rulerPos,ruler.tck,
              lwd.chr)

    ###################################################3
    #   ADD TITLE OF RULER MONOCEN
    ###################################################

      par(las=1)

    rulerTitle(ycoordShortRound,listOfdfChromSizeMonocen,MbUnit,specialyTitle,yTitle,ylabline,rulerTitleSize)

    } # monocen

    ######################################################################################### holocen ruler


    if (length( which(names(listOfdfChromSize) %in% holocenNames) ) > 0 ) {

    maxChrRound<-lapply(1:length(listOfdfChromSizeHolocen),
                        function(x) {
                          return(tryCatch(
                        customCeiling(max(listOfdfChromSizeHolocen[[x]]$chrSize),
                        ceilingFactor),
                        error=function(e) NA )
                         )}
                       ) # l

    fromZerotoMaxChr<-list()

    for (i in 1:length(maxChrRound)) {

      corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfChromSizeHolocen)[[i]] )
      divisor2<-as.numeric(attr(listOfdfChromSize[[corr_index]],"divisor"))

      if ( attr(listOfdfChromSizeHolocen[[i]], "ytitle" )=="cM" ) {
        if (!missing(rulerIntervalcM)) {
          if (rulerIntervalcM > divisor2/11 ) {
            rulerInterval2<-rulerIntervalcM/divisor2
          } else {
            message(crayon::red("rulerIntervalCM too small, using default"))
            rulerInterval2<-1
          }
        } else {
          rulerInterval2<-1
        }
      } else if( attr(listOfdfChromSizeHolocen[[i]], "ytitle" )=="Mb" ) {
        if (!missing(rulerIntervalMb)) {
          if (rulerIntervalMb > divisor2/11 ) {
            rulerInterval2<-rulerIntervalMb/(divisor2)
          } else {
            message(crayon::red("rulerIntervalMb too small, using default"))
            rulerInterval2<-1
          }
        } else {
          rulerInterval2<-1
        }
      } else if(attr(listOfdfChromSizeHolocen[[i]], "ytitle" )=="notMb" ) {
        # rulerInterval2<-rulerInterval2/divisor2
        rulerInterval2 <- ifelse(missing(rulerInterval),
                                 1,
                                 rulerInterval/divisor2
        )
      }

      if(rulerInterval2 > maxChrRound[[i]]){
        message(crayon::red(paste0("rulerInterval too big. Use smaller rulerInterval; or use rulerIntervalMb or rulerIntervalcM
                                   if you have Mb data or specialOTUNames, respectively" ) )
        )
      }


      fromZerotoMaxChr[[i]]<- tryCatch(seq(
        from = 0,
        to = (maxChrRound[[i]] + (rulerInterval2 * ifelse(maxChrRound[[i]] %% rulerInterval2>0,1,0) ) # ifelseinloop
        - maxChrRound[[i]] %% rulerInterval2),
        by = rulerInterval2), error=function(e) NA
                                       ) # try
      remove(rulerInterval2)
    } # for maxchrround

    ycoordChrRound  <- lapply(1:length(fromZerotoMaxChr), function(x) {
      pos<-as.numeric(attr(listOfdfChromSizeHolocen[[x]],"position") )
      unlist(
        lapply(1:length(fromZerotoMaxChr[[x]]), function(y)
          (karHeight/2+(fromZerotoMaxChr[[x]][y]*normalizeToOne))+(1*karHeiSpace*(pos-1)) )         # 0+(from
            ) # u
      }
    ) # l

    names(ycoordChrRound)<-holocenNames2

      if(is.na(addMissingOTUAfter[1])){
        if(karSepar){
          for (s in 1:(length(ymCopy2)-1) ) {

            diffnext<-abs(min(ymCopy2[[s+1]] ) - max(ymCopy2[[s]]) )
            ymCopy2[[s+1]]=ymCopy2[[s+1]]-diffnext
            ifelse(amoSepar>diffnext,amoSepar2<-diffnext,amoSepar2<-amoSepar)
            ymCopy2[[s+1]]=ymCopy2[[s+1]]+amoSepar2

            nameYm<-names(ymCopy2)[[s+1]]

            if(length(which(names(ycoordChrRound)==nameYm))>0 ){
            ycoordChrRound[[which(names(ycoordChrRound)==nameYm)]] <- ycoordChrRound[[which(names(ycoordChrRound)==nameYm)]] - diffnext
            ycoordChrRound[[which(names(ycoordChrRound)==nameYm)]] <- ycoordChrRound[[which(names(ycoordChrRound)==nameYm)]] + amoSepar2
            } # if
          } # for
        } # redu if
      } # if
    # )

    ycoordChrRound<-ycoordChrRound[!is.na(ycoordChrRound)]

    ####################
    #
    #   add rulers holocen
    #
    ####################

    opar<-graphics::par(no.readonly = TRUE)
    on.exit(suppressWarnings(par(opar)) )
    graphics::par(mgp=c(3,rulerNumberPos,0) )

    rulerPlot(ycoordChrRound,listOfdfChromSize,listOfdfChromSizeHolocen,fromZerotoMaxChr,rulerNumberSize,rulerPosMod,rulerPos,ruler.tck,
              lwd.chr)

    #
    #   title of ruler for HOLOCEN
    #

    par(las=1)
    rulerTitle(ycoordChrRound,listOfdfChromSizeHolocen,MbUnit,specialyTitle,yTitle,ylabline,rulerTitleSize)

  } # end holocen

 }   # end rulers if
} # END     not    circular ruler
  #
  #   groups line > 1.1.0
  #
if(circularPlot==FALSE){
    if("group" %in% colnames(dfChrSizeInternal)){

      groupSegmentDistance <- ifelse(groupUp, 1, 2)

    for (s in 1:length(xmnoNA)){
      if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2<-chrWidth
      }
      ngroup<-length(table(listOfdfChromSizenoNA[[s]]$group ) )
      attr(listOfdfChromSizenoNA[[s]],"groupPresence") <- ngroup
      for (g in 1: ngroup){
        x0= xmnoNA[[s]][,3][ifelse(length(cumsum(table(listOfdfChromSizenoNA[[s]]$group))[g-1] )==0, # ifelseinloop
                               1,
                               cumsum(table(listOfdfChromSizenoNA[[s]]$group) )[g-1]+1
        )]
        x1= xmnoNA[[s]][,3][cumsum(table(listOfdfChromSizenoNA[[s]]$group) )[g] ] + chrWidth2
      segments(x0=x0,
               y0=(min(ymnoNA[[s]])-((distTextChr/3)*groupSegmentDistance)),
               x1=x1,
               y1=(min(ymnoNA[[s]])-((distTextChr/3)*groupSegmentDistance) )
      ) # seg
      ########################
      #     group name, after 1.1.0
      ########################3
      text( (x0+x1)/2,
            min(ymnoNA[[s]]) - ( ( (distTextChr/3)* groupSegmentDistance ) + (distTextChr/3) ),
           labels = names( table(listOfdfChromSizenoNA[[s]]$group)[g] ),
           cex=indexIdTextSize
      )# text end
      } # for group
    } # for sp
  } # if group column
} # circular FALSE
 #


  ###################################################################################################################
  #                                                    chromosome names
  ###################################################################################################################
  chrNameDistance <-1
  # original
  if(circularPlot==FALSE){
  if(chrId=="original"){
      for (s in 1:length(xmnoNA)){
        if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}
        if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
        } else {
          chrWidth2<-chrWidth
        }
        if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 & groupUp ) {groupCount=2   } else{groupCount=0}

      graphics::text(xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/armFactor)]+chrWidth2/2,
                     rep( (min(ymnoNA[[s]])-(distTextChr/3) * (chrNameDistance + groupCount) ),(nrow(xmnoNA[[s]])/armFactor) ),
                     # labels = listOfdfChromSize[[s]][,"chrName"][orderlist[[s]]],
                     labels = listOfdfChromSizenoNA[[s]][,"chrName"],

                     cex=indexIdTextSize
      ) # end graphics::text
      } # for
  } # fi original
  else if (chrId=="simple"){
    # Simple numbering from 1 to ...
      for (s in 1:length(xmnoNA)){
        if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}
        if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
        } else {
          chrWidth2<-chrWidth
        }
        if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 & groupUp ) {groupCount=2   } else{groupCount=0}

        graphics::text(xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/armFactor)]+chrWidth2/2,
                     rep( (min(ymnoNA[[s]])-(distTextChr/3)* ( chrNameDistance + groupCount) ),(nrow(xmnoNA[[s]])/armFactor) ),
                     # rep( (min(ymnoNA[[s]])-.1),(nrow(xmnoNA[[s]])/2) ),
                     labels = 1:(nrow(xmnoNA[[s]])/armFactor),
                     cex=indexIdTextSize
      ) # t
      } # for
  } # elif simple
} # end names CIRCULAR FALSE

  #################################
  # horizontal chromosome index
  #################################

    if(circularPlot==FALSE){

    chrIdCount<-ifelse(chrId=="",1,0)

    morphoCount<-ifelse(morpho=="Guerra" | morpho=="Levan", 1,
                        ifelse(morpho=="both",2,0
                        )
    ) #mC

    indexCount<-ifelse(chrIndex=="CI" | chrIndex == "AR", 1,
                        ifelse(chrIndex == "both",2,0
                        )
    ) #mC

    newDistVector<-morphoCount+indexCount

  chrIndDistance<-ifelse(exists("grouporderlist"),4,2)

  if(chrIndex=="both"){bothAddI=1} else {bothAddI=0}

  #
  #   add CI
  #
  if(chrIndex=="both" | chrIndex == "CI" ){

      for (s in 1:length(listOfdfChromSizenoNA) ) {
        if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {
          groupCount=2
        } else {
          groupCount=0
        } # end ifelse
        if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
        } else {
          chrWidth2<-chrWidth
        }
        # if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="failure") {indexCount2=indexCount*0} else{indexCount2=indexCount}
        if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="success") {

          graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth2/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+(chrWidth2/2)),
                     rep( (min(ymnoNA[[s]])-( ( (distTextChr/3) * (chrIdCount+groupCount+indexCount-bothAddI+1) )) ),(nrow(xmnoNA[[s]])/2)+1 ),
                     #chrIndDistance
                     labels = tryCatch(c("CI",listOfdfChromSizenoNA[[s]][,"CI"] ),error=function(e){NA})
                     ,cex=indexIdTextSize
                    ) # end graphics::text
        # ) # end lapply
       } # success
      } # FOR
  } # BORH OR CI
}# CIRCULAR FALSE

  #
  #   add AR (radius)
  #

if(circularPlot==FALSE){
  if(chrIndex=="both" | chrIndex == "AR" ){

    for (s in 1:length(listOfdfChromSizenoNA) ) {
      if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {groupCount=2   } else{groupCount=0}
      if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2<-chrWidth
      }
      if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="success") {
      graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth2/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth2/2),
                     rep( (min(ymnoNA[[s]])-( ( (distTextChr/3) * (chrIdCount+groupCount+indexCount+1) )) ),(nrow(xmnoNA[[s]])/2)+1 ),
                     labels = tryCatch(c("r",listOfdfChromSizenoNA[[s]][,"AR"] ),error=function(e){NA})
                     ,cex=indexIdTextSize
      ) # end graphics::text
      } # success
    } # FOR
  } # fi BOTH OR AR
} # CIRCULAR false

  #################################
  # horizontal chromosome morphology categories
  #################################

  #
  #   add Guerra and Levan
  #
if(circularPlot==FALSE){
  if(morpho=="both"){bothAdd=1} else {bothAdd=0}

  #
  #   add Guerra
  #
  if(morpho=="both" | morpho == "Guerra" ){ # & "AR" %in% colnames(listOfdfChromSizenoNA[[1]]) ) {
    # lapply(1:length(xmnoNA), function(s)

    for (s in 1:length(listOfdfChromSizenoNA) ) {
      if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {groupCount=2   } else{groupCount=0}
      if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2<-chrWidth
      }
      if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="success") {

      graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth2/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth2/2),
                     rep( (min(ymnoNA[[s]])-( ( (distTextChr/3)*(chrIdCount+morphoCount+indexCount-bothAdd+groupCount+1) ) ) ), (nrow(xmnoNA[[s]])/2)+1 ),
                     # distVectorGue[decVector]
                     labels = tryCatch(c("Guerra",listOfdfChromSizenoNA[[s]][,"Guerra"]),error=function(e){NA})
                     ,cex=indexIdTextSize
      ) # end graphics::text
      } # if success
    } # for
  } # if guerra

  #
  #   add Levan
  #

  if(morpho=="both" | morpho == "Levan" ) {
      for (s in 1:length(listOfdfChromSizenoNA) ) {
        if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {groupCount=2   } else {groupCount=0}
        if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
        } else {
          chrWidth2<-chrWidth
        }
        if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="success") {

          graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth2/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth2/2),
                         rep( (min(ymnoNA[[s]])-( ( (distTextChr/3)* (chrIdCount+morphoCount+indexCount+groupCount+1) ) ) ),(nrow(xmnoNA[[s]])/2)+1 ),
                         #distVectorGue[decVector]
                         labels = tryCatch(c("Levan",listOfdfChromSizenoNA[[s]][,"Levan"]),error=function(e){NA})
                         ,cex=indexIdTextSize
          ) # end graphics::text
        # ) # end lapply
        } # if success
      } # for
  } # fi
} # circular FALSE

#################################
# karyotype index (left side)
#################################
if(circularPlot==FALSE){
  if(karIndex){
    # message(crayon::green(paste0("karyotype indices section start" ) ) )

    for (i in 1:length(listOfdfChromSizenoNA) ) { # for each OTU

      if(attr(listOfdfChromSizenoNA[[i]],"cenType")=="monocen"){
            if(is.character(names(listOfdfChromSizenoNA)[[i]]  ) ){
              message(crayon::green(paste0(names(listOfdfChromSizenoNA)[[i]],":" ) ) # otu name:  Calc. (asymmetry)
              ) # mess
            }

        ind<-asymmetry(listOfdfChromSizenoNA[[i]])
        if(is.null(ind)){
          message(crayon::red("Fix short/long measures or use karIndex=FALSE"))
        }

        if(!is.null(ind)){
            graphics::text( c( (xmnoNA[[i]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
                            rep( min(ymnoNA[[i]][,1]), 1 ), #2
                            labels = paste("A ",ind$A ),
                            cex=indexIdTextSize,
                            adj=c(1) # justif
            ) # end graphics::text
            graphics::text(c( (xmnoNA[[i]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
                           rep( (min(ymnoNA[[i]][,1])-(distTextChr/3) ) , 1 ), # avoid overlap
                           labels = paste("A2",ind$A2 ),
                           cex=indexIdTextSize,
                           adj=c(1) # 0.5 centered
            ) # end graphics::text
        } # null

     } # if monocen

      else if(attr(listOfdfChromSizenoNA[[i]],"cenType")=="holocen"){
        if(is.character(names(listOfdfChromSizenoNA)[[i]]  ) ){
          message(crayon::green(paste0(names(listOfdfChromSizenoNA)[[i]],":" ) )
          ) # mess
        }
        ind<-asymmetryA2(listOfdfChromSizenoNA[[i]])
        if(!is.null(ind)){
        graphics::text(c( (xmnoNA[[i]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
                       (max(ymnoNA[[i]]) + min(ymnoNA[[i]]) ) /2 #(distTextChr/3)   #[,1]  was /3
                       ,labels = ifelse(ind$A2=="NA","",paste("A2",ind$A2 ) ),
                       cex=indexIdTextSize,
                       adj= c(1) # 0.5 centered
        ) # end graphics::text
        } # null
      } # holocen
    } # for
  } # fi
} # CIRCULAR FALSE

  #########################################################################
  # add species names
  #########################################################################
if (circularPlot==FALSE){
if(OTUasNote){
    addOTUName<-FALSE
      if(!missing(notes)){
          message(crayon::blurred("Error: OTUasNote is TRUE, other notes will be removed"))
      }
    notes<-data.frame(OTU=unique(dfChrSizeInternal$OTU), note=unique(dfChrSizeInternal$OTU) )
}

if(addOTUName){
    # message(crayon::green(paste0("OTU section start" ) ) )

    for (s in 1:length(xmnoNA) ) {
      if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {
        groupCount=2
        } else {
          groupCount=0
      } # end ifelse

      if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="failure") {indexCount2=indexCount*0} else {indexCount2=indexCount}
      if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="failure") {morphoCount2=morphoCount*0} else {morphoCount2=morphoCount}

    # lapply(1:length(xmnoNA), function(s) {
      if(attr(xmnoNA[[s]],"cenType")=="holocen") {
        # decVector<-5
        holocenDisCount <- morphoCount2 + indexCount2 #newDistVector #+bothAdd
      } else {
        holocenDisCount <- 0
      } # ifelse holocen

      graphics::text( c( (xmnoNA[[s]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),
                      ydistance <- (min(ymnoNA[[s]]) - ((distTextChr/3) * (chrIdCount + morphoCount2 + indexCount2 + groupCount + 3 - holocenDisCount) ) ),
                      # distVector[decVector] ),
                      labels = paste("",names(listOfdfChromSizenoNA)[[s]] ),
                      cex=OTUTextSize,
                      adj=c(0) # justif 0 =left
                      ,font=   ifelse( !missing(OTUfont),   OTUfont,   1)
                      ,family= ifelse( !missing(OTUfamily), OTUfamily, defaultFontFamily2)
      ) # end graphics::text
    } # for
    # ) # end lapply
    # message(crayon::green(paste0("OTU section END" ) ) )

  } # fi add OTU name
} # CIRCULAR FALSE
                                                ##########################
                                                #       MARKS            #
                                                ##########################

  ##########################################################################################################3
  #
  #                           painting Marks monocen              square marks
  #
  ############################################################################################################

  if (exists("listOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ){
    # message(crayon::green(paste0("monocen. marks section start" ) ) )


    xMarkSq<-yMarkSq<-listOfdfMarkPosSq<-list()

    j<-1
    for (k in 1:length(listOfdfMarkPosMonocen)) {
      currName<-names(listOfdfMarkPosMonocen)[[k]]
      if(nrow(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="square"),])>0){
        listOfdfMarkPosSq<-c(listOfdfMarkPosSq,list(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="square"),]))
        names(listOfdfMarkPosSq)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listOfdfMarkPosSq)>0){
    for (sm in 1:length(listOfdfMarkPosSq)) {
      yMark1<-NULL
      xMark1<-NULL
      # which(names(listOfdfChromSize) %in% names(listOfdfMarkPosSq)[[sm]] )

      corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosSq)[[sm]] )

      for (m in 1:nrow(listOfdfMarkPosSq[[sm]]) ){
        ifelse(listOfdfMarkPosSq[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
        ifelse(listOfdfMarkPosSq[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
        ifelse(listOfdfMarkPosSq[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
        rowIndex<- nrow(listOfdfChromSize[[corr_index]] ) * longORshort + (listOfdfMarkPosSq[[sm]][,"neworder"][m])
        armStart<-ym[[corr_index]][rowIndex,column]
        yprox <- armStart +
          (listOfdfMarkPosSq[[sm]][m,"markDistCen"]                                                             *normalizeToOne*mySign)

        yter <- armStart +
          ( sum( listOfdfMarkPosSq[[sm]][m,"markDistCen"] , listOfdfMarkPosSq[[sm]][m,"markSize"], na.rm=TRUE ) *normalizeToOne*mySign )

        yMark1[[m]]<-if(longORshort==0) {c(yprox,yter,yter,yprox)} else {c(yter,yprox,yprox,yter)}

        attr(yMark1[[m]],"arm")<-listOfdfMarkPosSq[[sm]][m,"chrRegion"]
        # attr(yMark1[[m]],"armStart")<-armStart
        attr(yMark1[[m]],"rowIndex")<-rowIndex

        xMark1[[m]]<-xm[[corr_index]][listOfdfMarkPosSq[[sm]][,"neworder"][m],]
        attr(xMark1[[m]],"rowIndex")<-rowIndex

      }
      yMarkSq[[sm]]<-yMark1
      attr(yMarkSq[[sm]], "spname")<-names(listOfdfMarkPosSq)[[sm]]
      xMarkSq[[sm]]<-xMark1
      attr(xMarkSq[[sm]], "spname")<-names(listOfdfMarkPosSq)[[sm]]
    } # end for

    # markList<-addChrNameAttrMark(xMarkSq,yMarkSq,x)
    #
    # xMarkSq<-markList$xMark
    # yMarkSq<-markList$yMark
                                                ########################
                                                #                      #
                                                #   add marks to plot monocen #
                                                #                      #
                                                ########################
      # if(circularPlot==FALSE){
      roundPlotMark(roundness, xMarkSq, yMarkSq,
                    dfMarkColorInternal,
                    listOfdfMarkPosSq,
                    chrWidth, #use for calc r2
                    specialChrWidth,
                    yfactor,
                    n,
                    lwd.chr,
                    listOfdfChromSize,
                    circularPlot,
                    y,
                    markLabelSize,
                    pattern,
                    separFactor,
                    labelSpacing,circleCenter,circleCenterY,radius,
                    legend,ylistTransChrSimple,rotation=rotation,labelOutwards)

    } #     if(length(listOfdfMarkPosSq)>0)

    else {remove(listOfdfMarkPosSq)}

  # square labels not cen (monocen.)
  booleanForsquareInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosSq") & circularPlot==FALSE

  if(booleanForsquareInlineLabel) {
    textLabel(xMarkSq,yMarkSq,listOfdfChromSize,listOfdfMarkPosSq,specialChrSpacing,chrSpacing,markLabelSize,pattern)
  }

  } # if presence end painting marks

  ##################################################################################################
  #
  #                                                 painting Marks square holocen
  #
  ##################################################################################################

  if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
    # message(crayon::green(paste0("holocen. marks section start" ) ) )
    xMarkSq<-yMarkSq<-listOfdfMarkPosSq<-list()

    j<-1
    for (k in 1:length(listOfdfMarkPosHolocen)) {
      currName<-names(listOfdfMarkPosHolocen)[[k]]
      if(nrow(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="square"),])>0){
        listOfdfMarkPosSq<-c(listOfdfMarkPosSq,list(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="square"),]) )
        names(listOfdfMarkPosSq)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listOfdfMarkPosSq)>0){
      for (sm in 1:length(listOfdfMarkPosSq)) {

      yMark1<-NULL
      xMark1<-NULL
      corr_index<-which(names(ym) %in% names(listOfdfMarkPosSq)[[sm]] )

      for (m in 1:nrow(listOfdfMarkPosSq[[sm]])){
        rowIndex<-(listOfdfMarkPosSq[[sm]][,"neworder"][m])
        chrStart<-ym[[corr_index]][rowIndex ,2]
        yinf <- chrStart +                        # was ysup
          (listOfdfMarkPosSq[[sm]][m,"markPos"]                                                          *normalizeToOne)

        ysup <- chrStart +
          (  sum(listOfdfMarkPosSq[[sm]][m,"markPos"],listOfdfMarkPosSq[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

        yMark1[[m]]<-c(ysup,yinf,yinf,ysup)
        attr(yMark1[[m]],"rowIndex")<-rowIndex

        xMark1[[m]]<-xm[[corr_index]][listOfdfMarkPosSq[[sm]][,"neworder"][m],]
        attr(xMark1[[m]],"rowIndex")<-rowIndex

      }
      yMarkSq[[sm]]<-yMark1
      attr(yMarkSq[[sm]], "spname")<-names(listOfdfMarkPosSq)[[sm]]
      xMarkSq[[sm]]<-xMark1
      attr(xMarkSq[[sm]], "spname")<-names(listOfdfMarkPosSq)[[sm]]
    } # end for

# markList<-addChrNameAttrMark(xMarkSq,yMarkSq,x)

# xMarkSq<-markList$xMark
# yMarkSq<-markList$yMark

                                                          #####################
                                                          #   add sq marks to plot holocen
                                                          #####################

# if(roundness<20){
#   myY<- y #newLongy
# } else {
#   myY<-y
# }


      roundPlotMark(roundness, xMarkSq, yMarkSq,
                    dfMarkColorInternal,
                    listOfdfMarkPosSq,
                    chrWidth, #use for calc r2
                    specialChrWidth,
                    yfactor,
                    n,
                    lwd.chr,
                    listOfdfChromSize,
                    circularPlot,
                    y,
                    markLabelSize,
                    pattern,
                    separFactor,
                    labelSpacing,circleCenter,circleCenterY,radius,
                    legend,ylistTransChrSimple,rotation=rotation,labelOutwards) #

    } #     if(length(listOfdfMarkPosSq)>0){

    else {remove(listOfdfMarkPosSq)}

  #
  #   inline legend holocen
  #

    booleanForsquareInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosSq") & circularPlot==FALSE

    if(booleanForsquareInlineLabel) {
      textLabel(xMarkSq,yMarkSq,listOfdfChromSize,listOfdfMarkPosSq,specialChrSpacing,chrSpacing,markLabelSize,pattern)
    }

  } #   if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") )

                                                      # end square marks  #



    ## up arrows

    ##########################################################################################################3
    #
    #                           painting Marks monocen              upArrow marks
    #
    ############################################################################################################

    arrowhead2<-1-arrowhead
    if (exists("listOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ){
      yMarkUpAr<-xMarkUpAr<-listOfdfMarkPosUpAr<-list()

      j<-1

      for (k in 1:length(listOfdfMarkPosMonocen)) {
        currName<-names(listOfdfMarkPosMonocen)[[k]]
        if(nrow(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="upArrow"),])>0){
          listOfdfMarkPosUpAr<-c(listOfdfMarkPosUpAr,list(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="upArrow"),]))
          names(listOfdfMarkPosUpAr)[[j]]<-currName
          j<-j+1
        }
      }

      if(length(listOfdfMarkPosUpAr)>0){
        for (sm in 1:length(listOfdfMarkPosUpAr)) {
          yMark1<-NULL
          xMark1<-NULL
          # which(names(listOfdfChromSize) %in% names(listOfdfMarkPosUpAr)[[sm]] )

          corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosUpAr)[[sm]] )

          for (m in 1:nrow(listOfdfMarkPosUpAr[[sm]]) ){
            ifelse(listOfdfMarkPosUpAr[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
            ifelse(listOfdfMarkPosUpAr[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
            ifelse(listOfdfMarkPosUpAr[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
            rowIndex<- nrow(listOfdfChromSize[[corr_index]] ) * longORshort + (listOfdfMarkPosUpAr[[sm]][,"neworder"][m])
            armStart<-ym[[corr_index]][rowIndex,column]
            yprox <- armStart +
              (listOfdfMarkPosUpAr[[sm]][m,"markDistCen"]                                                             *normalizeToOne*mySign)

            yter <- armStart +
              ( sum( listOfdfMarkPosUpAr[[sm]][m,"markDistCen"] , listOfdfMarkPosUpAr[[sm]][m,"markSize"], na.rm=TRUE ) *normalizeToOne*mySign )

            if(longORshort==0) {
                ybase<-yter
                ysize<-abs(yprox-yter)
            } else {
                ybase<-yprox
                ysize<-abs(yter-yprox)
            }

            yArrow<-c(ysize,arrowhead2*ysize,
                 arrowhead2*ysize,0,
                 0,arrowhead2*ysize,
                 arrowhead2*ysize,ysize
            )

            yMark1[[m]]<- yArrow + ybase

            attr(yMark1[[m]],"arm")<-listOfdfMarkPosUpAr[[sm]][m,"chrRegion"]
            # attr(yMark1[[m]],"armStart")<-armStart
            attr(yMark1[[m]],"rowIndex")<-rowIndex

            # xMark1[[m]] <- xm[[corr_index]][listOfdfMarkPosUpAr[[sm]][,"neworder"][m],]
            fourX <- xm[[corr_index]][listOfdfMarkPosUpAr[[sm]][,"neworder"][m],]



            xsize<-max(fourX)-min(fourX)

            if(arrowsToSide){
              xsize<-xsize/2
            }

            xbase<-min(fourX)

            if(arrowsToSide){
              xbase<-xbase+xsize
            }

            xArrow<-c(.5*xsize, xsize-(arrowheadWidthShrink*xsize),
                 (1-shrinkArrow)*xsize,(1-shrinkArrow)*xsize,
                 shrinkArrow*xsize,shrinkArrow*xsize,
                 0+(arrowheadWidthShrink*xsize),.5*xsize
            )
            # +1 represents base (min)
            xMark1[[m]] <- xArrow + xbase

            attr(xMark1[[m]],"rowIndex")<-rowIndex

          }
          yMarkUpAr[[sm]]<-yMark1
          attr(yMarkUpAr[[sm]], "spname")<-names(listOfdfMarkPosUpAr)[[sm]]
          xMarkUpAr[[sm]]<-xMark1
          attr(xMarkUpAr[[sm]], "spname")<-names(listOfdfMarkPosUpAr)[[sm]]
        } # end for

        # markList<-addChrNameAttrMark(xMarkUpAr,yMarkUpAr,x)
        #
        # xMarkUpAr<-markList$xMark
        # yMarkUpAr<-markList$yMark
        ########################
        #                      #
        #   add marks to plot monocen #
        #                      #
        ########################
        # if(circularPlot==FALSE){
        arrowPlotMark(roundness, xMarkUpAr, yMarkUpAr,
                      dfMarkColorInternal,
                      listOfdfMarkPosUpAr,
                      chrWidth, #use for calc r2
                      n,
                      lwd.chr,

                      circularPlot,
                      y,
                      x,
                      markLabelSize,
                      separFactor,
                      labelSpacing,circleCenter,circleCenterY,radius,
                      ylistTransChrSimple,rotation=rotation,
                      arrowheadWidthShrink)

      } #     if(length(listOfdfMarkPosUpAr)>0)

      else {remove(listOfdfMarkPosUpAr)}

      # upArrow labels not cen (monocen.)

      # booleanForupArrowInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosUpAr") & circularPlot==FALSE
      #
      # if(booleanForupArrowInlineLabel) {
      #   textLabel(xMarkUpAr,yMarkUpAr,listOfdfChromSize,listOfdfMarkPosUpAr,specialChrSpacing,chrSpacing,markLabelSize,pattern)
      # }

    } # if presence end painting marks

    ##################################################################################################
    #
    #                                                 painting Marks upArrow holocen
    #
    ##################################################################################################

    if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
      # message(crayon::green(paste0("holocen. marks section start" ) ) )
      xMarkUpAr<-yMarkUpAr<-listOfdfMarkPosUpAr<-list()
      j<-1
      for (k in 1:length(listOfdfMarkPosHolocen)) {
        currName<-names(listOfdfMarkPosHolocen)[[k]]
        if(nrow(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="upArrow"),])>0){
          listOfdfMarkPosUpAr<-c(listOfdfMarkPosUpAr,list(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="upArrow"),]) )
          names(listOfdfMarkPosUpAr)[[j]]<-currName
          j<-j+1
        }
      }

      if(length(listOfdfMarkPosUpAr)>0){
        for (sm in 1:length(listOfdfMarkPosUpAr)) {

          yMark1<-NULL
          xMark1<-NULL
          corr_index<-which(names(ym) %in% names(listOfdfMarkPosUpAr)[[sm]] )

          for (m in 1:nrow(listOfdfMarkPosUpAr[[sm]])){
            rowIndex<-(listOfdfMarkPosUpAr[[sm]][,"neworder"][m])
            chrStart<-ym[[corr_index]][rowIndex ,2]
            yinf <- chrStart +                        # was ysup
              (listOfdfMarkPosUpAr[[sm]][m,"markPos"]                                                          *normalizeToOne)

            ysup <- chrStart +
              (  sum(listOfdfMarkPosUpAr[[sm]][m,"markPos"],listOfdfMarkPosUpAr[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

            ysize <- abs(ysup-yinf)

            yArrow <- c(ysize,arrowhead2*ysize,
                 arrowhead2*ysize,0,
                 0,arrowhead2*ysize,
                 arrowhead2*ysize,ysize
            )

            yMark1[[m]] <- yArrow + yinf

            # yMark1[[m]] <- c(ysup,yinf,yinf,ysup)
            attr(yMark1[[m]],"rowIndex")<-rowIndex

            # xMark1[[m]]<-xm[[corr_index]][listOfdfMarkPosUpAr[[sm]][,"neworder"][m],]

            fourX <- xm[[corr_index]][listOfdfMarkPosUpAr[[sm]][,"neworder"][m],]

            xsize <- max(fourX) - min(fourX)
            xbase <- min(fourX)

            if(arrowsToSide){
              xsize<-xsize/2
            }

            if(arrowsToSide){
              xbase<-xbase+xsize
            }


            xArrow<-c(.5*xsize, xsize-(arrowheadWidthShrink*xsize),
                      (1-shrinkArrow)*xsize,(1-shrinkArrow)*xsize,
                 shrinkArrow*xsize,shrinkArrow*xsize,
                 0+(arrowheadWidthShrink*xsize),.5*xsize
            )
            # +1 represents base (min)
            xMark1[[m]] <- xArrow + xbase

            attr(xMark1[[m]],"rowIndex")<-rowIndex

          }
          yMarkUpAr[[sm]]<-yMark1
          attr(yMarkUpAr[[sm]], "spname")<-names(listOfdfMarkPosUpAr)[[sm]]
          xMarkUpAr[[sm]]<-xMark1
          attr(xMarkUpAr[[sm]], "spname")<-names(listOfdfMarkPosUpAr)[[sm]]
        } # end for

        # markList<-addChrNameAttrMark(xMarkUpAr,yMarkUpAr,x)

        # xMarkUpAr<-markList$xMark
        # yMarkUpAr<-markList$yMark

        #####################
        #   add UpAr marks to plot holocen
        #####################

        # if(roundness<20){
        #   myY<- y #newLongy
        # } else {
        #   myY<-y
        # }


        arrowPlotMark(roundness, xMarkUpAr, yMarkUpAr,
                      dfMarkColorInternal,
                      listOfdfMarkPosUpAr,
                      chrWidth, #use for calc r2


                      n,
                      lwd.chr,

                      circularPlot,
                      y,
                      x,
                      markLabelSize,
                      separFactor,
                      labelSpacing,circleCenter,circleCenterY,radius,
                      ylistTransChrSimple,rotation=rotation,
                      arrowheadWidthShrink) #

      } #     if(length(listOfdfMarkPosUpAr)>0){

      else {remove(listOfdfMarkPosUpAr)}

      #
      #   inline legend holocen
      #

      # booleanForupArrowInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosUpAr") & circularPlot==FALSE
      #
      # if(booleanForupArrowInlineLabel) {
      #   textLabel(xMarkUpAr,yMarkUpAr,listOfdfChromSize,listOfdfMarkPosUpAr,specialChrSpacing,chrSpacing,markLabelSize,pattern)
      # }

    } #   if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") )

    # end upArrow marks  #


    ## down arrows

    ##########################################################################################################3
    #
    #                           painting Marks monocen              downArrow marks
    #
    ############################################################################################################

    arrowhead2<-1-arrowhead
    if (exists("listOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ){

      xMarkDwAr<-yMarkDwAr<-listOfdfMarkPosDwAr<-list()

      j<-1

      for (k in 1:length(listOfdfMarkPosMonocen)) {
        currName<-names(listOfdfMarkPosMonocen)[[k]]
        if(nrow(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="downArrow"),])>0){
          listOfdfMarkPosDwAr<-c(listOfdfMarkPosDwAr,list(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="downArrow"),]))
          names(listOfdfMarkPosDwAr)[[j]]<-currName
          j<-j+1
        }
      }

      if(length(listOfdfMarkPosDwAr)>0){
        for (sm in 1:length(listOfdfMarkPosDwAr)) {
          yMark1<-NULL
          xMark1<-NULL

          corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosDwAr)[[sm]] )

          for (m in 1:nrow(listOfdfMarkPosDwAr[[sm]]) ){
            ifelse(listOfdfMarkPosDwAr[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
            ifelse(listOfdfMarkPosDwAr[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
            ifelse(listOfdfMarkPosDwAr[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
            rowIndex<- nrow(listOfdfChromSize[[corr_index]] ) * longORshort + (listOfdfMarkPosDwAr[[sm]][,"neworder"][m])
            armStart<-ym[[corr_index]][rowIndex,column]
            yprox <- armStart +
              (listOfdfMarkPosDwAr[[sm]][m,"markDistCen"]                                                             *normalizeToOne*mySign)

            yter <- armStart +
              ( sum( listOfdfMarkPosDwAr[[sm]][m,"markDistCen"] , listOfdfMarkPosDwAr[[sm]][m,"markSize"], na.rm=TRUE ) *normalizeToOne*mySign )

            if(longORshort==0) {
              ybase<-yter
              ysize<-abs(yprox-yter)
            } else {
              ybase<-yprox
              ysize<-abs(yter-yprox)
            }

            yArrow<-c(ysize,arrowhead*ysize,
                      arrowhead*ysize,0,
                      arrowhead*ysize,arrowhead*ysize,
                      ysize , ysize
            )

            yMark1[[m]]<- yArrow + ybase

            attr(yMark1[[m]],"arm")<-listOfdfMarkPosDwAr[[sm]][m,"chrRegion"]
            # attr(yMark1[[m]],"armStart")<-armStart
            attr(yMark1[[m]],"rowIndex")<-rowIndex

            # xMark1[[m]] <- xm[[corr_index]][listOfdfMarkPosDwAr[[sm]][,"neworder"][m],]
            fourX <- xm[[corr_index]][listOfdfMarkPosDwAr[[sm]][,"neworder"][m],]

            xsize <- max(fourX)-min(fourX)

            if(arrowsToSide){
              xsize<-xsize/2
            }

            xbase<-min(fourX)

            xArrow<-c((1-shrinkArrow)*xsize , (1-shrinkArrow)*xsize,
                      xsize-(arrowheadWidthShrink*xsize),.5*xsize,
                      0+(arrowheadWidthShrink*xsize),shrinkArrow*xsize,
                      shrinkArrow*xsize,(1-shrinkArrow)*xsize
            )

            # +1 represents base (min)
            xMark1[[m]] <- xArrow + xbase

            attr(xMark1[[m]],"rowIndex")<-rowIndex

          }
          yMarkDwAr[[sm]]<-yMark1
          attr(yMarkDwAr[[sm]], "spname")<-names(listOfdfMarkPosDwAr)[[sm]]
          xMarkDwAr[[sm]]<-xMark1
          attr(xMarkDwAr[[sm]], "spname")<-names(listOfdfMarkPosDwAr)[[sm]]
        } # end for

        # markList<-addChrNameAttrMark(xMarkDwAr,yMarkDwAr,x)
        #
        # xMarkDwAr<-markList$xMark
        # yMarkDwAr<-markList$yMark
        ########################
        #                      #
        #   add marks to plot monocen #
        #                      #
        ########################
        # if(circularPlot==FALSE){
        arrowPlotMark(roundness, xMarkDwAr, yMarkDwAr,
                      dfMarkColorInternal,
                      listOfdfMarkPosDwAr,
                      chrWidth, #use for calc r2


                      n,
                      lwd.chr,

                      circularPlot,
                      y,
                      x,
                      markLabelSize,
                      separFactor,
                      labelSpacing,circleCenter,circleCenterY,radius,
                      ylistTransChrSimple,rotation=rotation,
                      arrowheadWidthShrink)

      } #     if(length(listOfdfMarkPosDwAr)>0)

      else {remove(listOfdfMarkPosDwAr)}

      # downArrow labels not cen (monocen.)

      # booleanFordownArrowInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosDwAr") & circularPlot==FALSE
      #
      # if(booleanFordownArrowInlineLabel) {
      #   textLabel(xMarkDwAr,yMarkDwAr,listOfdfChromSize,listOfdfMarkPosDwAr,specialChrSpacing,chrSpacing,markLabelSize,pattern)
      # }

    } # if presence end painting marks

    ##################################################################################################
    #
    #                                                 painting Marks downArrow holocen
    #
    ##################################################################################################

    if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
      # message(crayon::green(paste0("holocen. marks section start" ) ) )
      xMarkDwAr<-yMarkDwAr<-listOfdfMarkPosDwAr<-list()

      j<-1
      for (k in 1:length(listOfdfMarkPosHolocen)) {
        currName<-names(listOfdfMarkPosHolocen)[[k]]
        if(nrow(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="downArrow"),])>0){
          listOfdfMarkPosDwAr<-c(listOfdfMarkPosDwAr,list(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="downArrow"),]) )
          names(listOfdfMarkPosDwAr)[[j]]<-currName
          j<-j+1
        }
      }

      if(length(listOfdfMarkPosDwAr)>0){
        for (sm in 1:length(listOfdfMarkPosDwAr)) {

          yMark1<-NULL
          xMark1<-NULL
          corr_index<-which(names(ym) %in% names(listOfdfMarkPosDwAr)[[sm]] )

          for (m in 1:nrow(listOfdfMarkPosDwAr[[sm]])){
            rowIndex<-(listOfdfMarkPosDwAr[[sm]][,"neworder"][m])
            chrStart<-ym[[corr_index]][rowIndex ,2]
            yinf <- chrStart +                        # was ysup
              (listOfdfMarkPosDwAr[[sm]][m,"markPos"]                                                          *normalizeToOne)

            ysup <- chrStart +
              (  sum(listOfdfMarkPosDwAr[[sm]][m,"markPos"],listOfdfMarkPosDwAr[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

            ysize <- abs(ysup-yinf)

            yArrow<-c(ysize,arrowhead*ysize,
                      arrowhead*ysize,0,
                      arrowhead*ysize,arrowhead*ysize,
                      ysize , ysize
            )

            yMark1[[m]] <- yArrow + yinf

            # yMark1[[m]] <- c(ysup,yinf,yinf,ysup)
            attr(yMark1[[m]],"rowIndex")<-rowIndex

            # xMark1[[m]]<-xm[[corr_index]][listOfdfMarkPosDwAr[[sm]][,"neworder"][m],]

            fourX <- xm[[corr_index]][listOfdfMarkPosDwAr[[sm]][,"neworder"][m],]

            xsize <- max(fourX) - min(fourX)

            if(arrowsToSide){
              xsize<-xsize/2
            }

            xbase <- min(fourX)

            xArrow<-c((1-shrinkArrow)*xsize , (1-shrinkArrow)*xsize,
                      xsize-(arrowheadWidthShrink*xsize),.5*xsize,
                      0+(arrowheadWidthShrink*xsize),shrinkArrow*xsize,
                      shrinkArrow*xsize,(1-shrinkArrow)*xsize
            )
            # +1 represents base (min)
            xMark1[[m]] <- xArrow + xbase

            attr(xMark1[[m]],"rowIndex") <- rowIndex
          }
          yMarkDwAr[[sm]]<-yMark1
          attr(yMarkDwAr[[sm]], "spname")<-names(listOfdfMarkPosDwAr)[[sm]]
          xMarkDwAr[[sm]]<-xMark1
          attr(xMarkDwAr[[sm]], "spname")<-names(listOfdfMarkPosDwAr)[[sm]]
        } # end for

        # markList<-addChrNameAttrMark(xMarkDwAr,yMarkDwAr,x)

        # xMarkDwAr<-markList$xMark
        # yMarkDwAr<-markList$yMark

        #####################
        #   add DwAr marks to plot holocen
        #####################

        arrowPlotMark(roundness, xMarkDwAr, yMarkDwAr,
                      dfMarkColorInternal,
                      listOfdfMarkPosDwAr,
                      chrWidth, #use for calc r2

                      n,
                      lwd.chr,

                      circularPlot,
                      y,
                      x,
                      markLabelSize,
                      separFactor,
                      labelSpacing,circleCenter,circleCenterY,radius,
                      ylistTransChrSimple,rotation=rotation,
                      arrowheadWidthShrink) #

      } #     if(length(listOfdfMarkPosDwAr)>0){

      else {remove(listOfdfMarkPosDwAr)}

      #
      #   inline legend holocen
      #

      # booleanFordownArrowInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosDwAr") & circularPlot==FALSE
      #
      # if(booleanFordownArrowInlineLabel) {
      #   textLabel(xMarkDwAr,yMarkDwAr,listOfdfChromSize,listOfdfMarkPosDwAr,specialChrSpacing,chrSpacing,markLabelSize,pattern)
      # }

    } #   if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") )

    # end downArrow marks  #

    ##
  ##########################################################################################################################

                                          ##########################################
                                          #
                                          #   painting Marks monocen cM style
                                          #
                                          ##########################################

if (exists("listOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ){
    # message(crayon::green(paste0("monocen. marks section start" ) ) )

    yMarkcM<- xMarkcM<- listOfdfMarkPoscM<-list()

    j<-1

    for (k in 1:length(listOfdfMarkPosMonocen)) {
      currName<-names(listOfdfMarkPosMonocen)[[k]]
      if(nrow(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="cM"),])>0){
        listOfdfMarkPoscM<-c(listOfdfMarkPoscM,list(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="cM"),]))
        names(listOfdfMarkPoscM)[[j]]<-currName
        j<-j+1
      }
    }
#    listOfdfMarkPoscM3790<<-listOfdfMarkPoscM

    if(length(listOfdfMarkPoscM)>0){
      for (sm in 1:length(listOfdfMarkPoscM)) {
        yMark1<-NULL
        xMark1<-NULL
        for (m in 1:nrow(listOfdfMarkPoscM[[sm]]) ){

          if("protruding" %in% colnames(listOfdfMarkPoscM[[sm]]) ){
            ifelse(is.na(listOfdfMarkPoscM[[sm]][m,"protruding"] ) ,
                   protruding2<-protruding,
                   protruding2<-listOfdfMarkPoscM[[sm]][m,"protruding"]
                   )
          } else {
            protruding2<-protruding
          }
          ifelse(listOfdfMarkPoscM[[sm]][m,"chrRegion"]=="q",longORshort<-0,longORshort<-1)
          ifelse(listOfdfMarkPoscM[[sm]][m,"chrRegion"]=="q",column<-1,column<-2)
          ifelse(listOfdfMarkPoscM[[sm]][m,"chrRegion"]=="q",mySign<--1,mySign<-1)

          corr_index<-which(names(ym) %in% names(listOfdfMarkPoscM)[[sm]] )
          rowIndex <- nrow(listOfdfChromSize[[corr_index]])*longORshort+(listOfdfMarkPoscM[[sm]][,"neworder"][m])
          ypos<-ym[[corr_index]][ rowIndex  ,column]+
            (listOfdfMarkPoscM[[sm]][m,"markDistCen"]                                      *normalizeToOne*mySign)

          yMark1[[m]]<-c(ypos,ypos)
          attr(yMark1[[m]],"rowIndex")<-rowIndex

          xMark1[[m]]<-xm[[corr_index]][listOfdfMarkPoscM[[sm]][,"neworder"][m],][c(1,3)]

          initOrig2<-xMark1[[m]][2]

          if(cMBeginCenter){
            xMark1[[m]][2] <- xMark1[[m]][2] +   ( (xMark1[[m]][1] - xMark1[[m]][2])  / 2)
          }

          xMark1[[m]][1] <- xMark1[[m]][1] +   ( (xMark1[[m]][1] - initOrig2  )  * protruding2) # 1st protruding

          attr(xMark1[[m]],"rowIndex")<-rowIndex
        }
        yMarkcM[[sm]]<-yMark1
        attr(yMarkcM[[sm]], "spname")<-names(listOfdfMarkPoscM)[[sm]] # added 1.14
        xMarkcM[[sm]]<-xMark1
        attr(xMarkcM[[sm]], "spname")<-names(listOfdfMarkPoscM)[[sm]]
      } # end for

      #####################
      #   add marks to plot
      #####################
      # cMPlotMark(xMarkcM, yMarkcM, dfMarkColorInternal,listOfdfMarkPoscM, lwd.cM2,circularPlot)
      cMPlotMark(xMarkcM, yMarkcM,y, x, dfMarkColorInternal,listOfdfMarkPoscM, lwd.cM2,circularPlot,
        radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,n,labelSpacing,chrWidth,
        ylistTransChrSimple,rotation=rotation,labelOutwards)

    } #     if(length(listOfdfMarkPoscM)>0){
      else {
      remove(listOfdfMarkPoscM)
      }

  #
  #   labels cM inline monocen.
  #

booleanColorInternalMarkcM<- exists ("dfMarkColorInternal") & exists ("listOfdfMarkPoscM") & circularPlot==FALSE

if(booleanColorInternalMarkcM)  {

      textLabel(xMarkcM,yMarkcM,listOfdfChromSize,listOfdfMarkPoscM,specialChrSpacing,chrSpacing,markLabelSize,pattern)
}

} # if presence end painting marks

                                  ########################################
                                  #
                                  #   painting Marks cM holocen
                                  #
                                  ########################################

if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
    # message(crayon::green(paste0("holocen. marks section start" ) ) )
  xMarkcM<-yMarkcM<-listOfdfMarkPoscM<-list()

    j<-1
    for (k in 1:length(listOfdfMarkPosHolocen)) {
      currName<-names(listOfdfMarkPosHolocen)[[k]]
      if(nrow(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="cM"),])>0){
        listOfdfMarkPoscM<-c(listOfdfMarkPoscM,list(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="cM"),]) )
        names(listOfdfMarkPoscM)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listOfdfMarkPoscM)>0) {
      for (sm in 1:length(listOfdfMarkPoscM)) {

        yMark1<-NULL
        xMark1<-NULL

        for (m in 1:nrow(listOfdfMarkPoscM[[sm]])){

          if("protruding" %in% colnames(listOfdfMarkPoscM[[sm]]) ){
            ifelse(is.na(listOfdfMarkPoscM[[sm]][m,"protruding"] ) ,
                   protruding2<-protruding,
                   protruding2<-listOfdfMarkPoscM[[sm]][m,"protruding"]
            )
          } else {
            protruding2<-protruding
          }

          corr_index<-which(names(ym) %in% names(listOfdfMarkPoscM)[[sm]] )
          rowIndex<-(listOfdfMarkPoscM[[sm]][,"neworder"][m])
          ypos<-  ym[[corr_index]][ rowIndex , 2]+
            (listOfdfMarkPoscM[[sm]][m,"markPos"]                                      *normalizeToOne*1)

          yMark1[[m]]<-c(ypos,ypos)
          attr(yMark1[[m]],"rowIndex")<-rowIndex

          xMark1[[m]]<-xm[[corr_index]][ rowIndex , ][c(1,3)]

          initOrig2<-xMark1[[m]][2]

          if(cMBeginCenter){
            xMark1[[m]][2] <- xMark1[[m]][2] +   ( (xMark1[[m]][1] - xMark1[[m]][2])  / 2)
          }

          xMark1[[m]][1]<-xMark1[[m]][1]+((xMark1[[m]][1] - initOrig2) *protruding2)

          attr(xMark1[[m]],"rowIndex")<-rowIndex

        }
        yMarkcM[[sm]]<-yMark1
        attr(yMarkcM[[sm]], "spname")<-names(listOfdfMarkPoscM)[[sm]] # added 1.14
        xMarkcM[[sm]]<-xMark1
        attr(xMarkcM[[sm]], "spname")<-names(listOfdfMarkPoscM)[[sm]]
      } # end for

      # markList<-addChrNameAttrMark(xMarkcM,yMarkcM,x) # 1.14

      # xMarkcM<-markList$xMark
      # yMarkcM<-markList$yMark


      #####################
      #   add marks to plot
      #####################

      # cMPlotMark(xMarkcM, yMarkcM,    dfMarkColorInternal,listOfdfMarkPoscM, lwd.cM2,circularPlot)
      cMPlotMark(xMarkcM, yMarkcM, y, x, dfMarkColorInternal,listOfdfMarkPoscM, lwd.cM2,circularPlot,
                 radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,n,labelSpacing,chrWidth,
                 ylistTransChrSimple,rotation=rotation,labelOutwards)

    } #     if(length(listOfdfMarkPoscM)>0){

    else { # length = 0
      remove(listOfdfMarkPoscM)
    }
    # message(crayon::green(paste0("holocen. marks section end" ) ) )

  #
  #   inline legend cM holocen
  #

  booleanColorInternalMarkcM<- exists ("dfMarkColorInternal") & exists ("listOfdfMarkPoscM") & circularPlot==FALSE

  if(booleanColorInternalMarkcM)  {

    textLabel(xMarkcM,yMarkcM,listOfdfChromSize,listOfdfMarkPoscM,specialChrSpacing,chrSpacing,markLabelSize,pattern)
  } # if
} # if listOfdfMarkPosHolocen


    ## cMLeft ######################################################################################################################

    ##########################################
    #
    #   painting Marks monocen cMLeft style
    #
    ##########################################

    if (exists("listOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ){
      # message(crayon::green(paste0("monocen. marks section start" ) ) )

      xMarkcMLeft <- yMarkcMLeft<-listOfdfMarkPoscMLeft<-list()
      j<-1

      for (k in 1:length(listOfdfMarkPosMonocen)) {
        currName<-names(listOfdfMarkPosMonocen)[[k]]
        if(nrow(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="cMLeft"),])>0){
          listOfdfMarkPoscMLeft<-c(listOfdfMarkPoscMLeft,list(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="cMLeft"),]))
          names(listOfdfMarkPoscMLeft)[[j]]<-currName
          j<-j+1
        }
      }

#      listOfdfMarkPoscMLeft3987<<-listOfdfMarkPoscMLeft

      if(length(listOfdfMarkPoscMLeft)>0){
        for (sm in 1:length(listOfdfMarkPoscMLeft)) {
          yMark1<-NULL
          xMark1<-NULL
          for (m in 1:nrow(listOfdfMarkPoscMLeft[[sm]]) ){

            if("protruding" %in% colnames(listOfdfMarkPoscMLeft[[sm]]) ){
              ifelse(is.na(listOfdfMarkPoscMLeft[[sm]][m,"protruding"] ) ,
                     protruding2<-protruding,
                     protruding2<-listOfdfMarkPoscMLeft[[sm]][m,"protruding"]
              )
            } else {
              protruding2<-protruding
            }

            ifelse(listOfdfMarkPoscMLeft[[sm]][m,"chrRegion"]=="q",longORshort<-0,longORshort<-1)
            ifelse(listOfdfMarkPoscMLeft[[sm]][m,"chrRegion"]=="q",column<-1,column<-2)
            ifelse(listOfdfMarkPoscMLeft[[sm]][m,"chrRegion"]=="q",mySign<--1,mySign<-1)

            corr_index<-which(names(ym) %in% names(listOfdfMarkPoscMLeft)[[sm]] )
            rowIndex <- nrow(listOfdfChromSize[[corr_index]])*longORshort+(listOfdfMarkPoscMLeft[[sm]][,"neworder"][m])
            ypos<-ym[[corr_index]][ rowIndex  ,column]+
              (listOfdfMarkPoscMLeft[[sm]][m,"markDistCen"]                                      *normalizeToOne*mySign)

            yMark1[[m]]<-c(ypos,ypos)
            attr(yMark1[[m]],"rowIndex")<-rowIndex

            xMark1[[m]]<-xm[[corr_index]][listOfdfMarkPoscMLeft[[sm]][,"neworder"][m],][c(1,3)]

            initOrig1<-xMark1[[m]][1]

            if(cMBeginCenter){
              xMark1[[m]][1] <- xMark1[[m]][1] -   ( (xMark1[[m]][1] - xMark1[[m]][2])  / 2)
            }

            xMark1[[m]][2] <- xMark1[[m]][2] -   ( (initOrig1 - xMark1[[m]][2])  * protruding2) # left
            attr(xMark1[[m]],"rowIndex")<-rowIndex
          }
          yMarkcMLeft[[sm]]<-yMark1
          attr(yMarkcMLeft[[sm]], "spname")<-names(listOfdfMarkPoscMLeft)[[sm]] # added 1.14
          xMarkcMLeft[[sm]]<-xMark1
          attr(xMarkcMLeft[[sm]], "spname")<-names(listOfdfMarkPoscMLeft)[[sm]]
        } # end for

        #####################
        #   add marks to plot
        #####################

        cMLeftPlotMark(xMarkcMLeft, yMarkcMLeft,y, x, dfMarkColorInternal,listOfdfMarkPoscMLeft, lwd.cM2,circularPlot,
                   radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,n,labelSpacing,chrWidth,
                   ylistTransChrSimple,rotation=rotation,labelOutwards)

      } #     if(length(listOfdfMarkPoscMLeft)>0){
      else {
        remove(listOfdfMarkPoscMLeft)
      }

      #
      #   labels cMLeft inline monocen.
      #

      booleanColorInternalMarkcMLeft<- exists ("dfMarkColorInternal") & exists ("listOfdfMarkPoscMLeft") & circularPlot==FALSE

      if(booleanColorInternalMarkcMLeft)  {

        textLabelLeft(xMarkcMLeft,yMarkcMLeft,listOfdfChromSize,listOfdfMarkPoscMLeft,specialChrSpacing,chrSpacing,markLabelSize,pattern)
      }

    } # if presence end painting marks

    ########################################
    #
    #   painting Marks cMLeft holocen
    #
    ########################################

    if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
      # message(crayon::green(paste0("holocen. marks section start" ) ) )
      xMarkcMLeft<- yMarkcMLeft<-listOfdfMarkPoscMLeft<-list()
      j<-1
      for (k in 1:length(listOfdfMarkPosHolocen)) {
        currName<-names(listOfdfMarkPosHolocen)[[k]]
        if(nrow(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="cMLeft"),])>0){
          listOfdfMarkPoscMLeft<-c(listOfdfMarkPoscMLeft,list(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="cMLeft"),]) )
          names(listOfdfMarkPoscMLeft)[[j]]<-currName
          j<-j+1
        }
      }

      if(length(listOfdfMarkPoscMLeft)>0) {
        for (sm in 1:length(listOfdfMarkPoscMLeft)) {

          yMark1<-NULL
          xMark1<-NULL

          for (m in 1:nrow(listOfdfMarkPoscMLeft[[sm]])){

            if("protruding" %in% colnames(listOfdfMarkPoscMLeft[[sm]]) ){
              ifelse(is.na(listOfdfMarkPoscMLeft[[sm]][m,"protruding"] ) ,
                     protruding2<-protruding,
                     protruding2<-listOfdfMarkPoscMLeft[[sm]][m,"protruding"]
              )
            } else {
              protruding2<-protruding
            }

            corr_index<-which(names(ym) %in% names(listOfdfMarkPoscMLeft)[[sm]] )
            rowIndex<-(listOfdfMarkPoscMLeft[[sm]][,"neworder"][m])
            ypos<-  ym[[corr_index]][ rowIndex , 2]+
              (listOfdfMarkPoscMLeft[[sm]][m,"markPos"]                                      *normalizeToOne*1)

            yMark1[[m]]<-c(ypos,ypos)
            attr(yMark1[[m]],"rowIndex")<-rowIndex

            xMark1[[m]]<-xm[[corr_index]][ rowIndex , ][c(1,3)]

            initOrig1<-xMark1[[m]][1]

            if(cMBeginCenter){
              xMark1[[m]][1] <- xMark1[[m]][1] -   ( (xMark1[[m]][1] - xMark1[[m]][2])  / 2)
            }

            xMark1[[m]][2] <- xMark1[[m]][2] -   ( (initOrig1 - xMark1[[m]][2])  * protruding2) # left
            # xMark1[[m]][2] <- xMark1[[m]][2] -   ( (xMark1[[m]][1] - xMark1[[m]][2])  * protruding)

            attr(xMark1[[m]],"rowIndex")<-rowIndex

          }
          yMarkcMLeft[[sm]]<-yMark1
          attr(yMarkcMLeft[[sm]], "spname")<-names(listOfdfMarkPoscMLeft)[[sm]] # added 1.14
          xMarkcMLeft[[sm]]<-xMark1
          attr(xMarkcMLeft[[sm]], "spname")<-names(listOfdfMarkPoscMLeft)[[sm]]
        } # end for

        # markList<-addChrNameAttrMark(xMarkcMLeft,yMarkcMLeft,x) # 1.14

        # xMarkcMLeft<-markList$xMark
        # yMarkcMLeft<-markList$yMark


        #####################
        #   add marks to plot
        #####################

        # cMLeftPlotMark(xMarkcMLeft, yMarkcMLeft,    dfMarkColorInternal,listOfdfMarkPoscMLeft, lwd.cMLeft2,circularPlot)
        cMLeftPlotMark(xMarkcMLeft, yMarkcMLeft, y, x, dfMarkColorInternal,listOfdfMarkPoscMLeft, lwd.cM2,circularPlot,
                   radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,n,labelSpacing,chrWidth,
                   ylistTransChrSimple,rotation=rotation,labelOutwards)

      } #     if(length(listOfdfMarkPoscMLeft)>0){

      else { # length = 0
        remove(listOfdfMarkPoscMLeft)
      }
      # message(crayon::green(paste0("holocen. marks section end" ) ) )

      #
      #   inline legend cMLeft holocen
      #

      booleanColorInternalMarkcMLeft<- exists ("dfMarkColorInternal") & exists ("listOfdfMarkPoscMLeft") & circularPlot==FALSE

      if(booleanColorInternalMarkcMLeft)  {

        textLabelLeft(xMarkcMLeft,yMarkcMLeft,listOfdfChromSize,listOfdfMarkPoscMLeft,specialChrSpacing,chrSpacing,markLabelSize,pattern)
      } # if
    } # if listOfdfMarkPosHolocen

                                                            #
                                                            #         DOTS
                                                            #

  ##########################################################################################################################
  #
  # dot style of marks                        monocen dots
  #
  ##########################################################################################################################

  if (exists("listOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ) {

    listOfdfMarkPosCr<-list()

    yMarkCr<-xMarkCr<-rad<-  colBorderCr<-colCr<-list()

    j<-1

    for (k in 1:length(listOfdfMarkPosMonocen)) {
      currName<-names(listOfdfMarkPosMonocen)[[k]]
      if(nrow(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="dots"),])>0){
      listOfdfMarkPosCr<-c(listOfdfMarkPosCr,list(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="dots"),]) )
      names(listOfdfMarkPosCr)[[j]]<-currName
      j<-j+1
      }
    }

    if(length(listOfdfMarkPosCr)>0) {

    for (k in 1:length(listOfdfMarkPosCr) ) {

      colBorderCr1<-colCr1<-rad1<-yMarkCr1<-xMarkCr1<-NULL

      for (i in 1:nrow(listOfdfMarkPosCr[[k]])){
        # i=1
        ifelse(listOfdfMarkPosCr[[k]][i,"chrRegion"]=="q",longORshort<-0,longORshort<-1) #ifelseinloop
        ifelse(listOfdfMarkPosCr[[k]][i,"chrRegion"]=="q",column<-1,column<-2)
        ifelse(listOfdfMarkPosCr[[k]][i,"chrRegion"]=="q",mySign<--1,mySign<-1)
        corr_index<-which(names(ym) %in% names(listOfdfMarkPosCr)[[k]] )

        rowIndex <- nrow(listOfdfChromSize[[corr_index]])*longORshort+(listOfdfMarkPosCr[[k]][,"neworder"][i])
        armStart <- ym[[corr_index]][ rowIndex,column]
        ysupCr<- armStart +
          (listOfdfMarkPosCr[[k]][i,"markDistCen"]                                                      *normalizeToOne*mySign)

        yinfCr <- armStart +
          ( sum(listOfdfMarkPosCr[[k]][i,"markDistCen"],listOfdfMarkPosCr[[k]][i,"markSize"],na.rm=TRUE)*normalizeToOne*mySign)

        yMarkCr1[[i]]<-rep(list(mean(c(yinfCr,ysupCr))),2)
        attr(yMarkCr1[[i]], "rowIndex")<- rowIndex

        xBoundaries<-xm[[corr_index]][listOfdfMarkPosCr[[k]][,"neworder"][i],3:2]
        xBoundariesQuar<-(xBoundaries[2]-xBoundaries[1])/4
        xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar),list(xBoundaries[1]+3*xBoundariesQuar) )
        attr(xMarkCr1[[i]], "rowIndex")<- rowIndex

        rad1[[i]]<-rep(list(abs(ysupCr-yinfCr)/2 ),2)
        colCr1[[i]] <- rep(list(dfMarkColorInternal$markColor[match(listOfdfMarkPosCr[[k]]$markName[i], dfMarkColorInternal$markName)]),2)
        colBorderCr1[[i]] <- rep(list(dfMarkColorInternal$markBorderColor[match(listOfdfMarkPosCr[[k]]$markName[i], dfMarkColorInternal$markName)]),2)

      }
      yMarkCr[[k]]<-yMarkCr1
      attr(yMarkCr[[k]], "spname")<-names(listOfdfMarkPosCr)[[k]] # added 1.14
      xMarkCr[[k]]<-xMarkCr1
      attr(xMarkCr[[k]], "spname")<-names(listOfdfMarkPosCr)[[k]] # added 1.14

      rad[[k]]<-rad1
      colCr[[k]]<-colCr1
      colBorderCr[[k]]<-colBorderCr1

    } # end for k OTU

      # markListCr<-addChrNameAttrMarkDots(xMarkCr,yMarkCr,x) # 1.14
      #
      # xMarkCr <- markListCr$xMark
      # yMarkCr <- markListCr$yMark

    #####################
    #   add to plot MarkCrs DOTS monocen
    #####################

    # plotDotMarks(xMarkCr,yMarkCr,rad,colCr,n,xfactor,colBorderCr)
      plotDotMarks(xMarkCr,yMarkCr, rad, colCr,n,xfactor,colBorderCr,circularPlot, y,x,radius,circleCenter,circleCenterY,separFactor,
                   chrWidth,listOfdfMarkPosCr,markLabelSize,pattern,labelSpacing,useOneDot,legend,ylistTransChrSimple,rotation=rotation,
                   labelOutwards)


    } #     if(length(listOfdfMarkPosCr)>0){

   else {
     remove(listOfdfMarkPosCr)
   }
  } # end painting MarkCrs

  #
  #         WRITE INLINE LEGENDS WHEN DOTS
  #

  booleanDotsLabels<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosCr") & circularPlot==FALSE

  if (booleanDotsLabels){
    textLabelDots(xMarkCr,yMarkCr,listOfdfChromSize,listOfdfMarkPosCr,specialChrSpacing,
                  chrSpacing,markLabelSize,pattern,xBoundariesQuar)
  }

  ##############################################################################################################################
  #
  #                                            dot style of marks holocen
  #
  ########################################

  if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ){
    # message(crayon::green(paste0("holocen. dot marks section start" ) ) )
    listOfdfMarkPosCr<-list()

    colBorderCr<-colCr<-rad<-xMarkCr<-yMarkCr<-list()
    j<-1

    for (k in 1:length(listOfdfMarkPosHolocen)) {
      currName<-names(listOfdfMarkPosHolocen)[[k]]

      if(nrow(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="dots"),])>0){
        listOfdfMarkPosCr<-c(listOfdfMarkPosCr,list(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="dots"),]) )
        names(listOfdfMarkPosCr)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listOfdfMarkPosCr)>0){

    for (k in 1:length(listOfdfMarkPosCr) ) {

      yMarkCr1<-NULL
      xMarkCr1<-NULL
      rad1<-NULL
      colCr1<-NULL
      colBorderCr1<-NULL

      for (i in 1:nrow(listOfdfMarkPosCr[[k]])){
        corr_index<-which(names(ym) %in% names(listOfdfMarkPosCr)[[k]] )
        rowIndex <- (listOfdfMarkPosCr[[k]][,"neworder"][i])
        chrStart <- ym[[corr_index]][ rowIndex , 2]
        ysupCr<- chrStart +
          (listOfdfMarkPosCr[[k]][i,"markPos"]                                      *normalizeToOne*1)
        yinfCr<- chrStart +
          ( sum(listOfdfMarkPosCr[[k]][i,"markPos"], listOfdfMarkPosCr[[k]][i,"markSize"], na.rm=TRUE )*normalizeToOne)
        yMarkCr1[[i]]<-rep(list(mean(c(yinfCr,ysupCr))),2)
        attr(yMarkCr1[[i]], "rowIndex")<- rowIndex

        xBoundaries<-xm[[corr_index]][listOfdfMarkPosCr[[k]][,"neworder"][i],3:2]
        xBoundariesQuar<-(xBoundaries[2]-xBoundaries[1])/4
        xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar),list(xBoundaries[1]+3*xBoundariesQuar) )
        attr(xMarkCr1[[i]], "rowIndex")<- rowIndex

        rad1[[i]]<-rep(list(abs(ysupCr-yinfCr)/2 ),2)
        colCr1[[i]] <- rep(list(dfMarkColorInternal$markColor[match(listOfdfMarkPosCr[[k]]$markName[i], dfMarkColorInternal$markName)]),2)
        colBorderCr1[[i]] <- rep(list(dfMarkColorInternal$markBorderColor[match(listOfdfMarkPosCr[[k]]$markName[i], dfMarkColorInternal$markName)]),2)
      }

      yMarkCr[[k]]<-yMarkCr1
      attr(yMarkCr[[k]], "spname")<-names(listOfdfMarkPosCr)[[k]] # added 1.14
      xMarkCr[[k]]<-xMarkCr1
      attr(xMarkCr[[k]], "spname")<-names(listOfdfMarkPosCr)[[k]] # added 1.14
      rad[[k]]<-rad1
      colCr[[k]]<-colCr1
      colBorderCr[[k]]<-colBorderCr1

    } # end for

    markListCr<-addChrNameAttrMarkDots(xMarkCr,yMarkCr,x) # 1.14

    xMarkCr <- markListCr$xMark
    yMarkCr <- markListCr$yMark

#    listOfdfMarkPosCrInternal<<-listOfdfMarkPosCr

    #####################
    #   add to plot MarkCrs DOTS holocen
    #####################

    plotDotMarks(xMarkCr,yMarkCr, rad, colCr,n,xfactor,colBorderCr,circularPlot, y,x,radius,circleCenter,circleCenterY,separFactor,
    chrWidth,listOfdfMarkPosCr,markLabelSize,pattern,labelSpacing,useOneDot,legend,ylistTransChrSimple, rotation=rotation,
    labelOutwards)

    } # if(length(listOfdfMarkPosCr)>0){

  else {remove(listOfdfMarkPosCr)}
  } # end painting MarkCrs

  #
  #     write inline legend dots holocen
  #

  booleanDotsLabels<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosCr") & circularPlot==FALSE

  if (booleanDotsLabels){
    textLabelDots(xMarkCr,yMarkCr,listOfdfChromSize,listOfdfMarkPosCr,specialChrSpacing,
                            chrSpacing,markLabelSize,pattern,xBoundariesQuar)
  }

  ################################
  #
  #   centromeres calculate
  #
  ################################

  if(exists("listOfdfChromSizeMonocen") ) {

#   listOfdfChromSizeMonocenI<<-listOfdfChromSizeMonocen
#   listOfdfChromSizeI<<-listOfdfChromSize
#   centromereSize2I<<-centromereSize2
#   normalizeToOneI<<-normalizeToOne

    #
    # cannot be based on listofmonocen because of addmissingotu
    #

    CentsList<-lapply(1:length(listOfdfChromSize), function(x) tryCatch(rep(0, nrow(listOfdfChromSize[[x]]) ), error=function(e) NA )
                      ) # l
    names(CentsList)<-names(listOfdfChromSize)

    ycoordCents <-list()

    for (i in 1:length(CentsList)){
      corr_index<-which(names(listOfdfChromSize) %in% names(CentsList)[[i]] )
      centromereSize2<-tryCatch(as.numeric(attr(listOfdfChromSize[[corr_index]],"centromere") ) , error=function(e) NA )

      # ycoordCents[[i]] <- t(replicate(length(CentsList[[i]])*2, (c(rep(  karHeight+  (karHeiSpace*(i-1)),2  ),
      #                                         rep(  karHeight+(centromereSize2*normalizeToOne)+(karHeiSpace*(i-1)),2  )
      # )      )      ) # radius
      # ) #t
      ycoordCents[[i]] <- t(replicate(length(CentsList[[i]])*1, (c(rep(  karHeight+  (karHeiSpace*(i-1)), 2  ),
                                                                   rep(  karHeight+(centromereSize2*normalizeToOne)+(karHeiSpace*(i-1)),2  )
      )      )      ) # radius
      ) #t
    } # for
    names(ycoordCents)<-names(CentsList)

# ycoordCents[areNA]<-NA

# suppressWarnings(
if(is.na(addMissingOTUAfter[1])){
    if(karSepar){
      for (s in 1:(length(ymCopyC)-1) ) {
        diffnext<-abs(min(ymCopyC[[s+1]] ) - max(ymCopyC[[s]]) )
        ymCopyC[[s+1]]=ymCopyC[[s+1]]-diffnext

        # corrIndex<-which(names(ycoordCents) %in% names(ymCopyC[s]) )

        ycoordCents[[s+1]] <-ycoordCents[[s+1]] -diffnext

        ifelse(amoSepar>diffnext,amoSepar2<-diffnext,amoSepar2<-amoSepar)

        ymCopyC[[s+1]]=ymCopyC[[s+1]]+amoSepar2
        ycoordCents[[s+1]] <-ycoordCents[[s+1]] +amoSepar2
      } # for
    } # redu
}
    ycoordCentsS<- lapply(1:length(ycoordCents), function(j)
      tryCatch(base::split(ycoordCents[[j]], row(ycoordCents[[j]]) ),
               error=function(e) NA
               )
      )

    names(ycoordCentsS)<-names(listOfdfChromSize)

    #
    #   remove holocen
    #

    ycoordCentsS<-tryCatch(ycoordCentsS[which(names(ycoordCentsS) %in% monocenNames)] , error=function(e) NA )
    ycoordCents<-tryCatch(ycoordCents[which(names(ycoordCents) %in% monocenNames)] , error=function(e) NA )

    ycoordCentsS<-ycoordCentsS[!is.na(ycoordCentsS)]
    ycoordCents<-ycoordCents[!is.na(ycoordCents)]



    if (roundness<=20){
      r3 <- chrWidth2/(roundness*3)
    } else {
      r3 <- 0
    }

    xcoordCents<-list()
#    xmnoNAI<<-xmnoNA
#    listOfdfChromSizenoNAI2<<-listOfdfChromSizenoNA
#    monocenNamesI2<<-monocenNames

    for (s in 1:length(xmnoNA)){
      if(names(listOfdfChromSizenoNA[s]) %in% monocenNames ){
        if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
        } else {
          chrWidth2<-chrWidth
        }
#        chrWidth2I<<-chrWidth2
#        r3I<<-r3
        # xcoordCents[[s]] <-cbind(sweep(xmnoNA[[s]][,2:3], 2, c(-r3,r3), "+"),
        #     sweep(xmnoNA[[s]][,2:3], 2, c(-r3,r3), "+")
        #     ) #bind
        xmMatrix<-xmnoNA[[s]][1:(nrow(xmnoNA[[s]])/2),2:3]
        if(!inherits(xmMatrix, "matrix")) {
          xmMatrix<-  t(as.matrix(xmMatrix) )
        }
        xcoordCents[[s]] <-cbind(sweep(xmMatrix, 2, c(-r3,r3), "+"),
                                 sweep(xmMatrix, 2, c(-r3,r3), "+")
        ) #bind

        # attr(xcoordCents[[s]], "spname")<-names(xmnoNA)[[s]]
        names(xcoordCents)[s] <- names(listOfdfChromSizenoNA[s])
        } # monocen
    }
    xcoordCents<-Filter(function(x) {length(x) >= 1}, xcoordCents)

    xcoordCentsS<- lapply(1:length(xcoordCents), function(j) base::split(xcoordCents[[j]], row(xcoordCents[[j]]) ) )
    names(xcoordCentsS)<-names(xcoordCents)

    # xcoordCentsS<-tryCatch(xcoordCentsS[which(names(xcoordCentsS) %in% monocenNames)], error= function(e) NA )
    xcoordCentsS<-xcoordCentsS[!is.na(xcoordCentsS)]

    if(!missing(chrBorderColor)){
      cenBorder<-chrBorderColor
    } else {
      cenBorder<-cenColor2
    }
    # this protects for weak colors, adds strong border

    if(!missing(fixCenBorder)){
      if (fixCenBorder){
            if (chrColor != cenColor2){
              cenBorder<-chrColor
            } else {
              cenBorder<-cenColor2
            }
      }
    }

    #
    #     plot centromeres
    #

#    xcoordCentsSI<<-xcoordCentsS
#    ycoordCentsSI<<-ycoordCentsS
#    cenBorderI<<-cenBorder



      if(circularPlot){

        spnamexcoord<- names(xcoordCentsS)[1]
        corrSp<- which(names(x) %in% spnamexcoord)
        diffXRounded <- max(x[[corrSp]][[1]]) - max(xcoordCentsS[[1]][[1]]) # needed for all cen marks

        if(roundness <= 20){
#            xI<<-x
#            diffXRoundedI<<-diffXRounded
#            monocenNamesI<<-monocenNames

            xRounded<-xForRounded(x,diffXRounded,monocenNames)

            xlistNewChr<-xHortoVerRoundCen(xRounded,diffXRounded)

        } else {

          xlistNewChr<-xHortoVer(x)
          # names(xlistNewChr)<-names(x)

        } # roundness bool

        xlistNewCen <- makeCenCoordsX(xlistNewChr,monocenNames)
        names(xlistNewCen)<-monocenNames

        #
        #   y
        #

        yInterSimple<-intercalate(y,monocenNames)
        names(yInterSimple)<-names(y)

        ylistNewChrSimple<-yVertoHor(yInterSimple, monocenNames = monocenNames)
        names(ylistNewChrSimple)<-names(y)

        #
        #   x horizontal to vertical Y for cent. is same for rounded or squared
        #

        ylistTransChrSimple<-transYList(ylistNewChrSimple,shrinkFactor,monocenNames)
        names(ylistTransChrSimple) <- names(ylistNewChrSimple)
#        ylistTransChrSimpleI<<-ylistTransChrSimple

        ylistNewCen <- makeCenCoordsY(ylistTransChrSimple,monocenNames)
        if(!is.null(cenColor2) ) {
#        ylistNewCenI<<-ylistNewCen
        circleMapsCen  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistNewCen,xlistNewCen,n,0,chrWidth,rotation=rotation)

        drawCen(circleMapsCen,cenColor2,cenBorder,lwd.chr)
        }

      } else if(circularPlot==FALSE) {

        if(!is.null(cenColor2) ) {

      if(length(xcoordCentsS)>0){
        lapply(1:length(xcoordCentsS), function(w) mapply(function(x,y,z) graphics::polygon(x=x, y=y,
                                                                                            col=cenColor2,
                                                                                            lwd=lwd.chr,
                                                                                            border=z),
                                                          x=xcoordCentsS[[w]],
                                                          y=ycoordCentsS[[w]],
                                                          z=cenBorder
                                                    ) #m
        ) #l
      } # if length xcoordCentsS
    }
    } # circular else not
  # } # cenColor2 NULL

  } # monocen exist listOfdfChromSizeMonocen

                                                #####################################
                                                #   centromere with marks - cen marks
                                                #####################################

  if (exists("listOfdfDataCen") & exists("dfMarkColorInternal") ) {
#    listOfdfDataCenI <<- listOfdfDataCen
#    ycoordCentsI<<-ycoordCents
    # names(xcoordCents)<-names(listOfdfChromSizenoNA) #1:length(xcoordCents)

    xMarkCen<-yMarkCen<-list()

    for (k in 1:length(listOfdfDataCen)){
      yMarkCen1<-NULL
      xMarkCen1<-NULL
      for (i in 1:nrow(listOfdfDataCen[[k]])){
        corr_index<-which(names(xcoordCents) %in% names(listOfdfDataCen)[[k]] )
        rowIndex<-(listOfdfDataCen[[k]][,"neworder"][i])
        ysup<-ycoordCents[[corr_index]][rowIndex,3]
        yinf<-ycoordCents[[corr_index]][rowIndex,1]

        yMarkCen1[[i]]<-c(yinf,yinf,ysup,ysup)
        attr(yMarkCen1[[i]],"rowIndex")<-rowIndex

        xMarkCen1[[i]]<-xcoordCents[[corr_index]][ rowIndex ,]
        attr(xMarkCen1[[i]],"rowIndex")<-rowIndex

      } # each mark
      yMarkCen[[k]]<-yMarkCen1
      xMarkCen[[k]]<-xMarkCen1
      attr(yMarkCen[[k]], "spname") <- names(listOfdfDataCen)[[k]] # added 1.14
      attr(xMarkCen[[k]], "spname") <- names(listOfdfDataCen)[[k]] # added 1.14
    } # each df

    if(!missing(fixCenBorder)){
      if (fixCenBorder){
        fixCenBorder2<-TRUE
        } else {
        fixCenBorder2<-FALSE
        }
    } else {
      fixCenBorder2<-FALSE
    }

                                                    #####################
                                                    #   marks CENTROMERE
                                                    #####################

#    xMarkCenI<<-xMarkCen
#    yMarkCen<<-yMarkCen

#    dfMarkColorInternalI<<-dfMarkColorInternal
#    fixCenBorder2I<<-fixCenBorder2

    if(circularPlot){

      xlistMarkCen <- xHortoVerRoundCen(xMarkCen,diffXRounded)
      names(xlistMarkCen)<-names(xMarkCen)

      yMarkCenNew <- yVertoHorCenMarks(yMarkCen, ylistNewCen)

      circleMapsMarksCen  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,yMarkCenNew,xlistMarkCen,n,0,chrWidth,rotation=rotation)

      drawCenMarks(circleMapsMarksCen,dfMarkColorInternal,listOfdfDataCen,lwd.chr,fixCenBorder2,chrColor)


      if(legend=="inline"){
        circleMapsMarksLabelCen  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,yMarkCenNew,xlistMarkCen,n,
                                                   labelSpacing ,chrWidth,rotation=rotation)

        # circLabelMark(circleMaps,             listOfdfMarkPos, markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY,iscM=FALSE)
        circLabelMark(circleMapsMarksLabelCen,listOfdfDataCen, markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY)
      }

    } else {
    # if(!is.null(cenColor) ){
    lapply(1:length(xMarkCen), function(m) mapply(function(x,y,z)
      graphics::polygon(
        x=x,
        y=y,
        col = dfMarkColorInternal$markColor[match(z, dfMarkColorInternal$markName)] ,
        lwd=lwd.chr,
        border=ifelse(fixCenBorder2,
                    chrColor,
                    dfMarkColorInternal$markBorderColor[match(z, dfMarkColorInternal$markName)] # z outside
                    ) # ifelse
      ), # p
       x=xMarkCen[[m]],
       y=yMarkCen[[m]],
       z=listOfdfDataCen[[m]]$markName # ifelse here gives error
    ) # mapply
    ) #l

    } # circular bool

  } # end centromeres with marks

  ## begin                                            cenStyle

  ##########################################################################################################3
  #
  #                           painting Marks monocen              cenStyle marks
  #
  ############################################################################################################

  if (exists("listOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ){
    # message(crayon::green(paste0("monocen. marks section start" ) ) )

    xMarkCenStyle<-yMarkCenStyle<-listOfdfMarkPosCenStyle<-list()

    j<-1

    for (k in 1:length(listOfdfMarkPosMonocen)) {
      currName<-names(listOfdfMarkPosMonocen)[[k]]
      if(nrow(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="cenStyle"),])>0){
        listOfdfMarkPosCenStyle<-c(listOfdfMarkPosCenStyle,list(listOfdfMarkPosMonocen[[k]][which(listOfdfMarkPosMonocen[[k]]$style=="cenStyle"),]))
        names(listOfdfMarkPosCenStyle)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listOfdfMarkPosCenStyle)>0){
      for (sm in 1:length(listOfdfMarkPosCenStyle)) {
        yMark1<-NULL
        xMark1<-NULL
        # which(names(listOfdfChromSize) %in% names(listOfdfMarkPosCenStyle)[[sm]] )

        corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosCenStyle)[[sm]] )

        for (m in 1:nrow(listOfdfMarkPosCenStyle[[sm]]) ){
          ifelse(listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
          ifelse(listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
          ifelse(listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
          rowIndex<- nrow(listOfdfChromSize[[corr_index]] ) * longORshort + (listOfdfMarkPosCenStyle[[sm]][,"neworder"][m])
          armStart<-ym[[corr_index]][rowIndex,column]
          yprox <- armStart +
            (listOfdfMarkPosCenStyle[[sm]][m,"markDistCen"]                                                             *normalizeToOne*mySign)

          yter <- armStart +
            ( sum( listOfdfMarkPosCenStyle[[sm]][m,"markDistCen"] , listOfdfMarkPosCenStyle[[sm]][m,"markSize"], na.rm=TRUE ) *normalizeToOne*mySign )

          # yMark1[[m]]<-if(longORshort==0) {c(yprox,yter,yter,yprox)} else {c(yter,yprox,yprox,yter)}
          yMark1[[m]]<-if(longORshort==0) {c(yprox,yter,yprox,yter)} else {c(yter,yprox,yter,yprox)}

          attr(yMark1[[m]],"arm")<-listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]
          # attr(yMark1[[m]],"armStart")<-armStart
          attr(yMark1[[m]],"rowIndex")<-rowIndex

          xMark1[[m]]<-xm[[corr_index]][listOfdfMarkPosCenStyle[[sm]][,"neworder"][m],]
          attr(xMark1[[m]],"rowIndex")<-rowIndex

        }
        yMarkCenStyle[[sm]]<-yMark1
        attr(yMarkCenStyle[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]
        xMarkCenStyle[[sm]]<-xMark1
        attr(xMarkCenStyle[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]
      } # end for

      # markList<-addChrNameAttrMark(xMarkCenStyle,yMarkCenStyle,x)
      #
      # xMarkCenStyle<-markList$xMark
      # yMarkCenStyle<-markList$yMark
      ########################
      #                      #
      #   add marks to plot monocen #
      #                      #
      ########################
      # if(circularPlot==FALSE){
      mimicCenPlotMark(roundness, xMarkCenStyle, yMarkCenStyle,
                       dfMarkColorInternal,
                       listOfdfMarkPosCenStyle,
                       chrWidth, #use for calc r2
                       specialChrWidth,
                       yfactor,
                       n,
                       lwd.chr,
                       listOfdfChromSize,
                       circularPlot,
                       y,
                       markLabelSize,

                       separFactor,
                       labelSpacing,circleCenter,circleCenterY,radius,
                       ylistTransChrSimple,rotation=rotation,labelOutwards)

    } #     if(length(listOfdfMarkPosCenStyle)>0)

    else {remove(listOfdfMarkPosCenStyle)}

    # cenStyle labels not cen (monocen.)
    # booleanForCenStyleInlineLabelmimicCenPlotMark<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosCenStyle") & circularPlot==FALSE
    #
    # if(booleanForCenStyleInlineLabelmimicCenPlotMark) {
    #   textLabel(xMarkCenStyle,yMarkCenStyle,listOfdfChromSize,listOfdfMarkPosCenStyle,specialChrSpacing,chrSpacing,markLabelSize,pattern)
    # }

  } # if presence end painting marks

  ##################################################################################################
  #
  #                                                 painting Marks cenStyle holocen
  #
  ##################################################################################################

  if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
    # message(crayon::green(paste0("holocen. marks section start" ) ) )
    xMarkCenStyle<-yMarkCenStyle<-listOfdfMarkPosCenStyle<-list()

    j<-1
    for (k in 1:length(listOfdfMarkPosHolocen)) {
      currName<-names(listOfdfMarkPosHolocen)[[k]]
      if(nrow(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="cenStyle"),])>0){
        listOfdfMarkPosCenStyle<-c(listOfdfMarkPosCenStyle,list(listOfdfMarkPosHolocen[[k]][which(listOfdfMarkPosHolocen[[k]]$style=="cenStyle"),]) )
        names(listOfdfMarkPosCenStyle)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listOfdfMarkPosCenStyle)>0){
      for (sm in 1:length(listOfdfMarkPosCenStyle)) {

        yMark1<-NULL
        xMark1<-NULL
        corr_index<-which(names(ym) %in% names(listOfdfMarkPosCenStyle)[[sm]] )

        for (m in 1:nrow(listOfdfMarkPosCenStyle[[sm]])){
          rowIndex<-(listOfdfMarkPosCenStyle[[sm]][,"neworder"][m])
          chrStart<-ym[[corr_index]][rowIndex ,2]
          yinf <- chrStart +                        # was ysup
            (listOfdfMarkPosCenStyle[[sm]][m,"markPos"]                                                          *normalizeToOne)

          ysup <- chrStart +
            (  sum(listOfdfMarkPosCenStyle[[sm]][m,"markPos"],listOfdfMarkPosCenStyle[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

          yMark1[[m]]<-c(ysup,yinf,ysup,yinf)
          attr(yMark1[[m]],"rowIndex")<-rowIndex

          xMark1[[m]]<-xm[[corr_index]][listOfdfMarkPosCenStyle[[sm]][,"neworder"][m],]
          attr(xMark1[[m]],"rowIndex")<-rowIndex

        }
        yMarkCenStyle[[sm]]<-yMark1
        attr(yMarkCenStyle[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]
        xMarkCenStyle[[sm]]<-xMark1
        attr(xMarkCenStyle[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]
      } # end for

      # markList<-addChrNameAttrMark(xMarkCenStyle,yMarkCenStyle,x)

      # xMarkCenStyle<-markList$xMark
      # yMarkCenStyle<-markList$yMark

      #####################
      #   add marks to plot holocen
      #####################

      # if(roundness<20){
      #   myY<- y #newLongy
      # } else {
      #   myY<-y
      # }


      mimicCenPlotMark(roundness, xMarkCenStyle, yMarkCenStyle,
                       dfMarkColorInternal,
                       listOfdfMarkPosCenStyle,
                       chrWidth, #use for calc r2
                       specialChrWidth,
                       yfactor,
                       n,
                       lwd.chr,
                       listOfdfChromSize,
                       circularPlot,
                       y,
                       markLabelSize,

                       separFactor,
                       labelSpacing,circleCenter,circleCenterY,radius,
                       ylistTransChrSimple,rotation=rotation,labelOutwards) #

    } #     if(length(listOfdfMarkPosCenStyle)>0){

    else {remove(listOfdfMarkPosCenStyle)}

    #
    #   inline legend holocen
    #

    # booleanForCenStyleInlineLabelmimicCenPlotMark<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosCenStyle") & circularPlot==FALSE
    #
    # if(booleanForCenStyleInlineLabelmimicCenPlotMark) {
    #   textLabel(xMarkCenStyle,yMarkCenStyle,listOfdfChromSize,listOfdfMarkPosCenStyle,specialChrSpacing,chrSpacing,markLabelSize,pattern)
    # }

  } #   if (exists("listOfdfMarkPosHolocen") & exists("dfMarkColorInternal") )
  # end cenStyle marks

  ## end cenStyle

  ###########################################
  #
  #   plotting labels inline CENTR.
  #
  ###########################################

  # cen labels
  booleanColorInternalMarkCen<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfDataCen") & circularPlot==FALSE

    if(booleanColorInternalMarkCen)  {

      textLabel(xMarkCen,yMarkCen,listOfdfChromSize,listOfdfDataCen,specialChrSpacing,chrSpacing,markLabelSize,pattern,TRUE)
    } # if



  ##############################################
  #
  #   labels to the right
  #
  ##############################################

  xNoNA <- removeNAFromList(x,areNA)
  yNoNA <- removeNAFromList(y,areNA)

  if(legend=="aside" & exists("dfMarkColorInternal") ){

    dfMarkColorInternalNocM<- dfMarkColorInternal[which(!dfMarkColorInternal$style %in% c("cM","cenStyle","cMLeft") ),]

    if(length(dfMarkColorInternalNocM)==0){
      remove(dfMarkColorInternalNocM)
    }

    if(exists("cenMarkNames") ) {
      dfMarkColorInternalcMnoCen <- dfMarkColorInternal[which(dfMarkColorInternal$style %in% c("cM","cenStyle","cMLeft") & dfMarkColorInternal$markName %in% cenMarkNames ) , ]
      if( exists("dfMarkColorInternalNocM") ) {
        dfMarkColorInternalNocM<-dplyr::bind_rows(dfMarkColorInternalNocM,dfMarkColorInternalcMnoCen)
      } else {
        dfMarkColorInternalNocM<-dfMarkColorInternalcMnoCen
      }
    }

    if(nrow(dfMarkColorInternalNocM)>0 )  {
      if(!exists("allMarkMaxSize") ){
        message(crayon::green(paste0("Seems you didn't provide markSize (NA), if you want cM style, set it in the dfMarkColor data.frame" ) ) )

        allMarkMaxSize<-1
      }


      if(circularPlot){
        maxx<-max(unlist(circleMaps), na.rm=TRUE )
      } else {
        maxx<-(max(unlist(xNoNA)) )
      }


    plotlabelsright(maxx,yNoNA, markLabelSpacer,chrWidth,dfMarkColorInternalNocM,allMarkMaxSize,normalizeToOne,
                              markLabelSize,xfactor,legendWidth, legendHeight, n*4, pattern,legendYcoord)
    } # is df
  } # if aside


  #################################
  #
  # add notes (to the right)
  #
  #################################

if(circularPlot==FALSE){

  if(!missing(notes)){
    if(!inherits(notes, "data.frame") ) {
      message(crayon::blurred("Notes are not in a data.frame, ignoring notes"))
    } else {
      for (i in 1:length(listOfdfChromSizenoNA) ) {
        if(attr(listOfdfChromSizenoNA[[i]],"cenType")=="monocen"){
          note<-notes[which(notes$OTU %in% names(listOfdfChromSizenoNA)[i] ), ]$note
          if(!is.null(note) & length(note)>0 ){
            graphics::text(
              max(xmnoNA[[i]]) + (xlimLeftMod*(notesPos/2) )
              ,min(ymnoNA[[i]][,1])
              ,labels = paste(note ),
              cex=notesTextSize,
              adj=0
              ,font=  ifelse(OTUasNote,ifelse( !missing(OTUfont),    OTUfont,   1),1),
              family=ifelse(OTUasNote,ifelse( !missing(OTUfamily),  OTUfamily, defaultFontFamily2),defaultFontFamily2) # justif
            ) # end graphics::text
          } # null

        } # if monocen
        else if(attr(listOfdfChromSizenoNA[[i]],"cenType")=="holocen"){
          note<-notes[which(notes$OTU %in% names(listOfdfChromSizenoNA)[i] ), ]$note

          if(!is.null(note)  & length(note)>0 ){
            graphics::text(
              max(xmnoNA[[i]]) + ( xlimLeftMod*(notesPos/2) )
              # ) ),
              ,(max(ymnoNA[[i]]) + min(ymnoNA[[i]]) ) /2
              ,labels = paste(note ),
              cex=notesTextSize,
              adj= 0 # 0.5 centered
              ,font=  ifelse(OTUasNote,ifelse( !missing(OTUfont),    OTUfont,   1),1),
              family=ifelse(OTUasNote,ifelse( !missing(OTUfamily),  OTUfamily, defaultFontFamily2),defaultFontFamily2) # justif
            ) # end graphics::text
          } # null
        } # holocen
      } # for
      # message(crayon::green(paste0("notes section end" ) ) )
    } # exists d.f.
  } # fi notes

} # CIRC false
}
