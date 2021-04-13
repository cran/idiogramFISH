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
#'   columns: \code{markName}, \code{markColor}, \code{style}. \code{style} accepts: \code{square}, \code{squareLeft}, \code{dots}, \code{cM},
#'    \code{"cMLeft"}, \code{"cenStyle"}, \code{"upArrow"}, \code{"downArrow"}, \code{"exProtein"}.
#'   (if column \code{style} missing all (except 5S) are plotted as in param. \code{defaultStyleMark}).
#' @param mycolors character vector, optional, i.e. \code{c("blue",} \code{} \code{"red",} \code{} \code{"green")} for specifying color of marks in order of appearance. if diverges with number of marks will be recycled if \code{dfMarkColor} present, mycolors will be ignored. To know the order of your marks use something like: \code{unique(c(dfMarkPos$markName,} \code{dfCenMarks$markName) ) }
#' @param addMissingOTUAfter character, when you want to add space (ghost OTUs) after one or several OTUs, pass the names of OTUs preceding the desired space in a character vector i.e. \code{c("species one",} \code{} \code{"species five")}
#' @param addMissingOTUBefore character, when you want to add space (ghost OTUs) before one or several OTUs, pass the names of OTUs after the desired space in a character vector i.e. \code{c("species one",} \code{} \code{"species five")}
#' @param missOTUspacings numeric, when you use \code{addMissingOTUAfter} this numeric vector should have the same length and corresponds to the number of free spaces (ghost OTUs) to add after each OTU respectively
#' @param moveKarHor character, OTUs' names of karyotypes that should be moved horizontally. See \code{mkhValue}
#' @param mkhValue numeric, value to move kar. hor. See \code{moveKarHor}
#' @param karAnchorLeft character, OTUs' add anchor to the left of this OTU names of karyotypes. For \code{verticalPlot=FALSE}
#' @param karAnchorRight character, OTUs' add anchor to the right of this OTU names of karyotypes. For \code{verticalPlot=FALSE}
#' @param moveAllKarValueHor numeric, similar to \code{mkhValue}, but affects all karyotypes.
#' @param moveAllKarValueY numeric, similar to \code{moveAllKarValueHor}, but affects y axis.
#' @param anchor boolean, when \code{TRUE}, plots a parent progeny structure in karyotypes in \code{moveKarHor}. Or a horizontal anchor to the left/right
#' of \code{karAnchorLeft, karAnchorRight} when \code{verticalPlot=FALSE}
#' @param anchorText character, text to add to \code{anchor} structure near symbol. See \code{anchor}. Defaults to \code{""}
#' @param anchorTextMParental character, designed to fill with a character object the space left of a missing parental in the \code{anchor} structure.
#' @param anchorTextMoveX numeric, for vertical plots with \code{anchorText} move text in X axis. Defaults to \code{0.5}
#' @param anchorTextMoveY numeric, for horizontal plots with \code{anchorText} move text in Y axis. Defaults to \code{1}
#' @param anchorTextMoveParenX numeric, for plots with \code{anchorTextMParental} move text in X axis. Defaults to \code{0}
#' @param anchorTextMoveParenY numeric, for plots with \code{anchorTextMParental} move text in Y axis. Defaults to \code{0}
#'
#' @param anchorLineLty numeric, type of line in \code{anchor}, corresponds to \code{lty}. Defaults to \code{1}
#' @param anchorVsizeF numeric, factor to modify vertical size of anchor \code{0.5} (default). Size itself is equal to \code{karHeiSpace}
#' @param moveAnchorV numeric, displace anchor vertical portion to right or left. See \code{anchor}
#' @param moveAnchorH numeric, displace anchor horizontal portion to right or left. See \code{anchor}
#' @param pchAnchor numeric, symbol for anchor, see \code{?points} and \code{anchor}
#' @param orderChr character, when \code{"size"}, sorts chromosomes by total
#'   length from the largest to the smallest. \code{"original"}: preserves d.f. order. \code{"name"}: sorts alphabetically; \code{"group"}: sorts by group name; \code{"chrNameUp"}: sorts according to column \code{chrNameUp}. See \code{chrNameUp}
#' @param centromereSize numeric, optional, this establishes the apparent size of cen. in the plot in \eqn{\mu}m. Automatic when \code{NA}. Default: \code{NA}
#' @param origin, For non-monocentric chr. (for holocentrics only) Use \code{"b"} (default) if distance to mark in (\code{"markPos"} column in \code{"dfMarkPos"}) data.frame measured from bottom of chromosome, use \code{"t"} for distance to mark from top of chr.
#' @param efZero, numeric, numbers below this one will be considered as zero, for comparison purposes. Defaults to \code{1e-5}
#' @param cMBeginCenter, boolean, start position of \code{cM} and \code{cMLeft} marks. If \code{TRUE}, starts in the center (width) of chr. . Defaults to \code{FALSE}
#' @param arrowhead numeric, proportion of head of arrow (mark styles: \code{upArrow,} \code{} \code{downArrow}). Defaults to \code{0.3}
#' @param shrinkArrow numeric, proportion, shrinks body of arrow. Defaults to \code{0.3333}
#' @param arrowheadWidthShrink numeric, proportion, shrinks head of arrow. Defaults to \code{0.1}
#' @param arrowsToSide boolean, when \code{FALSE} use a centered arrow, instead of an arrow next to chr. margins (\code{TRUE}, default). See \code{arrowsBothChrt}
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
#' @param fixCenBorder boolean, when \code{TRUE} uses \code{chrColor} as centromere (and cen. mark) border color. See also \code{cenColor},
#' \code{chrColor}, \code{colorBorderMark}, \code{borderOfWhiteMarks}. No default value. When \code{chrColor} is \code{"white"} this turns into \code{"black"}.
#' @param gishCenBorder boolean, when \code{TRUE}, cen. mark border color is the same as mark color, ignoring \code{colorBorderMark}. No default.
#' @param hideCenLines numeric, factor to multiply line width (lwd) used for covering cen. border, when \code{chrColor} is \code{white} or when \code{gishCenBorder=TRUE}
#' @param roundedCen deprecated, see cenFormat
#' @param cenFormat boolean, when \code{"triangle"}, cen. has triangular aspect. When \code{"rounded"}, it has rounded aspect (Default). \code{"inProtein"} for using the mark with style of same name.
#' @param cenFactor numeric, modifies any cen. mark and cen. size. Defaults to \code{1}
#' @param squareness numeric, shape of vertices of chromosomes and square marks,
#'   higher values more squared. Defaults to \code{4}
#' @param karHeight numeric, vertical size of karyotypes. See also  \code{karHeiSpace}. Defaults to \code{2}
#' @param karHeiSpace numeric, vertical size of karyotypes including spacing. Use with \code{karSepar=FALSE}. Proportional to \code{karHeight}, if overlap, increase. Defautl value \code{2.5}
#' @param karSepar boolean, reduce distance among karyotypes \code{FALSE} = equally
#'   sized karyotypes or \code{TRUE} = equally spaced karyotypes. Incompatible with \code{addMissingOTUAfter}
#' @param amoSepar numeric, depends on \code{karSepar=TRUE}, if zero your
#'   karyotypes will have no distance among them, if overlap,
#'   increase this and \code{karHeiSpace}
#' @param chrId character, print name of chromosome, \code{"original"} uses the original
#'   name in OTU column of dfChrSize, \code{"simple"} (just 1 to ...) or \code{""} (none).
#' @param chrIdPatternRem character, regex pattern to remove from chr. names
#' @param distTextChr numeric, distance from name of chromosome to chromosome,
#'   also affects vertical separation of indices. Defaults to \code{1}
#' @param groupUp boolean, when \code{TRUE} when groups present, they appear over the chr. name. Defaults to \code{FALSE}
#' @param groupName boolean, when \code{TRUE} (default), shows group names. When \code{FALSE} only line
#' @param groupSepar numeric, factor for affecting chr. spacing \code{chrSpacing} among groups. Defaults to \code{0.5}
#' @param chromatids boolean, when \code{TRUE} shows separated chromatids. Defaults to \code{TRUE}
#' @param arrowsBothChrt boolean, when \code{TRUE} (default) (for \code{chromatids=TRUE}) shows \code{upArrow,} \code{} \code{ downArrow} styles of marks in both chromatids when \code{arrowsToSide=TRUE}.
#' @param holocenNotAsChromatids boolean, when \code{TRUE} and \code{chromatids=TRUE} does not plot holocen kar. with chromatids. Defaults to \code{FALSE}. A value of \code{TRUE} modifies excHoloFrArrToSide to \code{TRUE} always.
#' @param excHoloFrArrToSide boolean, when \code{arrowsToSide=TRUE}, excludes holocen. from this behaviour, plotting a centered arrow only.
#' @param xModifier numeric, for \code{chromatids=TRUE}, separation among chromatids. Quotient for \code{chrWidth}. Defaults to \code{12 : chrWidth/12}
#' @param xModMonoHoloRate numeric, factor to shrink chromatid separ. for holocen. 5 means 5 times smaller (quotient).
#' @param indexIdTextSize numeric, font size of chr. and kar. indices and
#'   chromosome name. Defaults to \code{1}
#' @param OTUTextSize numeric, font size of OTU name (species). Defaults to \code{1}. When \code{OTUasNote} is \code{TRUE}, use  \code{notesTextSize} instead
#' @param legend character, \code{""} for no legend; \code{"inline"} prints labels near
#'   chromosomes; \code{"aside"} prints legend to the right of karyotypes (default). See \code{markLabelSpacer}
#' @param remSimiMarkLeg boolean, when \code{legend="aside"}, if you use \code{pattern}, you can have several marks with same name. When \code{TRUE} this remove this pseudoduplicates from legend. Be sure that this pseudoduplicates have the same color, otherwise you should use \code{TRUE} (default).
#' @param bannedMarkName character, character string or vector with mark names to be removed from plot. Not the marks but the labels. Except when \code{bMarkNameAside} is used.
#' @param bMarkNameAside boolean, when \code{TRUE} and \code{legend="inline"}, shows marks in \code{bannedMarkName} as \code{legend="aside"}.
#' @param forbiddenMark, character, character string or vector with mark names to be removed from plot. Not the marks but the labels.
#' @param legendWidth numeric, factor to increase width of squares and of legend. Defaults to \code{1.7}
#' @param legendHeight numeric, factor to increase height of squares and dots of legend. Automatic.
#' @param defaultStyleMark character, default style of mark, only used when \code{style} column of \code{dfMarkColor} data.frame is missing or in absence of this data.frame. Use \code{"square"} (default), \code{"squareLeft"}, \code{"dots"}, \code{"cM"}, \code{"cMLeft"},\code{"cenStyle"}, \code{"upArrow"}, \code{"downArrow"}.
#' @param colorBorderMark character, without default, pass a name of a color to use as border of marks. See \code{borderOfWhiteMarks}
#' @param borderOfWhiteMarks boolean, if \code{TRUE} (Default) uses black border for white marks. See \code{dfMarkColor}. Does not apply to marks with style \code{cenStyle}
#' @param lwd.mimicCen thickness of lines of \code{cenStyle} marks; affects only lateral borders. Defaults to \code{lwd.chr}
#' @param startPos numeric, factor to increase separation of \code{exProtein} marks to chromosome. Defaults to \code{0}
#' @param pMarkFac numeric, fraction of chr. size for \code{exProtein} style marks. Defaults to \code{0.25}
#' @param defCenStyleCol character, color of outer part of \code{cenStyle} marks. Defaults to \code{white}
#' @param protruding numeric, when style of mark is \code{"cM"}, fraction of chrWidth to stretch marker. Defaults to \code{0.2}. Introduced in 1.13
#' @param markLabelSize numeric, only if legend != (not) "", size of the font of
#'   labels of marks (legend). Defaults to \code{1}
#' @param markLabelSpacer numeric, only if \code{legend="aside"}, space from the
#'   rightmost chr. to legend. Defaults to \code{1}
#' @param legendYcoord numeric, modify Y position of legend when \code{legend="aside"}
#' @param markNewLine, character, character to split mark Names into different lines. Applies to \code{square} marks. Defaults to \code{NA}
#' @param mylheight, numeric, for \code{markNewLine!=NA}; is equivalent to \code{lheight} of \code{par}: "The line height multiplier. The height of a line of text (used to vertically space multi-line text) is found by multiplying the character height both by the current character expansion and by the line height multiplier." Defaults to \code{0.7}.
#' @param pattern REGEX pattern to remove from names of marks
#' @param chrIndex character, add arm ratio with \code{"AR"} and centromeric index with \code{"CI"}, or \code{"both"} (Default), or \code{""} for none
#' @param chrSize boolean, when \code{TRUE} adds total chr size under each chr. Defaults to \code{FALSE}
#' @param chrNameUp boolean, when \code{TRUE} adds secondary chromosome name from col. \code{chrNameUp} over chrs. Defaults to \code{FALSE}
#' @param classMbName character, name of "chromosome" when in Mbp. Defaults to \code{"Pm"}. See \code{MbUnit}
#' @param classcMName character, name of "chromosome" when OTU in \code{specialOTUNames}. Defaults to \code{"L.G."}
#' @param classChrName character, name of "chromosome" when in micrometers (apparently). Defaults to \code{"Chr."}. See \code{specialOTUnames}, \code{classMbName}, \code{classcMName}
#' @param classChrNameUp character, name of "chromosome" for col. \code{"chrNameUp"}. Defaults to \code{"Type"}
#' @param classGroupName character, name of groups. Defaults to \code{""}
#' @param nsmall numeric, rounding decimals for \code{chrSize} parameter. Defaults to \code{1}
#' @param chrSizeMbp boolean, when \code{TRUE} adds total Mbp chr. size to each chr. provided, there is a \code{Mbp} column in \code{dfChrSize} data.frame. Defaults to \code{FALSE}. If data in columns \code{shortArmSize}, or col. \code{chrSize} is in millions ("Mbp"). Use \code{chrSize=TRUE} not this one (not column \code{Mbp}, you don't need this).
#' @param markPer character, name of mark to calculate % of mark in chr. and add it to plot. See \code{perAsFraction}
#' @param showMarkPos boolean, adds position of marks under karyotype (fraction 0-1) when \code{TRUE}. Defaults to \code{FALSE}
#' @param bToRemove, character, bands to remove from calc. of pos.
#' @param perAsFraction boolean, when \code{TRUE} % is shown as fraction. Defaults to \code{FALSE}. See \code{markPer}
#' @param nameChrIndexPos numeric, modify position of name of chr. indices
#' @param karIndex logical, add karyotype indices A (intrachromosomal -
#'   centromere pos.) and A2 (interchromosomal asymmetry, variation among
#'   chromosome sizes)
#' @param karIndexPos numeric, move karyotype index
#' @param notesLeft deprecated, use a data.frame for \code{leftNotes}
#' @param notesPosX numeric, move right notes to the right or left (x axis)
#' @param notesPosY numeric, move right notes down or up (y axis)
#' @param leftNotesPosX numeric, move left notes to the right or left (x axis)
#' @param leftNotesPosY numeric, move left notes (\code{leftNotes}) down or up (y axis)
#' @param leftNotesUpPosY numeric, move up left notes (\code{leftNotesUp}) down or up (y axis)
#' @param morpho character, when \code{"both"} (default) prints the Guerra and Levan classif of cen. position, use also \code{"Guerra"} or  \code{"Levan"} or \code{""} for none. See also \code{?armRatioCI}.
#' @param addOTUName boolean, when \code{TRUE} adds OTU (species) name to karyotype
#' @param OTUfont numeric, \code{1} for normal,  \code{2} for bold,  \code{3} for italics,  \code{4} for bold-italics
#' @param OTUfamily character, font family for OTU name.
#' @param OTUasNote boolean, when \code{TRUE} adds OTU (species) name to the right, see \code{notes}
#' @param OTUasLeftNote boolean, when \code{TRUE} adds OTU (species) name to the left-up, see \code{leftNotesUp}
#' @param revOTUs boolean, The order of species is the one in the main
#'   data.frame, use \code{TRUE} to reverse
#' @param ruler boolean, display ruler to the left of karyotype, when \code{FALSE} no ruler
#' @param useMinorTicks boolean, display minor ticks between labeled ticks in ruler. See \code{miniTickFactor}. Defaults to \code{FALSE}. (ticks without label)
#' @param miniTickFactor numeric, number of minor ticks for each labeled tick. See \code{useMinorTicks}. Defaults to \code{10}
#' @param rulerPos numeric, absolute position of ruler, corresponds to \code{pos}
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
#' @param lwd.marks thickness of most marks. Except \code{cM} marks and centr. related marks. See \code{lwd.chr}, \code{lwd.cM}. Defaults to \code{lwd.chr}
#' @param lwd.chr thickness of border of chr., some marks and ruler. Thick of \code{cM} marks when \code{lwd.cM} absent and other marks when \code{lwd.marks} absent. Defaults to \code{0.5}
#' @param MbThreshold, numeric, if greater than this number (defaults to \code{10000}), \code{MbUnit} will apply and \code{specialyTitle} will not.
#' @param threshold, this is the max. value allowed for the main two significative digits, otherwise scale will shrink. For example, after 35 \eqn{\mu m} (Default), apparent size will be 3.5 and scale interval will change. See also \code{ceilingFactor}, you may have to use \code{-1}. Introduced in 1.13
#' @param MbUnit, character, text of units of title when \code{MbThreshold} met and OTU not in \code{specialOTUNames}. See \code{specialyTitle}
#' Defaults to \code{"Mb"}, but anything can be used. Introduced in 1.13. See \code{specialyTitle}
#' @param xPosRulerTitle, numeric, modify position of ruler title. See \code{yTitle,} \code{} \code{ specialyTitle,} \code{} \code{ MbUnit}. Defaults to \code{2.6}. A value of \code{2.6} means \code{2.6} times the value of \code{chrSpacing} to the left, from the first chr.
#' @param yPosRulerTitle, numeric, affects vertical position of ruler title. Defaults to \code{0}
#' @param rulerTitleSize, numeric font size of units of ruler. See also \code{xPosRulerTitle}
#' @param n, numeric vertices number for round corners
#' @param markN, numeric vertices number for round corners of marks
#' @param notes, data.frame, (to the right), with columns \code{OTU} and \code{note} for adding notes to each OTU, they appear to the right of chromosomes
#' @param leftNotes, data.frame, (to the left), with columns \code{OTU} and \code{note} for adding notes to each OTU, they appear to the left of chromosomes
#' @param leftNotesUp, data.frame, (to the left), similar to \code{leftNotes}, but intended for placement over chr.
#' @param leftNoteFont, numeric  \code{1} for normal,  \code{2} for bold,  \code{3} for italics,  \code{4} for bold-italics. See \code{leftNotes}
#' @param leftNoteFontUp, numeric  \code{1} for normal,  \code{2} for bold,  \code{3} for italics,  \code{4} for bold-italics. See \code{leftNotesUp}
#' @param noteFont, numeric  \code{1} for normal,  \code{2} for bold,  \code{3} for italics,  \code{4} for bold-italics. See \code{notes}
#' @param parseTypes, boolean, parse in \code{notes} the \emph{Citrus} chr. types names. Creates subindex pos. for FL. Defaults to \code{TRUE}. Incompatible with \code{parseStr2lang}
#' @param parseStr2lang, bolean, parse string in \code{notes} with function \code{str2lang(paste0("paste(",note,")") )} for ex: \code{"italic('C. sinensis'), ' Author'"}. See \code{notes}, \code{leftNotes},\code{leftNotesUp}.
#' @param notesTextSize numeric, font size of notes, see \code{notes}
#' @param propWidth, boolean, defaults to \code{FALSE}. Diminishes chr. width with increasing number of OTUs
#' @param asp, numeric, y x aspect of plot. Defaults to \code{1}
#' @param defaultFontFamily character. use this as the font family. No default value.
#' @param verticalPlot boolean, when \code{TRUE} karyotypes are plotted vertically, otherwise, horizontally Defaults to \code{TRUE}
#' @param karSpaceHor numeric, separation among horizontal karyotypes. When \code{verticalPlot=FALSE}. Defaults to \code{0}
#' @param circularPlot boolean, if \code{TRUE} chromosomes/karyotypes are plotted in concentric circles. Defaults to \code{FALSE}
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
#' @param useOneDot boolean, use one dot instead of two in style of marks \code{dots}. Defaults to \code{FALSE}. Not useful for \code{chromatids=TRUE}
#' @param dotsAsOval boolean, use oval instead of two dots in style of marks \code{dots}. Defaults to \code{FALSE}. See \code{useOneDot}. Not useful for \code{chromatids=TRUE} or \code{circularPlot=TRUE}
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
#' @param roundness deprecated, use \code{squareness}
#' @param ... accepts other arguments for the plot, see, \code{?plot}
#'
#' @keywords data.frame chromosome
#'
#' @importFrom graphics par plot segments mtext
#' @importFrom dplyr bind_rows mutate across everything
#' @importFrom plyr rbind.fill
#' @importFrom grDevices col2rgb
#' @importFrom utils read.csv
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
  karSepar=TRUE,
  amoSepar=10,
  addMissingOTUAfter=NA,
  addMissingOTUBefore=NA,
  missOTUspacings=0,
  moveKarHor,
  moveAllKarValueHor,
  moveAllKarValueY,
  karAnchorLeft,
  karAnchorRight,
  anchor,
  anchorLineLty=1,
  anchorText="",
  anchorTextMParental,
  anchorTextMoveX=0.5,
  anchorTextMoveY=1,

  anchorTextMoveParenX=0,
  anchorTextMoveParenY=0,

  anchorVsizeF=.5,
  pchAnchor=23,
  moveAnchorV=0,
  moveAnchorH=0,
  mkhValue=.5,
  n=50,
  markN=25,
  notes,
  leftNotes,
  leftNotesUp,
  notesTextSize=.4,
  notesLeft,
  notesPosX=.5,
  notesPosY=0,
  leftNotesPosX=.5,
  leftNotesPosY=0,
  leftNotesUpPosY=0,
  leftNoteFont=1,
  leftNoteFontUp=1,
  noteFont=1,
  parseTypes=TRUE,
  parseStr2lang=FALSE,

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
  OTUasLeftNote=FALSE,

  # chromosomes
  orderChr="size",
  chrId="original",
  classMbName =  "Pm.",
  classcMName =  "L.G.",
  classChrName=  "Chr.",
  classChrNameUp = "Type",
  classGroupName = "",
  chrNameUp=FALSE,
  chrIdPatternRem,
  # chrId="simple",
  indexIdTextSize=1,
  distTextChr=1,
  groupUp=FALSE,
  groupName=TRUE,
  groupSepar=0.5,
  # chromatids=FALSE,
  chromatids=TRUE,
  arrowsBothChrt=TRUE,

  holocenNotAsChromatids=FALSE,
  excHoloFrArrToSide=FALSE,

  xModifier=12,
  xModMonoHoloRate,
  chrWidth=0.5,
  chrSpacing=0.5,
  specialChrWidth=0.3, specialChrSpacing=0.7,

  chrColor="gray",
  chrBorderColor,
  centromereSize=NA,
  cenColor,
  fixCenBorder=NULL,
  gishCenBorder,
  hideCenLines=1.75,
  roundedCen,
  cenFormat="rounded",
  # cenFormat="inProtein",
  cenFactor=1,

  squareness=4,
  lwd.chr=0.5,
  lwd.cM,
  lwd.marks,

  #marks
  dfMarkPos, dfCenMarks,
  defaultStyleMark="square",
  markDistType="beg",
  protruding=0.2,
  startPos = 0,
  pMarkFac = 0.25,
  # markDistType="cen",
  origin="b",
  efZero=1e-5,
  # origin="t",
  cMBeginCenter=FALSE,
  arrowhead = .3,
  shrinkArrow = .3333,
  arrowheadWidthShrink = .1,
  arrowsToSide=TRUE,
  useOneDot=FALSE,
  dotsAsOval=FALSE,

  dfMarkColor,
  mycolors,
  borderOfWhiteMarks=TRUE,
  colorBorderMark,
  lwd.mimicCen,
  defCenStyleCol,
  pattern="",

  # mark labels
  legend="aside",
  remSimiMarkLeg=TRUE,
  bannedMarkName,
  bMarkNameAside=FALSE,
  forbiddenMark,
  # legend="inline",
  legendWidth=1.7,
  legendHeight=NA,
  markLabelSize=1,
  markLabelSpacer=1,
  legendYcoord=0,
  markNewLine=NA,
  mylheight=.7,

  #indices
  chrSize=FALSE,
  nsmall=1,
  chrSizeMbp=FALSE,
  markPer="",
  showMarkPos=FALSE,
  bToRemove="",
  perAsFraction=FALSE,
  chrIndex="both",
  morpho="both",
  nameChrIndexPos=2,
  karIndex=TRUE,
  # karIndex=FALSE,
  karIndexPos=.5,

  # rulers
  ruler=TRUE,
  useMinorTicks=FALSE,
  miniTickFactor=10,
  rulerPos=0,
  ruler.tck=-0.02,
  rulerNumberPos=0.5,
  rulerNumberSize=1,
  rulerInterval,
  rulerIntervalcM,
  rulerIntervalMb,
  ceilingFactor=0,
  xPosRulerTitle=2.6,
  yPosRulerTitle=0,
  rulerTitleSize=1,

  # margins
  xlimLeftMod=1, xlimRightMod=2,
  ylimBotMod=.2, ylimTopMod=.2,
  callPlot=TRUE,
  asp=1,

  # c plot
  circularPlot=FALSE,
  verticalPlot=TRUE,
  karSpaceHor=0,

  shrinkFactor=.9,
  separFactor=1.5,
  labelSpacing=.7,
  labelOutwards=FALSE,
  chrLabelSpacing=.5,
  # radius=1,
  radius=.5,

  rotation=0.5,

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
  roundness,
  ...) {

  xfactor <- yfactor <- 1

  chrWFactor<-specialChrWidth/chrWidth

  if(!missing(defaultFontFamily)){
    defaultFontFamily2<-defaultFontFamily
  } else {
    defaultFontFamily2<-"sans"
  }

  if(circularPlot){
    n<-n*2
  }

  if(dotsAsOval & circularPlot) {
    dotsAsOval<-FALSE
    message(crayon::red("\nIncompatible setting dotsAsOval=TRUE and circularPlot=TRUE, try useOneDot=TRUE"))
  }

  if(dotsAsOval & circularPlot==FALSE) {
    useOneDot <- TRUE
  }

  pattern<-paste0("inProtein|",pattern)

  xModifier <- chrWidth/xModifier

  if(missing(xModMonoHoloRate)){
    xModifierMono<-xModifier
    xModifierHolo<-xModifier
  } else {
    xModifierMono<-xModifier
    xModifierHolo<-xModifier/as.numeric(xModMonoHoloRate)
  }

  if(missing(defCenStyleCol) ) {
    defCenStyleCol <- "white"
  }

  if(!missing(roundness)){
    crayon::red("roundness is deprecated and was substituted with new param. 'squareness'")
  }

  if(!missing(roundedCen)){
    crayon::red("roundedCen is deprecated and was substituted with new param. 'cenFormat'")
  }

  if(!missing(notesLeft)){
    crayon::red("notesLeft is deprecated and was substituted with new param. 'leftNotes', use a data.frame")
  }

  if(!missing(lwd.cM)){
    lwd.cM2 <- lwd.cM
  } else {
    lwd.cM2 <- lwd.chr
  }
  if(!missing(lwd.marks)){
    lwd.marks2 <- lwd.marks
  } else {
    lwd.marks2 <- lwd.chr
  }
  if(!missing(lwd.mimicCen)){
    lwd.mimicCen2 <- lwd.mimicCen
  } else {
    lwd.mimicCen2 <- lwd.chr*4
  }

  OTUfont2 <- ifelse( !missing(OTUfont),   OTUfont,   1)
  OTUfamily2<-ifelse( !missing(OTUfamily), OTUfamily, defaultFontFamily2)

  if(!missing(dfChrSize)) {
    if(inherits(dfChrSize, "data.frame") ) {
    dfChrSizeInternal<-makeNumCols(dfChrSize)
    } else if (inherits(dfChrSize, "character") ) {
      if (file.exists(dfChrSize) ) {
        dfChrSize <-read.csv(dfChrSize, header = TRUE)
        dfChrSizeInternal<-makeNumCols(dfChrSize)
      }
    } else {
      message(crayon::red("dfChrSize is not a data.frame or .csv file") )
    }
  } else {
    message(crayon::red("Missing mandatory dfChrSize data.frame or .csv file"))
    return(NA)
  }

  if(!missing(dfMarkPos) ) {

    if (inherits(dfMarkPos, "character")) {
      if (file.exists(dfMarkPos)) {
        tryCatch(dfMarkPosInternal<-read.csv(dfMarkPos, header = TRUE), error = function(e){"invalid dfMarkPos file"} )
      }
    } else if ( !inherits(dfMarkPos, "data.frame") ) {
      message(crayon::red("dfMarkPos is not a data.frame object or .csv filename in quotes") )
    }
    if(inherits(dfMarkPos, "data.frame") ) {
      if(!nrow(dfMarkPos)>0){
        remove(dfMarkPos)
      } else {
       dfMarkPosInternal <- dfMarkPos
      }
    } else {
      remove(dfMarkPos)
    }

    if(exists("dfMarkPosInternal") ){
      if(!inherits(dfMarkPosInternal, "data.frame") ) {
        remove(dfMarkPosInternal)
      }
    }

  }

  if(exists("dfMarkPosInternal") ) { # is a d.f and has >0 rows

    #
    #   rename column markArm if necessary
    #

    if("markArm" %in% colnames(dfMarkPosInternal)  ) {
      message(crayon::red(paste(c("Column markArm in d.f. of marks renamed to chrRegion")))
      ) # mess
      colnames(dfMarkPosInternal)[which(names(dfMarkPosInternal)=="markArm")]<-"chrRegion"
    }

    dfMarkPosInternal[dfMarkPosInternal==""] <- NA



    copyDfMarkPosInternal1 <- dfMarkPosInternal <- makeNumCols(dfMarkPosInternal)

    tryCatch(initialMarkNames <- unique(as.character(copyDfMarkPosInternal1$markName) ), error=function(e) {} )

    if(is.null(copyDfMarkPosInternal1$markPos)){
      copyDfMarkPosInternal1$markPos<-NA
    }
    if(is.null(copyDfMarkPosInternal1$markSize)){
      copyDfMarkPosInternal1$markSize<-NA
    }
    if(is.null(copyDfMarkPosInternal1$markDistCen)){
      copyDfMarkPosInternal1$markDistCen<-NA
    }

    #
    # requires chrRegion

    if("chrRegion" %in% colnames(copyDfMarkPosInternal1) ) {

    dfCenMarksInternal <- copyDfMarkPosInternal1[which(copyDfMarkPosInternal1$chrRegion=="cen"),]

    if(nrow(dfCenMarksInternal)==0 ){
      remove(dfCenMarksInternal)
    }

    dfpGISHInternal <- copyDfMarkPosInternal1[which(copyDfMarkPosInternal1$chrRegion %in% "p" &
                                                  is.na(copyDfMarkPosInternal1$markSize) &
                                                  is.na(copyDfMarkPosInternal1$markDistCen)
                                                ),]
    if(nrow(dfpGISHInternal)==0 ){
      remove(dfpGISHInternal)
    }

    dfqGISHInternal <- copyDfMarkPosInternal1[which(copyDfMarkPosInternal1$chrRegion %in% "q" &
                                                  is.na(copyDfMarkPosInternal1$markSize) &
                                                  is.na(copyDfMarkPosInternal1$markDistCen)
    ),]
    if(nrow(dfqGISHInternal)==0 ){
      remove(dfqGISHInternal)
    }

    dfwholeGISHInternal <- copyDfMarkPosInternal1[which(copyDfMarkPosInternal1$chrRegion %in% "w" &
                                                  is.na(copyDfMarkPosInternal1$markSize) &
                                                  (is.na(copyDfMarkPosInternal1$markDistCen) |
                                                   is.na(copyDfMarkPosInternal1$markPos) )
    ),]

    if(nrow(dfwholeGISHInternal)==0 ){
      remove(dfwholeGISHInternal)
    }
    } else {
      remove(copyDfMarkPosInternal1) # absence of chrRegion
    }

} # df mark pos

  ##############################################################################
  #
  #   adds name of otu when missing
  #
  ##############################################################################

  if (exists("dfMarkPosInternal")) {

    listOfdfMarkPosInternal<-dfToListColumn(dfMarkPosInternal)

    # dfMarkPosInternal <- dplyr::bind_rows(listOfdfMarkPosInternal, .id = "OTU")

    dfMarkPosInternal <- suppressWarnings(bind_rows( (lapply(
      listOfdfMarkPosInternal, function(x) { mutate(x, across(.cols=everything(), as.character) ) } ) )
      ,.id = "OTU") )

    dfMarkPosInternal <- makeNumCols(dfMarkPosInternal)

  } # df of marks

  if(!missing(dfCenMarks)  ) {
    dfCenMarksInternal2<-makeNumCols(dfCenMarks)

    parlistOfdfMarkPosDataCen2 <- dfToListColumn(dfCenMarksInternal2)

    # dfCenMarksInternal2 <- dplyr::bind_rows(parlistOfdfMarkPosDataCen2, .id = "OTU")

    dfCenMarksInternal2 <- suppressWarnings(bind_rows( (lapply(
      parlistOfdfMarkPosDataCen2, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
      ,.id = "OTU") )

    dfCenMarksInternal2 <- makeNumCols(dfCenMarksInternal2)

    remove(parlistOfdfMarkPosDataCen2)
  }

  if (exists("dfCenMarksInternal")) {

    parlistOfdfMarkPosDataCen <- dfToListColumn(dfCenMarksInternal)

    # dfCenMarksInternal <- dplyr::bind_rows(parlistOfdfMarkPosDataCen, .id = "OTU")

    dfCenMarksInternal <- suppressWarnings(bind_rows( (lapply(
      parlistOfdfMarkPosDataCen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
    ,.id = "OTU") )

    dfCenMarksInternal <- makeNumCols(dfCenMarksInternal)

    remove(parlistOfdfMarkPosDataCen)

  } # df of marks

  cendfs <- mget(ls(pattern = "^dfCenMarksInternal" ) )

  if(length(cendfs) ) {

    # dfCenMarksInternal <- suppressWarnings(dplyr::bind_rows(cendfs) )

    dfCenMarksInternal <- suppressWarnings(bind_rows( (lapply(
      cendfs, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
      ) )

    dfCenMarksInternal <- makeNumCols(dfCenMarksInternal)

  }

  #
  #   mark style
  #

  if(!missing(dfMarkColor)) {

    if(inherits(dfMarkColor, "data.frame") ) {
      tryCatch(dfMarkColorInternal<-makeNumCols(dfMarkColor), error=function(e){"empty data.frame"} )
    } else if (inherits(dfMarkColor, "character")) {
      if (file.exists(dfMarkColor)) {
        tryCatch(dfMarkColor<-read.csv(dfMarkColor, header = TRUE), error = function(e){"invalid dfMarkColor file"} )
        tryCatch(dfMarkColorInternal<-makeNumCols(dfMarkColor), error = function(e){""} )
      }
    } else {
      message(crayon::red("dfMarkColor is not a data.frame or .csv file") )
    }
    if(exists("dfMarkColorInternal") ){
      if(!inherits(dfMarkColorInternal, "data.frame") ) {
        remove(dfMarkColorInternal)
      }
    }
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
    OTUasLeftNote <- FALSE
  }
  listOfdfChromSize <- dfToListColumn(dfChrSizeInternal) # adds OTU as name of list

  # dfChrSizeInternal <- dplyr::bind_rows(listOfdfChromSize, .id = "OTU") # names of list to column
  dfChrSizeInternal <- suppressWarnings(bind_rows( (lapply(
    listOfdfChromSize, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
    , .id = "OTU") )

  dfChrSizeInternal <- makeNumCols(dfChrSizeInternal)
  #
  # col OTU
  #

  #
  #   reconstitute dfChrSizeInternal OTU
  #

  #
  #   Classify data.frames from list as monocen or holocen Add attribute cenType
  #

  #############################################################

  #
  #   careful this operation uses divisor2, changing size, adds centromere attr
  #

listOfdfChromSize <- addAttributesDfChrSize(listOfdfChromSize,threshold
                                            ,specialOTUNames,centromereSize
                                            ,MbThreshold,cenFactor,
                                            chrWidth,specialChrWidth,squareness)
  # dfChrSizeInternalDivisor <- dplyr::bind_rows(listOfdfChromSize, .id = "OTU")

  dfChrSizeInternalDivisor <- suppressWarnings(bind_rows( (lapply(
    listOfdfChromSize, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
    , .id = "OTU") )

  dfChrSizeInternalDivisor <- makeNumCols(dfChrSizeInternalDivisor)

  # important must be after bind_rows
listOfdfChromSize <- lapply(listOfdfChromSize, function(x) makeNumCols(x))

#
#   create inProtein marks simulating cen. divisor applied
#

if(cenFormat=="inProtein") {

parlistOfdfMarkPosMonocenCen <- list()

for (i in 1:length(listOfdfChromSize) ){
  if(attr(listOfdfChromSize[[i]], "cenType")=="monocen"){ # only for monocen
    cenSize<-as.numeric(attr(listOfdfChromSize[[i]],"centromere") ) #* inProteinCenFactor
    if(cenSize>0){
    otu<- names(listOfdfChromSize[i])

    parlistOfdfMarkPosMonocenCen[[i]] <- data.frame(
      chrName=as.character(listOfdfChromSize[[i]]$chrName)
       ,markName="inProteinCentromere"
       ,markSize= cenSize
       ,markDistCen= - (cenSize/2)
       ,chrRegion="p"
    )

    names(parlistOfdfMarkPosMonocenCen)[i]<-otu
    }
  }
}

parlistOfdfMarkPosMonocenCen <- Filter(function(x) {!is.null(x) }, parlistOfdfMarkPosMonocenCen)
parlistOfdfMarkPosMonocenCen <- Filter(function(x) {nrow(x) >= 1}, parlistOfdfMarkPosMonocenCen)

if(length(parlistOfdfMarkPosMonocenCen)==0){
  remove(parlistOfdfMarkPosMonocenCen)
} else {
  #
  #   inproteincentromere marks (pseudocentromeres must go before mark, but are merge with rev)
  #
  # dfMarkPosInternal4 <- dplyr::bind_rows(parlistOfdfMarkPosMonocenCen, .id = "OTU") # names of list to column

  dfMarkPosInternal4 <- suppressWarnings(bind_rows( (lapply(
    parlistOfdfMarkPosMonocenCen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
    , .id = "OTU") )

  dfMarkPosInternal4 <- makeNumCols(dfMarkPosInternal4)

  remove(parlistOfdfMarkPosMonocenCen)
  if(!inherits(dfMarkPosInternal4, "data.frame" ) ) {
    remove(dfMarkPosInternal4)
  }
}

} # inProtein

  #
  #     calculate armRatioCI only for chr. with centromere attr cenType =  holocen.
  #

  #
  #    generate Chromosome indexes for Monocen
  #

  if(chrIndex=="both" | chrIndex=="AR"| chrIndex=="CI" | morpho=="both" | morpho=="Guerra" | morpho == "Levan" | chrSize == TRUE | markPer!="" | showMarkPos) {
    for (i in 1:length(listOfdfChromSize)) {
      if(attr(listOfdfChromSize[[i]], "cenType")=="monocen"){ # only for monocen

        listOfdfChromSize[[i]] <- armRatioCI(listOfdfChromSize[[i]])

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

      # dfpGISHInternalMonocen <- dplyr::bind_rows(listOfdfpGISHInternalMonocen, .id = "OTU")

      dfpGISHInternalMonocen <- suppressWarnings(bind_rows( (lapply(
        listOfdfpGISHInternalMonocen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfpGISHInternalMonocen <- makeNumCols(dfpGISHInternalMonocen)

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

      listOfdfqGISHInternalMonocen <- Filter(function(x) {nrow(x) >= 1}, listOfdfqGISHInternalMonocen)

      # dfqGISHInternalMonocen <- dplyr::bind_rows(listOfdfqGISHInternalMonocen, .id = "OTU")

      dfqGISHInternalMonocen <- suppressWarnings(bind_rows( (lapply(
        listOfdfqGISHInternalMonocen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfqGISHInternalMonocen <- makeNumCols(dfqGISHInternalMonocen)

    } # else

    # q marks of Holocen MUST NOt exist

    checkArmHolocenError(listOfdfqGISHInternal,holocenNames)

  } #   if (exists("dfpGISHInternal")){

  ##########################################3
  #
  #     gish whole
  #
  #########################################

  if(exists("dfwholeGISHInternal")) {

    listOfdfwholeGISHInternal<-dfToListColumn(dfwholeGISHInternal)

    ###########################################################################################################################3
    #
    # MONOCEN GISH TO P Q CEN
    #

    listOfdfwholeGISHMonocen<-listOfdfwholeGISHInternal[which(names(listOfdfwholeGISHInternal) %in% monocenNames)]

    if(length(listOfdfwholeGISHMonocen)==0) {
      remove(listOfdfwholeGISHMonocen)
    } else {
      listOfdfwholeGISHMonocen<-Filter(function(x) {nrow(x) >= 1}, listOfdfwholeGISHMonocen)

      #
      #   p part
      #

      listOfdfpGISHInternalMonocen2 <- listOfdfwholeGISHMonocen

      # dfpGISHInternalMonocen2 <- dplyr::bind_rows(listOfdfpGISHInternalMonocen2, .id = "OTU")

      dfpGISHInternalMonocen2 <- suppressWarnings(bind_rows( (lapply(
        listOfdfpGISHInternalMonocen2, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfpGISHInternalMonocen2 <- makeNumCols(dfpGISHInternalMonocen2)

      dfpGISHInternalMonocen2$chrRegion<-"p"
      dfpGISHInternalMonocen2$chrRegionOrig<-"w"

      #
      #   q part
      #

      listOfdfqGISHInternalMonocen2 <- listOfdfwholeGISHMonocen

      # dfqGISHInternalMonocen2 <- dplyr::bind_rows(listOfdfqGISHInternalMonocen2, .id = "OTU")

      dfqGISHInternalMonocen2 <- suppressWarnings(bind_rows( (lapply(
        listOfdfqGISHInternalMonocen2, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfqGISHInternalMonocen2 <- makeNumCols(dfqGISHInternalMonocen2)

      dfqGISHInternalMonocen2$chrRegion<-"q"
      dfqGISHInternalMonocen2$chrRegionOrig<-"w"

      #
      # cen part
      #

      listOfdfCenMarksInternal2 <- listOfdfwholeGISHMonocen

      # dfCenMarksInternal2 <- dplyr::bind_rows(listOfdfCenMarksInternal2, .id = "OTU")

      dfCenMarksInternal2<- suppressWarnings(bind_rows( (lapply(
        listOfdfCenMarksInternal2, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfCenMarksInternal2 <- makeNumCols(dfCenMarksInternal2)

      dfCenMarksInternal2$chrRegion<-"cen"

      # dfCenMarksInternal2$chrRegionOrig<-"w" leaving this hides w names completely in inline

      cendfs <- mget(ls(pattern = "^dfCenMarksInternal" ) )

      if(length(cendfs) ) {
        # dfCenMarksInternal <- suppressWarnings(dplyr::bind_rows(cendfs) )

        dfCenMarksInternal<- suppressWarnings(bind_rows((lapply(
          cendfs, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

        dfCenMarksInternal <- makeNumCols(dfCenMarksInternal)

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

    # dfwholeGISHHolocen <- dplyr::bind_rows(listOfdfwholeGISHHolocen, .id = "OTU")

    dfwholeGISHHolocen<- suppressWarnings(bind_rows( (lapply(
      listOfdfwholeGISHHolocen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
      ,.id = "OTU" ) )

    dfwholeGISHHolocen <- makeNumCols(dfwholeGISHHolocen)

    #
    # remake chrom sizes df
    #


    dfwholeGISHHolocen$r2<-as.numeric(NA)
    dfwholeGISHHolocen$markSizeProtein<-as.numeric(NA)
    dfwholeGISHHolocen$markPosProtein <-as.numeric(NA)

    for(i in 1:length(dfwholeGISHHolocen$OTU)){
      corr_index <- which(names(listOfdfChromSize) %in% dfwholeGISHHolocen$OTU[i] )
      dfwholeGISHHolocen$r2[i] <- as.numeric(attr(listOfdfChromSize[[corr_index]],"r2"))
    }

    dfwholeGISHHolocen$markSize <- dfChrSizeInternalDivisor[match(interaction(dfwholeGISHHolocen[c("OTU","chrName")] ),
                                                                  interaction(dfChrSizeInternalDivisor[c("OTU","chrName") ] )
    ),]$chrSize

    dfwholeGISHHolocen$markSizeProtein<-dfwholeGISHHolocen$markSize-(dfwholeGISHHolocen$r2*2)

    dfwholeGISHHolocen$markPos <- 0

    if(markDistType=="cen") { # center
      dfwholeGISHHolocen$markPos <- dfChrSizeInternalDivisor[match(interaction(dfwholeGISHHolocen[c("OTU","chrName")] ),
                                                           interaction(dfChrSizeInternalDivisor[c("OTU","chrName") ] )
      ),]$chrSize/2
    }

    dfwholeGISHHolocen$markPosProtein <- dfwholeGISHHolocen$markPos + dfwholeGISHHolocen$r2


    #
    #   merge dfMarkPosInternal and dfwholeGISHHolocen
    #

    if(exists("dfMarkPosInternal") & exists("dfwholeGISHHolocen") ) {
      # dfMarkPosInternal <- dplyr::bind_rows(dfMarkPosInternal,dfwholeGISHHolocen)

      dfMarkPosInternal<- suppressWarnings(bind_rows((lapply(
        list(dfMarkPosInternal,dfwholeGISHHolocen), function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

      dfMarkPosInternal <- makeNumCols(dfMarkPosInternal)

    }
    if(!exists("dfMarkPosInternal") & exists("dfwholeGISHHolocen") ) {
      dfMarkPosInternal <- dfwholeGISHHolocen
    }

    } #     if(length(listOfdfwholeGISHHolocen)==0){

  }  #  end   if(exists("dfwholeGISHInternal")){

#
#     transform cen marks into inProtein, divisor applied
#

if(cenFormat=="inProtein" & exists("dfCenMarksInternal") ) {

  dfMarkPosInternal3 <- dfCenMarksInternal

  remove(dfCenMarksInternal)

  dfMarkPosInternal3$chrRegion<-"p"
  cenSize<- dfChrSizeInternalDivisor[match(interaction(dfMarkPosInternal3[c("OTU","chrName")] )
                                           ,interaction(dfChrSizeInternalDivisor[c("OTU","chrName") ] )
  ),]$centromereSize # * inProteinCenFactor

  dfMarkPosInternal3$markDistCen <- -(cenSize/2)
  dfMarkPosInternal3$markSize <- cenSize

  #
  #   mark colors
  #

  nameList <- unique(dfMarkPosInternal3$markName)

  dfMarkPosInternal3$markNameOld<-dfMarkPosInternal3$markName

  dfMarkPosInternal3$markName <- paste0("inProtein",dfMarkPosInternal3$markName)

  if(!missing(bannedMarkName)){
    toBan <- nameList[which(nameList %in% bannedMarkName)]
    if(length(toBan)){
      bannedMarkName4 <- paste0("inProtein",toBan)
    }
  }

  if(!missing(forbiddenMark)){
    toBan<- NULL
    toBan <- nameList[which(nameList %in% forbiddenMark)]
    if(length(toBan)){
      forbiddenMark4 <- paste0("inProtein",toBan)
    }
  }

  toBan <-NULL
  toBan <- nameList[which(nameList %in% bToRemove)]
  if(length(toBan)){
    bToRemove <- c(bToRemove,paste0("inProtein",bToRemove) )
  }

}

  #################################################################################################################3
  #
  #   merge p
  #

  gishMonocenDfsP <- mget(ls(pattern = "^dfpGISHInternalMonocen" ) )

  if(length(gishMonocenDfsP) ) {
    # MdfpGISHInternalMonocen <- suppressWarnings(dplyr::bind_rows(gishMonocenDfsP) )

    MdfpGISHInternalMonocen<- suppressWarnings(bind_rows((lapply(
      gishMonocenDfsP, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    MdfpGISHInternalMonocen <- makeNumCols(MdfpGISHInternalMonocen)

  }

  if(exists("MdfpGISHInternalMonocen") ) {
    #
    #   divisor not used see 990
    #
    MdfpGISHInternalMonocen <- markDistCenGISHfix(MdfpGISHInternalMonocen,dfChrSizeInternal
                                                  ,"shortArmSize",markDistType
                                                  ,listOfdfChromSize)
  } # p gish

  ############################################################################################
  # q

  gishMonocenDfsQ <- mget(ls(pattern = "^dfqGISHInternalMonocen" ) )

  if(length(gishMonocenDfsQ) ) {
    # MdfqGISHInternalMonocen <- suppressWarnings(dplyr::bind_rows(gishMonocenDfsQ) )

    MdfqGISHInternalMonocen<- suppressWarnings(bind_rows((lapply(
      gishMonocenDfsQ, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    MdfqGISHInternalMonocen <- makeNumCols(MdfqGISHInternalMonocen)

  }

  if(exists("MdfqGISHInternalMonocen") ) {
    #
    #   divisor not used see 990
    #
    MdfqGISHInternalMonocen <- markDistCenGISHfix(MdfqGISHInternalMonocen,dfChrSizeInternal
                                                  ,"longArmSize",markDistType
                                                  ,listOfdfChromSize)
  } # q gish

  ##################################################################################################
  #
  #       merging p and q
  #
  ##################################################################################################
#
  gishMonocenDfsPQ <- mget(ls(pattern = "^Mdf" ) )

  if(length(gishMonocenDfsPQ) ) {
    # dfMarkPosInternal2 <- suppressWarnings(dplyr::bind_rows(gishMonocenDfsPQ) )

    dfMarkPosInternal2<- suppressWarnings(bind_rows((lapply(
      gishMonocenDfsPQ, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    dfMarkPosInternal2 <- makeNumCols(dfMarkPosInternal2)

  }

  #
  #    merge dfMarkPosInternal2 dfMarkPosInternal  dfMarkPosInternal3
  #

  mDfMarkPosI <- mget(ls(pattern = "^dfMarkPosInternal" ) )

  if(length(mDfMarkPosI) ) {
    #
    #   rev gish must be first to be background color
    #
    dfMarkPosInternal<- suppressWarnings(bind_rows(rev(lapply(
      mDfMarkPosI, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    dfMarkPosInternal <- makeNumCols(dfMarkPosInternal)

    # dfMarkPosInternal <- suppressWarnings(dplyr::bind_rows(rev(mDfMarkPosI) ) )
  }

  #
  #     DF OF marks to list
  #

  if (exists("dfMarkPosInternal") ) {

    dfMarkPosInternal <- unique(dfMarkPosInternal)

    listOfdfMarkPosInternal <- dfToListColumn(dfMarkPosInternal)

    #
    #              monocen marks list
    #

    # mDfMarkPosMonoI <- mget(ls(pattern = "^parlistOfdfMarkPosMonocen" ) )

    # if(length(mDfMarkPosMonoI) ) {
      # dfMarkPosInternal <- suppressWarnings(dplyr::bind_rows(rev(mDfMarkPosMonoI) ) )

    parlistOfdfMarkPosMonocen <- listOfdfMarkPosInternal[which(names(listOfdfMarkPosInternal) %in% monocenNames)]

    if(length(parlistOfdfMarkPosMonocen)==0){
      remove(parlistOfdfMarkPosMonocen)
    } else {
      for (i in 1:length(parlistOfdfMarkPosMonocen)) {

        #
        #   requires chrRegion
        #
        missingCol <-setdiff(c("chrRegion"),
                            colnames(parlistOfdfMarkPosMonocen[[i]]) )
        if(length (missingCol )==0 ) {
           parlistOfdfMarkPosMonocen[[i]]  <- parlistOfdfMarkPosMonocen[[i]][which(parlistOfdfMarkPosMonocen[[i]]$chrRegion!="cen"),]
        } else {
          message(crayon::red("missing column chrRegion in dfMarkPos, unable to plot monocen. marks"
                              ))
        }
      } # for

      parlistOfdfMarkPosMonocen<-Filter(function(x) {nrow(x) >= 1}, parlistOfdfMarkPosMonocen)

      if(length(parlistOfdfMarkPosMonocen)==0){
        remove(parlistOfdfMarkPosMonocen)
      }
    } # else

    # if(exists("parlistOfdfMarkPosMonocen")) {
    # }

    #
    #                holocen marks list
    #

    parlistOfdfMarkPosHolocen <- listOfdfMarkPosInternal[which(names(listOfdfMarkPosInternal) %in% holocenNames)]

    if(length(parlistOfdfMarkPosHolocen)==0){
      remove(parlistOfdfMarkPosHolocen)
    }

} # end missing dfMarkPosInternal

  #
  #   df of cen marks to list
  #

  if (exists("dfCenMarksInternal")){

    #
    #   creation parlistOfdfMarkPosDataCen
    #

    parlistOfdfMarkPosDataCen <- dfToListColumn(dfCenMarksInternal)

    parlistOfdfMarkPosDataCen <- parlistOfdfMarkPosDataCen[which(names(parlistOfdfMarkPosDataCen) %in% monocenNames)]

    if(length(parlistOfdfMarkPosDataCen)==0){
      remove(parlistOfdfMarkPosDataCen)
    } else {
      #
      #   remove columns without info.
      #
      for (i in 1:length(parlistOfdfMarkPosDataCen)){
        parlistOfdfMarkPosDataCen[[i]][parlistOfdfMarkPosDataCen[[i]]==""]<-NA
        parlistOfdfMarkPosDataCen[[i]]<-  parlistOfdfMarkPosDataCen[[i]][, !apply(is.na(parlistOfdfMarkPosDataCen[[i]]), 2, all)]
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

  if(exists("parlistOfdfMarkPosMonocen")){
    message(crayon::black(
      "\nChecking mandatory columns from dfMarkPos: chrName, markName, chrRegion,markDistCen\n (column OTU  is necessary if more than one species)\nmarkSize can be absent when cM style"
    ) )# cat

    for (i in 1:length(parlistOfdfMarkPosMonocen ) ) {

      parlistOfdfMarkPosMonocen[[i]][parlistOfdfMarkPosMonocen[[i]]==""] <- NA
      parlistOfdfMarkPosMonocen[[i]] <- parlistOfdfMarkPosMonocen[[i]][, !apply(is.na(parlistOfdfMarkPosMonocen[[i]]), 2, all)]

      #
      #   rename column markpos if necessary
      #

      if(!"markDistCen" %in% colnames(parlistOfdfMarkPosMonocen[[i]]) & "markPos" %in% colnames(parlistOfdfMarkPosMonocen[[i]])  ){
        message(crayon::red(
          paste(c("Column markPos in d.f. of marks of OTU",names(parlistOfdfMarkPosMonocen)[[i]]
                  ,"renamed to markDistCen")))
        ) # mess
        colnames(parlistOfdfMarkPosMonocen[[i]])[which(names(parlistOfdfMarkPosMonocen[[i]])=="markPos")]<-"markDistCen"
      }

      #
      #   REMOVE GISH DATA incomplete duplicated data
      #

      parlistOfdfMarkPosMonocen[[i]] <- parlistOfdfMarkPosMonocen[[i]][setdiff(1:length(parlistOfdfMarkPosMonocen[[i]]$chrRegion),
                                                                         which(parlistOfdfMarkPosMonocen[[i]]$chrRegion %in% "p" &
                                                                                 is.na(parlistOfdfMarkPosMonocen[[i]]$markSize) &
                                                                                 is.na(parlistOfdfMarkPosMonocen[[i]]$markDistCen)
      ) ) ,]

      parlistOfdfMarkPosMonocen[[i]] <- parlistOfdfMarkPosMonocen[[i]][setdiff(1:length(parlistOfdfMarkPosMonocen[[i]]$chrRegion),
                                                                         which(parlistOfdfMarkPosMonocen[[i]]$chrRegion %in% "q" &
                                                                         is.na(parlistOfdfMarkPosMonocen[[i]]$markSize) &
                                                                         is.na(parlistOfdfMarkPosMonocen[[i]]$markDistCen)
      ) ) ,]

      parlistOfdfMarkPosMonocen[[i]] <- parlistOfdfMarkPosMonocen[[i]][setdiff(1:length(parlistOfdfMarkPosMonocen[[i]]$chrRegion),
                                                                         which(parlistOfdfMarkPosMonocen[[i]]$chrRegion %in% "w"
      ) ) ,]

      #
      #   column error check
      #

      missingCol<-setdiff(c("chrName", "markName", "chrRegion","markDistCen"),
                    colnames(parlistOfdfMarkPosMonocen[[i]]) )

      if(length (missingCol )>0 ) {
        message(crayon::red(paste(c("ERROR Missing columns in d.f. of marks of OTU"
                                    ,names(parlistOfdfMarkPosMonocen)[[i]] ,":"
                                    ,missingCol) , sep="\n", collapse = " "
                                  )
        )
        ) # cat
        message(crayon::red(paste("\nERRORS PRESENT, see above, dfMarksPos of OTU"
                                  , names(parlistOfdfMarkPosMonocen)[[i]]
                                  ,"REMOVED\n")
        ) ) #m
        parlistOfdfMarkPosMonocen[[i]]<-NA
      } # fi setdiff
      #
      #   column without error
      #
      else { # if no error

        corr_index<-which(names(listOfdfChromSize) %in% names(parlistOfdfMarkPosMonocen)[[i]] )

        divisor2 <- as.numeric(attr(listOfdfChromSize[[corr_index]],"divisor"))

        #
        #   careful divisor applied
        #

        #
        #  dont apply divisor to inProtein marks, check that $centromereSize behave similarly
        #

        selectionNoInProtein <- which(FALSE==parlistOfdfMarkPosMonocen[[i]]$markName %in%
                                        grep("inProtein", parlistOfdfMarkPosMonocen[[i]]$markName,
                                             value =TRUE) )

        parlistOfdfMarkPosMonocen[[i]][selectionNoInProtein,]$markDistCen <-
          parlistOfdfMarkPosMonocen[[i]][selectionNoInProtein,]$markDistCen/divisor2

          # parlistOfdfMarkPosMonocen[[i]]$markDistCen<-parlistOfdfMarkPosMonocen[[i]]$markDistCen/divisor2

          if("markSize" %in% colnames(parlistOfdfMarkPosMonocen[[i]])){
              # parlistOfdfMarkPosMonocen[[i]]$markSize<-parlistOfdfMarkPosMonocen[[i]]$markSize/divisor2

              parlistOfdfMarkPosMonocen[[i]][selectionNoInProtein,]$markSize <-
                parlistOfdfMarkPosMonocen[[i]][selectionNoInProtein,]$markSize/divisor2
          }

        message(crayon::black(paste("\nOK marks of OTU",names(parlistOfdfMarkPosMonocen)[[i]],"checked \n")
                ) ) #m
        if(markDistType=="cen") { # this is from center
          #
          #   fix bug when markDistType is cen (center) but cM style of marks have NA in markSize column
          #
          # halfMarkSize<-ifelse(is.na(parlistOfdfMarkPosMonocen[[i]]$markSize/2),0,(parlistOfdfMarkPosMonocen[[i]]$markSize/2) )
          if("markSize" %in% colnames(parlistOfdfMarkPosMonocen[[i]])){
          parlistOfdfMarkPosMonocen[[i]]$markDistCen <- psum(parlistOfdfMarkPosMonocen[[i]]$markDistCen,
                                                          ( - parlistOfdfMarkPosMonocen[[i]]$markSize/2),
                                                          na.rm=TRUE)
          }
        } # if
      } # else No Error
    } # for each data.frame of Marks of Monocen

  parlistOfdfMarkPosMonocen<-parlistOfdfMarkPosMonocen[!is.na(parlistOfdfMarkPosMonocen)]
  # do as before with holo 27/09
} # fi parlistOfdfMarkPosMonocen

  ##################################################################################################################
  #
  #   holocen check mark
  #

  if(exists("parlistOfdfMarkPosHolocen")){
    message(crayon::black("\nChecking mandatory columns from dfMarkPos (without cen.): chrName, markName, markPos\n (column OTU  is necessary if more than one species)\nmarkSize column is not necessary for style of mark cM"
    ) )# mess

    for (i in 1:length(parlistOfdfMarkPosHolocen ) ) {

      parlistOfdfMarkPosHolocen[[i]][parlistOfdfMarkPosHolocen[[i]]==""]<-NA
      parlistOfdfMarkPosHolocen[[i]]<-  parlistOfdfMarkPosHolocen[[i]][, !apply(is.na(parlistOfdfMarkPosHolocen[[i]]), 2, all)]

      #
      #   REMOVE GISH DATA incomplete duplicated data
      #

      parlistOfdfMarkPosHolocen[[i]] <- parlistOfdfMarkPosHolocen[[i]][setdiff(1:length(parlistOfdfMarkPosHolocen[[i]]$chrName),
                                                                         which(parlistOfdfMarkPosHolocen[[i]]$chrRegion %in% "w" &
                                                                                 is.na(parlistOfdfMarkPosHolocen[[i]]$markSize )
      ) ) ,]

      #
      #   rename column markdistcen if necessary
      #

      if(!"markPos" %in% colnames(parlistOfdfMarkPosHolocen[[i]]) & "markDistCen" %in% colnames(parlistOfdfMarkPosHolocen[[i]])  ){
        message(crayon::red(paste(c("Columns markDistCen in d.f. of marks of OTU",names(parlistOfdfMarkPosHolocen)[[i]] ,"renamed to markPos")))
        ) # mess
        colnames(parlistOfdfMarkPosHolocen[[i]])[which(names(parlistOfdfMarkPosHolocen[[i]])=="markDistCen")]<-"markPos"
      }

      #
      #   column error
      #

      # if(length (setdiff(c("chrName", "markName", "markPos","markSize"),
      if(length (setdiff(c("chrName", "markName", "markPos"),
                           colnames(parlistOfdfMarkPosHolocen[[i]]) ) )>0 ){
          message(crayon::red(paste(c("ERROR Missing columns:",
                                      # setdiff(c("chrName", "markName", "markPos","markSize"),
                                              setdiff(c("chrName", "markName", "markPos"),
                                              colnames(parlistOfdfMarkPosHolocen[[i]]) ) ) , sep="\n", collapse = " " )
          )
          ) # cat
          message(crayon::red(paste("\nERRORS PRESENT, see above, dfMarksPos of OTU", names(parlistOfdfMarkPosHolocen)[[i]] ,"REMOVED\n")
          ) ) #m
          # parlistOfdfMarkPosHolocen<-parlistOfdfMarkPosHolocen[-i]
          parlistOfdfMarkPosHolocen[[i]]<-NA
        } # fi
      #
      #   column without error
      #

      else { # if no error
        message(crayon::black(paste("\nOK marks of OTU",names(parlistOfdfMarkPosHolocen)[[i]],"checked \n")
        ) ) #m
        if(any(is.na(parlistOfdfMarkPosHolocen[[i]]$markPos))){
          message(crayon::blue(paste("\nholocen. mark(s) without pos. might get unexpected results\n")
          ))
        }
        if(origin=="t"){
          parlistOfdfMarkPosHolocen[[i]]$markPos2<-parlistOfdfMarkPosHolocen[[i]]$markPos
          parlistOfdfMarkPosHolocen[[i]]$chrSize<-
            dfChrSizeInternalDivisor[match(interaction( parlistOfdfMarkPosHolocen[[i]][c("OTU", "chrName")]),
                                    interaction( dfChrSizeInternalDivisor[c("OTU", "chrName")] )
                                    ),]$chrSize

          if(markDistType=="beg"){
            if("markSize" %in% colnames(parlistOfdfMarkPosHolocen[[i]])){
              # markSize2<-ifelse(is.na( parlistOfdfMarkPosHolocen[[i]]$markSize  ) ,0, ( parlistOfdfMarkPosHolocen[[i]]$markSize ) )
              parlistOfdfMarkPosHolocen[[i]]$markPos<- psum(parlistOfdfMarkPosHolocen[[i]]$chrSize,
                                                         - parlistOfdfMarkPosHolocen[[i]]$markPos2,
                                                         - parlistOfdfMarkPosHolocen[[i]]$markSize,
                                                         na.rm=TRUE)
            } # markSize column exist

          } else if(markDistType=="cen"){
              if("markSize" %in% colnames(parlistOfdfMarkPosHolocen[[i]])){
                # halfMarkSize<-ifelse(is.na(parlistOfdfMarkPosHolocen[[i]]$markSize/2) ,0, ( (parlistOfdfMarkPosHolocen[[i]]$markSize/2) ) )
                parlistOfdfMarkPosHolocen[[i]]$markPos<-psum( parlistOfdfMarkPosHolocen[[i]]$chrSize,
                                                           - parlistOfdfMarkPosHolocen[[i]]$markPos2,
                                                           (- parlistOfdfMarkPosHolocen[[i]]$markSize/2),
                                                           na.rm=TRUE)
              } # col markSize exists
          } # cen

        } else if (origin=="b") { # if t else b

          if(markDistType=="cen") { # center
            if("markSize" %in% colnames(parlistOfdfMarkPosHolocen[[i]])){

            parlistOfdfMarkPosHolocen[[i]]$markPos <- psum(parlistOfdfMarkPosHolocen[[i]]$markPos,
                                                        (- parlistOfdfMarkPosHolocen[[i]]$markSize/2),
                                                        na.rm=TRUE)
            } # if col markSize exist
          } # cen
        } # origin b
      } # else No Error
    } # for each data.frame of Marks of Monocen

  parlistOfdfMarkPosHolocen<-parlistOfdfMarkPosHolocen[!is.na(parlistOfdfMarkPosHolocen)]

  for (i in 1:length(parlistOfdfMarkPosHolocen)) {

    corr_index<-which(names(listOfdfChromSize) %in% names(parlistOfdfMarkPosHolocen)[[i]] )
    divisor2 <- as.numeric(attr(listOfdfChromSize[[corr_index]],"divisor"))

    # if(attr(parlistOfdfMarkPosHolocen[i],"name") %in% MbNames ){
      parlistOfdfMarkPosHolocen[[i]]$markPos <-parlistOfdfMarkPosHolocen[[i]]$markPos/divisor2
      if("markSize" %in% colnames(parlistOfdfMarkPosHolocen[[i]])){
        parlistOfdfMarkPosHolocen[[i]]$markSize <- parlistOfdfMarkPosHolocen[[i]]$markSize/divisor2
      }
    # } # if
  } # for
} # fi holocen exists

  ################################################################################################################################
  #
  #   cen Mark check
  #

  if(exists("parlistOfdfMarkPosDataCen")) {
    message(crayon::black("\nChecking mandatory columns from dfCenMarks: chrName, markName\n (column OTU  is necessary if more than one species)\n")
    ) # mess

    for (i in 1:length(parlistOfdfMarkPosDataCen)){
      #
      #   columns with error
      #

    if(length(setdiff(c("chrName", "markName"),
                      colnames(parlistOfdfMarkPosDataCen[[i]]) ) )>0 ){
      message(crayon::red(paste(c("ERROR Missing columns:",
                              setdiff(c("chrName", "markName"),
                                      colnames(parlistOfdfMarkPosDataCen[[i]]) ),"in OTU", names(parlistOfdfMarkPosDataCen)[[i]]   ), sep="\n", collapse = " " )
      )
      ) # cat
      message(crayon::red(paste("\nERRORS PRESENT, see above, dfCenMarks of OTU", names(parlistOfdfMarkPosDataCen)[[i]] ,"REMOVED\n")
      ) ) #m
      parlistOfdfMarkPosDataCen[[i]] <- NA

    } # fi

      #
      #   columns without error
      #

    else { # if no error
      message(crayon::black(paste("\nOK cen. marks of OTU",names(parlistOfdfMarkPosDataCen)[[i]],"checked \n")
      ))# mess
    } # else
    } # for

  parlistOfdfMarkPosDataCen<-parlistOfdfMarkPosDataCen[!is.na(parlistOfdfMarkPosDataCen)]

  } # fi   if(exists("parlistOfdfMarkPosDataCen"))

  ##############################################################################################################
  #
  #   OTU cross check of d.fs
  #

  if(exists("parlistOfdfMarkPosMonocen")){

    parlistOfdfMarkPosMonocen<- filterExtraOTU(listOfdfChromSize,parlistOfdfMarkPosMonocen)

  } # exists

  if(exists("parlistOfdfMarkPosHolocen")){
    # message(crayon::black("\n####\ndfMarkPos exists, if error will be removed\n") )

    parlistOfdfMarkPosHolocen<- filterExtraOTU(listOfdfChromSize,parlistOfdfMarkPosHolocen)

  } # exists

  #
  #     check chromosomes names  from d.f. marks to chr. size. d.f.
  #

  if(exists("parlistOfdfMarkPosMonocen") ) {

    listOfChecksChr<-checkNameChrDfMarks(listOfdfChromSize,parlistOfdfMarkPosMonocen)
    listOfdfChromSize <-listOfChecksChr[[1]]

    parlistOfdfMarkPosMonocen <-listOfChecksChr[[2]]

    if(length(parlistOfdfMarkPosMonocen)==0){
      remove(parlistOfdfMarkPosMonocen)
    } else {

      #
      #  allMarkNames creation
      #

      allMarkNames <- unique(listOfChecksChr[[3]])


      allMarkNamesInProtein <- allMarkNames[which(allMarkNames %in% grep("inProtein", allMarkNames
                                                                       , value=TRUE, invert = FALSE) ) ]

      markNamesCentromere <- allMarkNamesInProtein[
        which(allMarkNamesInProtein %in% grep("inProteinCentromere", allMarkNamesInProtein
                                              , value=TRUE, invert = FALSE) ) ]

      allMarkNamesInProtein <- allMarkNamesInProtein[
        which(allMarkNamesInProtein %in% grep("inProteinCentromere", allMarkNamesInProtein
                                                                         , value=TRUE, invert = TRUE) ) ]

      allMarkNames <- allMarkNames[which(allMarkNames %in% grep("inProtein", allMarkNames
                                                                , value=TRUE, invert = TRUE) ) ]

      if(exists("allMarkNames")) {if(!length(allMarkNames)){remove(allMarkNames)} }

      if(length(listOfChecksChr[[4]])>0) {
        allMarkMaxSize<-max(listOfChecksChr[[4]], na.rm=TRUE)
      }
    }
  } # parlistOfdfMarkPosMonocen


  if(exists("parlistOfdfMarkPosHolocen") ) {

    listOfChecksChr<-checkNameChrDfMarks(listOfdfChromSize,parlistOfdfMarkPosHolocen)
    listOfdfChromSize<-listOfChecksChr[[1]]

    parlistOfdfMarkPosHolocen<-listOfChecksChr[[2]]

    if(length(parlistOfdfMarkPosHolocen)==0){
      remove(parlistOfdfMarkPosHolocen)
    } else {

      if(exists("allMarkNames")) {
        allMarkNames<-unique(c(allMarkNames,listOfChecksChr[[3]] ) )
      } else {
        allMarkNames<-unique(listOfChecksChr[[3]] )
      }

      allMarkNamesInProtein2 <- allMarkNames[which(allMarkNames %in% grep("inProtein", allMarkNames
                                                                       , value=TRUE, invert = FALSE) ) ]

      aMNList <- ls(pattern = "^allMarkNamesInProtein" )

      if(length(aMNList)){
        aMNList <- lapply(mget(aMNList ), function(x) unname(x) )
        allMarkNamesInProtein <- suppressWarnings(unlist(aMNList) )
        remove(allMarkNamesInProtein2)
      }

      allMarkNames <- allMarkNames[which(allMarkNames %in% grep("inProtein", allMarkNames
                                                                , value=TRUE, invert = TRUE) ) ]

      if(exists("allMarkNames")) {if(!length(allMarkNames)) {remove(allMarkNames)} }

      if(length(listOfChecksChr[[4]])>0){
        if (exists("allMarkMaxSize")){
          allMarkMaxSize<-max(c(allMarkMaxSize,max(listOfChecksChr[[4]], na.rm=TRUE) ), na.rm=TRUE)
        } else {
          allMarkMaxSize<-max(listOfChecksChr[[4]], na.rm=TRUE)
        }
      }
    }
  } # holocen

    if(exists("parlistOfdfMarkPosDataCen") ) {
      listOfChecksChr  <- checkNameChrDfMarks(listOfdfChromSize,parlistOfdfMarkPosDataCen)

      listOfdfChromSize<- listOfChecksChr[[1]]

      parlistOfdfMarkPosDataCen<-listOfChecksChr[[2]]

      if(length(parlistOfdfMarkPosDataCen)==0){
        remove(parlistOfdfMarkPosDataCen)
      } else {

      if(exists("allMarkNames")) {

        allMarkNames <-unique(c(allMarkNames,listOfChecksChr[[3]] ) )
      } else {
        allMarkNames <-unique(listOfChecksChr[[3]])
      }

        allMarkNamesInProtein3 <- allMarkNames[which(allMarkNames %in% grep("inProtein", allMarkNames
                                                                            , value=TRUE, invert = FALSE) ) ]

        allMarkNamesInProtein3 <- allMarkNamesInProtein3[
          which(allMarkNamesInProtein3 %in% grep("inProteinCentromere", allMarkNamesInProtein3
                                                , value=TRUE, invert = TRUE) ) ]

        aMNList <- ls(pattern = "^allMarkNamesInProtein" )

        #
        # last AMNIP
        #

      if(length(aMNList)){
          aMNList <- lapply(mget(aMNList ), function(x) unname(x) )
          allMarkNamesInProtein <- suppressWarnings(unlist(aMNList) )
          if(length(allMarkNamesInProtein)==0){
            remove(allMarkNamesInProtein)
          }
          remove(allMarkNamesInProtein3)
        }

      if(exists("allMarkNames")) {if(!length(allMarkNames)){remove(allMarkNames)} }

      cenMarkNames <- unique(listOfChecksChr[[3]])

      } # parlistOfdfMarkPosDataCen 0
    } # parlistOfdfMarkPosDataCen

  ###############################################################################
  #
  #   remake dfMarkPosInternal (for per. mark) after filtering from lists
  #
  if(exists("allMarkNames") & markPer!="" ) {
    markPer <- intersect(markPer,allMarkNames)
  }

  mlists <- ls(pattern = "^parlistOfdfMarkPos" )

  if(length(mlists)) {
    plist <- lapply(mget(mlists ), function(x) unname(x) )
    #
    #   last dfMarkPosInternal
    #
    # dfMarkPosInternal <- suppressWarnings(dplyr::bind_rows(plist) )

    plist <- rbind.fill(lapply(plist, rbind.fill) )

    dfMarkPosInternal <- makeNumCols(plist)

  }

  ###############################################################################
  #
  #   check compatibility of columns dfMarkColor
  #

  if(chrColor==""){
    chrColor<-"gray"
  }

  if(!missing(cenColor)) {
    #
    if(length(is.na(cenColor)) ) {
      if (is.na(cenColor) ){
        cenColor2<-NULL
      } else if (cenColor==""){
        cenColor2<-chrColor
      } else if (cenColor=="NULL"){
        cenColor2<-NULL
      } else {
        cenColor2<-cenColor
      }
    } else if( is.null(cenColor) ){
      cenColor2<-NULL
    }
  } else {
    cenColor2<-chrColor
  }


  if (!missing(mycolors) ) {

    mycolors2 <- mycolors[mycolors!=""]

    mycolors2 <- tryCatch(mycolors2[sapply(mycolors2, function(X) {
      tryCatch(is.matrix(col2rgb(X)),
               error = function(e) {
                 message(crayon::red(paste("Color",X,"invalid, removed")
                 ) ); return(FALSE)
               })
    } )], error=function(e) {character(0) } )

    mycolors2 <- mycolors2[!mycolors2 %in% c(chrColor,cenColor2)]

    if(length(mycolors2)==0){
      remove(mycolors2)
    }
  }

  conMNames <- mget(ls(pattern = "^allMarkNames" ) ) # to merge with InProtein see above

  if(length(conMNames) ) {
    consolMarkNames <- suppressWarnings(unlist(conMNames) )
    if(!length(consolMarkNames)){
      remove(consolMarkNames)
    }
  }

    if( exists("consolMarkNames") & exists("dfMarkColorInternal") ) {

      reservedfMarkColorInternal <- dfMarkColorInternal[which(FALSE==(dfMarkColorInternal$markName %in% consolMarkNames) ) ,]

      if (nrow(reservedfMarkColorInternal)==0) {
        remove(reservedfMarkColorInternal)
      }

      dfMarkColorInternal <- dfMarkColorInternal[which(dfMarkColorInternal$markName %in% consolMarkNames) ,]

      if (nrow(dfMarkColorInternal)==0) {
        message(crayon::red("\nError in dfMarkColor markNames respect to Marks pos. data.frames, dfMarkColor REMOVED\n")
        ) # cat
        remove(dfMarkColorInternal)
      }
    }

    if(exists("dfMarkColorInternal") ) {

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
                        colnames(dfMarkColorInternal) ) )>0 ) {
      #
      #   removal
      #
      message(crayon::red(paste(c("ERROR Missing column:",
                              setdiff(c("markName", "markColor","style"),
                                      colnames(dfMarkColorInternal) ) ) , sep="\n", collapse = " ")
      )
      )# cat
      remove(dfMarkColorInternal)
      message(crayon::red("\nError in dfMarkColor, REMOVED\n") )

    } else {        # column names ok

      # missing color for constric cenStyle

      tryCatch(dfMarkColorInternal[which(dfMarkColorInternal$markColor==""),]$markColor <- NA, error= function(e){NA})

      tryCatch(bannedMarkName2 <- dfMarkColorInternal[
        which(is.na(dfMarkColorInternal$markColor) ) , ]$markName, error= function(e){NA} )

      tryCatch(dfMarkColorInternal[which(is.na(dfMarkColorInternal$markColor) &
                     dfMarkColorInternal$style %in% "cenStyle"),]$markColor <- chrColor,
               error= function (e) NA )

      if(exists("consolMarkNames") ) {

        if ( length(setdiff(consolMarkNames,unique(dfMarkColorInternal$markName) ) )>0 ) { # nrow not 0
          message(crayon::black("\nColors provided in to dfMarkColor are not enough, internal colors will be used.\n") )
          dfMarkColorInternal <- makedfMarkColor(dfMarkColorInternal,consolMarkNames, c(chrColor,cenColor2) )
        } else { # nrow not 0
          message(crayon::black("\nCheck OK\n") )
        }

      } else { # all Mark Names does not exist
        message(crayon::red("\nError in dfMarkColor Names respect to Marks data.frames, dfMarkColor REMOVED\n")
        )
        remove(dfMarkColorInternal)
      } # else
    } # else column names ok
  } else if (!exists("mycolors2") ) { # if dfMarkColor not exist and missing mycolors

    if(exists("consolMarkNames")  ) {

        dfMarkColorInternal <- makedfMarkColor(idiogramFISH::dfMarkColor
                                             ,consolMarkNames
                                             ,c(chrColor,cenColor2)
                                             ,defaultStyleMark
                                             )
        } # allmarknames
  } else if (exists("mycolors2") ) {

    #
    # if dfMarkColor does not exist , mycolors exist
    #

      if(exists("initialMarkNames") & exists("consolMarkNames")) {
        #
        #   remove unwanted
        #


        inter <- intersect(initialMarkNames,consolMarkNames)

        #
        #   keep original order
        #

        consolMarkNames<-c(inter, setdiff(consolMarkNames,inter) )

        if(exists("consolMarkNames")) {if(!length(consolMarkNames)){remove(consolMarkNames)} }

      }

    if(exists("consolMarkNames") ) {


        if(length(mycolors2) ){

        dfMarkColorInternal <- makedfMarkColorMycolors(consolMarkNames
                                                         ,mycolors2
                                                         ,c(chrColor,cenColor2)
                                                         ,defaultStyleMark
                                                         )
        }

    }
  } # elif mycolors2

  # chunk

  #
  #   cenStyle check
  #

  if(cenFormat=="inProtein") {


    if(length(cenColor2) ){
      dfMarkColorInternalCentro <- data.frame(markName="inProteinCentromere"
                                              ,markColor=cenColor2
                                              ,style="inProtein")
    }

    bToRemove <- c(bToRemove,"inProteinCentromere")


} # cenFormat Protein

  dfMarkColordfs <- mget(ls(pattern = "^dfMarkColorInternal" ) )

  if(length(dfMarkColordfs) ) {

    # dfMarkColorInternal <- suppressWarnings(dplyr::bind_rows(dfMarkColordfs) )

    dfMarkColorInternal<- suppressWarnings(bind_rows((lapply(
      dfMarkColordfs, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    dfMarkColorInternal <- makeNumCols(dfMarkColorInternal)

  }

  #
  #   add border, remove cen if necessary
  #

  if(exists("dfMarkColorInternal")) {


      conMNames <- mget(ls(pattern = "^allMarkNames" ) ) # mergewith inProtein

      if(length(conMNames) ) {
        consolMarkNames <- suppressWarnings(unlist(conMNames) )
      }

      #
      #   not used from mycolors
      #
      if(exists("mycolors2")){
        notUsed<- setdiff(mycolors2,unique(dfMarkColorInternal$markColor) )
        if(length(notUsed)) {
          lenUnMarks<-length(dfMarkColorInternal[which(is.na(dfMarkColorInternal$markColor)),]$markColor)
          lenNotUsed<-length(notUsed)
          coMin<-min(lenNotUsed,lenUnMarks)
          if(coMin>0){
            dfMarkColorInternal[which(is.na(dfMarkColorInternal$markColor)),]$markColor[1:coMin]<-
            notUsed[1:coMin]
          }
        }
      }

     if(exists("markNamesCentromere")) {
       consolMarkNamesCen<-c(consolMarkNames,markNamesCentromere)
     } else {
       consolMarkNamesCen<-consolMarkNames
     }

      if(exists("reservedfMarkColorInternal")) {
        dfMarkColorInternal <- makedfMarkColor(dfMarkColorInternal,consolMarkNamesCen, c(chrColor,cenColor2)
                                           ,reserveDF=reservedfMarkColorInternal )
      } else {

        #   last makedfMarkColor
        #
        dfMarkColorInternal <- makedfMarkColor(dfMarkColorInternal,consolMarkNamesCen, c(chrColor,cenColor2)
                          )

      }

   if(length(cenColor2)==0){
     dfMarkColorInternal <- dfMarkColorInternal[which(FALSE==dfMarkColorInternal$markName %in% "inProteinCentromere"),]
   }


   dfMarkColorInternal$markBorderColor<-dfMarkColorInternal$markColor

    if(!missing(colorBorderMark)) {

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

  #
  #   inline labels of arrows
  #

  if (exists("dfMarkColorInternal") & legend=="inline") {
    dfMarkColorInternalCopy <- dfMarkColorInternal
    dfMarkColorInternalArrowsLabels<-dfMarkColorInternalCopy[which(dfMarkColorInternalCopy$style %in% c("downArrow","upArrow") ),]
    remove(dfMarkColorInternalCopy)
    if(nrow(dfMarkColorInternalArrowsLabels)>0) {
      tryCatch(dfMarkColorInternalArrowsLabels[which(dfMarkColorInternalArrowsLabels$style %in% "downArrow"),]$style<-"cMLeft",error = function(e) {""})
      tryCatch(dfMarkColorInternalArrowsLabels[which(dfMarkColorInternalArrowsLabels$style %in% "upArrow"),]$style<-"cM",error = function(e) {""})
      #
      # adds space for new pseudomark
      #
      tryCatch(dfMarkColorInternalArrowsLabels$markName <- paste0(dfMarkColorInternalArrowsLabels$markName," "),error = function(e) {""} )
      #
      #   last dfMarkColorInternal
      #

      # dfMarkColorInternal <- dplyr::bind_rows(dfMarkColorInternal,dfMarkColorInternalArrowsLabels)

      dfMarkColorInternal<- suppressWarnings(bind_rows((lapply(
        list(dfMarkColorInternal,dfMarkColorInternalArrowsLabels)
        , function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

      dfMarkColorInternal <- makeNumCols(dfMarkColorInternal)

    }
  }

  if (exists("copyDfMarkPosInternal1") & exists("dfMarkColorInternal") & cenFormat!="inProtein" ) {
    mWithW <- copyDfMarkPosInternal1[which(copyDfMarkPosInternal1$chrRegion %in% "w"),]$markName
    Wstyles<- dfMarkColorInternal[which(dfMarkColorInternal$markName %in% mWithW),]$style
    if(length(Wstyles)){
      if("inProtein" %in% Wstyles){
        message(crayon::magenta("You have used w (chrRegion) marks of 'inProtein' style, use cenFormat='inProtein'" ) )
      }
    }
  }
  # copyDfMarkPosInternal1 dfMarkColorInternal

  fMN <- mget(ls(pattern = "^forbiddenMark" ) )

  if(length(fMN)){
    bannedMarkNameFor <- c(unlist(fMN),"inProteinCentromere")
  } else {
    bannedMarkNameFor<-"inProteinCentromere"
  }

  bMN <- mget(ls(pattern = "^bannedMarkName" ) )

  if(length(bMN)){
    bannedMarkName3 <- unlist(bMN)
  } else {
    bannedMarkName3 <- NULL
  }

  #
  #   CREATION OF CHILD DATAFRAMES MARKS
  #

  ###################
  # REQUIRES STYLE
  ###################

  # substituted by:

  if(exists("parlistOfdfMarkPosMonocen") & exists("dfMarkColorInternal")  ) {
    speListOfdfMarkPosMonocenArrowsLabels<-list()

    for (i in 1:length(parlistOfdfMarkPosMonocen)){
      if(class(parlistOfdfMarkPosMonocen[[i]])=="data.frame" ) {

        parlistOfdfMarkPosMonocen[[i]]$style <- dfMarkColorInternal$style[
          match(parlistOfdfMarkPosMonocen[[i]]$markName, dfMarkColorInternal$markName)]

        if (legend=="inline") {
          speListOfdfMarkPosMonocenArrowsLabels[[i]]<-parlistOfdfMarkPosMonocen[[i]]
          speListOfdfMarkPosMonocenArrowsLabels[[i]]<-speListOfdfMarkPosMonocenArrowsLabels[[i]][which(speListOfdfMarkPosMonocenArrowsLabels[[i]]$style %in% c("downArrow","upArrow") ),]
          if(nrow(speListOfdfMarkPosMonocenArrowsLabels[[i]])>0) {
            tryCatch(speListOfdfMarkPosMonocenArrowsLabels[[i]][which(speListOfdfMarkPosMonocenArrowsLabels[[i]]$style %in% "downArrow"),]$style<-"cMLeft",error = function(e) {""})

            tryCatch(speListOfdfMarkPosMonocenArrowsLabels[[i]][which(speListOfdfMarkPosMonocenArrowsLabels[[i]]$style %in% "upArrow"),]$style<-"cM",error = function(e) {""})

            tryCatch({
              downleftp <- which(speListOfdfMarkPosMonocenArrowsLabels[[i]]$style %in% "cMLeft" &
                                 speListOfdfMarkPosMonocenArrowsLabels[[i]]$chrRegion %in% "p" )
              speListOfdfMarkPosMonocenArrowsLabels[[i]][downleftp,]$markDistCen<-
                speListOfdfMarkPosMonocenArrowsLabels[[i]][downleftp,]$markDistCen+
                speListOfdfMarkPosMonocenArrowsLabels[[i]][downleftp,]$markSize
                },    error = function(e) {""} )
            tryCatch({
              uprightq <- which(speListOfdfMarkPosMonocenArrowsLabels[[i]]$style %in% "cM" &
                                   speListOfdfMarkPosMonocenArrowsLabels[[i]]$chrRegion %in% "q" )
              speListOfdfMarkPosMonocenArrowsLabels[[i]][uprightq,]$markDistCen<-
                speListOfdfMarkPosMonocenArrowsLabels[[i]][uprightq,]$markDistCen+
                speListOfdfMarkPosMonocenArrowsLabels[[i]][uprightq,]$markSize
            },    error = function(e) {""} )

            tryCatch(speListOfdfMarkPosMonocenArrowsLabels[[i]]$markName <- paste0(speListOfdfMarkPosMonocenArrowsLabels[[i]]$markName," ")
                     ,error = function(e) {""} )

            parlistOfdfMarkPosMonocen[[i]] <- dplyr::bind_rows(
              parlistOfdfMarkPosMonocen[[i]],speListOfdfMarkPosMonocenArrowsLabels[[i]])
          } # arrows present
        } # inline

        tryCatch(parlistOfdfMarkPosMonocen[[i]]$protruding <- dfMarkColorInternal$protruding[match(parlistOfdfMarkPosMonocen[[i]]$markName,
                                                                                                dfMarkColorInternal$markName)]
                 ,error = function(e) {""}
                 )



      if(nrow(parlistOfdfMarkPosMonocen[[i]][which(parlistOfdfMarkPosMonocen[[i]]$style %in% "cenStyle"),])>0) {
        if(!"markSize" %in% colnames(parlistOfdfMarkPosMonocen[[i]])){
          parlistOfdfMarkPosMonocen[[i]]$markSize<-NA
        }
        for (m in 1:nrow(parlistOfdfMarkPosMonocen[[i]][which(parlistOfdfMarkPosMonocen[[i]]$style %in% "cenStyle"),] ) ) {
          if( is.na(parlistOfdfMarkPosMonocen[[i]][which(parlistOfdfMarkPosMonocen[[i]]$style %in% "cenStyle"),]$markSize[m]) ) {

         parlistOfdfMarkPosMonocen[[i]][which(parlistOfdfMarkPosMonocen[[i]]$style %in% "cenStyle"),]$markSize[m] <-
                dfChrSizeInternalDivisor[match(interaction( parlistOfdfMarkPosMonocen[[i]][which(
                parlistOfdfMarkPosMonocen[[i]]$style %in% "cenStyle"),][m,c("OTU", "chrName")]),
                                      interaction( dfChrSizeInternalDivisor[c("OTU", "chrName")] )
              ),]$centromereSize

            } # if
          } # for

        if(cenFormat=="inProtein") {
            if(nrow(parlistOfdfMarkPosMonocen[[i]][which(parlistOfdfMarkPosMonocen[[i]]$style=="cenStyle"),])>0 ){
              parlistOfdfMarkPosMonocen[[i]][which(parlistOfdfMarkPosMonocen[[i]]$style=="cenStyle"),]$style<-"inProtein"
              message(crayon::red("you tried to use cenStyle marks concomitantly with cenFormat='inProtein'
                                \ncenStyle marks are now inProtein marks"))
            }
        }
      }

      } # if data.frame
    } # for each monocen
  } # fi exists monocen

  if(exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal")  ) {

    speListOfdfMarkPosHolocenArrowsLabels<-list()
    for (i in 1:length(parlistOfdfMarkPosHolocen)) {
      if(class(parlistOfdfMarkPosHolocen[[i]])=="data.frame" ) {

        parlistOfdfMarkPosHolocen[[i]]$style<-dfMarkColorInternal$style[match(parlistOfdfMarkPosHolocen[[i]]$markName, dfMarkColorInternal$markName)]

        if (legend=="inline"){
          speListOfdfMarkPosHolocenArrowsLabels[[i]]<-parlistOfdfMarkPosHolocen[[i]]
          speListOfdfMarkPosHolocenArrowsLabels[[i]]<-speListOfdfMarkPosHolocenArrowsLabels[[i]][which(speListOfdfMarkPosHolocenArrowsLabels[[i]]$style %in% c("downArrow","upArrow") ),]
          if(nrow(speListOfdfMarkPosHolocenArrowsLabels[[i]])>0){
            tryCatch(speListOfdfMarkPosHolocenArrowsLabels[[i]][which(speListOfdfMarkPosHolocenArrowsLabels[[i]]$style %in% "downArrow"),]$style<-"cMLeft",error = function(e) {""})
            tryCatch(speListOfdfMarkPosHolocenArrowsLabels[[i]][which(speListOfdfMarkPosHolocenArrowsLabels[[i]]$style %in% "upArrow"),]$style<-"cM",error = function(e) {""})

            tryCatch({
              downleft <- which(speListOfdfMarkPosHolocenArrowsLabels[[i]]$style %in% "cMLeft" )

              speListOfdfMarkPosHolocenArrowsLabels[[i]][downleft,]$markPos <-
                speListOfdfMarkPosHolocenArrowsLabels[[i]][downleft,]$markPos+
                speListOfdfMarkPosHolocenArrowsLabels[[i]][downleft,]$markSize
            },    error = function(e) {""} )

            tryCatch(speListOfdfMarkPosHolocenArrowsLabels[[i]]$markName <- paste0(speListOfdfMarkPosHolocenArrowsLabels[[i]]$markName," "),error = function(e) {""} )
            parlistOfdfMarkPosHolocen[[i]] <- dplyr::bind_rows(
              parlistOfdfMarkPosHolocen[[i]],speListOfdfMarkPosHolocenArrowsLabels[[i]])
          }
        } # inline
        tryCatch(parlistOfdfMarkPosHolocen[[i]]$protruding <- dfMarkColorInternal$protruding[match(parlistOfdfMarkPosHolocen[[i]]$markName,
                                                                                                dfMarkColorInternal$markName)]
                 ,error = function(e) {""}
        )


      if(nrow(parlistOfdfMarkPosHolocen[[i]][which(parlistOfdfMarkPosHolocen[[i]]$style %in% "cenStyle"),])>0) {
        if(!"markSize" %in% colnames(parlistOfdfMarkPosHolocen[[i]]) ) {
          parlistOfdfMarkPosMonocen[[i]]$markSize<-NA
        }
        for (m in 1:nrow(parlistOfdfMarkPosHolocen[[i]][which(parlistOfdfMarkPosHolocen[[i]]$style %in% "cenStyle"),] ) ) {
          if( is.na(parlistOfdfMarkPosHolocen[[i]][which(parlistOfdfMarkPosHolocen[[i]]$style %in% "cenStyle"),]$markSize[m]) ) {
            parlistOfdfMarkPosHolocen[[i]][which(parlistOfdfMarkPosHolocen[[i]]$style %in% "cenStyle"),]$markSize[m]<-

              dfChrSizeInternalDivisor[match(interaction( parlistOfdfMarkPosHolocen[[i]][which(
                parlistOfdfMarkPosHolocen[[i]]$style %in% "cenStyle"),][m,c("OTU", "chrName")]),
                interaction( dfChrSizeInternalDivisor[c("OTU", "chrName")] )
              ),]$centromereSize

            } # if
          } # for

        if(cenFormat=="inProtein") {
          if(nrow(parlistOfdfMarkPosHolocen[[i]][which(parlistOfdfMarkPosHolocen[[i]]$style=="cenStyle"),])>0 ){
            parlistOfdfMarkPosHolocen[[i]][which(parlistOfdfMarkPosHolocen[[i]]$style=="cenStyle"),]$style<-"inProtein"
            message(crayon::red("you tried to use cenStyle marks concomitantly with cenFormat='inProtein'
                                \ncenStyle marks are now inProtein marks"))
          }
        }




        } # if nrow
      } # if data.frame
    } # for
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
                                which(names(listOfdfChromSize)==addMissingOTUAfter[[i]])
                                ) # append
    }
  } # fi

  if(!is.na(addMissingOTUBefore[1]) ){
    if (length(missOTUspacings) != length(addMissingOTUBefore) ){
      missOTUspacings<-rep(missOTUspacings, abs(length(addMissingOTUBefore)/length(missOTUspacings)) ) [1:length(addMissingOTUBefore)]
    }
    for (i in 1:length(addMissingOTUBefore)){
      listOfdfChromSize <- append(listOfdfChromSize,
                                  rep(NA,missOTUspacings[[i]]),
                                  which(names(listOfdfChromSize)==addMissingOTUBefore[[i]] )-1
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

  for (s in 1:length(listOfdfChromSize) ) {
    OTUname<-names(listOfdfChromSize[s])
    if (exists("parlistOfdfMarkPosHolocen") ){
      OTUparlistOfdfMarkPosHolocen<-parlistOfdfMarkPosHolocen[which(names(parlistOfdfMarkPosHolocen) %in% OTUname)]
      if(length(OTUparlistOfdfMarkPosHolocen)==0){
        remove(OTUparlistOfdfMarkPosHolocen)
      }
    }
    if (exists("parlistOfdfMarkPosMonocen") ){
      OTUparlistOfdfMarkPosMonocen<-parlistOfdfMarkPosMonocen[which(names(parlistOfdfMarkPosMonocen) %in% OTUname)]
      if(length(OTUparlistOfdfMarkPosMonocen)==0){
        remove(OTUparlistOfdfMarkPosMonocen)
      }
    }
    if (exists("parlistOfdfMarkPosDataCen") ) {
      OTUparlistOfdfMarkPosDataCen<-parlistOfdfMarkPosDataCen[which(names(parlistOfdfMarkPosDataCen) %in% OTUname)]
      if(length(OTUparlistOfdfMarkPosDataCen)==0){
        remove(OTUparlistOfdfMarkPosDataCen)
      }
    }
    mybooleanChrName <- !exists("OTUparlistOfdfMarkPosHolocen") & !exists("OTUparlistOfdfMarkPosMonocen") &
      !exists("OTUparlistOfdfMarkPosDataCen")
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

    if(orderChr=="size") {
        orderlist<-lapply(totalLength, function(x) order(x, decreasing = TRUE) )
    } else if(orderChr=="name"){
        orderlist<-lapply(listOfdfChromSize, function(x) tryCatch(order(x$chrName), error=function(e) NA ) )
    } else if(orderChr=="original" | orderChr=="group" | orderChr=="chrNameUp") {
        orderlist<-lapply(listOfdfChromSize, function(x) tryCatch(1:max(order(x$chrName) ), error=function(e) NA ) )
    }

    ##################################################
    #
    #   add column of new chro index to data.frames
    #
    ##################################################

  listOfdfChromSize <- addNeworderColumn(listOfdfChromSize, orderlist)

    ###################################################
    #     groups
    ###################################################

    if("group" %in% colnames(dfChrSizeInternalDivisor)) {
      message(crayon::blue("group column present - remove column if not using") )
      grouporderlist<-lapply(listOfdfChromSize, function(x) tryCatch(order(x$group), error=function(e) NA ) )

      if(orderChr=="group") {

        for (s in 1:length(listOfdfChromSize)){
          if(class(listOfdfChromSize[[s]])=="data.frame") {
            if(!anyNA(grouporderlist[[s]] ) ) {
              listOfdfChromSize[[s]]<-listOfdfChromSize[[s]][grouporderlist[[s]], ]
              listOfdfChromSize[[s]]$neworder<-1:nrow(listOfdfChromSize[[s]])
            } else {
              message(crayon::blue(paste("Ordering by group was not possible for", names(listOfdfChromSize)[s],"check that column") )
                      )
            }
          } # df
        } # end for

      } # order

    } # if group colname

  ###################################################
  #     order by chrNameUp
  ###################################################

  if("chrNameUp" %in% colnames(dfChrSizeInternalDivisor)) {
    chrNameUpOrderlist<-lapply(listOfdfChromSize, function(x) tryCatch(order(x$chrNameUp), error=function(e) NA ) )

    if(orderChr=="chrNameUp") {

      for (s in 1:length(listOfdfChromSize)){
        if(class(listOfdfChromSize[[s]])=="data.frame") {
          if( !anyNA(chrNameUpOrderlist[[s]] ) ){
            listOfdfChromSize[[s]]<-listOfdfChromSize[[s]][chrNameUpOrderlist[[s]], ]
            listOfdfChromSize[[s]]$neworder<-1:nrow(listOfdfChromSize[[s]])
          } else {
            message(crayon::blue(paste("Ordering by chrNameUp was not possible for", names(listOfdfChromSize)[s],"check that column") )
                    )
          }
        } #fi
      } # end for

    } # order

  } # if group colname

    ##
    # order by group and chromosome
    ##

    ##################################################
    #
    #   important - add new indexer to df of marks - adds neworder column to listOfdfChromSizeCenType
    #
    ##################################################

    if(exists("parlistOfdfMarkPosHolocen") ){

      parlistOfdfMarkPosHolocen<-newOrderColumn(listOfdfChromSize,parlistOfdfMarkPosHolocen)

    } # end if presence of parlistOfdfMarkPosHolocen order

    if(exists("parlistOfdfMarkPosMonocen") ) {

      parlistOfdfMarkPosMonocen<-newOrderColumn(listOfdfChromSize,parlistOfdfMarkPosMonocen)

    } # end if presence of parlistOfdfMarkPosMonocen

    ######################################################
    #
    #   important - add new indexer to d.f DataCen
    #
    ######################################################

    if (exists("parlistOfdfMarkPosDataCen")){

      parlistOfdfMarkPosDataCen<-newOrderColumn(listOfdfChromSize,parlistOfdfMarkPosDataCen)

    }  # end presence of dfCenMarksInternal

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

    chromosome_ns <- sapply(listOfdfChromSize, function(x) nrow(x) )

    # listOfdfChromSize ->     chromosome_ns ->     arms_number ->     armRepVector

    arms_number <- sapply(chromosome_ns, function(x) x*2)

    armRepVector<-lapply(arms_number, function(x) return(tryCatch( c(rep(rownumber,ceiling(x/rownumber) ) ), error=function(e) NA ) ))

    rownumber<-1

    chromRepVector <- lapply(chromosome_ns, function(x) return(tryCatch( c(rep(rownumber,ceiling(x/rownumber) ) ), error=function(e) NA ) ))

    #
    #   creating x y main coords fro chr.
    #

    ym <- xm <-  x <- list()
    # y <- list()
    segX0<-segX1<-segY0<-segY1<-list()
}
########################################################################################################################################
  if(!missing(moveKarHor)) {
    moveKarHor2<-tryCatch(intersect(moveKarHor,unique(dfChrSizeInternal$OTU)),error=function(e) {
      "moveKarHor OTU not found"} )
  } else {# if
    moveKarHor2<-""
  }
  if(!missing(karAnchorLeft)) {
    karAnchorLeft2 <- karAnchorLeft
  } else {# if
    karAnchorLeft2 <- ""
  }
  if(!missing(karAnchorRight)) {
    karAnchorRight2 <- karAnchorRight
  } else {# if
    karAnchorRight2 <- ""
  }

  if(!missing(moveAllKarValueY)){
    if(is.numeric(moveAllKarValueY)) {
      karHeight <- karHeight + moveAllKarValueY
    } else {
      message(crayon::blue("moveAllKarValueY must be numeric"))
    }
  }

for (s in 1:num_species) {

      if(class(listOfdfChromSize[[s]])=="data.frame") {

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


      for (i in 1:length(armRepVector[[s]] )) {

        centromereSize3 <- as.numeric(attr(listOfdfChromSize[[s]],"centromere"))

        if(cenFormat=="inProtein"){
          centromereSize3<-0
        }

        croybot[i]<-tryCatch(list(c(karHeight,
                                    rep((karHeight-(listOfdfChromSize[[s]][,"longArmSize"]*normalizeToOne)[i]),2),
                                    karHeight
                          )#c
                        ), error=function(e) NA ) # list

        croytop[i]<-tryCatch( list(c( (karHeight+(centromereSize3*normalizeToOne)+(listOfdfChromSize[[s]][,"shortArmSize"]*normalizeToOne)[i] ),
                                       rep((karHeight+(centromereSize3*normalizeToOne)),2),
                                      (karHeight+(centromereSize3*normalizeToOne)+(listOfdfChromSize[[s]][,"shortArmSize"]*normalizeToOne)[i] )
        ) # c
        ), error=function(e) NA) # list
      } # for

      crox <- matrix(rep(NA,length(armRepVector[[s]])*4),ncol=4, nrow=length(armRepVector[[s]]) )


      for (i in 1:length(armRepVector[[s]])) {
        crox[i,] <- c(
          rep( ( (chrSpacing2 * i + ( i * chrWidth2) ) + chrWidth2),2),
          rep( ( (chrSpacing2 * i + ( i * chrWidth2) )  ),2)
        ) # c rox
      } # for

      if(!missing(moveAllKarValueHor)) {
        if(is.numeric(moveAllKarValueHor)) {
          crox <- crox + moveAllKarValueHor
        } else {
          message(crayon::blue("moveAllKarValueHor must be numeric"))
        }
      }

      if(names(listOfdfChromSize)[s] %in% moveKarHor2 ) {
        crox <- crox + mkhValue

        if(!missing(anchor) & verticalPlot) {
          if(anchor){
            x1 <-min(crox)-mkhValue
            x2 <-min(crox)-chrWidth2
            y0<- ( (min(unlist(croytop) ) + max(unlist(croybot)) ) /2 ) +  karHeiSpace*(s-1)
            yh<-y0+karHeiSpace*anchorVsizeF
            yl<-y0-karHeiSpace*anchorVsizeF
            segX0[[s]]<-c(x1+moveAnchorV,x1+moveAnchorV)
            segY0[[s]]<-c(y0,yh)
            segX1[[s]]<-c(x2+moveAnchorH,x1+moveAnchorV)
            segY1[[s]]<-c(y0,yl)
          }
        }
      }

      #
      #   groups
      #

      if(circularPlot==FALSE & "group" %in% colnames(listOfdfChromSize[[s]])  ) {
        lens <- rle(listOfdfChromSize[[s]]$group)$lengths
        names(lens)<-rle(listOfdfChromSize[[s]]$group)$values
        clens<-cumsum(lens)
        clens2<-clens[!is.na(names(clens))]
        lens2<-lens[!is.na(names(clens))]

        # groupSepar=.5
        if(length(clens2)>0 ) {
          for (i in 1:(length(clens2) ) ) {
            groupSize <- lens2[[i]]
            if( clens2[[i]] - lens2[[i]] != tryCatch(clens2[[i-1]], error=function(e){0} ) ) {

            tryCatch(crox[(clens2[[i]]-groupSize+1):nrow(crox),] <- crox[(clens2[[i]]-groupSize+1):nrow(crox),] + chrSpacing2*groupSepar
                     , error=function(e){"nothing before group"}
                     )
            } # if
          }
          for (i in 1:(length(clens2) ) ){
            tryCatch(crox[(clens2[[i]]+1):nrow(crox),] <- crox[(clens2[[i]]+1):nrow(crox),] + chrSpacing2*groupSepar, error=function(e){"nothing after group" })
          }
        } # if
      } # group


      #
      #   create xm monocen
      #

      xm[[s]] <- rbind(crox,crox)

      if(verticalPlot==FALSE & s>1){
        # if(s>1){
        xm[[s]] <- xm[[s]] + tryCatch(max(xm[[s-1]]),warning=function(w){0},error=function(e){0} )  + karSpaceHor
      }


      #
      #   horizontal anchor monocen
      #

      if(names(listOfdfChromSize)[s] %in% karAnchorLeft2 ) {
        # crox <- crox + mkhValue
        if(!missing(anchor) & verticalPlot==FALSE) {
          if(anchor){

            x2 <- tryCatch(max(xm[[s-1]]),warning=function(w){0},error=function(e){0} ) + karSpaceHor + chrWidth2
            x1 <- tryCatch(max(xm[[s-1]]),warning=function(w){0},error=function(e){0} ) + chrWidth2
            x0 <- (x2+x1)/2 # dont use x
            yh <- ( (min(unlist(croytop) ) + max(unlist(croybot)) ) /2 )
            yl <- yh - karHeiSpace * anchorVsizeF * 2
            segX0[[s]]<-c(x1+moveAnchorH,x0+moveAnchorH)
            segY0[[s]]<-c(yh,yh)
            segX1[[s]]<-c(x2+moveAnchorH,x0+moveAnchorH)
            segY1[[s]]<-c(yh,yl)
          }
        }
      }

      if(names(listOfdfChromSize)[s] %in% karAnchorRight2 ) {
        # crox <- crox + mkhValue
        if(!missing(anchor) & verticalPlot==FALSE) {
          if(anchor){

            x2 <- max(xm[[s]]) + karSpaceHor + chrWidth2
            x1 <- max(xm[[s]]) + chrWidth2
            x0 <- (x2+x1)/2 # dont use x
            yh <- ( (min(unlist(croytop) ) + max(unlist(croybot)) ) /2 )
            yl <- yh - karHeiSpace * anchorVsizeF * 2
            segX0[[s]]<-c(x1+moveAnchorH,x0+moveAnchorH)
            segY0[[s]]<-c(yh,yh)
            segX1[[s]]<-c(x2+moveAnchorH,x0+moveAnchorH)
            segY1[[s]]<-c(yh,yl)
          }
        }
      }

      #
      # create x
      #


      x[[s]] <- base::split(xm[[s]], row(xm[[s]]) )

      ifelse(any(is.na(x[[s]]) ), x[[s]]<-NA,"") # ifelseinloop

        if(verticalPlot | circularPlot) {
          ym[[s]] <- t(sapply (c(croybot,croytop ), function (x) {length (x) ; return (x)} ) ) + karHeiSpace*(s-1)
        } else if (verticalPlot==FALSE) {
          ym[[s]] <- t(sapply (c(croybot,croytop ), function (x) {length (x) ; return (x)} ) )
        }
      } # fi is monocen
        ###########################################################################################              HOLOCEN

      if (attr(listOfdfChromSize[[s]], "cenType")=="holocen" ) {

        if(attr(listOfdfChromSize[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
          chrSpacing2<-specialChrSpacing
        } else {
          chrWidth2  <-chrWidth
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

      crox <- matrix(rep(NA,length(chromRepVector[[s]])*4),ncol=4, nrow=length(chromRepVector[[s]]) )

      for (i in 1:length(chromRepVector[[s]])){
        crox[i,] <- c(
                      rep( ( (chrSpacing2*i+( i*chrWidth2))+chrWidth2),2),
                      rep( ( (chrSpacing2*i+( i*chrWidth2))     ),2)
                    ) # c rox
      } # for

      if(!missing(moveAllKarValueHor)){
        if(is.numeric(moveAllKarValueHor)) {
          crox <- crox + moveAllKarValueHor
        } else {
          message(crayon::blue("moveAllKarValueHor must be numeric"))
        }
      }

        if(names(listOfdfChromSize)[s] %in% moveKarHor2 ) {
          crox<-crox+mkhValue

          if(!missing(anchor) & verticalPlot) {
            if(anchor){
              x1<-min(crox)-mkhValue
              x2<-min(crox)-chrWidth2
              y0<- ( (min(unlist(croytop) )+max(unlist(croytop)) ) /2 ) +  karHeiSpace*(s-1)
              yh<-y0+karHeiSpace*anchorVsizeF
              yl<-y0-karHeiSpace*anchorVsizeF
              # segments(c(x1,x1),c(y0,yh),c(x2,x1),c(y0,yl))
              segX0[[s]]<-c(x1+moveAnchorV,x1+moveAnchorV)
              segY0[[s]]<-c(y0,yh)
              segX1[[s]]<-c(x2+moveAnchorH,x1+moveAnchorV)
              segY1[[s]]<-c(y0,yl)

            }
          }
        }

      #
      #   horizontal anchor holocen
      #

      if(names(listOfdfChromSize)[s] %in% karAnchorLeft2 ) {
        # crox <- crox + mkhValue
        if(!missing(anchor) & verticalPlot==FALSE) {
          if(anchor){

            x2 <- tryCatch(max(xm[[s-1]]),warning=function(w){0},error=function(e){0} ) + karSpaceHor + chrWidth2
            x1 <- tryCatch(max(xm[[s-1]]),warning=function(w){0},error=function(e){0} ) + chrWidth2
            x0 <- (x2+x1)/2
            yh<- ( (min(unlist(croytop) ) + max(unlist(croybot)) ) /2 )
            yl<-yh - karHeiSpace * anchorVsizeF * 2
            segX0[[s]]<-c(x1+moveAnchorH,x0+moveAnchorH)
            segY0[[s]]<-c(yh,yh)
            segX1[[s]]<-c(x2+moveAnchorH,x0+moveAnchorH)
            segY1[[s]]<-c(yh,yl)
          }
        }
      }

      if(names(listOfdfChromSize)[s] %in% karAnchorRight2 ) {
        # crox <- crox + mkhValue
        if(!missing(anchor) & verticalPlot==FALSE) {
          if(anchor){

            x2 <- max(xm[[s]]) + karSpaceHor + chrWidth2
            x1 <- max(xm[[s]]) + chrWidth2
            x0 <- (x2+x1)/2 # dont use x
            yh <- ( (min(unlist(croytop) ) + max(unlist(croybot)) ) /2 )
            yl <- yh - karHeiSpace * anchorVsizeF * 2
            segX0[[s]]<-c(x1+moveAnchorH,x0+moveAnchorH)
            segY0[[s]]<-c(yh,yh)
            segX1[[s]]<-c(x2+moveAnchorH,x0+moveAnchorH)
            segY1[[s]]<-c(yh,yl)
          }
        }
      }

      #
      # group
      #

      if(circularPlot==FALSE & "group" %in% colnames(listOfdfChromSize[[s]])  ) {
        lens <- rle(listOfdfChromSize[[s]]$group)$lengths
        names(lens)<-rle(listOfdfChromSize[[s]]$group)$values
        clens<-cumsum(lens)
        clens2<-clens[!is.na(names(clens))]
        lens2<-lens[!is.na(names(clens))]

        # groupSepar=.5
        if(length(clens2)>0 ) {
          for (i in 1:(length(clens2) ) ) {
            groupSize<-lens2[[i]]

            if( clens2[[i]] - lens2[[i]] != tryCatch(clens2[[i-1]], error=function(e){0} ) ) {
            tryCatch(crox[(clens2[[i]]-groupSize+1):nrow(crox),] <- crox[(clens2[[i]]-groupSize+1):nrow(crox),] + chrSpacing2*groupSepar
                     , error=function(e){"nothing before group"})
            } #if
          }

          for (i in 1:(length(clens2) ) ){


            tryCatch(crox[(clens2[[i]]+1):nrow(crox),] <- crox[(clens2[[i]]+1):nrow(crox),] + chrSpacing2*groupSepar
                     , error=function(e){"nothing after group" })
          }

        } # if
      } # group cP F

      #
      # xm holocen
      #

      xm[[s]] <- crox

      if(verticalPlot==FALSE & s > 1) {
        # if(s>1){
        xm[[s]] <- xm[[s]] + tryCatch(max(xm[[s-1]]),warning=function(w){0},error=function(e){0}) + karSpaceHor
      }

      #
      #   creation of x from xm
      #


      x[[s]]  <- base::split(xm[[s]], row(xm[[s]]))

      ifelse(any(is.na(x[[s]]) ),x[[s]]<-NA,"") # ifelseinloop

      # listOfdfChromSize[[s]]<-x[[s]]
      if(verticalPlot | circularPlot) {
        ym[[s]] <- t(sapply ( croytop, function (x) {length (x) ; return (x)}) ) + (karHeiSpace * (s-1)  )
      } else if (verticalPlot==FALSE) {
        ym[[s]] <- t(sapply ( croytop, function (x) {length (x) ; return (x)}) )
      }
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



  names(ym) <- names(listOfdfChromSize)[1:length(ym)] # important here


    ymCopyC<-ymCopy2<-ymCopy<-ym # important must stay here before modifying ym
#
    #
    # not useful when addMiss... present
    #

    if(is.na(addMissingOTUAfter[1]) & is.na(addMissingOTUBefore[1])  ){
      if(karSepar){
        for (s in 1:(length(ym)-1)) {
          diffnext<-abs( min(ym[[s+1]] ) - max(ym[[s]]) )

          ym[[s+1]]   =ym[[s+1]]   -diffnext
          tryCatch(segY0[[s+1]]<-segY0[[s+1]]-diffnext, error=function(e){})
                   tryCatch(segY1[[s+1]]<-segY1[[s+1]]-diffnext, error=function(e){})

          ifelse(amoSepar>diffnext,amoSepar2<-diffnext,amoSepar2<-amoSepar)

          ym[[s+1]]=ym[[s+1]]+amoSepar2
          tryCatch(segY0[[s+1]]<-segY0[[s+1]]+amoSepar2, error=function(e){})
                   tryCatch(segY1[[s+1]]<-segY1[[s+1]]+amoSepar2, error=function(e){} )

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

  #
  #   xNoNA
  #

  x <- removeNAFromList(x,areNA)

  #
  # yNoNA
  #

  y <- lapply(1:length(ymnoNA), function(s) base::split(ymnoNA[[s]], row(ymnoNA[[s]] )) )

  names(xm)<-names(listOfdfChromSize)[1:length(xm)]

  names(x)<-names(listOfdfChromSizenoNA)
  names(y)<-names(listOfdfChromSizenoNA)


for (s in 1:length(y) ) {
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


    #
    #     mark percentages %
    #

    if (exists("dfMarkPosInternal") & markPer!="" ) {
      perList <- perMark(dfMarkPosInternal,listOfdfChromSizenoNA)
    } # exist

    if (exists("dfMarkPosInternal") & showMarkPos ) {
      posTib <- posCalc(dfMarkPosInternal,listOfdfChromSizenoNA,bToRemove)
    } # exist


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
      if (is.na(chrBorderColor) | chrBorderColor=="" ){
        if(chrColor=="white"){
          chrBorderColor2 <- "black"
        } else {
          chrBorderColor2 <- chrColor
        }
      } else {
      chrBorderColor2<-chrBorderColor
      }
    } else {
      if(chrColor=="white"){
        chrBorderColor2 <- "black"
      } else {
        chrBorderColor2 <- chrColor
      }
    } # else

#{ # plot types

    if(squareness < 1) {
      squareness<- 1
    }
    # plot types

      yInter<-intercalate(y,monocenNames)
      names(yInter)<-names(y)

      ylistNewChrSimple<-yVertoHor(yInter,monocenNames)
      names(ylistNewChrSimple)<-names(y)

      ylistTransChrSimple<-transYList(ylistNewChrSimple,shrinkFactor,monocenNames)
      names(ylistTransChrSimple)<-names(ylistNewChrSimple)

      if(squareness > 20) {  ########################################  squareness > 20 #################### > 20

        if(circularPlot==FALSE) {

        #####################################################################################################################
          if(callPlot){
            graphics::plot("",xlim = c( (min(unlist(x), na.rm=TRUE) - xlimLeftMod) , (max(unlist(x), na.rm=TRUE)+xlimRightMod ) ),
                       ylim = c( ylimBotMod*-1 , ( (max(unlist(y), na.rm = TRUE) )+ylimTopMod) ) ,
                       ylab = "", xaxt='n',
                       xlab="", yaxt='n',main = NULL, frame.plot = FALSE, asp=asp, ...
                       )
             }
        ######################################################################################################################
          horizPlot <- !missing(karAnchorLeft) | !missing(karAnchorRight)  & verticalPlot==FALSE
          refKar<- !missing(moveKarHor) | horizPlot

          #
          #   anchor
          #

          if(!missing(anchor) & refKar  ) if(anchor) {
            lapply(1:length(segX0), function(s) mapply(function(w,x,y,z) graphics::segments(w,
                                                                                   x,
                                                                                   y,
                                                                                   z
                                                                                   ,lty=anchorLineLty
                                                                                   ),
                                                   w=segX0[[s]],
                                                   x=segY0[[s]],
                                                   y=segX1[[s]],
                                                   z=segY1[[s]]
                  ) #m
            ) # l
            myI<-ifelse(!missing(moveKarHor),1,2)

            lapply(1:length(segX0), function(s) mapply(function(x,y) graphics::points(x,
                                                                                      y,
                                                                                      pch=pchAnchor,
                                                                                      bg="black"
                                                                                      ),
                y=segY0[[s]][myI],
                x=segX0[[s]][myI]
                ) #m
            ) # l

            moveX <- ifelse(!missing(moveKarHor),anchorTextMoveX,0)
            moveY <- ifelse(!missing(moveKarHor),0,anchorTextMoveY)

            adj <- ifelse(!missing(moveKarHor),1,0.5) # 1 right(vert. anchor), 0.5= centered (hor. anchor)

            lapply(1:length(segX0), function(s) mapply(function(x,y) graphics::text(x,
                                                                                      y,
                                                                                      labels=anchorText
                                                                                    ,adj=adj
                ),
              y=segY0[[s]][myI]+moveY,
              x=segX0[[s]][myI]-moveX
              ) #m
            ) # l

            sign1 <- ifelse(verticalPlot, 1,-1)

            if(!missing(anchorTextMParental)) {
              posSegX<-ifelse(!missing(moveKarHor),
                              min(unlist(segX0) ),
                              ifelse(!is.na(addMissingOTUAfter[1]),
                                     max(unlist(segX1)  )
                                     ,min(unlist(segX0) )
                              )
              ) + anchorTextMoveParenX

              posSegY <- ifelse(!missing(moveKarHor), min(unlist(segY1) ), max(unlist(segY0)  )
                              ) - anchorTextMoveParenY


              adj2 <- ifelse(verticalPlot, 0,1)

              graphics::text(posSegX + (anchorTextMoveX*sign1),
                             posSegY,
                             labels=anchorTextMParental
                             ,adj=adj2 # 0 left
                             ,cex=OTUTextSize
                             ,font=   ifelse( !missing(OTUfont),   OTUfont,   1)
                             ,family= ifelse( !missing(OTUfamily), OTUfamily, defaultFontFamily2)
              )
            }
          } # anchor

          #
          #   simplest plot
          #

          if(chromatids==FALSE) {

          lapply(1:length(y), function(s) mapply(function(x,y) graphics::polygon(x=x,
                                                                                 y=y,
                                                                                 col=chrColor,
                                                                                 lwd=lwd.chr,
                                                                                 border=chrBorderColor2),
                                                 x=x[[s]],
                                                 y=y[[s]]
                                          ) #m
                 ) # l

          } # ct FALSE

          if (chromatids & holocenNotAsChromatids) {

            for (s in 1:length(y) ) {
              if(attr(listOfdfChromSizenoNA[[s]],"cenType")=="holocen"){

                  mapply(function(x,y) graphics::polygon(x=x,
                                                          y=y,
                                                          col=chrColor,
                                                          lwd=lwd.chr,
                                                          border=chrBorderColor2),
                                                       x=x[[s]],
                                                       y=y[[s]]
                  ) #m

              } # holocen
            } # for
          } # if

          if (chromatids) { # CHROMAT TRUE

            XSA<-YSA<-XLA<-YLA<-list()

            XHO1<-XHO2<-YHO1<-YHO2<-list()

            for (s in 1:length(y) ) {

              if(class(listOfdfChromSizenoNA[[s]])=="data.frame") {
                ########################################################

                if(attr(listOfdfChromSizenoNA[[s]], "cenType")=="monocen" ) {                 ############# monocen ###########



                  chrtXchrtYSA <- mapXYchromatidSA( (length(y[[s]])/2)+1 ,
                                                    length(y[[s]]),
                                                    y[[s]],
                                                    x[[s]],
                                                    xModifierMono )

                  chrtXchrtYLA <- mapXYchromatidLA( 1,
                                                   (length(y[[s]])/2) ,
                                                    y[[s]],
                                                    x[[s]],
                                                    xModifierMono )



                  XSA[[s]]<-chrtXchrtYSA$shortArmChrtx
                  YSA[[s]]<-chrtXchrtYSA$shortArmChrty

                  XLA[[s]]<-chrtXchrtYLA$longArmChrtx
                  YLA[[s]]<-chrtXchrtYLA$longArmChrty

                  attr(YSA[[s]], "positionnoNA")<- attr(listOfdfChromSizenoNA[[s]],"positionnoNA")
                  # attr(YLA[[s]], "positionnoNA")<- attr(listOfdfChromSizenoNA[[s]],"positionnoNA")

                  # important integration of long arm and short arms info

                  YSA[[s]][sapply(YSA[[s]], is.null)]<-YLA[[s]][!sapply(YLA[[s]], is.null)]

                  XSA[[s]][sapply(XSA[[s]], is.null)]<-XLA[[s]][!sapply(XLA[[s]], is.null)]

                  # short arm indices are in the second half of list, so YLA is a smaller list

                  names(XSA)[s]<-names(YSA)[s]<-names(y[s])

                  for (a in 1: length(YSA[[s]]) ){
                    # attr(roundedY[[s]][[a]],"chrName1")<- attr(y[[s]][[a]],"chrName1")
                    names(YSA[[s]])[a]<- names(y[[s]][a])
                    names(XSA[[s]])[a]<- names(y[[s]][a])
                  }

              } else if (attr(listOfdfChromSizenoNA[[s]], "cenType")=="holocen" & holocenNotAsChromatids==FALSE ) { # if monocen else  #####  holo

                  # mapXYchromatidHolo <- function(start,end,y,x,xModifier ){
                  chrtXchrtYHolo<-mapXYchromatidHolo(1 ,
                                                 (length(y[[s]]) ) ,
                                    y[[s]],
                                    x[[s]],
                                    xModifierHolo
                    )

                    XHO1[[s]]<-chrtXchrtYHolo$xCT1
                    XHO2[[s]]<-chrtXchrtYHolo$xCT2
                    YHO1[[s]]<-chrtXchrtYHolo$yCT1
                    YHO2[[s]]<-chrtXchrtYHolo$yCT2

                    attr(YHO1[[s]], "positionnoNA")<- attr(listOfdfChromSizenoNA[[s]],"positionnoNA")
                    attr(YHO2[[s]], "positionnoNA")<- attr(listOfdfChromSizenoNA[[s]],"positionnoNA")

                    names(YHO1)[s]<-names(XHO1)[s]<-names(YHO2)[s]<-names(XHO2)[s]<-names(y[s])

                    for (a in 1: length(YHO1[[s]])){
                      names(YHO1[[s]])[a]<- names(y[[s]][a])
                      names(XHO1[[s]])[a]<- names(y[[s]][a])
                      names(YHO2[[s]])[a]<- names(y[[s]][a])
                      names(XHO2[[s]])[a]<- names(y[[s]][a])
                    }
                } # holo holocenNotAsChromatids F
              } # df
            } # for s

            #make YSA same length as Y

            if(length(YSA)>0) { # monocen plot chromatid sq

            # this is not only SA, were integrated

            YSA<-YSA[lengths(YSA) != 0]
            XSA<-XSA[lengths(XSA) != 0]

              lapply(1:length(YSA), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                                     col=chrColor,
                                                                                     lwd=lwd.chr,
                                                                                     border=chrBorderColor2),
                                                     x=XSA[[s]],
                                                     y=YSA[[s]]
                   )#m
              ) #l

            } # if YSA

            if(length(YHO1)>0){ # PLOT HOLOCEN SQ CHROMATID

              YHO1 <- YHO1[lengths(YHO1) != 0]
              XHO1 <- XHO1[lengths(XHO1) != 0]
              YHO2 <- YHO2[lengths(YHO2) != 0]
              XHO2 <- XHO2[lengths(XHO2) != 0]

              # LEFT CHROMATID
              lapply(1:length(YHO1), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                                     col=chrColor,
                                                                                     lwd=lwd.chr,
                                                                                     border=chrBorderColor2),
                                                     x=XHO1[[s]],
                                                     y=YHO1[[s]]
              )#m
              ) #l
              # RIGHT CHROMATID
              lapply(1:length(YHO1), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                                     col=chrColor,
                                                                                     lwd=lwd.chr,
                                                                                     border=chrBorderColor2),
                                                     x=XHO2[[s]],
                                                     y=YHO2[[s]]
              )#m
              ) #l
            } # if YHO1


          } # chromatids FALSE TRUE

        } else { # circular false # circular TRUE sq > 20

          #
          #   x horizontal to vertical
          #

          xlistNewChr<-xHortoVer(x)

          circleMaps <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransChrSimple
                                       ,xlistNewChr,n,0,
                                       chrWidth,rotation=rotation)

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
          } # callPlot

          #
          #   plot chr
          #

          drawPlot(circleMaps,chrColor,lwd.chr,chrBorderColor2)

          #
          #   add OTU names
          #

          if(addOTUName) {

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

          if(chrId!="") {
            listXChrCenter <- mapChrCenter(xlistNewChr)
            listYChrCenter <- mapChrCenter(ylistTransChrSimple)
            names(listYChrCenter)<-names(ylistTransChrSimple)
            chrNames <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,
                                       listYChrCenter,listXChrCenter,n,-chrLabelSpacing,chrWidth,
                                       specialOTUNames=specialOTUNames,chrWFactor=chrWFactor,rotation=rotation)
            plotChrNames(chrNames, indexIdTextSize, chrId,monocenNames,chrColor)
          }

        } # cP true if else

      } else {  # if squareness > 20 ###########         else             ########                                squareness <= 20

        pts <- seq(-pi/2, pi*1.5, length.out = n*4)
        ptsl<- split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

        #
        #   yMod creation
        #

        yMod <- y

        roundedY <- roundedX <- list()

        XSARO<-YSARO<-XLARO<-YLARO<-list()

        XHO1Ro<-YHO1Ro<-XHO2Ro<-YHO2Ro<-list()

        for (s in 1:length(yMod) ) {

          if(class(listOfdfChromSizenoNA[[s]])=="data.frame") {
            ########################################################

          if(attr(listOfdfChromSizenoNA[[s]], "cenType")=="monocen" ) {                 ############# monocen ###########
            if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
              chrWidth2  <-specialChrWidth
            } else {
              chrWidth2 <- chrWidth
            }
            r2<- chrWidth2/(squareness*2)

            if(chromatids==FALSE | circularPlot) {

                  shortxyCoords<-mapXY(( (length(yMod[[s]])/2)+1 ) , length(yMod[[s]]),
                                           y[[s]],yMod[[s]],x[[s]],
                                           yfactor,r2,
                                       ptsl[[1]],ptsl[[2]],ptsl[[4]],ptsl[[3]]
                                      )

                  longxyCoords<-mapXY( 1 , (length(yMod[[s]])/2 ) ,
                                          y[[s]], yMod[[s]] ,x[[s]],
                                          yfactor,r2,
                                        ptsl[[1]],ptsl[[2]],ptsl[[4]],ptsl[[3]]
                                      )

                  # integration of X coords of short and long
                  shortxyCoords$roundedX[sapply(shortxyCoords$roundedX, is.null)] <-
                    longxyCoords$roundedX[!sapply(longxyCoords$roundedX, is.null)]

                  # integration of Y coords of short and long
                  shortxyCoords$roundedY[sapply(shortxyCoords$roundedY, is.null)] <-
                    longxyCoords$roundedY[!sapply(longxyCoords$roundedY, is.null)]

                                 # this is not short only
                  roundedX[[s]]<-shortxyCoords$roundedX
                  roundedY[[s]]<-shortxyCoords$roundedY

                  attr(roundedY[[s]], "positionnoNA")<- attr(listOfdfChromSizenoNA[[s]],"positionnoNA")
                  attr(roundedY[[s]], "cenType")<- attr(listOfdfChromSizenoNA[[s]],"cenType")

                  for (a in 1: length(roundedY[[s]])){
                    # attr(roundedY[[s]][[a]],"chrName1")<- attr(y[[s]][[a]],"chrName1")
                    names(roundedY[[s]])[a]<- names(y[[s]][a])
                    names(roundedX[[s]])[a]<- names(y[[s]][a])
                  }
                  names(roundedX)[s]<-names(roundedY)[s]<-names(y[s])

            } else if(chromatids==TRUE & circularPlot==FALSE) { # chromatids FALSE TRUE

              pts <- seq(-pi/2, pi*1.5, length.out = n*4)

              chrtXchrtYSARo <- mapXYchromatidSARo(
                (length(y[[s]])/2)+1  ,
                (length(y[[s]]) )
                ,y[[s]],
                x[[s]],
                r2,
                xModifierMono ,
                pts)

              chrtXchrtYLARo <- mapXYchromatidLARo(
                1 ,
                (length(y[[s]])/2 )
                ,y[[s]],
                x[[s]],
                r2,
                xModifierMono,
                pts)

              XSARO[[s]] <- chrtXchrtYSARo$RoundedSAChrtx
              YSARO[[s]] <-chrtXchrtYSARo$RoundedSAChrty
              XLARO[[s]] <- chrtXchrtYLARo$RoundedLAChrtx
              YLARO[[s]] <-chrtXchrtYLARo$RoundedLAChrty

              attr(YSARO[[s]], "positionnoNA") <- attr(listOfdfChromSizenoNA[[s]],"positionnoNA")

              # important integration of long arm and short arms info
              YSARO[[s]][sapply(YSARO[[s]], is.null)] <- YLARO[[s]][!sapply(YLARO[[s]], is.null)]

              XSARO[[s]][sapply(XSARO[[s]], is.null)] <- XLARO[[s]][!sapply(XLARO[[s]], is.null)]
              # short arm indices are in the second half of list, so YLA is a smaller list

              names(XSARO)[s]<-names(YSARO)[s]<-names(y[s])

              for (a in 1: length(YSARO[[s]]) ){
                # attr(roundedY[[s]][[a]],"chrName1")<- attr(y[[s]][[a]],"chrName1")
                names(YSARO[[s]])[a]<- names(y[[s]][a])
                names(XSARO[[s]])[a]<- names(y[[s]][a])
              }

            } # chromatids TRUE

          } else if (attr(listOfdfChromSizenoNA[[s]], "cenType")=="holocen") { # if monocen else  holocen #########################

            if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
              chrWidth2 <- specialChrWidth
            } else {
              chrWidth2 <- chrWidth
            }

            r2<- chrWidth2/(squareness*2)

            if(chromatids==FALSE | holocenNotAsChromatids | circularPlot) {

            xyCoords <- mapXY(1 , (length(yMod[[s]]) ) ,
                                y[[s]],
                                yMod[[s]] ,
                                x[[s]],
                                yfactor,r2,
                                ptsl[[1]],ptsl[[2]],ptsl[[4]],ptsl[[3]]
                            )

            roundedX[[s]] <- xyCoords$roundedX
            roundedY[[s]] <- xyCoords$roundedY

            attr(roundedY[[s]], "positionnoNA")<- attr(listOfdfChromSizenoNA[[s]],"positionnoNA")
            attr(roundedY[[s]], "cenType")     <- attr(listOfdfChromSizenoNA[[s]],"cenType")

            for (a in 1: length(roundedY[[s]])){
              names(roundedY[[s]])[a] <- names(y[[s]][a])
              names(roundedX[[s]])[a] <- names(y[[s]][a])
            }

            names(roundedX)[s] <- names(roundedY)[s] <- names(y[s])

            } else if ( chromatids==TRUE & holocenNotAsChromatids==FALSE | circularPlot==FALSE) { # chromatids FALSE TRUE

              pts<- seq(-pi/2, pi*1.5, length.out = n*4)

              chrtXchrtYHoloRo<-mapXYchromatidHoloRo(1 ,
                                                 (length(y[[s]]) ) ,
                                                 y[[s]],
                                                 x[[s]],
                                                 r2,
                                                 xModifierHolo,
                                                 pts
              )

              XHO1Ro[[s]]<-chrtXchrtYHoloRo$holoRightx
              YHO1Ro[[s]]<-chrtXchrtYHoloRo$holoRighty

              XHO2Ro[[s]]<-chrtXchrtYHoloRo$holoLeftx
              YHO2Ro[[s]]<-chrtXchrtYHoloRo$holoLefty

              attr(YHO1Ro[[s]], "positionnoNA")<- attr(listOfdfChromSizenoNA[[s]],"positionnoNA")
              attr(YHO2Ro[[s]], "positionnoNA")<- attr(listOfdfChromSizenoNA[[s]],"positionnoNA")

              names(YHO1Ro)[s]<-names(XHO1Ro)[s]<-names(YHO2Ro)[s]<-names(XHO2Ro)[s]<-names(y[s])

              for (a in 1: length(YHO1Ro[[s]])){
                names(YHO1Ro[[s]])[a]<- names(y[[s]][a])
                names(XHO1Ro[[s]])[a]<- names(y[[s]][a])
                names(YHO2Ro[[s]])[a]<- names(y[[s]][a])
                names(XHO2Ro[[s]])[a]<- names(y[[s]][a])
              }

            } # chrt FALSE

          } # holocen ################################################################################# END HOLOCEN
    } # d.f.

   } # for species

  if(exists("roundedY") ){
    roundedY<-roundedY[!is.na(roundedY)]
    roundedX<-roundedX[!is.na(roundedX)]
  }

  if(circularPlot==FALSE) {

    ##################################################################################################################### < 20
    if(callPlot){
    graphics::plot("",xlim=c( (min(unlist(x), na.rm=TRUE)-xlimLeftMod),(max(unlist(x), na.rm=TRUE)+xlimRightMod ) ),
                   ylim = c( ylimBotMod*-1 ,( (max(unlist(y), na.rm = TRUE) )+ylimTopMod) ) , ylab = "", xaxt='n',
                   xlab="", yaxt='n',main = NULL, frame.plot = FALSE, asp=asp, ...)
    }
    ######################################################################################################################
    horizPlot <- !missing(karAnchorLeft) | !missing(karAnchorRight)  & verticalPlot==FALSE

    refKar<- !missing(moveKarHor) | horizPlot

    if(!missing(anchor) & refKar) if(anchor) {

      lapply(1:length(segX0), function(s) mapply(function(w,x,y,z) graphics::segments(w,
                                                                                      x,
                                                                                      y,
                                                                                      z
                                                                                      ,lty=anchorLineLty
      ),
      w=segX0[[s]],
      x=segY0[[s]],
      y=segX1[[s]],
      z=segY1[[s]]
      ) #m
      ) # l
      myI<-ifelse(!missing(moveKarHor),1,2)
      lapply(1:length(segX0), function(s) mapply(function(x,y) graphics::points(x,
                                                                                y,
                                                                                pch=pchAnchor,
                                                                                bg="black"
                                                                                ),
      y=segY0[[s]][myI],
      x=segX0[[s]][myI]
      ) #m
      ) # l

      moveX <- ifelse(!missing(moveKarHor),anchorTextMoveX,0)
      moveY <- ifelse(!missing(moveKarHor),0,anchorTextMoveY)
      adj <- ifelse(!missing(moveKarHor),1,0.5)

      lapply(1:length(segX0), function(s) mapply(function(x,y) graphics::text(x,
                                                                              y,
                                                                              labels=anchorText
                                                                              ,adj=adj
      ),
      y=segY0[[s]][myI]+moveY,
      x=segX0[[s]][myI]-moveX
      ) #m
      ) # l

      sign1 <- ifelse(verticalPlot, 1,-1)

      if(!missing(anchorTextMParental)) {
        posSegX <- ifelse(!missing(moveKarHor),
                        min(unlist(segX0) ),
                        ifelse(!is.na(addMissingOTUAfter[1]),
                               max(unlist(segX1)  )
                               ,min(unlist(segX0) )
                        )
        ) + anchorTextMoveParenX

        posSegY <- ifelse(!missing(moveKarHor), min(unlist(segY1) ), max(unlist(segY0)  )
                          ) - anchorTextMoveParenY

        adj2 <- ifelse(verticalPlot, 0,1) # adj2 <- 0

        graphics::text(posSegX + (anchorTextMoveX*sign1),
                       posSegY,
                       labels=anchorTextMParental
                       ,adj=adj2 # 0 left
                       ,cex=OTUTextSize
                       ,font=   ifelse( !missing(OTUfont),   OTUfont,   1
                                        )
                       ,family= ifelse( !missing(OTUfamily), OTUfamily, defaultFontFamily2
                                        )
        )
      }
    } # anchor

      if (chromatids==FALSE ) {
            lapply(1:length(y), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                                     col=chrColor,
                                                                                     lwd=lwd.chr,
                                                                                     border=chrBorderColor2),
                                                     x=roundedX[[s]],
                                                     y=roundedY[[s]]
                                              )#m
            ) #l
      }
      if (chromatids & holocenNotAsChromatids) {

      for (s in 1:length(roundedY) ) {
        if(length(attr(roundedY[[s]],"cenType"))>0){
        if(attr(roundedY[[s]],"cenType")=="holocen"){

          mapply(function(x,y) graphics::polygon(x=x,
                                                 y=y,
                                                 col=chrColor,
                                                 lwd=lwd.chr,
                                                 border=chrBorderColor2),
                 x=roundedX[[s]],
                 y=roundedY[[s]]
          ) #m

        } # holocen
        }
      } # for
    } # if
     if (chromatids) { #        CHRT TRUE

        # this is not only SA, arms were integrated
        if(length(YSARO)>0) {
        YSARO<-YSARO[lengths(YSARO) != 0]
        XSARO<-XSARO[lengths(XSARO) != 0]

        lapply(1:length(YSARO), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                                 col=chrColor,
                                                                                 lwd=lwd.chr,
                                                                                 border=chrBorderColor2),
                                                 x=XSARO[[s]],
                                                 y=YSARO[[s]]
          )#m
         ) #l
        } # YHARO len

        if(length(YHO1Ro)>0){ # PLOT HOLOCEN SQ CHROMATID

          YHO1Ro <- YHO1Ro[lengths(YHO1Ro) != 0]
          XHO1Ro <- XHO1Ro[lengths(XHO1Ro) != 0]
          YHO2Ro <- YHO2Ro[lengths(YHO2Ro) != 0]
          XHO2Ro <- XHO2Ro[lengths(XHO2Ro) != 0]

          # LEFT CHROMATID
          lapply(1:length(YHO1Ro), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                                    col=chrColor,
                                                                                    lwd=lwd.chr,
                                                                                    border=chrBorderColor2),
                                                    x=XHO1Ro[[s]],
                                                    y=YHO1Ro[[s]]
          )#m
          ) #l
          # RIGHT CHROMATID
          lapply(1:length(YHO1Ro), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                                    col=chrColor,
                                                                                    lwd=lwd.chr,
                                                                                    border=chrBorderColor2),
                                                    x=XHO2Ro[[s]],
                                                    y=YHO2Ro[[s]]
          )#m
          ) #l
        } # if YHO1Ro

      } # CHRT T
   } else {  # cP FALSE # cP TRUE         ####################################################         circular - squareness < 20

    yInterLong<-intercalate(roundedY,monocenNames)
    names(yInterLong)<-names(y)

    ylistNewChrLong<-yVertoHor(yInterLong,monocenNames )
    names(ylistNewChrLong)<-names(y)

    #
    #   x horizontal to vertical
    #

    xlistNewChr <- xHortoVer(roundedX)

    ylistTransChr <- transYList(ylistNewChrLong,shrinkFactor,monocenNames)

    names(ylistTransChr)<-names(ylistNewChrLong)
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

      drawPlot(circleMaps,chrColor,lwd.chr,chrBorderColor2)

      #
      #   add OTU names
      #

      if(addOTUName){

      firstXchrEach <- lapply(xlistNewChr, `[[`, 1)

      firstYchrEach <- lapply(ylistTransChr, `[[`, 1)

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


        listXChrCenter <- mapChrCenter(xlistNewChr)

       chrNames <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,
                                  listYChrCenter,listXChrCenter,n,
                                  -chrLabelSpacing, # inner
                                  chrWidth,
                                  specialOTUNames=specialOTUNames,chrWFactor=chrWFactor,rotation=rotation)

       plotChrNames(chrNames, indexIdTextSize, chrId,monocenNames ,chrColor)

       } # if
} # circularplot ####################################################################################################

} # squareness if squareness <20 end plot

  ####################################################################################
  #
  #                                                ruler calculate
  #
  ####################################################################################

if(circularPlot==FALSE){
  if (ruler) {
    if (length( which(names(listOfdfChromSize) %in% monocenNames) )>0 ) {

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

      if(cenFormat=="inProtein"){
        centromereSize2<-0
      }

      attr(fromZerotoMaxShort[[i]],"centromere") <- (centromereSize2/divisor2)*cenFactor

      remove(rulerInterval2)

    } # for maxshortround

    names(fromZerotoMaxShort)<-names(maxShortRound)

    ycoordLongRound <-lapply(1:length(fromZerotoMaxLong), function(x) {
      pos <- as.numeric(attr(listOfdfChromSizeMonocen[[x]],"position") )-1
      pos<-ifelse(verticalPlot,pos,0)
      unlist(
        lapply(1:length(fromZerotoMaxLong[[x]]), function(y)
          (karHeight - (fromZerotoMaxLong[[x]][y] * normalizeToOne) ) + (karHeiSpace*(pos))
        ) #l
      ) #u
    } # ycoordLongRound
    ) # l

    names(ycoordLongRound)<-(monocenNames2)

    ycoordShortRound  <-lapply(1:length(fromZerotoMaxShort), function(x){
      pos<-as.numeric(attr(listOfdfChromSizeMonocen[[x]],"position") )-1
      pos<-ifelse(verticalPlot,pos,0)
      unlist(
        lapply(1:length(fromZerotoMaxShort[[x]]), function(y)
          # (karHeight+(centromereSize*normalizeToOne)+(fromZerotoMaxShort[[x]][y]*normalizeToOne))+(1*karHeiSpace*(pos-1))
          (karHeight+(as.numeric(attr(fromZerotoMaxShort[[x]],"centromere"))*normalizeToOne)+(fromZerotoMaxShort[[x]][y]*normalizeToOne))+(1*karHeiSpace*(pos))

        ) # l
      ) # u
    }
    ) #l

    names(ycoordShortRound)<-(monocenNames2)

    if(is.na(addMissingOTUAfter[1] ) &  is.na(addMissingOTUBefore[1]) ){
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

    ########################################################################################
    #
    #                                      add rulers
    #
    ########################################################################################

    ###############
    # short arm ruler labels
    ###############
    opar<-graphics::par(no.readonly = TRUE)
    on.exit(suppressWarnings(par(opar) ))
    graphics::par(mgp=c(3,rulerNumberPos,0))

    rulerPlot(ycoordShortRound,listOfdfChromSize,listOfdfChromSizeMonocen,fromZerotoMaxShort,rulerNumberSize,rulerPos,ruler.tck,
              lwd.chr,moveKarHor2,mkhValue,useMinorTicks,miniTickFactor)

    ################

    # long arm ruler labels

    ################

    rulerPlot(ycoordLongRound,listOfdfChromSize,listOfdfChromSizeMonocen,fromZerotoMaxLong,rulerNumberSize,rulerPos,ruler.tck,
              lwd.chr,moveKarHor2,mkhValue,useMinorTicks,miniTickFactor)



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
      pos<-as.numeric(attr(listOfdfChromSizeHolocen[[x]],"position") )-1
      pos<-ifelse(verticalPlot,pos,0)
      unlist(
        lapply(1:length(fromZerotoMaxChr[[x]]), function(y)
          (karHeight/2+(fromZerotoMaxChr[[x]][y]*normalizeToOne))+(1*karHeiSpace*(pos)) )         # 0+(from
            ) # u
      }
    ) # l

    names(ycoordChrRound)<-holocenNames2

      if(is.na(addMissingOTUAfter[1]) & is.na(addMissingOTUBefore[1])){
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

    rulerPlot(ycoordChrRound,listOfdfChromSize,listOfdfChromSizeHolocen,fromZerotoMaxChr,rulerNumberSize,rulerPos,ruler.tck,
              lwd.chr,moveKarHor2,mkhValue,useMinorTicks,miniTickFactor)

    #
    #   title of ruler for HOLOCEN
    #

    # par(las=1)
    # rulerTitle(ycoordChrRound,listOfdfChromSizeHolocen,MbUnit,specialyTitle,yTitle,xPosRulerTitle,rulerTitleSize)

    # rulerTitle(xmnoNA,ymnoNA,chrSpacing,yPosRulerTitle,listOfdfChromSizeHolocen,MbUnit,specialyTitle,yTitle,xPosRulerTitle,rulerTitleSize)

  } # end holocen

    ###################################################3
    #   ADD TITLE OF RULER
    ###################################################

    par(las=1)

    # rulerTitle(ycoordShortRound,listOfdfChromSizeMonocen,MbUnit,specialyTitle,yTitle,xPosRulerTitle,rulerTitleSize)
    rulerTitle(xmnoNA,ymnoNA,chrSpacing,yPosRulerTitle,listOfdfChromSizenoNA,MbUnit,specialyTitle,yTitle,xPosRulerTitle,rulerTitleSize)


 }   # end rulers if
} # END     not    circular ruler

  ############################################################
  #
  #   groups line
  #
  ###########################################################

if(circularPlot==FALSE) {

    if("group" %in% colnames(dfChrSizeInternalDivisor) ) {

      # groupSegmentDistance <- ifelse(groupUp, 1, 2)
      groupSegmentDistance <- 1 #ifelse(groupUp, 1, 2)

      # chrIdCount  <- ifelse(groupUp==FALSE & chrId=="",0,1) # warn
      chrIdCount  <- ifelse(groupUp | chrId=="" ,0, 1 ) # warn

    for (s in 1:length(xmnoNA)) {
      if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2<-chrWidth
      }

      # allowing not organized groups
      if("group" %in% colnames(listOfdfChromSizenoNA[[s]] ) ) {
      lens <- rle(listOfdfChromSizenoNA[[s]]$group)$lengths
      names(lens)<-rle(listOfdfChromSizenoNA[[s]]$group)$values

      ngroup<-length(lens)
      attr(listOfdfChromSizenoNA[[s]],"groupPresence") <- ngroup

      for (g in 1: ngroup) {

        if(!is.na(names(lens)[g] ) ) {
        x0 <- xmnoNA[[s]][,3][ifelse(length(cumsum(lens)[g-1] )==0, # ifelseinloop
                               1,
                               # cumsum(table(listOfdfChromSizenoNA[[s]]$group) )[g-1]+1
                               cumsum(lens )[g-1]+1
        ) ]
        x0 <- x0[!is.na(names(lens)[g])]
        x1 <- xmnoNA[[s]][,3][cumsum(lens )[g] ] + chrWidth2
        x1 <- x1[!is.na(names(lens)[g] ) ]

        y01 <- min(ymnoNA[[s]]) - ( (distTextChr/3) * (groupSegmentDistance+chrIdCount) )
        y01 <- y01[!is.na(names(lens)[g] ) ]

        segments(x0=x0,
                 y0=y01,
                 x1=x1,
                 y1=y01
        ) # seg


        if(groupName) {

          ytext <- min(ymnoNA[[s]]) - ( (distTextChr/3) * (groupSegmentDistance+chrIdCount+1 ) )

          text(  (x0+x1)/2 ,
                ytext,
                labels =  names( lens[g] ) ,
                cex=indexIdTextSize
          ) # text end
        } # groupName


        } # if not NA

      } # for group

      ########################
      #     group name
      ########################

      if(groupName){

        ytext <- min(ymnoNA[[s]]) - ( (distTextChr/3) * (groupSegmentDistance+chrIdCount+1 ) )

        text( xmnoNA[[s]][,3][1] - (chrWidth/2) * nameChrIndexPos   ,
              ytext,
              labels = classGroupName,
              cex=indexIdTextSize
        ) # text end
      } # groupName

      } # group col if

    } # for sp

  } # if group column
} # circular FALSE


  ###################################################################################################################
  #                                                    chromosome names non-Circular
  ###################################################################################################################

  chrNameDistance <-1

  # original
  if(circularPlot==FALSE){

    if(chrId=="original" | chrId=="simple"){

      for (s in 1:length(xmnoNA)) {

        if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

        if(chrId=="original"){
          mylabels<-listOfdfChromSizenoNA[[s]][,"chrName"]

          if(!missing(chrIdPatternRem)){
            mylabels<- sub(chrIdPatternRem,"",mylabels )
          }

        } else if (chrId=="simple"){
          mylabels <- 1:(nrow(xmnoNA[[s]])/armFactor)
        }

        if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
        } else {
          chrWidth2<-chrWidth
        }

        if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 & groupUp ) {
          # groupCount = 2
          ifelse(groupName,groupCount<-2,groupCount<-1)
        } else {
          groupCount=0
        }

        #########################################
        # y pos.
        #########################################

        ybasic <- min(ymnoNA[[s]] ) - ( (distTextChr/3) * ( chrNameDistance + groupCount) )

        #
        # chr. word
        #

        if( attr(listOfdfChromSizenoNA[[s]], "ytitle" )=="Mb" ){
          unit<- classMbName
        } else if ( attr(listOfdfChromSizenoNA[[s]], "ytitle" )=="cM" ) {
          unit<- classcMName
        } else {
          unit<- classChrName
        }

        graphics::text(xmnoNA[[s]][,3][1] - (chrWidth/2) * nameChrIndexPos
                       ,ybasic
                       ,labels = unit
                       ,cex=indexIdTextSize
        ) # end graphics::text

        for( lab in 1:length(mylabels) ) {

          # uncommon<- mylabels[lab][which( mylabels[lab] %in% grep("FL0|FL+", mylabels[lab], value=TRUE) )]
          uncommon <- mylabels[lab][which( mylabels[lab] %in% grep("FL", mylabels[lab], value=TRUE) )]
          uncommonF<- mylabels[lab][which( mylabels[lab] %in% grep("F\\+", mylabels[lab], value=TRUE) )]

          if(length(uncommon)==1) {

          splUncommon<-strsplit(uncommon,"")

            splUncommon1<-splUncommon[[1]]
            first<-splUncommon1[1]
            sec  <-splUncommon1[2]
            third<-splUncommon1[3]
            third<-ifelse(third==" "|is.na(third),"",third)

            graphics::text(xmnoNA[[s]][,3][lab] + chrWidth2/2 ,
                           ybasic  ,
                           labels = bquote(paste(
                             .(first)[.(sec)]^.(third) )
                           ),
                           cex=indexIdTextSize
            ) # text

          } else if (length(uncommonF)==1) {
            splUncommon<-strsplit(uncommonF,"")

            splUncommon1<-splUncommon[[1]]
            first<-splUncommon1[1]
            sec  <-splUncommon1[2]
            # sec  <-ifelse(sec==" "|is.na(sec),"",sec)
            graphics::text(xmnoNA[[s]][,3][lab] + chrWidth2/2 ,
                           ybasic  ,
                           labels = bquote(paste(
                             .(first)^.(sec) )
                           ),
                           cex=indexIdTextSize
            ) # text
          } else {
            graphics::text(xmnoNA[[s]][,3][lab] + chrWidth2/2,
                           ybasic ,
                           labels = mylabels[lab],
                           cex=indexIdTextSize
            )
          }
        } # for mylabels[lab]

      } # for

    } # if chrId

} # end names CIRCULAR FALSE


  #################################
  # horizontal chromosome index
  #################################

    if(circularPlot==FALSE){

    chrIdCount  <- ifelse(chrId=="",0,1) # warn

    chrSizeShow <- ifelse(chrSize==TRUE,1,0)

    chrSizeMbpShow <- ifelse(chrSizeMbp==TRUE,1,0)

    perPresence <- ifelse(markPer != "",1,0)

    posPresence <- ifelse(showMarkPos,1,0)

    morphoCount<-ifelse(morpho=="Guerra" | morpho=="Levan", 1,
                        ifelse(morpho=="both",2,0
                        )
    ) #mC

    indexCount<-ifelse(chrIndex=="CI" | chrIndex == "AR", 1,
                        ifelse(chrIndex == "both",2,0
                        )
    ) # mC

  if(chrIndex=="both"){bothAddI=1} else {bothAddI=0}

  #
  #   add Chr name up
  #


  if(chrNameUp) {
    for (s in 1:length(listOfdfChromSizenoNA) ) {
      if("chrNameUp" %in% (colnames(listOfdfChromSizenoNA[[s]] ) ) ) {

        if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

        if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
        } else {
          chrWidth2<-chrWidth
        }

        chrNamesUp<- listOfdfChromSizenoNA[[s]][,"chrNameUp"]

        if(!missing(chrIdPatternRem)){
          chrNamesUp <- sub(chrIdPatternRem,"",chrNamesUp  )
        }

        graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2) * nameChrIndexPos , xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/armFactor)]+(chrWidth2/2) ),
                       max(ymnoNA[[s]] ) + (distTextChr/3)   ,
                       labels = tryCatch( c(classChrNameUp,chrNamesUp), error=function(e){NA} )
                       ,cex=indexIdTextSize
        ) # end graphics::text
      }
    } # FOR

  }
    #
    #   add Chr Size
    #

  if(chrSize==TRUE ) {

    for (s in 1:length(listOfdfChromSizenoNA) ) {

      if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

      if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {
        ifelse(groupName,groupCount<-2,groupCount<-1)
      } else {
        groupCount=0
      } # end ifelse

      if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2 <- specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }

      if( attr(listOfdfChromSizenoNA[[s]], "ytitle" )=="Mb" ){
        unit<-MbUnit
      } else if ( attr(listOfdfChromSizenoNA[[s]], "ytitle" )=="cM" ) {
        unit<-specialyTitle
      } else {
        unit<-yTitle
      }

      divisor2<-as.numeric(attr(listOfdfChromSizenoNA[[s]],"divisor"))

      if ( attr(listOfdfChromSizenoNA[[s]], "ytitle" )=="cM" ) {
        labels<-listOfdfChromSizenoNA[[s]][,"chrSize"]*divisor2
      } else if ( attr(listOfdfChromSizenoNA[[s]], "ytitle" )=="Mb" ) {
        labels<-listOfdfChromSizenoNA[[s]][,"chrSize"]*divisor2/1e6
      } else  { # ytitle notmb
        labels<-listOfdfChromSizenoNA[[s]][,"chrSize"]*divisor2
      }

      graphics::text(c( xmnoNA[[s]][,3][1] - (chrWidth/2) * nameChrIndexPos
                       , xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/armFactor)] + (chrWidth2/2)
                       ),
                       min(ymnoNA[[s]]) - ( (distTextChr/3) * (chrIdCount + chrSizeShow + groupCount ) )  ,
                       labels = tryCatch(
                         c(paste0("S (",unit,")"), format(round(labels,nsmall),nsmall=nsmall) ),
                         error=function(e){NA}
                         )
                       ,cex=indexIdTextSize
      ) # end graphics::text
    } # FOR
  } # chrSize

    #
    #   add Chr Size column Mbp
    #

    if(chrSizeMbp==TRUE ) {

      for (s in 1:length(listOfdfChromSizenoNA) ) {

        if("Mbp" %in% (colnames(listOfdfChromSizenoNA[[s]] ) ) ) {

        if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

        if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {
          ifelse(groupName,groupCount<-2,groupCount<-1)
        } else {
          groupCount=0
        } # end ifelse

        if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
        } else {
          chrWidth2<-chrWidth
        }

        mbpLabel<-  format(round(listOfdfChromSizenoNA[[s]][,"Mbp"],nsmall),nsmall=nsmall )

        graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2) * nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/armFactor)]+(chrWidth2/2) ),
                       min(ymnoNA[[s]]) - ( ( (distTextChr/3) * (chrIdCount + chrSizeShow + chrSizeMbpShow + groupCount ) ) )  ,
                       labels = tryCatch( c("S (Mbp)", mbpLabel), error=function(e){NA} )
                       ,cex=indexIdTextSize
        ) # end graphics::text
        }
      } # FOR


    } # chrSize

  #
  #   add CI
  #

  if(chrIndex=="both" | chrIndex == "CI" ) {

      for (s in 1:length(listOfdfChromSizenoNA) ) {

        if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {
          ifelse(groupName,groupCount<-2,groupCount<-1)
        } else {
          groupCount=0
        } # end ifelse
        if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
        } else {
          chrWidth2<-chrWidth
        }

        if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="success") {

          graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+(chrWidth2/2)),
                     min(ymnoNA[[s]]) - ( ( (distTextChr/3) * (chrIdCount+chrSizeShow+chrSizeMbpShow+groupCount+indexCount-bothAddI ) ))  ,
                     labels = tryCatch(c("CI",listOfdfChromSizenoNA[[s]][,"CI"] ),error=function(e){NA})
                     ,cex=indexIdTextSize
                    ) # end graphics::text
       } # success
      } # FOR
  } # BORH OR CI

  #
  #   add AR (radius)
  #

  if(chrIndex=="both" | chrIndex == "AR" ){

    for (s in 1:length(listOfdfChromSizenoNA) ) {
      if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {ifelse(groupName,groupCount<-2,groupCount<-1)   } else{groupCount=0}
      if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2<-chrWidth
      }
      if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="success") {

      ARLabel<-  format(round( listOfdfChromSizenoNA[[s]][,"AR"],nsmall),nsmall=nsmall )

      graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth2/2),
                     rep( min(ymnoNA[[s]]) - ( ( (distTextChr/3) * (chrIdCount+chrSizeShow+chrSizeMbpShow+groupCount+indexCount) ) ) ,(nrow(xmnoNA[[s]])/2)+1 ),
                     labels = tryCatch(c("r", ARLabel ),error=function(e){NA})
                     ,cex=indexIdTextSize
      ) # end graphics::text
      } # success
    } # FOR
  } # fi BOTH OR AR


  #################################
  # horizontal chromosome morphology categories
  #################################

  #
  #   add Guerra and Levan
  #

  if(morpho=="both"){bothAdd=1} else {bothAdd=0}

  #
  #   add Guerra
  #

  if(morpho=="both" | morpho == "Guerra" ) {
    for (s in 1:length(listOfdfChromSizenoNA) ) {
      if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {ifelse(groupName,groupCount<-2,groupCount<-1)   } else{groupCount=0}
      if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2<-chrWidth
      }

      if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="success") {

      graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth2/2),
                     min(ymnoNA[[s]]) - ( (distTextChr/3)*(chrIdCount+chrSizeShow+chrSizeMbpShow+morphoCount+indexCount-bothAdd+groupCount) ),
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
        if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {ifelse(groupName,groupCount<-2,groupCount<-1)   } else {groupCount=0}
        if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
        } else {
          chrWidth2<-chrWidth
        }
        if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="success") {

          graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth2/2),
                         min(ymnoNA[[s]]) - ( ( (distTextChr/3)* (chrIdCount+chrSizeShow+chrSizeMbpShow+morphoCount+indexCount+groupCount) ) ) ,
                         #distVectorGue[decVector]
                         labels = tryCatch(c("Levan",listOfdfChromSizenoNA[[s]][,"Levan"]),error=function(e){NA})
                         ,cex=indexIdTextSize
          ) # end graphics::text
        } # if success
      } # for
  } # fi


      #
      #   add % Het
      #

      if(markPer!="" & exists("allMarkNames") ) {
        for (s in 1:length(listOfdfChromSizenoNA) ) {

          if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

          if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {ifelse(groupName,groupCount<-2,groupCount<-1)   } else {groupCount=0}

          if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
            chrWidth2 <-specialChrWidth
          } else {
            chrWidth2 <- chrWidth
          }
          if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="success") {
            yForPer<-min(ymnoNA[[s]]) - ( ( (distTextChr/3)* (chrIdCount+chrSizeShow+chrSizeMbpShow+morphoCount+indexCount+groupCount+perPresence) ) )
          } else {
            yForPer<-min(ymnoNA[[s]]) - ( ( (distTextChr/3)* (chrIdCount+chrSizeShow+chrSizeMbpShow+groupCount+perPresence+1) ) )
          }

          # fraction
          perValue <- t(perList[[s]])[,paste0(markPer,"_per")]

          if (perAsFraction==FALSE){
            perValue <- perValue*100
          }
            graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/armFactor)]+chrWidth2/2 )
                            ,yForPer
                            ,labels = tryCatch(c(paste0("% ",markPer), format(round(perValue ,nsmall ),nsmall=nsmall) )
                                               ,error=function(e){NA}
                                               )
                            ,cex=indexIdTextSize
            ) # end graphics::text

        } # for
      } # fi


    #
    #   add marks' pos
    #

    if(showMarkPos & exists("dfMarkPosInternal") ) {
      for (s in 1:length(listOfdfChromSizenoNA) ) {

        if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

        if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {
            ifelse(groupName,groupCount<-2,groupCount<-1)
        } else {
            groupCount=0
        }

        if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
          chrWidth2 <-specialChrWidth
        } else {
          chrWidth2 <- chrWidth
        }

        if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="success") {
          yForPos<-min(ymnoNA[[s]]) - ( ( (distTextChr/3)* (chrIdCount+chrSizeShow+chrSizeMbpShow+morphoCount+indexCount+groupCount+perPresence+posPresence ) ) )
        } else {
          yForPos<-min(ymnoNA[[s]]) - ( ( (distTextChr/3)* (chrIdCount+chrSizeShow+chrSizeMbpShow+groupCount+perPresence+posPresence) ) )
        }

        # fraction

        posValue <- tryCatch(lapply(t(posTib[[s]])[,1], sort), error=function(e){"no data"} )

        if(posValue[1]=="no data") {

        posValue <- unlist(lapply(posValue, function(x) tryCatch( (paste0(format(round(x , 2),nsmall=2), collapse="/") ), error=function(e){NA} ) ) )

        graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/armFactor)]+chrWidth2/2 )
                       ,yForPos
                       ,labels = tryCatch(c(paste0("pos.")
                                            # , format(round(perValue ,nsmall ),nsmall=nsmall)
                                            ,posValue
                                            )
                                            ,error=function(e){NA}
                       )
                       ,cex=indexIdTextSize
        ) # end graphics::text

        }

      } # for
    } # fi

} # circular FALSE


      #########################################################################
      # add species names otu names
      #########################################################################

      if (circularPlot==FALSE){

        if(OTUasNote){

          addOTUName<-FALSE

          if(!missing(notes)){
            message(crayon::blurred("Error: OTUasNote is TRUE, notes data.frame will be removed"))
          }
          notes <- data.frame(OTU=unique(dfChrSizeInternalDivisor$OTU), note=unique(dfChrSizeInternalDivisor$OTU) )
        }

        if(OTUasLeftNote){

          addOTUName<-FALSE

          if(!missing(leftNotesUp)){
            message(crayon::blurred("Error: OTUasLeftNote is TRUE, leftNotesUp data.frame will be removed"))
          }
          leftNotesUp <- data.frame(OTU=unique(dfChrSizeInternalDivisor$OTU), note=unique(dfChrSizeInternalDivisor$OTU) )
        }

        #
        #   ADD OTU NAME BELOW CHR.
        #

        if(addOTUName) {

          # message(crayon::green(paste0("OTU section start" ) ) )

          for (s in 1:length(xmnoNA) ) {
            if(as.numeric(attr(listOfdfChromSizenoNA[[s]],"groupPresence") ) > 0 ) {
              ifelse(groupName,groupCount<-2,groupCount<-1)
            } else {
              groupCount=0
            } # end ifelse

            if(attr(listOfdfChromSizenoNA[[s]],"indexStatus")=="failure") {
              indexCount2  <-0
              morphoCount2 <- 0
            } else {
              indexCount2 <- indexCount
              morphoCount2<- morphoCount
            }

            # lapply(1:length(xmnoNA), function(s) {
            if(attr(xmnoNA[[s]],"cenType")=="holocen") {
              holocenDisCount <- morphoCount2 + indexCount2 #newDistVector #+bothAdd
            } else {
              holocenDisCount <- 0
            } # ifelse holocen

            OTUcurrent<-names(listOfdfChromSizenoNA)[[s]]
            # hasQuotes<-grepl("'.*'",OTUcurrent)
            hasQuotes<-grepl("\\((.*?)\\)|'(.*?)'",OTUcurrent)
            hasF<-grepl("FL|FL\\+|FL0|F\\+",OTUcurrent)

            if(!missing(OTUfont) ) {

              if(OTUfont==3 & hasQuotes & parseTypes){
                if(hasF){
                  message(crayon::blue("patterns FL FL+ FL0 or F+ detected and processed; to avoid this, use parseTypes=FALSE"))
                }
                nameWithVar <- processNameVarAndFormula(OTUcurrent)
              } else if (OTUfont==3 & hasQuotes & parseTypes==FALSE) {
                nameWithVar <- processNameVar(OTUcurrent)
              }
            }  # !missing OTUfont

            if(OTUfont2 != 3 & parseTypes & hasF & !exists("nameWithVar") ) {
                message(crayon::blue("patterns FL FL+ FL0 or F+ detected and processed; to avoid this, use  parseTypes=FALSE"))
                noteLang <- formatFs(OTUcurrent,"FL|FL\\+|FL0|F\\+")
                nameWithVar<-str2lang(paste0("paste(",noteLang,")") )
            }

            if(!exists("nameWithVar")) {
              nameWithVar<- OTUcurrent
            }

            graphics::text( min(xmnoNA[[s]] ) # xlimLeftMod
                            # c( (xmnoNA[[s]][1,3] - ((karIndexPos/2) ) ) )
                            ,ydistance <- min(ymnoNA[[s]]) -
                                             ( (distTextChr/3) *
                                                 (chrIdCount + chrSizeShow+ chrSizeMbpShow + morphoCount2 +
                                                    indexCount2 + groupCount + perPresence + posPresence + 2 - holocenDisCount)
                                               )
                            ,labels = nameWithVar
                            # labels = paste("",names(listOfdfChromSizenoNA)[[s]] ),
                            ,cex=OTUTextSize
                            ,adj= 0 # justif 0 =left
                            ,font=   ifelse( !missing(OTUfont),   OTUfont,   1)
                            ,family= ifelse( !missing(OTUfamily), OTUfamily, defaultFontFamily2)
            ) # end graphics::text
            remove(nameWithVar)
          } # for

        } # fi add OTU name
      } # CIRCULAR FALSE

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
            graphics::text( c( (xmnoNA[[i]][1,3]-(1*(karIndexPos/2) ) ) ),
                            # c( (xmnoNA[[i]][1,3]-(xlimLeftMod*(karIndexPos/2) ) ) ),

                            rep( min(ymnoNA[[i]][,1]), 1 ), #2
                            labels = paste("A ",ind$A ),
                            cex=indexIdTextSize,
                            adj=c(1) # justif
            ) # end graphics::text
            graphics::text(c( (xmnoNA[[i]][1,3]-(1*(karIndexPos/2) ) ) ),
                           rep( (min(ymnoNA[[i]][,1])-(distTextChr/3) ) , 1 ), # avoid overlap
                           labels = paste("A2",ind$A2 ),
                           cex=indexIdTextSize,
                           adj=c(1) # 0.5 centered
            ) # end graphics::text
        } # null

     }  else if(attr(listOfdfChromSizenoNA[[i]],"cenType")=="holocen"){ #  if monocen
        if(is.character(names(listOfdfChromSizenoNA)[[i]]  ) ){
          message(crayon::green(paste0(names(listOfdfChromSizenoNA)[[i]],":" ) )
          ) # mess
        }
        ind<-asymmetryA2(listOfdfChromSizenoNA[[i]])
        if(!is.null(ind)){
        graphics::text(c( (xmnoNA[[i]][1,3]-(1*(karIndexPos/2) ) ) ), # xlimLeftMod
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

### cen

################################
#
#   centromeres calculate
#
################################

      if(missing(gishCenBorder) ) {
        gishCenBorder<-FALSE
      }

if(exists("listOfdfChromSizeMonocen") ) {

        #
        # cannot be based on listofmonocen because of addmissingotu
        #

        CentsList<-lapply(1:length(listOfdfChromSize), function(x) tryCatch(rep(0, nrow(listOfdfChromSize[[x]]) ), error=function(e) NA )
        ) # l
        names(CentsList)<-names(listOfdfChromSize)

        ycoordCents <-list()

        for (i in 1:length(CentsList)){
          khsFactor<-ifelse(verticalPlot,(i-1),0)
          corr_index <- which(names(listOfdfChromSize) %in% names(CentsList)[[i]] )

          centromereSize3 <- tryCatch(as.numeric(attr(listOfdfChromSize[[corr_index]],"centromere") ) , error=function(e) NA )

          ycoordCents[[i]] <- t(replicate(length(CentsList[[i]]), (c( rep(  karHeight+  (karHeiSpace*khsFactor ), 2  ),
                                                                      (  (karHeight+  (karHeiSpace*khsFactor ) ) +
                                                                         (karHeight + (centromereSize3*normalizeToOne)  +(karHeiSpace*khsFactor) )
                                                                         ) /2
                                                                     ,rep(  karHeight + (centromereSize3*normalizeToOne)+(karHeiSpace*khsFactor) ,2  )
                                                                     ,   ( (karHeight + (karHeiSpace*khsFactor ) ) +
                                                                           (karHeight + (centromereSize3*normalizeToOne)+(karHeiSpace*khsFactor) )
                                                                         )/2

          ) # c
          )
          ) # r
          ) #t
        } # for
        names(ycoordCents)<-names(CentsList)

        # ycoordCents[areNA]<-NA

        # suppressWarnings(
        if(is.na(addMissingOTUAfter[1] ) &  is.na(addMissingOTUBefore[1] )) {
          if(karSepar){
            for (s in 1:(length(ymCopyC)-1) ) {
              diffnext<-abs(min(ymCopyC[[s+1]] ) - max(ymCopyC[[s]]) )
              ymCopyC[[s+1]]=ymCopyC[[s+1]]-diffnext

              ycoordCents[[s+1]] <- ycoordCents[[s+1]] - diffnext

              ifelse(amoSepar>diffnext,amoSepar2<-diffnext,amoSepar2<-amoSepar)

              ymCopyC[[s+1]]=ymCopyC[[s+1]]+amoSepar2
              ycoordCents[[s+1]] <- ycoordCents[[s+1]] + amoSepar2
            } # for
          } # redu
        }

        ycoordCentsS <- lapply(1:length(ycoordCents), function(j)
          tryCatch(base::split(ycoordCents[[j]], row(ycoordCents[[j]]) ),
                   error=function(e) NA
          )
        )

        names(ycoordCentsS)<-names(listOfdfChromSize)

        #
        #   remove holocen
        #

        ycoordCentsS<-tryCatch(ycoordCentsS[which(names(ycoordCentsS) %in% monocenNames)] , error=function(e) NA )
        ycoordCents <-tryCatch(ycoordCents[which(names(ycoordCents) %in% monocenNames)] , error=function(e) NA )

        ycoordCentsS<-ycoordCentsS[!is.na(ycoordCentsS)]
        ycoordCents<-ycoordCents[!is.na(ycoordCents)]


        #
        # modify cen. position to account for squareness of chr.
        #

        if (squareness<=20){
          r3 <- chrWidth2/(squareness*3)
        } else {
          r3 <- 0
        }

        xcoordCents<-list()

        for (s in 1:length(xmnoNA)){
          if(names(listOfdfChromSizenoNA[s]) %in% monocenNames ){
            if(attr(listOfdfChromSizenoNA[[s]],"ytitle")=="cM"){
              chrWidth2  <-specialChrWidth
            } else {
              chrWidth2<-chrWidth
            }

            xmMatrix <- xmnoNA[[s]][1:(nrow(xmnoNA[[s]])/2),2:3]
            if(!inherits(xmMatrix, "matrix")) {
              xmMatrix<-  t(as.matrix(xmMatrix) )
            }

            #
            #   squareness
            #

            xcoordCents[[s]] <-cbind(sweep(xmMatrix, 2, c(-r3,r3), "+")
                                     ,rowMeans(xmMatrix)
                                     ,sweep(xmMatrix, 2, c(-r3,r3), "+")
                                     ,rowMeans(xmMatrix)
            ) #bind

            names(xcoordCents)[s] <- names(listOfdfChromSizenoNA[s])
          } # monocen
        } # for

        xcoordCents <- Filter(function(x) {length(x) >= 1}, xcoordCents)

        xcoordCentsS<- lapply(1:length(xcoordCents), function(j) base::split(xcoordCents[[j]], row(xcoordCents[[j]]) ) )
        names(xcoordCentsS)<-names(xcoordCents)

        xcoordCentsS<-xcoordCentsS[!is.na(xcoordCentsS)]

        if(!missing(chrBorderColor) ) {
          if(length(cenColor2!="white")) {
            if (cenColor2!="white") {
              cenBorder<-chrBorderColor2
            } else {
              cenBorder<-chrColor
            }
          } else {
            cenBorder<-chrColor # probably has gotten chr. color
          }
        } else {
          cenBorder<-chrColor
        }

        # this protects for weak colors, adds strong border
        if(!is.null(fixCenBorder)){

          if (fixCenBorder){
              cenBorder <- chrBorderColor2
          }
        }

        #
        #   lines when white cen.
        #

        cSbool<-if(is.numeric(centromereSize)) {
          if(centromereSize!=0){
            TRUE
          } else {
            FALSE
          }
        } else {
          TRUE
        }

        if(cenFormat=="triangle") {
          if(cSbool){
            if(length(cenColor2=="white") ) {
              if(cenColor2=="white"){

                xyLinesX1 <- xyLinesY1 <- xyLinesX2 <- xyLinesY2 <- list()

                for (s in 1:length(ycoordCentsS) ) {

                  xyLines<-mapXYCenLines(1 ,
                                         length(ycoordCentsS[[s]] )  ,
                                         ycoordCentsS[[s]],
                                         xcoordCentsS[[s]]
                  )

                  xyLinesX1[[s]] <- xyLines$X1
                  xyLinesY1[[s]] <- xyLines$Y1
                  xyLinesX2[[s]] <- xyLines$X2
                  xyLinesY2[[s]] <- xyLines$Y2

                } # for s

                for(s in 1: length(xyLinesX1)) {
                  for (a in 1: length(xyLinesX1[[s]])){
                    names(xyLinesY1[[s]])[a]<- names(ycoordCentsS[[s]][a])
                    names(xyLinesX1[[s]])[a]<- names(xcoordCentsS[[s]][a])
                    names(xyLinesY2[[s]])[a]<- names(ycoordCentsS[[s]][a])
                    names(xyLinesX2[[s]])[a]<- names(xcoordCentsS[[s]][a])

                  }
                }

                names(xyLinesY1) <- names(ycoordCentsS)
                names(xyLinesX1) <- names(ycoordCentsS)
                names(xyLinesY2) <- names(ycoordCentsS)
                names(xyLinesX2) <- names(ycoordCentsS)

              }
            }
          }
        } # roundedCen FALSE

        if(cenFormat=="rounded" ) {
          if(cSbool) {
            if(length(cenColor2=="white") ) {
              if(cenColor2=="white") {
                pts <- seq(-pi/2, pi*1.5, length.out = n*4)
                ptsl <- split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

                xyRoundLinesX1 <- xyRoundLinesY1 <- xyRoundLinesX2 <- xyRoundLinesY2 <- list()

                for (s in 1:length(ycoordCentsS) ) {

                  xyRoundLines<-mapxyRoundCenLines(1 ,
                                                   length(ycoordCentsS[[s]] )  ,
                                                   ycoordCentsS[[s]],
                                                   xcoordCentsS[[s]]
                                                   ,ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]]

                  )

                  xyRoundLinesX1[[s]] <- xyRoundLines$roundedX1
                  xyRoundLinesY1[[s]] <- xyRoundLines$roundedY1
                  xyRoundLinesX2[[s]] <- xyRoundLines$roundedX2
                  xyRoundLinesY2[[s]] <- xyRoundLines$roundedY2

                } # for s


                for(s in 1: length(xyRoundLinesX1)) {
                  for (a in 1: length(xyRoundLinesX1[[s]])){
                    names(xyRoundLinesY1[[s]])[a]<- names(ycoordCentsS[[s]][a])
                    names(xyRoundLinesX1[[s]])[a]<- names(xcoordCentsS[[s]][a])
                    names(xyRoundLinesY2[[s]])[a]<- names(ycoordCentsS[[s]][a])
                    names(xyRoundLinesX2[[s]])[a]<- names(xcoordCentsS[[s]][a])

                  }
                }

                names(xyRoundLinesY1) <- names(ycoordCentsS)
                names(xyRoundLinesX1) <- names(ycoordCentsS)
                names(xyRoundLinesY2) <- names(ycoordCentsS)
                names(xyRoundLinesX2) <- names(ycoordCentsS)
              }
            }
          }
        } # roundedCen FALSE

        ###############################
        #                             #
        #     plot centromeres        #
        #                             #
        ###############################

        ### generate newLongyCen for roundedCen

        if(cenFormat=="rounded") {
          pts <- seq(-pi/2, pi*1.5, length.out = n*4)
          ptsl <- split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

          newLongyCen <- newLongxCen<-list()

          for (s in 1:length(ycoordCentsS) ) {

            xyCoordsCen<-mapXYCen(1 ,
                                  (length(ycoordCentsS[[s]]) ) ,
                                  ycoordCentsS[[s]],
                                  xcoordCentsS[[s]] ,
                                  ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]]
            )

            newLongxCen[[s]] <- xyCoordsCen$roundedX
            newLongyCen[[s]] <- xyCoordsCen$roundedY

          } # for s


          for(s in 1: length(newLongyCen)) {
            for (a in 1: length(newLongyCen[[s]])){
              names(newLongyCen[[s]])[a]<- names(ycoordCentsS[[s]][a])
              names(newLongxCen[[s]])[a]<- names(xcoordCentsS[[s]][a])
            }
          }

          names(newLongyCen) <- names(ycoordCentsS)
          names(newLongxCen) <- names(ycoordCentsS)

        } # roundedCen
        ###

        if(circularPlot) {
          if(cenFormat=="triangle" | cenFormat =="rounded"){

          spnamexcoord<- names(xcoordCentsS)[1]
          corrSp<- which(names(x) %in% spnamexcoord)
          diffXRounded <- max(x[[corrSp]][[1]]) - max(xcoordCentsS[[1]][[1]]) # needed for all cen marks

          #
          #   y
          #

          yInterSimple<-intercalate(y,monocenNames)
          names(yInterSimple)<-names(y)

          ylistNewChrSimple<-yVertoHor(yInterSimple, monocenNames = monocenNames)
          names(ylistNewChrSimple) <- names(y)

          #
          #   x horizontal to vertical Y for cent. is same for rounded or squared
          #

          ylistTransChrSimple <- transYList(ylistNewChrSimple,shrinkFactor,monocenNames)
          names(ylistTransChrSimple) <- names(ylistNewChrSimple)

          #
          #  here is rounding for circular plot cen.
          #

          if(cenFormat=="rounded"){

            ### new way of making cen.
            xlistNew <- xHortoVerRoundCen(newLongxCen,diffXRounded)
            names(xlistNew) <- names(newLongxCen)

            yMarkPer <- markMapPerCen(newLongyCen,y)
            names(yMarkPer) <- names(newLongyCen)

          } else if (cenFormat=="triangle") {  # ROUNDEDCEN FALSE

            xlistNew <- xHortoVerRoundCen(xcoordCentsS,diffXRounded)
            names(xlistNew) <- names(xcoordCentsS)

            yMarkPer <- markMapPerCen(ycoordCentsS,y)
            names(yMarkPer) <- names(ycoordCentsS)

          } # roundedCen

          ylistTransMark<-transyListCen(yMarkPer,ylistTransChrSimple)

          names(ylistTransMark) <- names(yMarkPer)

          if(!is.null(cenColor2) ) {

            circleMapsCen  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMark,xlistNew,n,0,chrWidth,
                                             rotation=rotation)

            lwd.chrMod <- ifelse( length(cenBorder=="white") & cSbool ,
                                  ifelse(cenBorder=="white",lwd.chr*hideCenLines,lwd.chr),
                                  lwd.chr)


            #
            # draw cen
            #

            drawCen(circleMapsCen,cenColor2,cenBorder,lwd.chrMod)

            #
            # draw black lines when cen is white
            #

          if(cenFormat=="triangle" & cSbool) {

            if(length(cenColor2=="white") & cSbool ) {

              if(cenColor2=="white") {


            xlistNew <- xHortoVerRoundCen(xyLinesX1,diffXRounded)
            names(xlistNew) <- names(xyLinesX1)

            yMarkPer <- markMapPerCen(xyLinesY1,y)
            names(yMarkPer) <- names(xyLinesY1)

            ylistTransMark<-transyListCen(yMarkPer,ylistTransChrSimple)
            names(ylistTransMark) <- names(yMarkPer)

            circleMapsLines  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,    ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

            drawPlotMarkLine(circleMapsLines,chrBorderColor2,lwd.chr)
            #############
            xlistNew <- xHortoVerRoundCenExt(xyLinesX2,diffXRounded)
            names(xlistNew) <- names(xyLinesX2)

            yMarkPer <- markMapPerCen(xyLinesY2,y)
            names(yMarkPer) <- names(xyLinesY2)

            ylistTransMark<-transyListCen(yMarkPer,ylistTransChrSimple)
            names(ylistTransMark) <- names(yMarkPer)

            circleMapsLines  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,    ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

            drawPlotMarkLine(circleMapsLines,chrBorderColor2,lwd.chr)

              }
            }
          } else if (cenFormat=="rounded" & cSbool) {

              if(length(cenColor2=="white") & cSbool ) {
                if(cenColor2=="white") {

                  xlistNew <- xHortoVerRoundCen(xyRoundLinesX1,diffXRounded)
                  names(xlistNew) <- names(xyRoundLinesX1)

                  yMarkPer <- markMapPerCen(xyRoundLinesY1,y)
                  names(yMarkPer) <- names(xyRoundLinesY1)

                  ylistTransMark<-transyListCen(yMarkPer,ylistTransChrSimple)
                  names(ylistTransMark) <- names(yMarkPer)

                  circleMapsLines  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,    ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

                  drawPlotMarkLine(circleMapsLines,chrBorderColor2,lwd.chr)

                  #############

                  xlistNew <- xHortoVerRoundCen(xyRoundLinesX2,diffXRounded)
                  names(xlistNew) <- names(xyRoundLinesX2)

                  yMarkPer <- markMapPerCen(xyRoundLinesY2,y)
                  names(yMarkPer) <- names(xyRoundLinesY2)

                  ylistTransMark<-transyListCen(yMarkPer,ylistTransChrSimple)
                  names(ylistTransMark) <- names(yMarkPer)

                  circleMapsLines  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,    ylistTransMark,xlistNew,n,0,chrWidth,rotation=rotation)

                  drawPlotMarkLine(circleMapsLines,chrBorderColor2,lwd.chr)


                }
              }
            }
          } # cenColor NULL
} #cenFormat
        } else if(circularPlot==FALSE) { # end cP ################################ cP FALSE

          if(!is.null(cenColor2) ) {

            if(length(xcoordCentsS)>0){

              lwd.chrMod <- ifelse( length(cenBorder=="white") & cSbool ,
                                    ifelse(cenBorder=="white",lwd.chr*hideCenLines,lwd.chr),
                                    lwd.chr)

              if(cenFormat=="triangle" & cSbool) {

                lapply(1:length(xcoordCentsS), function(w) mapply(function(x,y,z) graphics::polygon(x=x, y=y,
                                                                                                    col=cenColor2,
                                                                                                    lwd=lwd.chrMod,
                                                                                                    border=z),
                                                                  x=xcoordCentsS[[w]],
                                                                  y=ycoordCentsS[[w]],
                                                                  z=cenBorder
                    ) #m
                ) #l

                #
                # add black lines when cen is white
                #

              if(length(cenColor2=="white") & cSbool ) {
                  if(cenColor2=="white") {
                    lapply(1:length(xyLinesX1), function(w) mapply(function(x,y) graphics::lines(x=x, y=y,
                                                                                                 col=chrBorderColor2,
                                                                                                 lwd=lwd.chr
                    ),
                    x=xyLinesX1[[w]],
                    y=xyLinesY1[[w]]
                    ) #m
                    ) #l

                    lapply(1:length(xyLinesX2), function(w) mapply(function(x,y) graphics::lines(x=x, y=y,
                                                                                                 col=chrBorderColor2,
                                                                                                 lwd=lwd.chr
                    ),
                    x=xyLinesX2[[w]],
                    y=xyLinesY2[[w]]
                    ) #m
                    ) #l

                  } # white lines
                }

              } else if(cenFormat=="rounded" & cSbool) { # roundedCen TRUE


                lapply(1:length(newLongyCen), function(s) mapply(function(x,y,z) graphics::polygon(x=x, y=y,
                                                                                                   col=cenColor2,
                                                                                                   lwd=lwd.chrMod,
                                                                                                   border=z),
                                                                 x=newLongxCen[[s]],
                                                                 y=newLongyCen[[s]],
                                                                 z=cenBorder
                ) #m
                ) #l

                if(length(cenColor2=="white") & cSbool ) {
                  if(cenColor2=="white") {
                    lapply(1:length(xyRoundLinesX1), function(w) mapply(function(x,y) graphics::lines(x=x, y=y,
                                                                                                      col=chrBorderColor2,
                                                                                                      lwd=lwd.chr
                    ),
                    x=xyRoundLinesX1[[w]],
                    y=xyRoundLinesY1[[w]]
                    ) #m
                    ) #l

                    lapply(1:length(xyRoundLinesX2), function(w) mapply(function(x,y) graphics::lines(x=x, y=y,
                                                                                                      col=chrBorderColor2,
                                                                                                      lwd=lwd.chr
                    ),
                    x=xyRoundLinesX2[[w]],
                    y=xyRoundLinesY2[[w]]
                    ) #m
                    ) #l

                  }
                } # white lines

              } # roundedCen
            } # if length xcoordCentsS
          } # cencolor null

        } # circular Plot else not

      } # monocen exist listOfdfChromSizeMonocen


                                                ##########################
                                                #       MARKS            #
                                                ##########################


      ##########################################################################################################3
      #
      #                           painting Marks monocen              square marks
      #
      ############################################################################################################

      if (exists("parlistOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ) {
        # message(crayon::green(paste0("monocen. marks section start" ) ) )


        xMarkSq<-yMarkSq<-listOfdfMarkPosSq<-list()

        j<-1
        for (k in 1:length(parlistOfdfMarkPosMonocen)) {
          currName <- names(parlistOfdfMarkPosMonocen)[[k]]
          if( nrow(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style %in% c("square","squareLeft") ) , ] ) > 0 ){
            listOfdfMarkPosSq<-c(listOfdfMarkPosSq,list(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style %in% c("square","squareLeft") ),]))
            names(listOfdfMarkPosSq)[[j]] <- currName
            j<-j+1
          }
        }

        if(length(listOfdfMarkPosSq)>0){
          for (sm in 1:length(listOfdfMarkPosSq)) {
            yMark1<-NULL
            xMark1<-NULL
            # which(names(listOfdfChromSize) %in% names(listOfdfMarkPosSq)[[sm]] )

            corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosSq)[[sm]] )

            for (m in 1:nrow(listOfdfMarkPosSq[[sm]]) ) {

              ifelse(listOfdfMarkPosSq[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
              ifelse(listOfdfMarkPosSq[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
              ifelse(listOfdfMarkPosSq[[sm]][m,"chrRegion"]=="q",endColumn<- 2, endColumn<- 1)
              ifelse(listOfdfMarkPosSq[[sm]][m,"chrRegion"]=="q",whichArm<- "long", whichArm<- "short")
              ifelse(listOfdfMarkPosSq[[sm]][m,"style"]=="square",squareSide<- "right", squareSide<- "left")


              ifelse(listOfdfMarkPosSq[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
              rowIndex <- nrow(listOfdfChromSize[[corr_index]] ) * longORshort + (listOfdfMarkPosSq[[sm]][,"neworder"][m] )

              armStart<-ym[[corr_index]][rowIndex,column]
              armEnd  <-ym[[corr_index]][rowIndex,endColumn]
              armLength <- max(armStart,armEnd) - min(armStart,armEnd)
              # detect if whole arm
              mSize <- listOfdfMarkPosSq[[sm]][m,"markSize"]*normalizeToOne

              if(is.na(mSize)) {
                message(crayon::blue(paste0("you let square marks without size in a monocen. while using markDistCen (not NA)\n, maybe you want to mark all arm, use NA in markDistCen and use p|q|w in chrRegion. Now 0 will be used" ) ) )
                # listOfdfMarkPosSq[[sm]][m,"markSize"]<-0
                mSize<-0
              }

              # mSize <- listOfdfMarkPosSq[[sm]][m,"markSize"]*normalizeToOne


              if(abs(sum(armLength, - mSize) ) < efZero ) {
                wholeArm <- 'true'
              } else {
                wholeArm <- 'false'
              }

              yprox <- armStart +
                (listOfdfMarkPosSq[[sm]][m,"markDistCen"]                                                             *normalizeToOne*mySign)

              yter <- armStart +
                ( sum( listOfdfMarkPosSq[[sm]][m,"markDistCen"] , listOfdfMarkPosSq[[sm]][m,"markSize"], na.rm=TRUE ) *normalizeToOne*mySign )

              yMark1[[m]] <- if(longORshort==0) {c(yprox,yter,yter,yprox)} else {c(yter,yprox,yprox,yter)}

              attr(yMark1[[m]],"arm") <- listOfdfMarkPosSq[[sm]][m,"chrRegion"]

              # attr(yMark1[[m]],"armStart")<-armStart
              attr(yMark1[[m]],"rowIndex")<-rowIndex
              attr(yMark1[[m]],"wholeArm")<-wholeArm
              attr(yMark1[[m]],"whichArm")<-whichArm
              attr(yMark1[[m]],"squareSide")<-squareSide

              xMark1[[m]]<-xm[[corr_index]][listOfdfMarkPosSq[[sm]][,"neworder"][m],]
              attr(xMark1[[m]],"rowIndex")<-rowIndex

            }
            yMarkSq[[sm]]<-yMark1
            attr(yMarkSq[[sm]], "spname")<-names(listOfdfMarkPosSq)[[sm]]
            xMarkSq[[sm]]<-xMark1
            attr(xMarkSq[[sm]], "spname")<-names(listOfdfMarkPosSq)[[sm]]
          } # end for

          ########################
          #                      #
          #   add marks to plot monocen #
          #                      #
          ########################
          if(chromatids==FALSE | circularPlot==TRUE) {

            # if(circularPlot==FALSE){
            roundPlotMark(bannedMarkName3,squareness, xMarkSq, yMarkSq,
                          dfMarkColorInternal,
                          listOfdfMarkPosSq,
                          chrWidth, #use for calc r2
                          specialChrWidth,
                          yfactor,
                          markN,
                          lwd.marks2,#lwd.chr,
                          listOfdfChromSize,
                          circularPlot,
                          y,
                          markLabelSize,
                          pattern,
                          separFactor,
                          labelSpacing,circleCenter,circleCenterY,radius,
                          legend,ylistTransChrSimple,rotation=rotation,labelOutwards)

          } else if (chromatids & circularPlot==FALSE) {

            chrtSqMark(squareness,yMarkSq,xMarkSq,xModifierMono,r2,dfMarkColorInternal,lwd.marks2,listOfdfMarkPosSq,markN)

          } # chromatids



        } else { #     if(length(listOfdfMarkPosSq)>0)
          remove(listOfdfMarkPosSq)
        }

        # square labels not centrom. (monocen.)
        booleanForsquareInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosSq") & circularPlot==FALSE

        if(booleanForsquareInlineLabel) {
          textLabel(xMarkSq,yMarkSq,listOfdfChromSize,listOfdfMarkPosSq,specialChrSpacing
                    ,chrSpacing,markLabelSize,pattern,bannedMarkName3,
                    markNewLine2=markNewLine,mylheight2=mylheight)
        }


      } # if presence end painting marks

      ##################################################################################################
      #
      #                                painting Marks square holocen
      #
      ##################################################################################################

      if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
        # message(crayon::green(paste0("holocen. marks section start" ) ) )
        xMarkSq<-yMarkSq<-listOfdfMarkPosSq<-list()

        j<-1
        for (k in 1:length(parlistOfdfMarkPosHolocen)) {
          currName<-names(parlistOfdfMarkPosHolocen)[[k]]
          if(nrow(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style %in% c("square","squareLeft") ),])>0){
            listOfdfMarkPosSq <- c(listOfdfMarkPosSq,
                                   list(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style %in% c("square","squareLeft")  ),])
                                   )
            names(listOfdfMarkPosSq)[[j]]<-currName
            j<-j+1
          }
        }

        if(length(listOfdfMarkPosSq)>0) {
          for (sm in 1:length(listOfdfMarkPosSq)) {

            yMark1<-NULL
            xMark1<-NULL

            corr_index<-which(names(ym) %in% names(listOfdfMarkPosSq)[[sm]] )

            for (m in 1:nrow(listOfdfMarkPosSq[[sm]])){
              rowIndex<-(listOfdfMarkPosSq[[sm]][,"neworder"][m])
              chrStart <- ym[[corr_index]][rowIndex ,2]

              if(!"markSize" %in% colnames(listOfdfMarkPosSq[[sm]])){
                listOfdfMarkPosSq[[sm]]$markSize<-NA
              }
              if(!"markPos" %in% colnames(listOfdfMarkPosSq[[sm]])){
                listOfdfMarkPosSq[[sm]]$markPos<-NA
              }
              mSize<-mPos<-NULL
              mSize <- listOfdfMarkPosSq[[sm]][m,"markSize"]*normalizeToOne
              mPos <- listOfdfMarkPosSq[[sm]][m,"markPos"]

              if(is.na(mSize) ){
                message(crayon::blue(paste0("mark without size, unexpected results possible")))
              }
              if(is.na(mSize) & is.na(mPos ) ) {
                message(crayon::blue(paste0("\nyou let square marks without size nor pos. in holocen. Will use 0 as size\nand pos. maybe you want to add column chrRegion with w for whole chr. mark" ) )
                )
                listOfdfMarkPosSq[[sm]][m,"markPos"] <-0
                listOfdfMarkPosSq[[sm]][m,"markSize"]<-0
              }

              yinf <- chrStart +                        # was ysup
                (listOfdfMarkPosSq[[sm]][m,"markPos"]                                                          *normalizeToOne)

              ysup <- chrStart +
                (  sum(listOfdfMarkPosSq[[sm]][m,"markPos"],listOfdfMarkPosSq[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

              yMark1[[m]]<-c(ysup,yinf,yinf,ysup)
              attr(yMark1[[m]],"rowIndex")<-rowIndex
              attr(yMark1[[m]],"wholeArm")<-'false' # this is to plot as mark not as arm see monocen.

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
          if(chromatids==FALSE | holocenNotAsChromatids | circularPlot==TRUE) {

            roundPlotMark(bannedMarkName3,squareness, xMarkSq, yMarkSq,
                          dfMarkColorInternal,
                          listOfdfMarkPosSq,
                          chrWidth, #use for calc r2
                          specialChrWidth,
                          yfactor,
                          markN,
                          lwd.marks2,#lwd.chr,
                          listOfdfChromSize,
                          circularPlot,
                          y,
                          markLabelSize,
                          pattern,
                          separFactor,
                          labelSpacing,circleCenter,circleCenterY,radius,
                          legend,ylistTransChrSimple,rotation=rotation,labelOutwards) #

          } else if (chromatids & holocenNotAsChromatids==FALSE & circularPlot==FALSE) {

            chrtSqMark(squareness,yMarkSq,xMarkSq,xModifierHolo,r2,dfMarkColorInternal,lwd.marks2,listOfdfMarkPosSq,markN)

          } # chromatids


        } #     if(length(listOfdfMarkPosSq)>0){

        else {remove(listOfdfMarkPosSq)}

        #
        #   inline legend holocen
        #

        booleanForsquareInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosSq") & circularPlot==FALSE

        if(booleanForsquareInlineLabel) {
          textLabel(xMarkSq,yMarkSq,listOfdfChromSize,listOfdfMarkPosSq,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3
                    ,markNewLine2=markNewLine,mylheight2=mylheight)
        }

      } #   if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") )

      # end square marks  #

      #####################################
      #   centromere with marks - cen marks
      #####################################

      if (exists("parlistOfdfMarkPosDataCen") & exists("dfMarkColorInternal") ) {

        xMarkCen<-yMarkCen<-list()

        for (k in 1:length(parlistOfdfMarkPosDataCen)){
          yMarkCen1<-NULL
          xMarkCen1<-NULL
          for (i in 1:nrow(parlistOfdfMarkPosDataCen[[k]])){
            corr_index <- which(names(xcoordCents) %in% names(parlistOfdfMarkPosDataCen)[[k]] )
            rowIndex <- (parlistOfdfMarkPosDataCen[[k]][,"neworder"][i])
            ysup <- max(ycoordCents[[corr_index]][rowIndex,] ) # was3
            yinf <- min(ycoordCents[[corr_index]][rowIndex,] ) # was 1
            ymiddle <- (abs(ysup-yinf)/2) + yinf

            yMarkCen1[[i]]<-c(yinf,yinf,ymiddle,ysup,ysup,ymiddle)
            attr(yMarkCen1[[i]],"rowIndex")<-rowIndex

            xMarkCen1[[i]]<-xcoordCents[[corr_index]][ rowIndex ,]
            attr(xMarkCen1[[i]],"rowIndex")<-rowIndex

          } # each mark

          yMarkCen[[k]]<-yMarkCen1
          xMarkCen[[k]]<-xMarkCen1
          attr(yMarkCen[[k]], "spname") <- names(parlistOfdfMarkPosDataCen)[[k]] # added 1.14
          attr(xMarkCen[[k]], "spname") <- names(parlistOfdfMarkPosDataCen)[[k]] # added 1.14
        } # each df

        if(!is.null(fixCenBorder)){
          if (fixCenBorder){
            fixCenBorder2<-TRUE
          } else {
            fixCenBorder2<-FALSE
          }
        } else {
          fixCenBorder2<-FALSE
        }

        if(cenFormat=="rounded"){
          # pts<- seq(-pi/2, pi*1.5, length.out = n*4)
          # ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

          newLongyCenMarks <- newLongxCenMarks<-list()

          for (s in 1:length(yMarkCen) ) {

            xyCoordsCenMarks <- mapXYCen(1 , (length(yMarkCen[[s]]) ) ,
                                         yMarkCen[[s]],
                                         xMarkCen[[s]] ,
                                         ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]]
            )

            newLongxCenMarks[[s]] <- xyCoordsCenMarks$roundedX
            newLongyCenMarks[[s]] <- xyCoordsCenMarks$roundedY

          } # for s

          # newLongyCenMarks<-newLongyCenMarks[!is.na(newLongyCenMarks)]
          # newLongxCenMarks<-newLongxCenMarks[!is.na(newLongxCenMarks)]

          for(s in 1: length(newLongyCenMarks)) {
            attr(newLongyCenMarks[[s]], "spname")<- attr(yMarkCen[[s]], "spname")
            attr(newLongxCenMarks[[s]], "spname")<- attr(xMarkCen[[s]], "spname")

            for (a in 1: length(newLongyCenMarks[[s]])){
              attr(newLongyCenMarks[[s]][[a]],"rowIndex")<- attr(yMarkCen[[s]][[a]],"rowIndex")
              attr(newLongxCenMarks[[s]][[a]],"rowIndex")<- attr(xMarkCen[[s]][[a]],"rowIndex")
            }
          }

        } # rC

        ###########################################
        #
        #   plotting labels inline CENTR. marks
        #
        ###########################################

        # cen labels
        booleanColorInternalMarkCen<- legend=="inline" & exists("dfMarkColorInternal") & exists("parlistOfdfMarkPosDataCen") & circularPlot==FALSE

        if(booleanColorInternalMarkCen)  {
          # textLabel(xMarkCen,yMarkCen,listOfdfChromSize,parlistOfdfMarkPosDataCen,specialChrSpacing,chrSpacing,markLabelSize,pattern,TRUE)
          textLabelCen(xMarkCen,yMarkCen,listOfdfChromSize,parlistOfdfMarkPosDataCen,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3)
        } # if


        #####################
        #   marks CENTROMERE
        #####################

        if(circularPlot) {

          if (cenFormat=="triangle"){

            xlistNew <- xHortoVerRoundCen(xMarkCen,diffXRounded)
            names(xlistNew) <- names(xMarkCen)

            yMarkPer <- markMapPer(yMarkCen,y)

            ylistTransMark<-transyListMark(yMarkPer,ylistTransChrSimple)
          } else if (cenFormat=="rounded") {

            xlistNew <- xHortoVerRoundCen(newLongxCenMarks,diffXRounded)
            names(xlistNew) <- names(newLongxCenMarks)

            yMarkPer<-markMapPer(newLongyCenMarks,y)

            ylistTransMark<-transyListMark(yMarkPer,ylistTransChrSimple)
          }

          # from mimic
          circleMapsMarksCen  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMark,xlistNew,n,0,chrWidth,
                                                rotation=rotation)

          drawCenMarks(circleMapsMarksCen,dfMarkColorInternal
                       ,parlistOfdfMarkPosDataCen,lwd.chr,fixCenBorder2,chrColor)

          if(legend=="inline"){
            circleMapsMarksLabelCen  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMark,xlistNew,n,
                                                       labelSpacing ,chrWidth,rotation=rotation)
            circLabelMark(bannedMarkName3,circleMapsMarksLabelCen,parlistOfdfMarkPosDataCen, markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY)
          }

        } else { # no circular

          lwd.chrMod <- ifelse( gishCenBorder & cSbool ,
                                lwd.chr*hideCenLines,
                                lwd.chr)

          if (cenFormat=="triangle") {
            lapply(1:length(xMarkCen), function(m) mapply(function(w,x,y,z)
              graphics::polygon(
                x=x,
                y=y,
                col = w ,
                lwd=lwd.chrMod,
                border = ifelse(fixCenBorder2|gishCenBorder,
                                ifelse(fixCenBorder2,chrBorderColor2,w),
                                dfMarkColorInternal$markBorderColor[match(z, dfMarkColorInternal$markName)] # z outside
                ) # ifelse
              ), # p
              w=dfMarkColorInternal$markColor[match(parlistOfdfMarkPosDataCen[[m]]$markName, dfMarkColorInternal$markName)],
              x=xMarkCen[[m]],
              y=yMarkCen[[m]],
              z=parlistOfdfMarkPosDataCen[[m]]$markName # ifelse here gives error

            ) # mapply
            ) #l

            #
            #   lateral black lines
            #

            if(length(cenColor2=="white") & cSbool ) {
              if(cenColor2=="white") {
                lapply(1:length(xyLinesX1), function(w) mapply(function(x,y) graphics::lines(x=x, y=y,
                                                                                             col=chrBorderColor2,
                                                                                             lwd=lwd.chr
                ),
                x=xyLinesX1[[w]],
                y=xyLinesY1[[w]]
                ) #m
                ) #l

                lapply(1:length(xyLinesX2), function(w) mapply(function(x,y) graphics::lines(x=x, y=y,
                                                                                             col=chrBorderColor2,
                                                                                             lwd=lwd.chr
                ),
                x=xyLinesX2[[w]],
                y=xyLinesY2[[w]]
                ) #m
                ) #l

              } # white lines
            }

          } else if (cenFormat=="rounded") { # roundedcen
            lapply(1:length(newLongyCenMarks), function(m) mapply(function(w,x,y,z)
              graphics::polygon(
                x=x,
                y=y,
                col = w ,
                lwd=lwd.chrMod,
                border=ifelse(fixCenBorder2|gishCenBorder,
                              ifelse(fixCenBorder2,chrBorderColor2,w),
                              dfMarkColorInternal$markBorderColor[match(z, dfMarkColorInternal$markName)] # z outside
                ) # ifelse
              ), # p
              w=dfMarkColorInternal$markColor[match(parlistOfdfMarkPosDataCen[[m]]$markName, dfMarkColorInternal$markName)],
              x=newLongxCenMarks[[m]],
              y=newLongyCenMarks[[m]],
              z=parlistOfdfMarkPosDataCen[[m]]$markName # ifelse here gives error
            ) # mapply
            ) #l

            #
            #   lateral black lines
            #

            if(length(cenColor2=="white") & cSbool ) {
              if(cenColor2=="white") {
                lapply(1:length(xyRoundLinesX1), function(w) mapply(function(x,y) graphics::lines(x=x, y=y,
                                                                                                  col=chrBorderColor2,
                                                                                                  lwd=lwd.chr
                ),
                x=xyRoundLinesX1[[w]],
                y=xyRoundLinesY1[[w]]
                ) #m
                ) #l

                lapply(1:length(xyRoundLinesX2), function(w) mapply(function(x,y) graphics::lines(x=x, y=y,
                                                                                                  col=chrBorderColor2,
                                                                                                  lwd=lwd.chr
                ),
                x=xyRoundLinesX2[[w]],
                y=xyRoundLinesY2[[w]]
                ) #m
                ) #l

              }
            } # white lines

          } # rC

        } # circular bool

      } #     end centromeres with marks


      ##########################################################################################################3
      #
      #                                               painting Marks monocen cenStyle marks
      #
      ############################################################################################################

      if (exists("parlistOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ){
        # message(crayon::green(paste0("monocen. marks section start" ) ) )

        yMarkLine<-xMarkRightLine<-xMarkLeftLine<-xMarkCenStyle<-yMarkCenStyle<-listOfdfMarkPosCenStyle<-list()

        j<-1

        for (k in 1:length(parlistOfdfMarkPosMonocen)) {
          currName<-names(parlistOfdfMarkPosMonocen)[[k]]
          if(nrow(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="cenStyle"),])>0){
            listOfdfMarkPosCenStyle<-c(listOfdfMarkPosCenStyle,list(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="cenStyle"),]))
            names(listOfdfMarkPosCenStyle)[[j]]<-currName
            j<-j+1
          }
        }
        if(length(listOfdfMarkPosCenStyle)>0) {

          for (sm in 1:length(listOfdfMarkPosCenStyle)) {
            yMark1<-yMark2<-NULL
            xMark1<-xMark2Min<-xMark2Max<-NULL
            corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosCenStyle)[[sm]] )

            for (m in 1:nrow(listOfdfMarkPosCenStyle[[sm]]) ){
              ifelse(listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
              ifelse(listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
              ifelse(listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
              rowIndex<- ( nrow(listOfdfChromSize[[corr_index]] ) * longORshort ) + listOfdfMarkPosCenStyle[[sm]][,"neworder"][m]
              armStart<-ym[[corr_index]][rowIndex,column]

              yprox <- armStart +
                (listOfdfMarkPosCenStyle[[sm]][m,"markDistCen"]                                                             *normalizeToOne*mySign)

              yter <- armStart +
                ( sum( listOfdfMarkPosCenStyle[[sm]][m,"markDistCen"] , listOfdfMarkPosCenStyle[[sm]][m,"markSize"], na.rm=TRUE ) *normalizeToOne*mySign )

              ymiddle<- abs(yprox-yter)/2

              yMark1[[m]]<-if(longORshort==0) {
                c(yprox,yter,(yter+ymiddle),yprox,yter,(yter+ymiddle))
              } else {
                c(yter,yprox,(yprox+ymiddle),yter,yprox,(yprox+ymiddle))
              }

              attr(yMark1[[m]],"arm")<-listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]
              attr(yMark1[[m]],"rowIndex")<-rowIndex

              ##
              yMark2[[m]]<-if(longORshort==0) {c(yprox,yter)} else {c(yter,yprox)}
              attr(yMark2[[m]],"arm")<-listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]
              attr(yMark2[[m]],"rowIndex")<-rowIndex
              ##

              # attr(yMark1[[m]],"armStart")<-armStart

              xMark1[[m]]<- xm[[corr_index]][ listOfdfMarkPosCenStyle[[sm]][,"neworder"][m],]


              xMark2Min[[m]]<-rep(min(xMark1[[m]]),2)
              xMark2Max[[m]]<-rep(max(xMark1[[m]]),2)

              meanXM1<- (min(xMark1[[m]] ) +max(xMark1[[m]]) )/2
              xMark1[[m]][3] <- meanXM1 # 3 as 6
              xMark1[[m]] <- c(xMark1[[m]], xMark1[[m]][4]) # 5
              xMark1[[m]] <- c(xMark1[[m]], xMark1[[m]][3]) # 6

              attr(xMark1[[m]],"rowIndex")<-rowIndex
              attr(xMark2Min[[m]],"rowIndex")<-rowIndex
              attr(xMark2Max[[m]],"rowIndex")<-rowIndex

            }
            yMarkCenStyle[[sm]]<-yMark1
            attr(yMarkCenStyle[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]

            yMarkLine[[sm]]<- yMark2
            attr(yMarkLine[[sm]], "spname")    <-names(listOfdfMarkPosCenStyle)[[sm]]

            ##
            xMarkCenStyle[[sm]]<-xMark1
            attr(xMarkCenStyle[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]

            xMarkRightLine[[sm]]<-xMark2Max
            xMarkLeftLine[[sm]] <-xMark2Min

            attr(xMarkLeftLine[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]
            attr(xMarkRightLine[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]

            ##

          } # end for

          if(cenFormat=="rounded") {
            pts<- seq(-pi/2, pi*1.5, length.out = n*4)
            ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

            newLongyCenMarksStyle <- newLongxCenMarksStyle<-list()

            for (s in 1:length(yMarkCenStyle) ) {

              xyCoordsCenMarksStyle <- mapXYCen(1 , (length(yMarkCenStyle[[s]]) ) ,
                                                yMarkCenStyle[[s]],
                                                xMarkCenStyle[[s]] ,
                                                ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]],
                                                mimic=TRUE
              )

              newLongxCenMarksStyle[[s]] <- xyCoordsCenMarksStyle$roundedX
              newLongyCenMarksStyle[[s]] <- xyCoordsCenMarksStyle$roundedY

            } # for s

            for(s in 1: length(newLongyCenMarksStyle)) {
              attr(newLongyCenMarksStyle[[s]], "spname")<- attr(yMarkCenStyle[[s]], "spname")
              attr(newLongxCenMarksStyle[[s]], "spname")<- attr(xMarkCenStyle[[s]], "spname")

              for (a in 1: length(newLongyCenMarksStyle[[s]])){
                attr(newLongyCenMarksStyle[[s]][[a]],"rowIndex")<- attr(yMarkCenStyle[[s]][[a]],"rowIndex")
                attr(newLongxCenMarksStyle[[s]][[a]],"rowIndex")<- attr(xMarkCenStyle[[s]][[a]],"rowIndex")

              }
            }

          } # rC

          ###############################
          #                             #
          #   add marks to plot monocen #
          #                             #
          ###############################

          if(cenFormat=="triangle"){
            mimicCenPlotMark(squareness, xMarkCenStyle, yMarkCenStyle,
                             defCenStyleCol,
                             listOfdfMarkPosCenStyle,
                             chrWidth, #use for calc r2
                             specialChrWidth,
                             yfactor,
                             n,
                             lwd.mimicCen2,
                             listOfdfChromSize,
                             circularPlot,
                             y,
                             markLabelSize,
                             separFactor,
                             labelSpacing,circleCenter,circleCenterY,radius,
                             ylistTransChrSimple,
                             rotation=rotation,labelOutwards,
                             yMarkLine,xMarkRightLine,xMarkLeftLine,
                             x)
          } else if(cenFormat=="rounded") {
            mimicCenPlotMark(squareness, newLongxCenMarksStyle, newLongyCenMarksStyle,
                             defCenStyleCol,
                             listOfdfMarkPosCenStyle,
                             chrWidth, #use for calc r2
                             specialChrWidth,
                             yfactor,
                             n,
                             lwd.mimicCen2,
                             listOfdfChromSize,
                             circularPlot,
                             y,
                             markLabelSize,
                             separFactor,
                             labelSpacing,circleCenter,circleCenterY,radius,
                             ylistTransChrSimple,
                             rotation=rotation,labelOutwards,
                             yMarkLine,xMarkRightLine,xMarkLeftLine,
                             x
                             ,cenFormat)
          } # rC

          ######## new ######################################## inside cenStyle

          xMarkCenStyleBody<-yMarkCenStyleBody<-list()

            for (sm in 1:length(listOfdfMarkPosCenStyle) ) {

              yMark1body <- NULL
              xMark1body <- NULL

              corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosCenStyle)[[sm]] )

              for (m in 1:nrow(listOfdfMarkPosCenStyle[[sm]]) ){

                ifelse(listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
                ifelse(listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
                ifelse(listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
                rowIndex<- ( nrow(listOfdfChromSize[[corr_index]] ) * longORshort ) + listOfdfMarkPosCenStyle[[sm]][,"neworder"][m]

                armStart<-ym[[corr_index]][rowIndex,column]
                yprox <- armStart +
                  (listOfdfMarkPosCenStyle[[sm]][m,"markDistCen"]                                                             *normalizeToOne*mySign)


                yter <- armStart +
                  ( sum( listOfdfMarkPosCenStyle[[sm]][m,"markDistCen"] , listOfdfMarkPosCenStyle[[sm]][m,"markSize"], na.rm=TRUE ) *normalizeToOne*mySign )

                ymiddle<- abs(yprox-yter)/2

                yMark1body[[m]]<-if(longORshort==0) {
                  c(yter,yter,(yter+ymiddle),yprox,yprox,(yter+ymiddle)) # in long arm yter is smaller
                } else {
                  c(yprox,yprox,(yprox+ymiddle),yter,yter,(yprox+ymiddle))
                }

                # yMark1body[[m]] <-if(longORshort==0) {c(yter,yter,yprox,yprox)} else {c(yprox,yprox,yter,yter)}

                attr(yMark1body[[m]],"arm")<-listOfdfMarkPosCenStyle[[sm]][m,"chrRegion"]
                attr(yMark1body[[m]],"rowIndex")<-rowIndex
                #
                fourX<-xm[[corr_index]][listOfdfMarkPosCenStyle[[sm]][,"neworder"][m],]

                meanXM1<- (min(fourX ) + max(fourX) )/2

                xMark1body[[m]] <- rep(c(min(fourX),max(fourX),meanXM1),2)

                attr(xMark1body[[m]],"rowIndex")<-rowIndex

              } # each mark

              yMarkCenStyleBody[[sm]]<-yMark1body
              attr(yMarkCenStyleBody[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]

              xMarkCenStyleBody[[sm]]<-xMark1body
              attr(xMarkCenStyleBody[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]

            } # for each df

            if (cenFormat=="rounded") {

              pts<- seq(-pi/2, pi*1.5, length.out = n*4)
              ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )


              newLongyCenMarksBodyStyle <- newLongxCenMarksBodyStyle<-list()

              for (s in 1:length(yMarkCenStyleBody) ) {

                xyCoordsCenMarksStyle <- mapXYCen(1 , (length(yMarkCenStyleBody[[s]]) ) ,
                                                  yMarkCenStyleBody[[s]],
                                                  xMarkCenStyleBody[[s]] ,
                                                  ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]]

                )

                newLongxCenMarksBodyStyle[[s]] <- xyCoordsCenMarksStyle$roundedX
                newLongyCenMarksBodyStyle[[s]] <- xyCoordsCenMarksStyle$roundedY

              } # for s

              for(s in 1: length(newLongyCenMarksBodyStyle)) {
                attr(newLongyCenMarksBodyStyle[[s]], "spname")<- attr(yMarkCenStyleBody[[s]], "spname")
                attr(newLongxCenMarksBodyStyle[[s]], "spname")<- attr(xMarkCenStyleBody[[s]], "spname")

                for (a in 1: length(newLongyCenMarksBodyStyle[[s]])){
                  attr(newLongyCenMarksBodyStyle[[s]][[a]],"rowIndex")<- attr(yMarkCenStyleBody[[s]][[a]],"rowIndex")
                  attr(newLongxCenMarksBodyStyle[[s]][[a]],"rowIndex")<- attr(xMarkCenStyleBody[[s]][[a]],"rowIndex")

                }
              } # f

            } # rC

          if(cenFormat=="triangle"){
            mimicCenPlotMarkInside(pattern,bannedMarkName3,squareness, xMarkCenStyleBody, yMarkCenStyleBody ,
                                   defCenStyleCol,
                                   dfMarkColorInternal,
                                   listOfdfMarkPosCenStyle,
                                   chrWidth, #use for calc r2
                                   specialChrWidth,
                                   yfactor,
                                   n,
                                   lwd.mimicCen2,
                                   listOfdfChromSize,
                                   circularPlot,
                                   y,
                                   markLabelSize,
                                   separFactor,
                                   labelSpacing,circleCenter,circleCenterY,radius,
                                   ylistTransChrSimple,
                                   rotation=rotation,labelOutwards,
                                   yMarkLine,xMarkRightLine,xMarkLeftLine,
                                   x,lwd.chr,legend)
          } else if(cenFormat=="rounded"){
            mimicCenPlotMarkInside(pattern,bannedMarkName3,squareness
                                   ,newLongxCenMarksBodyStyle, newLongyCenMarksBodyStyle,
                                   defCenStyleCol,
                                   dfMarkColorInternal,
                                   listOfdfMarkPosCenStyle,
                                   chrWidth, #use for calc r2
                                   specialChrWidth,
                                   yfactor,
                                   n,
                                   lwd.mimicCen2,
                                   listOfdfChromSize,
                                   circularPlot,
                                   y,
                                   markLabelSize,
                                   separFactor,
                                   labelSpacing,circleCenter,circleCenterY,radius,
                                   ylistTransChrSimple,
                                   rotation=rotation,labelOutwards,
                                   yMarkLine,xMarkRightLine,xMarkLeftLine,
                                   x,
                                   lwd.chr,
                                   legend
                                   ,cenFormat)
          } # rC

        } else { remove(listOfdfMarkPosCenStyle) } #     if(length(listOfdfMarkPosCenStyle)>0)

      } # if exists monocen end painting marks

      ###########################################
      #
      #   plotting labels inline mimicCENTR. monocen
      #
      ###########################################

      # cen labels
      booleanColorInternalMimicMarkCen<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosCenStyle") & circularPlot==FALSE

      if(booleanColorInternalMimicMarkCen)  {
        textLabelCen(xMarkCenStyleBody,yMarkCenStyleBody,listOfdfChromSize,listOfdfMarkPosCenStyle,specialChrSpacing,chrSpacing,markLabelSize,pattern,
                     bannedMarkName3)
      } # if

      ##################################################################################################
      #
      #                                        painting Marks cenStyle holocen
      #
      ##################################################################################################

      if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
        # message(crayon::green(paste0("holocen. marks section start" ) ) )
        yMarkLine<-xMarkRightLine<-xMarkLeftLine<-xMarkCenStyle<-yMarkCenStyle<-listOfdfMarkPosCenStyle<-list()

        j<-1
        for (k in 1:length(parlistOfdfMarkPosHolocen)) {
          currName<-names(parlistOfdfMarkPosHolocen)[[k]]
          if(nrow(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="cenStyle"),])>0){
            listOfdfMarkPosCenStyle<-c(listOfdfMarkPosCenStyle,list(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="cenStyle"),]) )
            names(listOfdfMarkPosCenStyle)[[j]]<-currName
            j<-j+1
          }
        }

        if(length(listOfdfMarkPosCenStyle)>0){
          for (sm in 1:length(listOfdfMarkPosCenStyle)) {

            yMark1<-yMark2<-NULL
            xMark1<-xMark2Min<-xMark2Max<-NULL
            corr_index<-which(names(ym) %in% names(listOfdfMarkPosCenStyle)[[sm]] )

            for (m in 1:nrow(listOfdfMarkPosCenStyle[[sm]])){
              rowIndex<-(listOfdfMarkPosCenStyle[[sm]][,"neworder"][m])
              chrStart<-ym[[corr_index]][rowIndex ,2]
              yinf <- chrStart +                        # was ysup
                (listOfdfMarkPosCenStyle[[sm]][m,"markPos"]                                                          *normalizeToOne)

              ysup <- chrStart +
                (  sum(listOfdfMarkPosCenStyle[[sm]][m,"markPos"],listOfdfMarkPosCenStyle[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

              ymiddle<- (yinf+ysup)/2

              yMark1[[m]]<-c(ysup,yinf,ymiddle,ysup,yinf,ymiddle)

              yMark2[[m]]<-c(yinf,ysup)

              attr(yMark1[[m]],"rowIndex")<-rowIndex
              attr(yMark2[[m]],"rowIndex")<-rowIndex

              # xMark1[[m]]<-xm[[corr_index]][listOfdfMarkPosCenStyle[[sm]][,"neworder"][m],]

              fourX<-      xm[[corr_index]][listOfdfMarkPosCenStyle[[sm]][,"neworder"][m],]

              meanXM1<- (min(fourX ) + max(fourX) )/2

              xMark1[[m]] <- c(max(fourX),max(fourX),meanXM1, min(fourX),min(fourX),meanXM1 )

              xMark2Min[[m]]<-rep(min(xMark1[[m]]),2)
              xMark2Max[[m]]<-rep(max(xMark1[[m]]),2)

              attr(xMark1[[m]],"rowIndex")<-rowIndex
              attr(xMark2Min[[m]],"rowIndex")<-rowIndex
              attr(xMark2Max[[m]],"rowIndex")<-rowIndex

            }

            yMarkCenStyle[[sm]]<-yMark1
            attr(yMarkCenStyle[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]

            yMarkLine[[sm]]<- yMark2
            attr(yMarkLine[[sm]], "spname")    <-names(listOfdfMarkPosCenStyle)[[sm]]

            #

            xMarkCenStyle[[sm]]<-xMark1
            xMarkRightLine[[sm]]<-xMark2Max
            xMarkLeftLine[[sm]] <-xMark2Min
            attr(xMarkCenStyle[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]
            attr(xMarkLeftLine[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]
            attr(xMarkRightLine[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]

          } # end for

          if(cenFormat=="rounded") {
            pts<- seq(-pi/2, pi*1.5, length.out = n*4)
            ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

            newLongyCenMarksStyle <- newLongxCenMarksStyle<-list()

            for (s in 1:length(yMarkCenStyle) ) {

              xyCoordsCenMarksStyle <- mapXYCen(1 , (length(yMarkCenStyle[[s]]) ) ,
                                                yMarkCenStyle[[s]],
                                                xMarkCenStyle[[s]] ,
                                                ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]],
                                                mimic=TRUE
              )

              newLongxCenMarksStyle[[s]] <- xyCoordsCenMarksStyle$roundedX
              newLongyCenMarksStyle[[s]] <- xyCoordsCenMarksStyle$roundedY

            } # for s

            for(s in 1: length(newLongyCenMarksStyle)) {
              attr(newLongyCenMarksStyle[[s]], "spname")<- attr(yMarkCenStyle[[s]], "spname")
              attr(newLongxCenMarksStyle[[s]], "spname")<- attr(xMarkCenStyle[[s]], "spname")

              for (a in 1: length(newLongyCenMarksStyle[[s]])){
                attr(newLongyCenMarksStyle[[s]][[a]],"rowIndex")<- attr(yMarkCenStyle[[s]][[a]],"rowIndex")
                attr(newLongxCenMarksStyle[[s]][[a]],"rowIndex")<- attr(xMarkCenStyle[[s]][[a]],"rowIndex")
              }
            }
          } # rC

          #####################
          #   add marks to plot holocen
          #####################

          if(cenFormat=="rounded") {
            mimicCenPlotMark(squareness, newLongxCenMarksStyle, newLongyCenMarksStyle,
                             defCenStyleCol,
                             listOfdfMarkPosCenStyle,
                             chrWidth, #use for calc r2
                             specialChrWidth,
                             yfactor,
                             n,
                             lwd.mimicCen2,
                             listOfdfChromSize,
                             circularPlot,
                             y,
                             markLabelSize,
                             separFactor,
                             labelSpacing,circleCenter,circleCenterY,radius,
                             ylistTransChrSimple,
                             rotation=rotation,labelOutwards,
                             yMarkLine,xMarkRightLine,xMarkLeftLine,
                             x
                             ,cenFormat) #

          } else if(cenFormat=="triangle") {
            mimicCenPlotMark(squareness, xMarkCenStyle, yMarkCenStyle,
                             defCenStyleCol,
                             listOfdfMarkPosCenStyle,
                             chrWidth, #use for calc r2
                             specialChrWidth,
                             yfactor,
                             n,
                             lwd.mimicCen2,
                             listOfdfChromSize,
                             circularPlot,
                             y,
                             markLabelSize,
                             separFactor,
                             labelSpacing,circleCenter,circleCenterY,radius,
                             ylistTransChrSimple,
                             rotation=rotation,labelOutwards,
                             yMarkLine,xMarkRightLine,xMarkLeftLine,
                             x) #
          }

          ######## new ######################################## inside cenStyle holocen

          xMarkCenStyleBody<-yMarkCenStyleBody<-list()

            for (sm in 1:length(listOfdfMarkPosCenStyle) ) {

              yMark1body <- NULL
              xMark1body <- NULL

              corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosCenStyle)[[sm]] )

              for (m in 1:nrow(listOfdfMarkPosCenStyle[[sm]]) ) {
                rowIndex<-(listOfdfMarkPosCenStyle[[sm]][,"neworder"][m])
                chrStart<-ym[[corr_index]][rowIndex ,2]

                yinf <- chrStart +                        # was ysup
                  (listOfdfMarkPosCenStyle[[sm]][m,"markPos"]                                                          *normalizeToOne)

                ysup <- chrStart +
                  (  sum(listOfdfMarkPosCenStyle[[sm]][m,"markPos"],listOfdfMarkPosCenStyle[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

                ymiddle<-(ysup+yinf)/2

                yMark1body[[m]]<-c(yinf,yinf,ymiddle,ysup,ysup,ymiddle)

                attr(yMark1body[[m]],"rowIndex")<-rowIndex

                fourX <-     xm[[corr_index]][listOfdfMarkPosCenStyle[[sm]][,"neworder"][m],]

                meanXM1<- (min(fourX ) + max(fourX) )/2

                # xMark1body[[m]] <- rep(c(min(fourX),max(fourX)),2)
                xMark1body[[m]] <- rep(c(min(fourX),max(fourX),meanXM1),2)


                attr(xMark1body[[m]],"rowIndex")<-rowIndex
              } # each mark

              yMarkCenStyleBody[[sm]]<-yMark1body
              attr(yMarkCenStyleBody[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]

              xMarkCenStyleBody[[sm]]<-xMark1body
              attr(xMarkCenStyleBody[[sm]], "spname")<-names(listOfdfMarkPosCenStyle)[[sm]]

            } # for each df

            if(cenFormat=="rounded") {
              pts<- seq(-pi/2, pi*1.5, length.out = n*4)
              ptsl<-split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

              newLongyCenMarksBodyStyle <- newLongxCenMarksBodyStyle<-list()

              for (s in 1:length(yMarkCenStyleBody) ) {

                xyCoordsCenMarksStyle <- mapXYCen(1 , (length(yMarkCenStyleBody[[s]]) ) ,
                                                  yMarkCenStyleBody[[s]],
                                                  xMarkCenStyleBody[[s]] ,
                                                  ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]]

                )

                newLongxCenMarksBodyStyle[[s]] <- xyCoordsCenMarksStyle$roundedX
                newLongyCenMarksBodyStyle[[s]] <- xyCoordsCenMarksStyle$roundedY

              } # for s

              for(s in 1: length(newLongyCenMarksBodyStyle)) {
                attr(newLongyCenMarksBodyStyle[[s]], "spname")<- attr(yMarkCenStyleBody[[s]], "spname")
                attr(newLongxCenMarksBodyStyle[[s]], "spname")<- attr(xMarkCenStyleBody[[s]], "spname")

                for (a in 1: length(newLongyCenMarksBodyStyle[[s]])){
                  attr(newLongyCenMarksBodyStyle[[s]][[a]],"rowIndex")<- attr(yMarkCenStyleBody[[s]][[a]],"rowIndex")
                  attr(newLongxCenMarksBodyStyle[[s]][[a]],"rowIndex")<- attr(xMarkCenStyleBody[[s]][[a]],"rowIndex")

                }
              }
            } # rC


            # } # chromatids

          if(cenFormat=="triangle") {
            mimicCenPlotMarkInside(pattern,bannedMarkName3,squareness, xMarkCenStyleBody, yMarkCenStyleBody ,
                                   defCenStyleCol,
                                   dfMarkColorInternal,
                                   listOfdfMarkPosCenStyle,
                                   chrWidth, #use for calc r2
                                   specialChrWidth,
                                   yfactor,
                                   markN,
                                   lwd.mimicCen2,
                                   listOfdfChromSize,
                                   circularPlot,
                                   y,
                                   markLabelSize,
                                   separFactor,
                                   labelSpacing,circleCenter,circleCenterY,radius,
                                   ylistTransChrSimple,
                                   rotation=rotation,labelOutwards,
                                   yMarkLine,xMarkRightLine,xMarkLeftLine,
                                   x,lwd.chr,legend)
          } else if(cenFormat=="rounded") {

            mimicCenPlotMarkInside(pattern,bannedMarkName3,squareness, newLongxCenMarksBodyStyle, newLongyCenMarksBodyStyle,
                                   defCenStyleCol,
                                   dfMarkColorInternal,
                                   listOfdfMarkPosCenStyle,
                                   chrWidth, #use for calc r2
                                   specialChrWidth,
                                   yfactor,
                                   markN,
                                   lwd.mimicCen2,
                                   listOfdfChromSize,
                                   circularPlot,
                                   y,
                                   markLabelSize,
                                   separFactor,
                                   labelSpacing,circleCenter,circleCenterY,radius,
                                   ylistTransChrSimple,
                                   rotation=rotation,labelOutwards,
                                   yMarkLine,xMarkRightLine,xMarkLeftLine,
                                   x,
                                   lwd.chr
                                   ,legend
                                   ,cenFormat)
          } # rC

          ########

        } else {  # if(length(listOfdfMarkPosCenStyle)>0){
          remove(listOfdfMarkPosCenStyle)
        }

        ###########################################
        #
        #   plotting labels inline mimicCENTR. holocen
        #
        ###########################################

        # cen labels
        booleanColorInternalMimicMarkCen<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosCenStyle") & circularPlot==FALSE

        if(booleanColorInternalMimicMarkCen)  {

          textLabelCen(xMarkCenStyleBody,yMarkCenStyleBody,listOfdfChromSize,listOfdfMarkPosCenStyle,
                    specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3)
        } # if

      } #   if (exists("parlistOfdfMarkPosHolocen") & ("dfMarkColorInternal") )
      # end cenStyle marks

      ## end cenStyle


      ## ex protein

      ##########################################################################################################3
      #
      #                           painting Marks monocen              ex protein marks
      #
      ############################################################################################################


  if (exists("parlistOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ) {
        yMarkExProt<-xMarkExProt<-listOfdfMarkPosExProt<-list()
        xMarkExProt_2nd <- list()

        j<-1

        for (k in 1:length(parlistOfdfMarkPosMonocen)) {
          currName<-names(parlistOfdfMarkPosMonocen)[[k]]
          if(nrow(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="exProtein"),])>0){
            listOfdfMarkPosExProt<-c(listOfdfMarkPosExProt,list(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="exProtein"),]))
            names(listOfdfMarkPosExProt)[[j]]<-currName
            j<-j+1
          }
        }

        if(length(listOfdfMarkPosExProt)>0){
          for (sm in 1:length(listOfdfMarkPosExProt)) {
            yMark1<-NULL
            xMark1<-NULL
            xMark1_2nd<-NULL

            corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosExProt)[[sm]] )

            for (m in 1:nrow(listOfdfMarkPosExProt[[sm]]) ){
              ifelse(listOfdfMarkPosExProt[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
              # ifelse(listOfdfMarkPosExProt[[sm]][m,"style"]=="exProtein", squareSide<- "right", squareSide<- "left")
              squareSide<- "exProtein"
              ifelse(listOfdfMarkPosExProt[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
              ifelse(listOfdfMarkPosExProt[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
              rowIndex<- nrow(listOfdfChromSize[[corr_index]] ) * longORshort + (listOfdfMarkPosExProt[[sm]][,"neworder"][m])

              armStart<-ym[[corr_index]][rowIndex,column]

              yprox <- armStart +
                (listOfdfMarkPosExProt[[sm]][m,"markDistCen"]      *normalizeToOne*mySign)

              yter <- armStart +
                ( sum( listOfdfMarkPosExProt[[sm]][m,"markDistCen"]
                       , listOfdfMarkPosExProt[[sm]][m,"markSize"], na.rm=TRUE ) *normalizeToOne*mySign )

              if("markDistCenProtein" %in% colnames(listOfdfMarkPosExProt[[sm]] ) ){
                if(!is.na(listOfdfMarkPosExProt[[sm]][m,"markDistCenProtein"] ) ) {
                  yprox <- armStart +
                    (listOfdfMarkPosExProt[[sm]][m,"markDistCenProtein"] * normalizeToOne*mySign)
                  yter <- armStart +
                    ( sum( listOfdfMarkPosExProt[[sm]][m,"markDistCenProtein"]
                           , listOfdfMarkPosExProt[[sm]][m,"markSizeProtein"], na.rm=TRUE ) *normalizeToOne*mySign )
                }
              }

              if(longORshort==0) {
                ybase<-yter
                ysize<-abs(yprox-yter)
              } else {
                ybase<-yprox
                ysize<-abs(yter-yprox)
              }

              fourX <- xm[[corr_index]][listOfdfMarkPosExProt[[sm]][,"neworder"][m],]
              xsizeOrig <- xsize <-max(fourX)-min(fourX)

              if(longORshort==0) {
                # long arm
                yProtein<-c(0
                            ,0     + (pMarkFac*3*(xsizeOrig*normalizeToOne) ) # good!
                            ,ysize - (pMarkFac*3*(xsizeOrig*normalizeToOne) )
                            ,ysize
                            ,0
                )
              } else {
                # short arm
                yProtein<-c(ysize
                            ,ysize - (pMarkFac*3*(xsizeOrig*normalizeToOne) )
                            ,0     + (pMarkFac*3*(xsizeOrig*normalizeToOne) ) # good!
                            ,0
                            ,ysize
                )
              }

              yMark1[[m]]<- yProtein + ybase

              attr(yMark1[[m]],"arm") <-listOfdfMarkPosExProt[[sm]][m,"chrRegion"]
              attr(yMark1[[m]],"rowIndex")<-rowIndex
              attr(yMark1[[m]],"squareSide")<-squareSide

              #
              #   right side
              #

              xsize <- xsize/2
              xWidth <- pMarkFac * xsizeOrig

              xPosWid <- startPos * xWidth
              xbase <- min(fourX) + xsize + xPosWid

              xProtein<-c(xsize
                          ,xsize + xWidth
                          ,xsize + xWidth
                          ,xsize
                          ,xsize
              )

              # +1 represents base (min)
              xMark1[[m]] <- xProtein + xbase

              attr(xMark1[[m]],"rowIndex")<-rowIndex

              # For chromatids, Proteins in both chromatids

              xbase <- min(fourX) - xPosWid

              xsize<-max(fourX)-min(fourX)

              xsize<-xsize/2
              # xsize<-xsize-xModifierMono

              #
              #   left side
              #

              xProtein<-c(0
                          , -xWidth
                          , -xWidth
                          ,0
                          ,0
              )

              xMark1_2nd[[m]] <- xbase + xProtein

              attr(xMark1_2nd[[m]],"rowIndex")<-rowIndex


            } # for each mark

            yMarkExProt[[sm]]<-yMark1
            attr(yMarkExProt[[sm]], "spname")<-names(listOfdfMarkPosExProt)[[sm]]

            xMarkExProt[[sm]]<-xMark1
            attr(xMarkExProt[[sm]], "spname")<-names(listOfdfMarkPosExProt)[[sm]]

            xMarkExProt_2nd[[sm]]<-xMark1_2nd
            attr(xMarkExProt_2nd[[sm]], "spname")<-names(listOfdfMarkPosExProt)[[sm]]

          } # end for

          ########################
          #                      #
          #   add marks to plot monocen #
          #                      #
          ########################
          # if(circularPlot==FALSE){
          arrowPlotMark(squareness, xMarkExProt, yMarkExProt,
                        dfMarkColorInternal,
                        listOfdfMarkPosExProt,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink)

          # plot second Protein when chrt
          # if(chromatids & ProteinsToSide & ProteinsBothChrt & circularPlot==FALSE) {
          arrowPlotMark(squareness, xMarkExProt_2nd, yMarkExProt,
                        dfMarkColorInternal,
                        listOfdfMarkPosExProt,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink)
          # }


        } #     if(length(listOfdfMarkPosExProt)>0)

        else {
          remove(listOfdfMarkPosExProt)
        }

        booleanForProteinInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosExProt") & circularPlot==FALSE

        if(booleanForProteinInlineLabel) {
          textLabel(xMarkExProt,yMarkExProt,listOfdfChromSize,listOfdfMarkPosExProt
                    ,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3,
                    markNewLine2=markNewLine,mylheight2=mylheight)
        }

  } # if presence end painting marks

      ##################################################################################################
      #
      #                                   painting Marks ExProtein holocen
      #
      ##################################################################################################

  if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
        # message(crayon::green(paste0("holocen. marks section start" ) ) )
        xMarkExProt<-yMarkExProt<-listOfdfMarkPosExProt<-list()
        xMarkExProt_2nd <- list()

        j<-1
        for (k in 1:length(parlistOfdfMarkPosHolocen)) {
          currName<-names(parlistOfdfMarkPosHolocen)[[k]]
          if(nrow(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="exProtein"),])>0){
            listOfdfMarkPosExProt<-c(listOfdfMarkPosExProt,list(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="exProtein"),]) )
            names(listOfdfMarkPosExProt)[[j]]<-currName
            j<-j+1
          }
        }

        if(length(listOfdfMarkPosExProt)>0){
          for (sm in 1:length(listOfdfMarkPosExProt)) {

            yMark1<-xMark1<-NULL
            xMark1_2nd<-NULL

            corr_index<-which(names(ym) %in% names(listOfdfMarkPosExProt)[[sm]] )

            for (m in 1:nrow(listOfdfMarkPosExProt[[sm]])) {
              squareSide<- "exProtein"
              rowIndex<-(listOfdfMarkPosExProt[[sm]][,"neworder"][m])
              chrStart<-ym[[corr_index]][rowIndex ,2]
              yinf <- chrStart +                        # was ysup
                (listOfdfMarkPosExProt[[sm]][m,"markPos"] *normalizeToOne)

              ysup <- chrStart +
                (  sum(listOfdfMarkPosExProt[[sm]][m,"markPos"]
                       ,listOfdfMarkPosExProt[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

              if("markPosProtein" %in% colnames(listOfdfMarkPosExProt[[sm]] ) ){
                if(!is.na(listOfdfMarkPosExProt[[sm]][m,"markPosProtein"] ) ) {
                  yinf <- chrStart +
                    (listOfdfMarkPosExProt[[sm]][m,"markPosProtein"]  *normalizeToOne)

                  ysup <- chrStart +
                    (  sum(listOfdfMarkPosExProt[[sm]][m,"markPosProtein"]
                           ,listOfdfMarkPosExProt[[sm]][m,"markSizeProtein"],na.rm=TRUE ) *normalizeToOne )
                }
              }

              ysize <- abs(ysup-yinf)

              fourX <- xm[[corr_index]][listOfdfMarkPosExProt[[sm]][,"neworder"][m],]

              xsizeOrig <- xsize <- max(fourX) - min(fourX)

              yProtein<-c(ysize
                          ,ysize - (pMarkFac*3*(xsizeOrig*normalizeToOne) )
                          ,0     + (pMarkFac*3*(xsizeOrig*normalizeToOne) ) # good!
                          ,0
                          ,ysize
              )

              yMark1[[m]] <- yProtein + yinf

              attr(yMark1[[m]],"rowIndex")<-rowIndex
              attr(yMark1[[m]],"squareSide")<-squareSide

              if(holocenNotAsChromatids){
                excHoloFrArrToSide<-TRUE
              }

              #
              #   right side
              #

              xsize <- xsize/2
              # startPos <- 1
              # pMarkFac <- 0.5
              xWidth <- pMarkFac * xsizeOrig

              xPosWid <- startPos * xWidth
              xbase <- min(fourX) + xsize + xPosWid

              xProtein<-c(xsize
                          ,xsize + xWidth
                          ,xsize + xWidth
                          ,xsize
                          ,xsize
              )

              xMark1[[m]] <- xProtein + xbase

              attr(xMark1[[m]],"rowIndex")<-rowIndex

              # For chromatids, Proteins in both chromatids

              #
              #   left side
              #

              xbase<-min(fourX) - xPosWid
              xsize<-max(fourX)-min(fourX)

              xsize<-xsize/2
              # xsize<-xsize-xModifierMono

              xProtein<-c(0
                          , - xWidth
                          , - xWidth
                          ,0
                          ,0
              )

              xMark1_2nd[[m]] <- xProtein + xbase

              attr(xMark1_2nd[[m]],"rowIndex")<-rowIndex

            } # for mark

            yMarkExProt[[sm]]<-yMark1
            attr(yMarkExProt[[sm]], "spname")<-names(listOfdfMarkPosExProt)[[sm]]
            xMarkExProt[[sm]]<-xMark1
            attr(xMarkExProt[[sm]], "spname")<-names(listOfdfMarkPosExProt)[[sm]]

            # if(chromatids & ProteinsToSide & ProteinsBothChrt & circularPlot==FALSE & excHoloFrArrToSide==FALSE ) {
              xMarkExProt_2nd[[sm]]<-xMark1_2nd
              attr(xMarkExProt_2nd[[sm]], "spname")<-names(listOfdfMarkPosExProt)[[sm]]
            # }

          } # end for

          #####################
          #   add ExProt marks to plot holocen
          #####################

          arrowPlotMark(squareness, xMarkExProt, yMarkExProt,
                        dfMarkColorInternal,
                        listOfdfMarkPosExProt,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink) #

          # if(chromatids & ProteinsToSide & ProteinsBothChrt & circularPlot==FALSE & excHoloFrArrToSide==FALSE) {
          arrowPlotMark(squareness, xMarkExProt_2nd, yMarkExProt,
                        dfMarkColorInternal,
                        listOfdfMarkPosExProt,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink) #
          # }

        } #     if(length(listOfdfMarkPosExProt)>0){

        else {
          remove(listOfdfMarkPosExProt)
        }

        # square labels not centrom. (monocen.)
        booleanForProteinInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosExProt") & circularPlot==FALSE

        if(booleanForProteinInlineLabel) {
          textLabel(xMarkExProt,yMarkExProt,listOfdfChromSize,listOfdfMarkPosExProt
                    ,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3,
                    markNewLine2=markNewLine,mylheight2=mylheight)
        }

      } #   if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") )

      # end ExProteins

      ##############################################################################################
      ##                                           inProteins monocen
      #
      ##############################################################################################

      if (exists("parlistOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ) {

        yMarkInProt<-xMarkInProt<-listOfdfMarkPosInProt<-list()
        xMarkInProt_2nd <- list()

        j<-1

        for (k in 1:length(parlistOfdfMarkPosMonocen)) {
          currName<-names(parlistOfdfMarkPosMonocen)[[k]]
          if(nrow(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="inProtein"),])>0){
            listOfdfMarkPosInProt<-c(listOfdfMarkPosInProt,list(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="inProtein"),]))
            names(listOfdfMarkPosInProt)[[j]]<-currName
            j<-j+1
          }
        }

        if(length(listOfdfMarkPosInProt)>0){
          for (sm in 1:length(listOfdfMarkPosInProt)) {
            yMark1<-NULL
            xMark1<-NULL
            xMark1_2nd<-NULL

            corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosInProt)[[sm]] )

            for (m in 1:nrow(listOfdfMarkPosInProt[[sm]]) ){
              ifelse(listOfdfMarkPosInProt[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
              squareSide<- "inProtein"
              ifelse(listOfdfMarkPosInProt[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
              ifelse(listOfdfMarkPosInProt[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
              rowIndex<- nrow(listOfdfChromSize[[corr_index]] ) * longORshort +
                (listOfdfMarkPosInProt[[sm]][,"neworder"][m])

              armStart<-ym[[corr_index]][rowIndex,column]

              yprox <- armStart +
                (listOfdfMarkPosInProt[[sm]][m,"markDistCen"]      *normalizeToOne*mySign)

              yter <- armStart +
                ( sum( listOfdfMarkPosInProt[[sm]][m,"markDistCen"]
                       , listOfdfMarkPosInProt[[sm]][m,"markSize"], na.rm=TRUE ) *normalizeToOne*mySign )

              if("markDistCenProtein" %in% colnames(listOfdfMarkPosInProt[[sm]] ) ){
                if(!is.na(listOfdfMarkPosInProt[[sm]][m,"markDistCenProtein"] ) ) {
                  yprox <- armStart +
                    (listOfdfMarkPosInProt[[sm]][m,"markDistCenProtein"] * normalizeToOne*mySign)
                  yter <- armStart +
                    ( sum( listOfdfMarkPosInProt[[sm]][m,"markDistCenProtein"]
                           , listOfdfMarkPosInProt[[sm]][m,"markSizeProtein"], na.rm=TRUE ) *normalizeToOne*mySign )
                }
              }

              if(longORshort==0) {
                ybase<-yter
                ysize<-abs(yprox-yter)
              } else {
                ybase<-yprox
                ysize<-abs(yter-yprox)
              }

              fourX <- xm[[corr_index]][listOfdfMarkPosInProt[[sm]][,"neworder"][m],]
              xsizeOrig <- xsize <-max(fourX)-min(fourX)

              if(longORshort==0) {
                # long arm
                yProtein<-c(0
                            ,0     + (xModifierMono*3*normalizeToOne )
                            ,ysize - (xModifierMono*3*normalizeToOne )
                            ,ysize
                            ,0
                )
              } else {
                # short arm
                yProtein<-c(ysize
                            ,ysize - (xModifierMono*3*normalizeToOne)
                            ,0     + (xModifierMono*3*normalizeToOne)
                            ,0
                            ,ysize
                )
              }

              yMark1[[m]]<- yProtein + ybase

              attr(yMark1[[m]],"arm") <-listOfdfMarkPosInProt[[sm]][m,"chrRegion"]
              attr(yMark1[[m]],"rowIndex")<-rowIndex
              attr(yMark1[[m]],"squareSide")<-squareSide

              #
              #   right side
              #

              xsize <- xsize/2

              xbase <- min(fourX) + xsize

              xProtein<-c(0
                          , - xModifierMono
                          , - xModifierMono
                          ,0
                          ,0
              )

              # +1 represents base (min)
              xMark1[[m]] <- xProtein + xbase + xModifierMono

              attr(xMark1[[m]],"rowIndex")<-rowIndex

              # For chromatids, Proteins in both chromatids

              xbase <- min(fourX)

              xsize<-max(fourX)-min(fourX)

              xsize<-xsize/2

              #
              #   left side
              #

              xProtein<-c(xsize
                          ,xsize + xModifierMono
                          ,xsize + xModifierMono
                          ,xsize
                          ,xsize
              )

              xMark1_2nd[[m]] <- xProtein + xbase - xModifierMono

              attr(xMark1_2nd[[m]],"rowIndex")<-rowIndex


            } # for each mark

            yMarkInProt[[sm]]<-yMark1
            attr(yMarkInProt[[sm]], "spname")<-names(listOfdfMarkPosInProt)[[sm]]

            xMarkInProt[[sm]]<-xMark1
            attr(xMarkInProt[[sm]], "spname")<-names(listOfdfMarkPosInProt)[[sm]]

            xMarkInProt_2nd[[sm]]<-xMark1_2nd
            attr(xMarkInProt_2nd[[sm]], "spname")<-names(listOfdfMarkPosInProt)[[sm]]

          } # end for

          ########################
          #                      #
          #   add marks to plot monocen #
          #                      #
          ########################
          # if(circularPlot==FALSE){
          arrowPlotMark(squareness, xMarkInProt, yMarkInProt,
                        dfMarkColorInternal,
                        listOfdfMarkPosInProt,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink)

          # plot second Protein when chrt
          # if(chromatids & ProteinsToSide & ProteinsBothChrt & circularPlot==FALSE) {
          arrowPlotMark(squareness, xMarkInProt_2nd, yMarkInProt,
                        dfMarkColorInternal,
                        listOfdfMarkPosInProt,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink)
          # }


        } #     if(length(listOfdfMarkPosInProt)>0)

        else {
          remove(listOfdfMarkPosInProt)
        }

        booleanForProteinInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosInProt") & circularPlot==FALSE

        if(booleanForProteinInlineLabel) {
          textLabel(xMarkInProt,yMarkInProt,listOfdfChromSize,listOfdfMarkPosInProt
                    ,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3,
                    markNewLine2=markNewLine,mylheight2=mylheight,xsize=xsize)
        }

      } # if presence end painting marks

      ################################################################################################
      #                                       inProtein holocen
      ################################################################################################

      if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
        # message(crayon::green(paste0("holocen. marks section start" ) ) )
        xMarkInProt<-yMarkInProt<-listOfdfMarkPosInProt<-list()
        xMarkInProt_2nd <- list()

        j<-1
        for (k in 1:length(parlistOfdfMarkPosHolocen)) {
          currName<-names(parlistOfdfMarkPosHolocen)[[k]]
          if(nrow(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="inProtein"),])>0){
            listOfdfMarkPosInProt<-c(listOfdfMarkPosInProt,list(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="inProtein"),]) )
            names(listOfdfMarkPosInProt)[[j]]<-currName
            j<-j+1
          }
        }

        if(length(listOfdfMarkPosInProt)>0){
          for (sm in 1:length(listOfdfMarkPosInProt)) {

            yMark1<-xMark1<-NULL
            xMark1_2nd<-NULL

            corr_index<-which(names(ym) %in% names(listOfdfMarkPosInProt)[[sm]] )

            for (m in 1:nrow(listOfdfMarkPosInProt[[sm]])) {
              squareSide<- "inProtein"
              rowIndex<-(listOfdfMarkPosInProt[[sm]][,"neworder"][m])
              chrStart<-ym[[corr_index]][rowIndex ,2]

              yinf <- chrStart +
                (listOfdfMarkPosInProt[[sm]][m,"markPos"] *normalizeToOne)

              ysup <- chrStart +
                (  sum(listOfdfMarkPosInProt[[sm]][m,"markPos"]
                       ,listOfdfMarkPosInProt[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

              if("markPosProtein" %in% colnames(listOfdfMarkPosInProt[[sm]] ) ){
                if(!is.na(listOfdfMarkPosInProt[[sm]][m,"markPosProtein"] ) ) {
                  yinf <- chrStart +
                    (listOfdfMarkPosInProt[[sm]][m,"markPosProtein"]  *normalizeToOne)

                  ysup <- chrStart +
                    (  sum(listOfdfMarkPosInProt[[sm]][m,"markPosProtein"]
                           ,listOfdfMarkPosInProt[[sm]][m,"markSizeProtein"],na.rm=TRUE ) *normalizeToOne )
                }
              }

              ysize <- abs(ysup-yinf)

              fourX <- xm[[corr_index]][listOfdfMarkPosInProt[[sm]][,"neworder"][m],]

              xsizeOrig <- xsize <- max(fourX) - min(fourX)

              yProtein<-c(ysize
                          ,ysize - (xModifierHolo*3*normalizeToOne )
                          ,0     + (xModifierHolo*3*normalizeToOne ) # good!
                          ,0
                          ,ysize
              )

              yMark1[[m]] <- yProtein + yinf

              attr(yMark1[[m]],"rowIndex")<-rowIndex
              attr(yMark1[[m]],"squareSide")<-squareSide

              if(holocenNotAsChromatids){
                excHoloFrArrToSide<-TRUE
              }

              #
              #   right side
              #

              xsize <- xsize/2
              # startPos <- 1
              # pMarkFac <- 0.5
              xWidth <- pMarkFac * xsizeOrig
              xPosWid <- startPos * xWidth
              xbase <- min(fourX) + xsize

              xProtein<-c(0
                          , -xModifierHolo
                          , -xModifierHolo
                          ,0
                          ,0
              )

              xMark1[[m]] <- xProtein + xbase + xModifierHolo

              attr(xMark1[[m]],"rowIndex")<-rowIndex

              # For chromatids, Proteins in both chromatids

              #
              #   left side
              #

              xbase<-min(fourX)

              xsize<-max(fourX)-min(fourX)

              xsize<-xsize/2

              xProtein<-c(xsize
                          ,xsize + xModifierHolo
                          ,xsize + xModifierHolo
                          ,xsize
                          ,xsize
              )

              xMark1_2nd[[m]] <- xProtein + xbase - xModifierHolo

              attr(xMark1_2nd[[m]],"rowIndex")<-rowIndex

            } # for mark

            yMarkInProt[[sm]]<-yMark1
            attr(yMarkInProt[[sm]], "spname")<-names(listOfdfMarkPosInProt)[[sm]]
            xMarkInProt[[sm]]<-xMark1
            attr(xMarkInProt[[sm]], "spname")<-names(listOfdfMarkPosInProt)[[sm]]

            # if(chromatids & ProteinsToSide & ProteinsBothChrt & circularPlot==FALSE & excHoloFrArrToSide==FALSE ) {
            xMarkInProt_2nd[[sm]]<-xMark1_2nd
            attr(xMarkInProt_2nd[[sm]], "spname")<-names(listOfdfMarkPosInProt)[[sm]]
            # }

          } # end for

          #####################
          #   add InProt marks to plot holocen
          #####################

          arrowPlotMark(squareness, xMarkInProt, yMarkInProt,
                        dfMarkColorInternal,
                        listOfdfMarkPosInProt,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink) #

          # if(chromatids & ProteinsToSide & ProteinsBothChrt & circularPlot==FALSE & excHoloFrArrToSide==FALSE) {
          arrowPlotMark(squareness, xMarkInProt_2nd, yMarkInProt,
                        dfMarkColorInternal,
                        listOfdfMarkPosInProt,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink) #
          # }

        } #     if(length(listOfdfMarkPosInProt)>0){

        else {
          remove(listOfdfMarkPosInProt)
        }

        # square labels not centrom. (monocen.)
        booleanForProteinInlineLabel<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosInProt") & circularPlot==FALSE

        if(booleanForProteinInlineLabel) {
          textLabel(xMarkInProt,yMarkInProt,listOfdfChromSize,listOfdfMarkPosInProt
                    ,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3,
                    markNewLine2=markNewLine,mylheight2=mylheight,xsize=xsize)
        }

      } #   if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") )

      ## end infProtein

    ## up arrows

    ##########################################################################################################3
    #
    #                           painting Marks monocen              upArrow marks
    #
    ############################################################################################################

    arrowhead2 <- 1-arrowhead
    if (exists("parlistOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ) {
      yMarkUpAr<-xMarkUpAr<-listOfdfMarkPosUpAr<-list()
      xMarkUpAr_2nd<-list()

      j<-1

      for (k in 1:length(parlistOfdfMarkPosMonocen)) {
        currName<-names(parlistOfdfMarkPosMonocen)[[k]]
        if(nrow(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="upArrow"),])>0){
          listOfdfMarkPosUpAr<-c(listOfdfMarkPosUpAr,list(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="upArrow"),]))
          names(listOfdfMarkPosUpAr)[[j]]<-currName
          j<-j+1
        }
      }

      if(length(listOfdfMarkPosUpAr)>0){
        for (sm in 1:length(listOfdfMarkPosUpAr)) {
          yMark1<-NULL
          xMark1<-NULL
          xMark1_2nd<-NULL

          corr_index<-which(names(listOfdfChromSize) %in% names(listOfdfMarkPosUpAr)[[sm]] )

          for (m in 1:nrow(listOfdfMarkPosUpAr[[sm]]) ){
            ifelse(listOfdfMarkPosUpAr[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
            ifelse(listOfdfMarkPosUpAr[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
            ifelse(listOfdfMarkPosUpAr[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
            rowIndex<- nrow(listOfdfChromSize[[corr_index]] ) * longORshort + (listOfdfMarkPosUpAr[[sm]][,"neworder"][m])
            armStart<-ym[[corr_index]][rowIndex,column]
            yprox <- armStart +
              (listOfdfMarkPosUpAr[[sm]][m,"markDistCen"]       *normalizeToOne*mySign)

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
            attr(yMark1[[m]],"rowIndex")<-rowIndex

            fourX <- xm[[corr_index]][listOfdfMarkPosUpAr[[sm]][,"neworder"][m],]
            xsize<-max(fourX)-min(fourX)
            xbase<-min(fourX)

            if(arrowsToSide){
              xsize<-xsize/2
              xbase <- xbase + xsize

              if(chromatids & circularPlot==FALSE){
                xsize<-xsize-xModifierMono
                xbase <- xbase + (xModifierMono)
              }

            }

            xArrow<-c(.5*xsize, xsize-(arrowheadWidthShrink*xsize),
                 (1-shrinkArrow)*xsize,(1-shrinkArrow)*xsize,
                 shrinkArrow*xsize,shrinkArrow*xsize,
                 0+(arrowheadWidthShrink*xsize),.5*xsize
            )
            # +1 represents base (min)
            xMark1[[m]] <- xArrow + xbase
            attr(xMark1[[m]],"rowIndex")<-rowIndex

            # For chromatids, arrows in both chromatids

            if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE){
              xbase<-min(fourX)
              xsize<-max(fourX)-min(fourX)

              xsize<-xsize/2
              xsize<-xsize-xModifierMono

              xArrow<-c(.5*xsize, xsize-(arrowheadWidthShrink*xsize),
                        (1-shrinkArrow)*xsize,(1-shrinkArrow)*xsize,
                        shrinkArrow*xsize,shrinkArrow*xsize,
                        0+(arrowheadWidthShrink*xsize),.5*xsize
              )

              xMark1_2nd[[m]] <- xArrow + xbase
              attr(xMark1_2nd[[m]],"rowIndex")<-rowIndex
            } # SECOND arrow if

          } # for each mark

          yMarkUpAr[[sm]]<-yMark1
          attr(yMarkUpAr[[sm]], "spname")<-names(listOfdfMarkPosUpAr)[[sm]]
          xMarkUpAr[[sm]]<-xMark1
          attr(xMarkUpAr[[sm]], "spname")<-names(listOfdfMarkPosUpAr)[[sm]]

          if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE) {
            xMarkUpAr_2nd[[sm]]<-xMark1_2nd
            attr(xMarkUpAr_2nd[[sm]], "spname")<-names(listOfdfMarkPosUpAr)[[sm]]
          }

        } # end for

        ########################
        #                      #
        #   add marks to plot monocen #
        #                      #
        ########################
        # if(circularPlot==FALSE){
        arrowPlotMark(squareness, xMarkUpAr, yMarkUpAr,
                      dfMarkColorInternal,
                      listOfdfMarkPosUpAr,
                      chrWidth, #use for calc r2
                      markN,
                      lwd.marks2,#lwd.chr,
                      circularPlot,
                      y,
                      x,
                      markLabelSize,
                      separFactor,
                      labelSpacing,circleCenter,circleCenterY,radius,
                      ylistTransChrSimple,rotation=rotation,
                      arrowheadWidthShrink)

        # plot second arrow when chrt
        if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE) {
          arrowPlotMark(squareness, xMarkUpAr_2nd, yMarkUpAr,
                        dfMarkColorInternal,
                        listOfdfMarkPosUpAr,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink)
        }


      } #     if(length(listOfdfMarkPosUpAr)>0)

      else {remove(listOfdfMarkPosUpAr)}

    } # if presence end painting marks

    ##################################################################################################
    #
    #                                                 painting Marks upArrow holocen
    #
    ##################################################################################################

    if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
      # message(crayon::green(paste0("holocen. marks section start" ) ) )
      xMarkUpAr<-yMarkUpAr<-listOfdfMarkPosUpAr<-list()
      xMarkUpAr_2nd<- list()
      j<-1
      for (k in 1:length(parlistOfdfMarkPosHolocen)) {
        currName<-names(parlistOfdfMarkPosHolocen)[[k]]
        if(nrow(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="upArrow"),])>0){
          listOfdfMarkPosUpAr<-c(listOfdfMarkPosUpAr,list(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="upArrow"),]) )
          names(listOfdfMarkPosUpAr)[[j]]<-currName
          j<-j+1
        }
      }

      if(length(listOfdfMarkPosUpAr)>0){
        for (sm in 1:length(listOfdfMarkPosUpAr)) {

          yMark1<-xMark1<-NULL
          xMark1_2nd<-NULL

          corr_index<-which(names(ym) %in% names(listOfdfMarkPosUpAr)[[sm]] )

          for (m in 1:nrow(listOfdfMarkPosUpAr[[sm]])) {
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

            attr(yMark1[[m]],"rowIndex")<-rowIndex

            fourX <- xm[[corr_index]][listOfdfMarkPosUpAr[[sm]][,"neworder"][m],]

            xsize <- max(fourX) - min(fourX)
            xbase <- min(fourX)

            if(holocenNotAsChromatids){
              excHoloFrArrToSide<-TRUE
            }

            if(arrowsToSide & excHoloFrArrToSide==FALSE){
              xsize<-xsize/2
              xbase <- xbase + xsize
              if(chromatids & holocenNotAsChromatids==FALSE & circularPlot==FALSE){
                xsize <- xsize - xModifierHolo
                xbase <- xbase + xModifierHolo
              }
            }

            # conflict holocenNotaschr TRUE
            #          excHoloFrArrToSide FALSE
            # HOLOCENNOTASCHROMATIDS TRUE REQUIRES EXCHOLOFRARRTOSIDE TRUE

            xArrow<-c(.5*xsize, xsize-(arrowheadWidthShrink*xsize),
                      (1-shrinkArrow)*xsize,(1-shrinkArrow)*xsize,
                 shrinkArrow*xsize,shrinkArrow*xsize,
                 0+(arrowheadWidthShrink*xsize),.5*xsize
            )

            # +1 represents base (min)
            xMark1[[m]] <- xArrow + xbase
            attr(xMark1[[m]],"rowIndex")<-rowIndex

            # For chromatids, arrows in both chromatids

            if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE & excHoloFrArrToSide==FALSE  ){
              xbase<-min(fourX)
              xsize<-max(fourX)-min(fourX)

              xsize<-xsize/2
              xsize<-xsize-xModifierHolo

              xArrow<-c(.5*xsize, xsize-(arrowheadWidthShrink*xsize),
                        (1-shrinkArrow)*xsize,(1-shrinkArrow)*xsize,
                        shrinkArrow*xsize,shrinkArrow*xsize,
                        0+(arrowheadWidthShrink*xsize),.5*xsize
              )

              xMark1_2nd[[m]] <- xArrow + xbase
              attr(xMark1_2nd[[m]],"rowIndex")<-rowIndex
            } # SECOND arrow if

          } # for mark

          yMarkUpAr[[sm]]<-yMark1
          attr(yMarkUpAr[[sm]], "spname")<-names(listOfdfMarkPosUpAr)[[sm]]
          xMarkUpAr[[sm]]<-xMark1
          attr(xMarkUpAr[[sm]], "spname")<-names(listOfdfMarkPosUpAr)[[sm]]

          if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE & excHoloFrArrToSide==FALSE ) {
            xMarkUpAr_2nd[[sm]]<-xMark1_2nd
            attr(xMarkUpAr_2nd[[sm]], "spname")<-names(listOfdfMarkPosUpAr)[[sm]]
          }

        } # end for

        #####################
        #   add UpAr marks to plot holocen
        #####################

        arrowPlotMark(squareness, xMarkUpAr, yMarkUpAr,
                      dfMarkColorInternal,
                      listOfdfMarkPosUpAr,
                      chrWidth, #use for calc r2
                      markN,
                      lwd.marks2,#lwd.chr,
                      circularPlot,
                      y,
                      x,
                      markLabelSize,
                      separFactor,
                      labelSpacing,circleCenter,circleCenterY,radius,
                      ylistTransChrSimple,rotation=rotation,
                      arrowheadWidthShrink) #

        if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE & excHoloFrArrToSide==FALSE) {
          arrowPlotMark(squareness, xMarkUpAr_2nd, yMarkUpAr,
                        dfMarkColorInternal,
                        listOfdfMarkPosUpAr,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink) #
        }

      } #     if(length(listOfdfMarkPosUpAr)>0){

      else {remove(listOfdfMarkPosUpAr)}

      #
      #   inline legend holocen
      #

    } #   if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") )

    # end upArrow marks  #


    ## down arrows

    ##########################################################################################################3
    #
    #                           painting Marks monocen              downArrow marks
    #
    ############################################################################################################

    arrowhead2<-1-arrowhead
    if (exists("parlistOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ){

      xMarkDwAr<-yMarkDwAr<-listOfdfMarkPosDwAr<-list()
      xMarkDwAr_2nd<-list()
      j<-1

      for (k in 1:length(parlistOfdfMarkPosMonocen)) {
        currName<-names(parlistOfdfMarkPosMonocen)[[k]]
        if(nrow(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="downArrow"),])>0){
          listOfdfMarkPosDwAr<-c(listOfdfMarkPosDwAr,list(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="downArrow"),]))
          names(listOfdfMarkPosDwAr)[[j]]<-currName
          j<-j+1
        }
      }

      if(length(listOfdfMarkPosDwAr)>0){
        for (sm in 1:length(listOfdfMarkPosDwAr)) {
          yMark1<-xMark1<-NULL
          yMark1_2nd<-xMark1_2nd<-NULL

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

            xbase<-min(fourX)

            if(arrowsToSide){
              xsize<-xsize/2

              if(chromatids  & circularPlot==FALSE){
                xsize<-xsize-xModifierMono
              }
            }

            xArrow<-c((1-shrinkArrow)*xsize , (1-shrinkArrow)*xsize,
                      xsize-(arrowheadWidthShrink*xsize),.5*xsize,
                      0+(arrowheadWidthShrink*xsize),shrinkArrow*xsize,
                      shrinkArrow*xsize,(1-shrinkArrow)*xsize
            )

            # +1 represents base (min)
            xMark1[[m]] <- xArrow + xbase
            attr(xMark1[[m]],"rowIndex")<-rowIndex

            # 2nd arrow

            if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE) {
              xsize <- max(fourX) - min(fourX)
              xbase <- min(fourX)

              xsize<-xsize/2
              xbase <- xbase + xsize

              xsize<-xsize-xModifierMono
              xbase <- xbase + (xModifierMono)



            xArrow<-c((1-shrinkArrow)*xsize , (1-shrinkArrow)*xsize,
                      xsize-(arrowheadWidthShrink*xsize),.5*xsize,
                      0+(arrowheadWidthShrink*xsize),shrinkArrow*xsize,
                      shrinkArrow*xsize,(1-shrinkArrow)*xsize
            )

            xMark1_2nd[[m]] <- xArrow + xbase
            attr(xMark1_2nd[[m]],"rowIndex")<-rowIndex

            }

          } # for mark

          yMarkDwAr[[sm]]<-yMark1
          attr(yMarkDwAr[[sm]], "spname")<-names(listOfdfMarkPosDwAr)[[sm]]
          xMarkDwAr[[sm]]<-xMark1
          attr(xMarkDwAr[[sm]], "spname")<-names(listOfdfMarkPosDwAr)[[sm]]

          if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE) {
            xMarkDwAr_2nd[[sm]]<-xMark1_2nd
            attr(xMarkDwAr_2nd[[sm]], "spname")<-names(listOfdfMarkPosDwAr)[[sm]]
          }



        } # end for

        ###############################
        #                             #
        #   add marks to plot monocen #
        #                             #
        ###############################

        arrowPlotMark(squareness, xMarkDwAr, yMarkDwAr,
                      dfMarkColorInternal,
                      listOfdfMarkPosDwAr,
                      chrWidth, #use for calc r2
                      markN,
                      lwd.marks2,#lwd.chr,
                      circularPlot,
                      y,
                      x,
                      markLabelSize,
                      separFactor,
                      labelSpacing,circleCenter,circleCenterY,radius,
                      ylistTransChrSimple,rotation=rotation,
                      arrowheadWidthShrink)

        if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE) {
          arrowPlotMark(squareness, xMarkDwAr_2nd, yMarkDwAr,
                        dfMarkColorInternal,
                        listOfdfMarkPosDwAr,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink)
        }

      } #     if(length(listOfdfMarkPosDwAr)>0)

      else {remove(listOfdfMarkPosDwAr)}

    } # if presence end painting marks

    ##################################################################################################
    #
    #                                                 painting Marks downArrow holocen
    #
    ##################################################################################################

    if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
      # message(crayon::green(paste0("holocen. marks section start" ) ) )
      xMarkDwAr<-yMarkDwAr<-listOfdfMarkPosDwAr<-list()
      xMarkDwAr_2nd<-list()

      j<-1
      for (k in 1:length(parlistOfdfMarkPosHolocen)) {
        currName<-names(parlistOfdfMarkPosHolocen)[[k]]
        if(nrow(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="downArrow"),])>0){
          listOfdfMarkPosDwAr<-c(listOfdfMarkPosDwAr,list(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="downArrow"),]) )
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

            attr(yMark1[[m]],"rowIndex")<-rowIndex

            fourX <- xm[[corr_index]][listOfdfMarkPosDwAr[[sm]][,"neworder"][m],]

            xsize <- max(fourX) - min(fourX)
            xbase <- min(fourX)

            if(holocenNotAsChromatids){
              excHoloFrArrToSide<-TRUE
            }

            if(arrowsToSide & excHoloFrArrToSide==FALSE ){
              xsize<-xsize/2
              if(chromatids & holocenNotAsChromatids==FALSE & circularPlot==FALSE){
                xsize<-xsize-xModifierHolo
              }
            }

            xArrow<-c((1-shrinkArrow)*xsize , (1-shrinkArrow)*xsize,
                      xsize-(arrowheadWidthShrink*xsize),.5*xsize,
                      0+(arrowheadWidthShrink*xsize),shrinkArrow*xsize,
                      shrinkArrow*xsize,(1-shrinkArrow)*xsize
            )
            # +1 represents base (min)
            xMark1[[m]] <- xArrow + xbase
            attr(xMark1[[m]],"rowIndex") <- rowIndex

            # 2nd arrow

            if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE & excHoloFrArrToSide==FALSE) {
              xsize <- max(fourX) - min(fourX)
              xbase <- min(fourX)

              xsize<-xsize/2
              xbase <- xbase + xsize

              xsize <- xsize - xModifierHolo
              xbase <- xbase + xModifierHolo

              xArrow<-c((1-shrinkArrow)*xsize , (1-shrinkArrow)*xsize,
                        xsize-(arrowheadWidthShrink*xsize),.5*xsize,
                        0+(arrowheadWidthShrink*xsize),shrinkArrow*xsize,
                        shrinkArrow*xsize,(1-shrinkArrow)*xsize
              )

              xMark1_2nd[[m]] <- xArrow + xbase
              attr(xMark1_2nd[[m]],"rowIndex")<-rowIndex
            }

          } # for mark
          yMarkDwAr[[sm]]<-yMark1
          attr(yMarkDwAr[[sm]], "spname")<-names(listOfdfMarkPosDwAr)[[sm]]
          xMarkDwAr[[sm]]<-xMark1
          attr(xMarkDwAr[[sm]], "spname")<-names(listOfdfMarkPosDwAr)[[sm]]

          if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE & excHoloFrArrToSide==FALSE ) {
            xMarkDwAr_2nd[[sm]]<-xMark1_2nd
            attr(xMarkDwAr_2nd[[sm]], "spname")<-names(listOfdfMarkPosDwAr)[[sm]]
          }

        } # end for

        #####################
        #   add DwAr marks to plot holocen
        #####################

        arrowPlotMark(squareness, xMarkDwAr, yMarkDwAr,
                      dfMarkColorInternal,
                      listOfdfMarkPosDwAr,
                      chrWidth, #use for calc r2
                      markN,
                      lwd.marks2,#lwd.chr,
                      circularPlot,
                      y,
                      x,
                      markLabelSize,
                      separFactor,
                      labelSpacing,circleCenter,circleCenterY,radius,
                      ylistTransChrSimple,rotation=rotation,
                      arrowheadWidthShrink)

        if(chromatids & arrowsToSide & arrowsBothChrt & circularPlot==FALSE & excHoloFrArrToSide==FALSE) {
          arrowPlotMark(squareness, xMarkDwAr_2nd, yMarkDwAr,
                        dfMarkColorInternal,
                        listOfdfMarkPosDwAr,
                        chrWidth, #use for calc r2
                        markN,
                        lwd.marks2,#lwd.chr,
                        circularPlot,
                        y,
                        x,
                        markLabelSize,
                        separFactor,
                        labelSpacing,circleCenter,circleCenterY,radius,
                        ylistTransChrSimple,rotation=rotation,
                        arrowheadWidthShrink
                        )
        }

      } #     if(length(listOfdfMarkPosDwAr)>0){

      else {remove(listOfdfMarkPosDwAr)}

      #
      #   inline legend holocen
      #

    } #   if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") )

    # end downArrow marks  #

    ##
  ##########################################################################################################################

                                          ##########################################
                                          #
                                          #   painting Marks monocen cM style
                                          #
                                          ##########################################

if (exists("parlistOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ){
    # message(crayon::green(paste0("monocen. marks section start" ) ) )

    yMarkcM<- xMarkcM<- listOfdfMarkPoscM<-list()

    j<-1

    for (k in 1:length(parlistOfdfMarkPosMonocen)) {
      currName<-names(parlistOfdfMarkPosMonocen)[[k]]
      if(nrow(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="cM"),])>0){
        listOfdfMarkPoscM<-c(listOfdfMarkPoscM,list(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="cM"),]))
        names(listOfdfMarkPoscM)[[j]]<-currName
        j<-j+1
      }
    }

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

          if(chromatids & circularPlot==FALSE) {
            xMark1[[m]][2] <- xMark1[[m]][2] +   ( (xMark1[[m]][1] - xMark1[[m]][2])  / 2) + xModifierMono
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
      cMPlotMark(bannedMarkName3,xMarkcM, yMarkcM,y, x, dfMarkColorInternal,listOfdfMarkPoscM, lwd.cM2,circularPlot,
        radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,markN,labelSpacing,chrWidth,
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

      textLabel(xMarkcM,yMarkcM,listOfdfChromSize,listOfdfMarkPoscM,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3
                ,markNewLine2=markNewLine,mylheight2=mylheight)
}

} # if presence end painting marks

                                  ########################################
                                  #
                                  #   painting Marks cM holocen
                                  #
                                  ########################################

if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
    # message(crayon::green(paste0("holocen. marks section start" ) ) )
  xMarkcM<-yMarkcM<-listOfdfMarkPoscM<-list()

    j<-1
    for (k in 1:length(parlistOfdfMarkPosHolocen)) {
      currName<-names(parlistOfdfMarkPosHolocen)[[k]]
      if(nrow(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="cM"),])>0){
        listOfdfMarkPoscM<-c(listOfdfMarkPoscM,list(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="cM"),]) )
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

          if(chromatids & holocenNotAsChromatids==FALSE & circularPlot==FALSE) {
            xMark1[[m]][2] <- xMark1[[m]][2] +   ( (xMark1[[m]][1] - xMark1[[m]][2])  / 2) + xModifierHolo
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
      cMPlotMark(bannedMarkName3,xMarkcM, yMarkcM, y, x, dfMarkColorInternal,listOfdfMarkPoscM, lwd.cM2,circularPlot,
                 radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,markN,labelSpacing,chrWidth,
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

    textLabel(xMarkcM,yMarkcM,listOfdfChromSize,listOfdfMarkPoscM,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3
              ,markNewLine2=markNewLine,mylheight2=mylheight)
  } # if
} # if parlistOfdfMarkPosHolocen


    ## cMLeft ######################################################################################################################

    ##########################################
    #
    #   painting Marks monocen cMLeft style
    #
    ##########################################

    if (exists("parlistOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ){
      # message(crayon::green(paste0("monocen. marks section start" ) ) )

      xMarkcMLeft <- yMarkcMLeft<-listOfdfMarkPoscMLeft<-list()
      j<-1

      for (k in 1:length(parlistOfdfMarkPosMonocen)) {
        currName<-names(parlistOfdfMarkPosMonocen)[[k]]
        if(nrow(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="cMLeft"),])>0){
          listOfdfMarkPoscMLeft<-c(listOfdfMarkPoscMLeft,list(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="cMLeft"),]))
          names(listOfdfMarkPoscMLeft)[[j]]<-currName
          j<-j+1
        }
      }


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

            if(chromatids & circularPlot==FALSE) {
              xMark1[[m]][1] <- xMark1[[m]][1] -   ( (xMark1[[m]][1] - xMark1[[m]][2])  / 2 ) - xModifierMono
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

        cMLeftPlotMark(bannedMarkName3,xMarkcMLeft, yMarkcMLeft,y, x, dfMarkColorInternal,listOfdfMarkPoscMLeft, lwd.cM2,circularPlot,
                   radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,markN,labelSpacing,chrWidth,
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

        textLabelLeft(xMarkcMLeft,yMarkcMLeft,listOfdfChromSize,listOfdfMarkPoscMLeft,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3)
      }

    } # if presence end painting marks

    ########################################
    #
    #   painting Marks cMLeft holocen
    #
    ########################################

    if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ) {
      # message(crayon::green(paste0("holocen. marks section start" ) ) )
      xMarkcMLeft<- yMarkcMLeft<-listOfdfMarkPoscMLeft<-list()
      j<-1
      for (k in 1:length(parlistOfdfMarkPosHolocen)) {
        currName<-names(parlistOfdfMarkPosHolocen)[[k]]
        if(nrow(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="cMLeft"),])>0){
          listOfdfMarkPoscMLeft<-c(listOfdfMarkPoscMLeft,list(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="cMLeft"),]) )
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

            if(chromatids & holocenNotAsChromatids==FALSE & circularPlot==FALSE) {
              xMark1[[m]][1] <- xMark1[[m]][1] -   ( (xMark1[[m]][1] - xMark1[[m]][2])  / 2 ) - xModifierHolo
            }

            xMark1[[m]][2] <- xMark1[[m]][2] -   ( (initOrig1 - xMark1[[m]][2])  * protruding2) # left
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
        cMLeftPlotMark(bannedMarkName3,xMarkcMLeft, yMarkcMLeft, y, x, dfMarkColorInternal,listOfdfMarkPoscMLeft, lwd.cM2,circularPlot,
                   radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,markN,labelSpacing,chrWidth,
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

        textLabelLeft(xMarkcMLeft,yMarkcMLeft,listOfdfChromSize,listOfdfMarkPoscMLeft,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3)
      } # if
    } # if parlistOfdfMarkPosHolocen

                                                            #
                                                            #         DOTS
                                                            #

  ##########################################################################################################################
  #
  # dot style of marks                        monocen dots
  #
  ##########################################################################################################################

  if (exists("parlistOfdfMarkPosMonocen") & exists("dfMarkColorInternal") ) {

    listOfdfMarkPosCr<-list()

    yMarkCr<-xMarkCr<-rad<-radX<-  colBorderCr<-colCr<-list()

    j<-1

    for (k in 1:length(parlistOfdfMarkPosMonocen)) {
      currName<-names(parlistOfdfMarkPosMonocen)[[k]]
      if(nrow(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="dots"),])>0){
      listOfdfMarkPosCr<-c(listOfdfMarkPosCr,list(parlistOfdfMarkPosMonocen[[k]][which(parlistOfdfMarkPosMonocen[[k]]$style=="dots"),]) )
      names(listOfdfMarkPosCr)[[j]]<-currName
      j<-j+1
      }
    }

    if(length(listOfdfMarkPosCr)>0) {

    for (k in 1:length(listOfdfMarkPosCr) ) {

      colBorderCr1<-colCr1<-rad1<-rad1X<-yMarkCr1<-xMarkCr1<-NULL

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
        attr(yMarkCr1[[i]], "rowIndex") <- rowIndex

        xBoundaries <- xm[[corr_index]][listOfdfMarkPosCr[[k]][,"neworder"][i],3:2]
        xBoundariesMm <-(xBoundaries[2]-xBoundaries[1])
        xBoundariesQuar <- xBoundariesMm/4

        if(chromatids & circularPlot==FALSE){
          xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar - (xModifierMono/2) ), list(xBoundaries[1]+ 3*xBoundariesQuar + (xModifierMono/2) ) )
        } else {
          xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar),list(xBoundaries[1]+ 3*xBoundariesQuar ) ) # default
        }

        attr(xMarkCr1[[i]], "rowIndex")<- rowIndex

        rad1[[i]]<-rep(list(abs(ysupCr-yinfCr)/2 ),2)
        attr(rad1[[i]], "rowIndex")<- rowIndex

        rad1X[[i]]<-rep(list(xBoundariesMm/2),2)
        attr(rad1X[[i]], "rowIndex")<- rowIndex

        colCr1[[i]] <- rep(list(dfMarkColorInternal$markColor[match(listOfdfMarkPosCr[[k]]$markName[i], dfMarkColorInternal$markName)]),2)
        colBorderCr1[[i]] <- rep(list(dfMarkColorInternal$markBorderColor[match(listOfdfMarkPosCr[[k]]$markName[i], dfMarkColorInternal$markName)]),2)

      }
      yMarkCr[[k]]<-yMarkCr1
      attr(yMarkCr[[k]], "spname")<-names(listOfdfMarkPosCr)[[k]] # added 1.14
      xMarkCr[[k]]<-xMarkCr1
      attr(xMarkCr[[k]], "spname")<-names(listOfdfMarkPosCr)[[k]] # added 1.14

      rad[[k]]<-rad1
      attr(rad[[k]], "spname") <-names(listOfdfMarkPosCr)[[k]] # added 1.16.4

      radX[[k]]<-rad1X
      attr(radX[[k]], "spname") <-names(listOfdfMarkPosCr)[[k]] # added 1.16.4

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
      plotDotMarks(bannedMarkName3,xMarkCr,yMarkCr, rad, radX, colCr,markN,xfactor,colBorderCr,circularPlot, y,x,radius,circleCenter,circleCenterY,separFactor,
                   chrWidth,listOfdfMarkPosCr,markLabelSize,pattern,labelSpacing,useOneDot,legend,ylistTransChrSimple,rotation=rotation,
                   labelOutwards,dotsAsOval)


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
                  chrSpacing,markLabelSize,pattern,bannedMarkName3,xBoundariesQuar)
  }

  ##############################################################################################################################
  #
  #                                            dot style of marks holocen
  #
  ########################################

  if (exists("parlistOfdfMarkPosHolocen") & exists("dfMarkColorInternal") ){
    # message(crayon::green(paste0("holocen. dot marks section start" ) ) )
    listOfdfMarkPosCr<-list()

    colBorderCr<-colCr<-rad<-radX<-xMarkCr<-yMarkCr<-list()
    j<-1

    for (k in 1:length(parlistOfdfMarkPosHolocen)) {
      currName<-names(parlistOfdfMarkPosHolocen)[[k]]

      if(nrow(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="dots"),])>0){
        listOfdfMarkPosCr<-c(listOfdfMarkPosCr,list(parlistOfdfMarkPosHolocen[[k]][which(parlistOfdfMarkPosHolocen[[k]]$style=="dots"),]) )
        names(listOfdfMarkPosCr)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listOfdfMarkPosCr)>0){

    for (k in 1:length(listOfdfMarkPosCr) ) {

      yMarkCr1<-xMarkCr1<-rad1<-rad1X<-colCr1<-colBorderCr1<-NULL

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
        xBoundariesMm  <-(xBoundaries[2]-xBoundaries[1])
        xBoundariesQuar<- xBoundariesMm/4
         #xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar),list(xBoundaries[1]+3*xBoundariesQuar) )

        if(chromatids & holocenNotAsChromatids==FALSE & circularPlot==FALSE){
          xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar - (xModifierHolo/2) ), list(xBoundaries[1]+ 3*xBoundariesQuar + (xModifierHolo/2) ) )
        } else {
          xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar),list(xBoundaries[1]+3*xBoundariesQuar) ) # default
        }

        attr(xMarkCr1[[i]], "rowIndex")<- rowIndex

        rad1[[i]]<-rep(list(abs(ysupCr-yinfCr)/2 ),2)
        attr(rad1[[i]], "rowIndex")<- rowIndex

        rad1X[[i]]<-rep(list(xBoundariesMm/2),2)
        attr(rad1X[[i]], "rowIndex")<- rowIndex

        colCr1[[i]] <- rep(list(dfMarkColorInternal$markColor[match(listOfdfMarkPosCr[[k]]$markName[i], dfMarkColorInternal$markName)]),2)
        colBorderCr1[[i]] <- rep(list(dfMarkColorInternal$markBorderColor[
          match(listOfdfMarkPosCr[[k]]$markName[i], dfMarkColorInternal$markName)]),2)
      }

      yMarkCr[[k]]<-yMarkCr1
      attr(yMarkCr[[k]], "spname")<-names(listOfdfMarkPosCr)[[k]] # added 1.14
      xMarkCr[[k]]<-xMarkCr1
      attr(xMarkCr[[k]], "spname")<-names(listOfdfMarkPosCr)[[k]] # added 1.14
      rad[[k]]<-rad1
      attr(rad[[k]], "spname")<-names(listOfdfMarkPosCr)[[k]] # added 1.16.4

      radX[[k]]<-rad1X
      attr(radX[[k]], "spname")<-names(listOfdfMarkPosCr)[[k]] # added 1.16.4

      colCr[[k]]<-colCr1
      colBorderCr[[k]]<-colBorderCr1

    } # end for

    markListCr<-addChrNameAttrMarkDots(xMarkCr,yMarkCr,x) # 1.14

    xMarkCr <- markListCr$xMark
    yMarkCr <- markListCr$yMark


    #####################
    #   add to plot MarkCrs DOTS holocen
    #####################

    plotDotMarks(bannedMarkName3,xMarkCr,yMarkCr, rad, radX, colCr,markN,xfactor,colBorderCr,circularPlot, y,x,radius,circleCenter,circleCenterY,separFactor,
    chrWidth,listOfdfMarkPosCr,markLabelSize,pattern,labelSpacing,useOneDot,legend,ylistTransChrSimple, rotation=rotation,
    labelOutwards,dotsAsOval)

    } # if(length(listOfdfMarkPosCr)>0){

  else {remove(listOfdfMarkPosCr)}
  } # end painting MarkCrs

  #
  #     write inline legend dots holocen
  #

  booleanDotsLabels<- legend=="inline" & exists("dfMarkColorInternal") & exists("listOfdfMarkPosCr") & circularPlot==FALSE

  if (booleanDotsLabels){
    textLabelDots(xMarkCr,yMarkCr,listOfdfChromSize,listOfdfMarkPosCr,specialChrSpacing,
                            chrSpacing,markLabelSize,pattern,bannedMarkName3,xBoundariesQuar)
  }

 ## cen was here

  ##############################################
  #
  #   labels to the right
  #
  ##############################################

  #
  # xnona was wrong
  #
  xNoNA <- x # removeNAFromList(x,areNA)

  yNoNA <- removeNAFromList(y,areNA)

  if(exists("dfMarkColorInternal") ){

    dfMarkColorInternalNocM <- dfMarkColorInternal[which(!dfMarkColorInternal$style %in% c("cM","cMLeft") ),]

    # remove bannedMarkNames

    if(length(bannedMarkName3) ) {
      dfMarkColorInternalNocM <- dfMarkColorInternalNocM[
        which(! (dfMarkColorInternalNocM$markName %in% bannedMarkName3) ) ,]
    }


    if(length(dfMarkColorInternalNocM)==0){
      remove(dfMarkColorInternalNocM)
    }

    #
    # allow cM if cen. because they are not shown as cM , they must be aside if desired
    #

    if(exists("cenMarkNames") ) {
      dfMarkColorInternalcMnoCen <- dfMarkColorInternal[which(dfMarkColorInternal$style    %in% c("cM","cMLeft") &
                                                              dfMarkColorInternal$markName %in% cenMarkNames ) , ]
      if( exists("dfMarkColorInternalNocM") ) {
        dfMarkColorInternalNocM<-dplyr::bind_rows(
          dfMarkColorInternalNocM,dfMarkColorInternalcMnoCen)
      } else {
        dfMarkColorInternalNocM<-dfMarkColorInternalcMnoCen
      }
    }

    if(remSimiMarkLeg){
      dfMarkColorInternalNocM$markName <- sub(pattern,"",dfMarkColorInternalNocM$markName )
      dfMarkColorInternalNocM<-dfMarkColorInternalNocM[!duplicated(dfMarkColorInternalNocM$markName),]
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

      if(legend=="aside" ){

        plotlabelsright(maxx,yNoNA, markLabelSpacer,chrWidth,dfMarkColorInternalNocM,allMarkMaxSize,normalizeToOne,
                              markLabelSize,xfactor,legendWidth, legendHeight, n*4, pattern,legendYcoord,useOneDot,
                        dotsAsOval,circularPlot)
      } # if aside

    } # is df




  if(legend=="inline" & bMarkNameAside & !missing(bannedMarkName) ) {

      dfMarkColorInternalBanned <- dfMarkColorInternal[which( dfMarkColorInternal$markName %in% bannedMarkName ) ,]

      # if(!missing(forbiddenMark)) {
        dfMarkColorInternalBanned <- dfMarkColorInternalBanned[
          which(! dfMarkColorInternalBanned$markName %in% bannedMarkNameFor ) ,]
      # }

    if(nrow(dfMarkColorInternalBanned) >0 ) {
    #
    dfMarkColorInternalBanned <- dfMarkColorInternalBanned[which(!dfMarkColorInternalBanned$style %in% c("cM","cMLeft") ),]

    # remove bannedMarkNames with cenStyle style

    if(exists("bannedMarkName2") ) {
      if(length(bannedMarkName2) ) {
        dfMarkColorInternalBanned <- dfMarkColorInternalBanned[
          which(! (dfMarkColorInternalBanned$markName %in% bannedMarkName2) ) ,]
      }
    }

    if(length(dfMarkColorInternalBanned)==0){
      remove(dfMarkColorInternalBanned)
    }

    #
    # allow cM if cen. because they are not shown as cM , they must be aside if desired
    #

    if(exists("cenMarkNames")  ) {

      dfMarkColorInternalBannedcMnoCen <- dfMarkColorInternal[which(dfMarkColorInternal$style    %in% c("cM","cMLeft") &
                                                                      dfMarkColorInternal$markName %in% cenMarkNames &
                                                                      dfMarkColorInternal$markName %in% bannedMarkName) , ]
      if( exists("dfMarkColorInternalBanned") ) {
        dfMarkColorInternalBanned<-dplyr::bind_rows(
          dfMarkColorInternalBanned,dfMarkColorInternalBannedcMnoCen)
      } else {
        dfMarkColorInternalBanned<-dfMarkColorInternalBannedcMnoCen
      }

    }

    if(remSimiMarkLeg){
      dfMarkColorInternalBanned$markName <- sub(pattern,"",dfMarkColorInternalBanned$markName )
      dfMarkColorInternalBanned<-dfMarkColorInternalBanned[!duplicated(dfMarkColorInternalBanned$markName),]
    }

    if(nrow(dfMarkColorInternalBanned)>0 )  {

      if(!exists("allMarkMaxSize") ){
        message(crayon::green(paste0("Seems you didn't provide markSize (NA), if you want cM style, set it in the dfMarkColor data.frame" ) ) )

        allMarkMaxSize<-1
      }


      if(circularPlot) {
        maxx<-max(unlist(circleMaps), na.rm=TRUE )
      } else {
        maxx<-(max(unlist(xNoNA)) )
      }

      plotlabelsright(maxx,yNoNA, markLabelSpacer,chrWidth,dfMarkColorInternalBanned,allMarkMaxSize,normalizeToOne,
                      markLabelSize,xfactor,legendWidth, legendHeight, n*4, pattern,legendYcoord,useOneDot,
                      dotsAsOval,circularPlot)

    } # is df
    #
    } # exists dfMarkColorInternalBanned

  } # inline

  } # if dfmarkcolor
  #################################
  #
  # add notes (to the right)
  #
  #################################

if(circularPlot==FALSE) {

  if(!missing(notes)) {

    if(missing(OTUfont)){
      OTUfont<-1
    }
    if(missing(OTUfamily)){
      OTUfamily<-NA
    }
    addNotes(notes,
             listOfdfChromSizenoNA, groupName,indexCount,morphoCount,
             xmnoNA,ymnoNA,distTextChr,chrIdCount, notesPosX,notesPosY,
             notesTextSize,defaultFontFamily2,
             OTUasNote,
             OTUfont, OTUfamily,
             downNote=FALSE,rightN=TRUE,
             leftNoteFont,
             leftNoteFontUp,
             noteFont,
             parseTypes,parseStr2lang
             )

  } # fi notes

  #
  #     leftNotes ##################################
  #

  if(!missing(leftNotes)){

    addNotes(leftNotes,listOfdfChromSizenoNA, groupName,indexCount,morphoCount,
                 xmnoNA,ymnoNA,distTextChr,chrIdCount, leftNotesPosX,leftNotesPosY,
                 notesTextSize,defaultFontFamily2,
                 FALSE,
                 NA, NA,
                 downNote=TRUE,rightN=FALSE,
             leftNoteFont,
             leftNoteFontUp,
             noteFont,
             parseTypes,parseStr2lang)
  } # fi notes missing

  if(!missing(leftNotesUp)){

    if(missing(OTUfont)){
      OTUfont<-1
    }
    if(missing(OTUfamily)){
      OTUfamily<-NA
    }

    addNotes(leftNotesUp,listOfdfChromSizenoNA, groupName,indexCount,morphoCount,
             xmnoNA,ymnoNA,distTextChr,chrIdCount, leftNotesPosX,leftNotesUpPosY,
             notesTextSize,defaultFontFamily2,
             OTUasLeftNote,
             OTUfont, OTUfamily,
             downNote=FALSE,rightN=FALSE,
             leftNoteFont,
             leftNoteFontUp,
             noteFont,
             parseTypes,parseStr2lang)
  } # fi notes missing

} # CIRC false

}
