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
#' @param dfMarkColor data.frame, optional, specifying colors and style for marks (sites);
#'   columns: \code{markName}, \code{markColor}, \code{style}. \code{style} accepts: \code{square}, \code{squareLeft}, \code{dots}, \code{cM},
#'    \code{"cMLeft"}, \code{"cenStyle"}, \code{"upArrow"}, \code{"downArrow"}, \code{"exProtein"}.
#'   (if column \code{style} missing all (except 5S) are plotted as in param. \code{defaultStyleMark}).
#' @param mycolors character vector, optional, i.e. \code{c("blue",} \code{"red",} \code{"green")} for specifying color of marks in order of appearance. if diverges with number of marks will be recycled if \code{dfMarkColor} present, mycolors will be ignored. To know the order of your marks use something like: \code{unique(dfMarkPos$markName)}
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
#' @param anchorHsizeF numeric, factor to modify horizontal size of anchor \code{1} (default).
#' @param moveAnchorV numeric, displace anchor vertical portion to right or left. See \code{anchor}
#' @param moveAnchorH numeric, displace anchor horizontal portion to right or left. See \code{anchor}
#' @param pchAnchor numeric, symbol for anchor, see \code{?points} and \code{anchor}
#' @param orderChr character, when \code{"size"}, sorts chromosomes by total
#'   length from the largest to the smallest. \code{"original"}: preserves d.f. order. \code{"name"}: sorts alphabetically; \code{"group"}: sorts by group name; \code{"chrNameUp"}: sorts according to column \code{chrNameUp}. See \code{chrNameUp}
#' @param centromereSize numeric, optional, this establishes the apparent size of cen. in the plot in \eqn{\mu}m. See \code{autoCenSize=TRUE}. Default: \code{0}. Use with \code{autoCenSize=FALSE}
#' @param autoCenSize boolean, when \code{TRUE} ignores \code{centromereSize}
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
#'   name in OTU column of dfChrSize, \code{"simple"} (just 1 to ...) or \code{"none"}.
#' @param chrIdPatternRem character, regex pattern to remove from chr. names
#' @param distTextChr numeric, distance from name of chromosome to chromosome,
#'   also affects vertical separation of indices. Defaults to \code{1}
#' @param groupUp boolean, when \code{TRUE} when groups present, they appear over the chr. name. Defaults to \code{FALSE}
#' @param groupName boolean, when \code{TRUE} (default), shows group names. When \code{FALSE} only line
#' @param groupSepar numeric, factor for affecting chr. spacing \code{chrSpacing} among groups. Defaults to \code{0.5}
#' @param chromatids boolean, when \code{TRUE} shows separated chromatids. Defaults to \code{TRUE}
#' @param arrowsBothChrt boolean, when \code{TRUE} (default) (for \code{chromatids=TRUE}) shows \code{upArrow,} \code{} \code{ downArrow} styles of marks in both chromatids when \code{arrowsToSide=TRUE}.
#' @param holocenNotAsChromatids boolean, when \code{TRUE} and \code{chromatids=TRUE} does not plot holocen kar. with chromatids. Defaults to \code{FALSE}.
#' @param excHoloFrArrToSide boolean, when \code{arrowsToSide=TRUE}, excludes holocen. from this behaviour, plotting a centered arrow only.
#' @param xModifier numeric, for \code{chromatids=TRUE}, separation among chromatids. Quotient for \code{chrWidth}. Defaults to \code{12 : chrWidth/12}
#' @param xModMonoHoloRate numeric, factor to shrink chromatid separ. for holocen. 5 means 5 times smaller (quotient).
#' @param indexIdTextSize numeric, font size of chr. and kar. indices and
#'   chromosome name. Defaults to \code{1}
#' @param OTUTextSize numeric, font size of OTU name (species). Defaults to \code{1}. When \code{OTUasNote} is \code{TRUE}, use  \code{notesTextSize} instead
#' @param legend character, \code{"none"} for no legend; \code{"inline"} prints labels near
#'   chromosomes; \code{"aside"} prints legend to the right of karyotypes (default). See \code{markLabelSpacer}
#' @param remSimiMarkLeg boolean, when \code{legend="aside"}, if you use \code{pattern}, you can have several marks with same name. When \code{TRUE} this remove this pseudoduplicates from legend. Be sure that this pseudoduplicates have the same color, otherwise you should use \code{FALSE}.
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
#' @param chrIndex character, add arm ratio with \code{"AR"} and centromeric index with \code{"CI"}, or \code{"both"} (Default), or \code{"none"} for none
#' @param chrSize boolean, when \code{TRUE} adds total chr size under each chr. Defaults to \code{FALSE}
#' @param chrNameUp boolean, when \code{TRUE} adds secondary chromosome name from col. \code{chrNameUp} over chrs. Defaults to \code{FALSE}
#' @param classMbName character, name of "chromosome" when in Mbp. Defaults to \code{"Pm"}. See \code{MbUnit}
#' @param classcMName character, name of "chromosome" when OTU in \code{specialOTUNames}. Defaults to \code{"L.G."}
#' @param classChrName character, name of "chromosome" when in micrometers (apparently). Defaults to \code{"Chr."}. See \code{specialOTUnames}, \code{classMbName}, \code{classcMName}
#' @param classChrNameUp character, name of "chromosome" for col. \code{"chrNameUp"}. Defaults to \code{"Type"}
#' @param classGroupName character, name of groups. Defaults to \code{""}
#' @param nsmall numeric, rounding decimals for \code{chrSize} parameter. Defaults to \code{1}
#' @param chrSizeMbp boolean, when \code{TRUE} adds total Mbp chr. size to each chr. provided, there is a \code{Mbp} column in \code{dfChrSize} data.frame. Defaults to \code{FALSE}. If data in columns \code{shortArmSize}, or col. \code{chrSize} is in millions ("Mbp"). Use \code{chrSize=TRUE} not this one (not column \code{Mbp}, you don't need this).
#' @param markPer character vector, name of mark(s) to calculate % of mark in chr. and add it to plot. See \code{perAsFraction}
#' @param showMarkPos boolean, adds position of marks under karyotype (fraction 0-1) when \code{TRUE}. Defaults to \code{FALSE}
#' @param bToRemove, character vector, bands to remove from calc. of pos., when `showMarkPos = TRUE`
#' @param perAsFraction boolean, when \code{TRUE} % is shown as fraction. Defaults to \code{FALSE}. See \code{markPer}
#' @param nameChrIndexPos numeric, modify position of name of chr. indices
#' @param karIndex logical, add karyotype indices A (intrachromosomal -
#'   centromere pos.) and A2 (interchromosomal asymmetry, variation among
#'   chromosome sizes)
#' @param karIndexPos numeric, move karyotype index. Defaults to \code{0.5}
#' @param notesLeft deprecated, use a data.frame for \code{leftNotes}
#' @param notesPosX numeric, move right notes to the right or left (x axis)
#' @param notesPosY numeric, move right notes down or up (y axis)
#' @param leftNotesPosX numeric, move left notes to the right or left (x axis)
#' @param leftNotesPosY numeric, move left notes (\code{leftNotes}) down or up (y axis)
#' @param leftNotesUpPosX numeric, move up left notes to the right or left (x axis)
#' @param leftNotesUpPosY numeric, move up left notes (\code{leftNotesUp}) down or up (y axis)
#' @param morpho character, when \code{"both"} (default) prints the Guerra and Levan classif of cen. position, use also \code{"Guerra"} or  \code{"Levan"} or \code{"none"} for none. See also \code{?armRatioCI}.
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
#' @param collapseCen boolean, avoid spacing in ruler between short arm and long arm.
#' @param ceilingFactor numeric, affects number of decimals for ceiling. Affects max. value of ruler. Defaults to \code{0}. When \code{threshold} is greater than \code{35} this may have to be negative. Introduced in 1.13
#' @param rulerInterval numeric, intervals in ruler. No default, automatic.
#' @param rulerIntervalcM numeric, intervals in ruler of OTU in \code{specialOTUNames}. No default. Introduced in 1.13
#' @param rulerIntervalMb numeric, intervals in ruler of OTU with data in Mb (>\code{MbThreshold}) and absent from \code{specialOTUNames}. No default. Usa data in millions
#' @param yTitle character, units for common title. Defaults to \eqn{\mu m}
#' @param specialOTUNames character vector, normally title of ruler is micrometer or Mb (big numbers). Use this param. to be able to put a different unit in ruler title. See \code{"specialyTitle"}
#' @param specialyTitle, character, title of ruler if OTU is in \code{specialOTUNames}. Will not apply if \code{MbThreshold} met. In that case use \code{MbUnit}
#' @param xlimLeftMod numeric, modifies \code{xlim} left argument of plot
#' @param xlimRightMod numeric, \code{xlim} right side modification by adding space to the right
#'   of idiograms. Defaults to \code{2}
#' @param ylimBotMod numeric, modify \code{ylim} bottom argument of plot
#' @param ylimTopMod numeric, modify \code{ylim} top argument of plot
#' @param lwd.cM thickness of cM marks. Defaults to \code{lwd.chr}
#' @param lwd.marks thickness of most marks. Except \code{cM} marks and centr. related marks. See \code{lwd.chr}, \code{lwd.cM}. Defaults to \code{lwd.chr} value when \code{99}
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
#' @param notes, data.frame, or csv file name in quotes, (shown to the right of kar.), with columns \code{OTU} and \code{note} for adding notes to each OTU, they appear to the right of chromosomes
#' @param leftNotes, data.frame, or csv file name in quotes (shown to the left), with columns \code{OTU} and \code{note} for adding notes to each OTU, they appear to the left of chromosomes
#' @param leftNotesUp, data.frame, or csv file name in quotes, (shown to the left-up), similar to \code{leftNotes}, but intended for placement over chr.
#' @param leftNoteFont, numeric  \code{1} for normal,  \code{2} for bold,  \code{3} for italics,  \code{4} for bold-italics. See \code{leftNotes}
#' @param leftNoteFontUp, numeric  \code{1} for normal,  \code{2} for bold,  \code{3} for italics,  \code{4} for bold-italics. See \code{leftNotesUp}
#' @param noteFont, numeric  \code{1} for normal,  \code{2} for bold,  \code{3} for italics,  \code{4} for bold-italics. See \code{notes}
#' @param parseTypes, boolean, parse in \code{notes} the \emph{Citrus} chr. types names. Creates subindex pos. for FL. Defaults to \code{TRUE}. Incompatible with \code{parseStr2lang}
#' @param parseStr2lang, bolean, parse string in \code{notes} with function \code{str2lang(paste0("paste(",note,")") )} for ex: \code{"italic('C. sinensis'), ' Author'"}. See \code{notes}, \code{leftNotes},\code{leftNotesUp}.
#' @param notesTextSize numeric, font size of notes, see \code{notes}
#' @param leftNotesTextSize numeric, font size of notes, see \code{leftNotes}
#' @param leftNotesUpTextSize numeric, font size of notes, see \code{leftNotesUp}
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
  moveKarHor="",
  moveAllKarValueHor=0,
  moveAllKarValueY=0,
  karAnchorLeft="",
  karAnchorRight="",
  anchor=FALSE,
  anchorLineLty=1,
  anchorText="",
  anchorTextMParental,
  anchorTextMoveX=0.5,
  anchorTextMoveY=1,

  anchorTextMoveParenX=0,
  anchorTextMoveParenY=0,

  anchorVsizeF=.5,
  anchorHsizeF=1,
  pchAnchor=23,
  moveAnchorV=0,
  moveAnchorH=0,
  mkhValue=.5,
  n=50,
  markN=25,
  notes,
  leftNotes,
  leftNotesUp,
  notesTextSize=1,
  leftNotesTextSize=1,
  leftNotesUpTextSize=1,
  notesLeft,

  notesPosX=.5,
  notesPosY=0,

  leftNotesPosX= 0.5,
  leftNotesPosY= 0,

  leftNotesUpPosX=0.5,
  leftNotesUpPosY=0,

  noteFont=1,
  leftNoteFont=1,
  leftNoteFontUp=1,

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
  OTUfamily="",
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
  xModMonoHoloRate=1,
  chrWidth=0.5,
  chrSpacing=0.5,
  specialChrWidth=0.3
  , specialChrSpacing=0.7,

  chrColor="gray",
  chrBorderColor,
  centromereSize=0,
  autoCenSize=TRUE,
  cenColor,
  fixCenBorder=NULL,
  gishCenBorder=FALSE,
  hideCenLines=1.75,
  roundedCen,
  cenFormat="rounded",
  # cenFormat="inProtein",
  cenFactor=1,

  squareness=4,
  lwd.chr=0.5,
  lwd.cM,
  lwd.marks=99,

  #marks
  dfMarkPos,
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
  colorBorderMark="",
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
  collapseCen=TRUE,
  rulerInterval=0,
  rulerIntervalcM=0,
  rulerIntervalMb=0,
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

  if(autoCenSize){
    centromereSize <- NA
  }

  if(verticalPlot==FALSE){
    karSepar <- FALSE
  }

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

  xModifierMono<-xModifier
  xModifierHolo<-xModifier/xModMonoHoloRate

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

  if(lwd.marks!=99) {
    lwd.marks2 <- lwd.marks
  } else {
    lwd.marks2 <- lwd.chr
  }

  if(!missing(lwd.mimicCen)){
    lwd.mimicCen2 <- lwd.mimicCen
  } else {
    lwd.mimicCen2 <- lwd.chr*4
  }

  OTUfont2   <- ifelse( !missing(OTUfont),   OTUfont,   1)
  OTUfamily2 <- ifelse( OTUfamily!= "", OTUfamily, defaultFontFamily2 )

  if(!missing(dfChrSize)) {
    if(inherits(dfChrSize, "data.frame") ) {
      dfChrSizeInt <- makeNumCols(dfChrSize)
    } else if (inherits(dfChrSize, "character") ) {
      if (file.exists(dfChrSize) ) {
        dfChrSize <-read.csv(dfChrSize, header = TRUE)
        dfChrSizeInt <- makeNumCols(dfChrSize)
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
        tryCatch(dfMarkPosInt <- read.csv(dfMarkPos, header = TRUE), error = function(e){"invalid dfMarkPos file"} )
      }
    } else if ( !inherits(dfMarkPos, "data.frame") ) {
      message(crayon::red("dfMarkPos is not a data.frame object or .csv filename in quotes") )
    }

    if(inherits(dfMarkPos, "data.frame") ) {
      if(!nrow(dfMarkPos)>0){
        remove(dfMarkPos)
      } else {
       dfMarkPosInt <- dfMarkPos
      }
    } else {
      remove(dfMarkPos)
    }

    if(exists("dfMarkPosInt") ){
      if(!inherits(dfMarkPosInt, "data.frame") ) {
        remove(dfMarkPosInt)
      } else {
        if(nrow(dfMarkPosInt)==0 ) {
          remove(dfMarkPosInt)
        }
      }
    }
  }

if(exists("dfMarkPosInt") ) { # is a d.f and has >0 rows

    #
    #   rename column markArm if necessary
    #

    if("markArm" %in% colnames(dfMarkPosInt)  ) {
      message(crayon::red(paste(c("Column markArm in d.f. of marks renamed to chrRegion")))
      ) # mess
      colnames(dfMarkPosInt)[which(names(dfMarkPosInt)=="markArm")]<-"chrRegion"
    }

    dfMarkPosInt[dfMarkPosInt==""] <- NA

    copyDfMarkPosInt1 <- dfMarkPosInt <- makeNumCols(dfMarkPosInt)

    tryCatch(initialMarkNames <- unique(as.character(copyDfMarkPosInt1$markName) ), error=function(e) {} )

    if(is.null(copyDfMarkPosInt1$markPos) & is.null(copyDfMarkPosInt1$markDistCen) ){
      copyDfMarkPosInt1$markPos<-NA
      dfMarkPosInt$markPos<-NA
      copyDfMarkPosInt1$markDistCen<-NA
      dfMarkPosInt$markDistCen<-NA
    }

    if(is.null(copyDfMarkPosInt1$markSize)){
      copyDfMarkPosInt1$markSize<-NA
      copyDfMarkPosInt1$markSizeOrig<-NA
      dfMarkPosInt$markSize<-NA
      dfMarkPosInt$markSizeOrig<-NA
    } else {
      dfMarkPosInt$markSizeOrig     <-dfMarkPosInt$markSize
      copyDfMarkPosInt1$markSizeOrig<-copyDfMarkPosInt1$markSize
    }

    #
    # requires chrRegion

    if("chrRegion" %in% colnames(copyDfMarkPosInt1) ) {

    dfCenMarksInt <- copyDfMarkPosInt1[which(copyDfMarkPosInt1$chrRegion %in%
                                                         c("cen","qcen","pcen") ),]

    if(nrow(dfCenMarksInt)==0 ){
      remove(dfCenMarksInt)
    }

    dfpGISHInt <- copyDfMarkPosInt1[which(copyDfMarkPosInt1$chrRegion %in% "p" &
                                                  is.na(copyDfMarkPosInt1$markSize) &
                                                  is.na(copyDfMarkPosInt1$markDistCen)
                                                ),]
    if(nrow(dfpGISHInt)==0 ){
      remove(dfpGISHInt)
    }

    dfqGISHInt <- copyDfMarkPosInt1[which(copyDfMarkPosInt1$chrRegion %in% "q" &
                                                  is.na(copyDfMarkPosInt1$markSize) &
                                                  is.na(copyDfMarkPosInt1$markDistCen)
    ),]
    if(nrow(dfqGISHInt)==0 ){
      remove(dfqGISHInt)
    }

    dfwholeGISHInt <- copyDfMarkPosInt1[which(copyDfMarkPosInt1$chrRegion %in% "w") ,]

    if(nrow(dfwholeGISHInt)==0 ){
      remove(dfwholeGISHInt)
    }

    } else {
      remove(copyDfMarkPosInt1) # absence of chrRegion
    }

} # df mark pos

  ##############################################################################
  #
  #   adds name of otu when missing
  #
  ##############################################################################

  if (exists("dfMarkPosInt")) {

    # dfMarkPosInt<-dfMarkPosInt700
    if(!"chrRegion" %in% colnames(dfMarkPosInt) ){
      dfMarkPosInt$chrRegion<-NA
    }

    # dfMarkPosInt<-dfMarkPosInt700

    dfMarkPosInt <- dfMarkPosInt[which(!dfMarkPosInt$chrRegion %in%
                                               c("cen","qcen","pcen") ),]

    if(nrow(dfMarkPosInt)==0 ){
      remove(dfMarkPosInt)
    }

    if(exists("dfMarkPosInt")) {
      if("markDistCen" %in% colnames(dfMarkPosInt) ){
        dfMarkPosInt <- dfMarkPosInt[which(!c( dfMarkPosInt$chrRegion %in% c("p","q") &
                                                    is.na(dfMarkPosInt$markSize) &
                                                    is.na(dfMarkPosInt$markDistCen) )
            ) ,]
      } else if ("markPos" %in% colnames(dfMarkPosInt) ){
        dfMarkPosInt <- dfMarkPosInt[which(!c( dfMarkPosInt$chrRegion %in% c("p","q") &
                                                is.na(dfMarkPosInt$markSize) &
                                                c(is.na(dfMarkPosInt$markPos) )
        ) ),]
      } else {
        dfMarkPosInt <- dfMarkPosInt[which(!c( dfMarkPosInt$chrRegion %in% c("p","q") &
                                                is.na(dfMarkPosInt$markSize)
        ) ) ,]
      }

      if(nrow(dfMarkPosInt)==0 ){
        remove(dfMarkPosInt)
      }

    }

    if(exists("dfMarkPosInt")) {

      dfMarkPosInt <- dfMarkPosInt[which(!dfMarkPosInt$chrRegion %in% "w") ,]

      if(nrow(dfMarkPosInt)==0 ){
        remove(dfMarkPosInt)
    }
    }

    if(exists("dfMarkPosInt")) {

      listMarkPosInt<-dfToListColumn(dfMarkPosInt)

      dfMarkPosInt <- suppressWarnings(bind_rows( (lapply(
        listMarkPosInt, function(x) { mutate(x, across(.cols=everything(), as.character) ) } ) )
        ,.id = "OTU") )

      dfMarkPosInt <- makeNumCols(dfMarkPosInt)
    }

  } # df of marks

  if (exists("dfCenMarksInt")) {

    pLiMaPosDataCen <- dfToListColumn(dfCenMarksInt)

    # dfCenMarksInt <- dplyr::bind_rows(pLiMaPosDataCen, .id = "OTU")

    dfCenMarksInt <- suppressWarnings(bind_rows( (lapply(
      pLiMaPosDataCen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
    ,.id = "OTU") )

    dfCenMarksInt <- makeNumCols(dfCenMarksInt)

    remove(pLiMaPosDataCen)

  }

  #
  #   mark style
  #

  if(!missing(dfMarkColor)) {

    if(inherits(dfMarkColor, "data.frame") ) {
      tryCatch(dfMarkColorInt <- makeNumCols(dfMarkColor), error=function(e){"empty data.frame"} )
    } else if (inherits(dfMarkColor, "character")) {
      if (file.exists(dfMarkColor)) {
        tryCatch(dfMarkColor <- read.csv(dfMarkColor, header = TRUE), error = function(e){"invalid dfMarkColor file"} )
        tryCatch(dfMarkColorInt <- makeNumCols(dfMarkColor), error = function(e){""} )
      }
    } else {
      message(crayon::red("dfMarkColor is not a data.frame or .csv file") )
    }

    if(exists("dfMarkColorInt") ) {
      if(!inherits(dfMarkColorInt, "data.frame") ) {
        remove(dfMarkColorInt)
      }
    }
  }

  message(paste("Making checks\n"))
  message(paste("In case of error see messages and the help ?functionName\n") )

  #
  #   dfChrSizeInt
  #

  #################################################################### LISTS

  #
  # transform dfs to list of df dfChrSizeInt
  #

  if (!"OTU" %in% colnames(dfChrSizeInt) ) {
    addOTUName<-FALSE
    OTUasNote <-FALSE
    OTUasLeftNote <- FALSE
  }

  #
  # col OTU
  #

  #
  #   reconstitute dfChrSizeInt OTU
  #

  listChrSize <- dfToListColumn(dfChrSizeInt) # adds OTU as name of list

  # dfChrSizeInt <- dplyr::bind_rows(listChrSize, .id = "OTU") # names of list to column
  dfChrSizeInt <- suppressWarnings(bind_rows( (lapply(
    listChrSize, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
    , .id = "OTU") )

  dfChrSizeInt <- makeNumCols(dfChrSizeInt)


  #
  #   Classify data.frames from list as monocen or holocen Add attribute cenType
  #

  #############################################################

  #
  #   careful this operation uses divisor2, changing size, adds centromere attr
  #
  #   changes chr. size for monocen. according to collapseCen of ruler
  #

listChrSizeOrig <- listChrSize

if(cenFormat=="inProtein") {
  collapseCen <- FALSE
}

if(collapseCen){
  listChrSize <- addAttributesDfChrSize(listChrSize,threshold
                                            ,specialOTUNames,centromereSize
                                            ,MbThreshold,cenFactor,
                                            chrWidth,specialChrWidth,squareness
                                            # ,cenFormat
                                            )
} else {
  listChrSize <- addAttributesDfChrSize(listChrSize,threshold
                                        ,specialOTUNames,centromereSize
                                        ,MbThreshold,cenFactor,
                                        chrWidth,specialChrWidth,squareness
                                        # ,cenFormat
                                        ,modifyChr=FALSE
                                        )
}

listChrSizeIndex <- addAttributesDfChrSize(listChrSizeOrig,threshold
                                              ,specialOTUNames,centromereSize
                                              ,MbThreshold,cenFactor,
                                              chrWidth,specialChrWidth,squareness
                                              # ,cenFormat
                                              ,modifyChr=FALSE
                                           ,mymessage=FALSE
                                            )


dfChrSizeIntDivisor <- suppressWarnings(bind_rows( (lapply(
    listChrSize, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
    , .id = "OTU") )

dfChrSizeIntDivisor <- makeNumCols(dfChrSizeIntDivisor)

# important must be after bind_rows
listChrSize      <- lapply(listChrSize, function(x) makeNumCols(x))
listChrSizeIndex <- lapply(listChrSizeIndex, function(x) makeNumCols(x))

#
#   create inProtein marks simulating cen. divisor applied
#

if(cenFormat=="inProtein") {

pLiMaPosMonocenCen <- list()

for (i in 1:length(listChrSize) ) {

  if(attr(listChrSize[[i]], "cenType")=="monocen") { # only for monocen

    cenSize <- as.numeric(attr(listChrSize[[i]],"centromere") ) #* inProteinCenFactor

    if(cenSize > 0) {
    otu <- names(listChrSize[i])

    pLiMaPosMonocenCen[[i]] <- data.frame(
      chrName=as.character(listChrSize[[i]]$chrName)
       ,markName="inProteinCentromere"
       ,markSize= cenSize
       ,markDistCen= - (cenSize/2)
       ,chrRegion="p"
    )
    names(pLiMaPosMonocenCen)[i]<-otu
    }
  }
}

pLiMaPosMonocenCen <- Filter(function(x) {!is.null(x) }, pLiMaPosMonocenCen)
pLiMaPosMonocenCen <- Filter(function(x) {nrow(x) >= 1}, pLiMaPosMonocenCen)

if(length(pLiMaPosMonocenCen)==0){
  remove(pLiMaPosMonocenCen)
} else {

  #
  #   inproteincentromere marks (pseudocentromeres must go before mark, but are merge with rev)
  #

  dfMarkPosInt4 <- suppressWarnings(bind_rows( (lapply(
    pLiMaPosMonocenCen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
    , .id = "OTU") )

  dfMarkPosInt4 <- makeNumCols(dfMarkPosInt4)

  remove(pLiMaPosMonocenCen)
  if(!inherits(dfMarkPosInt4, "data.frame" ) ) {
    remove(dfMarkPosInt4)
  }
}
} # inProtein

  #
  #     calculate armRatioCI only for chr. with centromere attr cenType =  holocen.
  #

  #
  #    generate Chromosome indexes for Monocen
  #
  if(chrIndex=="both" | chrIndex=="AR"| chrIndex=="CI" | morpho=="both" | morpho=="Guerra" | morpho == "Levan" | chrSize == TRUE | markPer[1]!="" | showMarkPos) {
    for (i in 1:length(listChrSizeIndex)) {
      if(attr(listChrSizeIndex[[i]], "cenType")=="monocen"){ # only for monocen

        listChrSizeIndex[[i]] <- armRatioCI(listChrSizeIndex[[i]])

        if(attr(listChrSizeIndex[[i]], "indexStatus")=="failure"){

          if("OTU" %in% colnames(listChrSizeIndex[[i]])){

            message(crayon::red(paste("in",unique(listChrSizeIndex[[i]]$OTU) ) ) )
          } # otu
            message(crayon::red("\nFix measures or use chrIndex=\"none\", and morpho=\"none\" ")
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
  monocenNames<-makeVectorNames(listChrSize,"cenType","monocen")
  holocenNames<-makeVectorNames(listChrSize,"cenType","holocen")
}

  ##########################################3
  #
  #     gish  p
  #
  #########################################

# message(crayon::green("GISH data loading"))

  if (exists("dfpGISHInt")) {

    listOfdfpGISHInt<-dfToListColumn(dfpGISHInt)

    # monocen

    listOfdfpGISHIntMonocen<-listOfdfpGISHInt[which(names(listOfdfpGISHInt) %in% monocenNames)]

    # names(listOfdfpGISHIntMonocen)
    if(length(listOfdfpGISHIntMonocen)==0){
      remove(listOfdfpGISHIntMonocen)
    } else {
      listOfdfpGISHIntMonocen<-Filter(function(x) {nrow(x) >= 1}, listOfdfpGISHIntMonocen)

      # dfpGISHIntMonocen <- dplyr::bind_rows(listOfdfpGISHIntMonocen, .id = "OTU")

      dfpGISHIntMonocen <- suppressWarnings(bind_rows( (lapply(
        listOfdfpGISHIntMonocen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfpGISHIntMonocen <- makeNumCols(dfpGISHIntMonocen)

      # dfpGISHIntMonocen$chrRegion<-"p"
    } # else

    # P marks of Holocen MUST NOt exist

    checkArmHolocenError(listOfdfpGISHInt,holocenNames)

  } #   if (exists("dfpGISHInt")){

  ##########################################3
  #
  #     gish  q
  #
  #########################################

  if (exists("dfqGISHInt")){

    listOfdfqGISHInt<-dfToListColumn(dfqGISHInt)

    # monocen

    listOfdfqGISHIntMonocen<-listOfdfqGISHInt[which(names(listOfdfqGISHInt) %in% monocenNames)]

    if(length(listOfdfqGISHIntMonocen)==0){
      remove(listOfdfqGISHIntMonocen)
    } else {

      listOfdfqGISHIntMonocen <- Filter(function(x) {nrow(x) >= 1}, listOfdfqGISHIntMonocen)

      # dfqGISHIntMonocen <- dplyr::bind_rows(listOfdfqGISHIntMonocen, .id = "OTU")

      dfqGISHIntMonocen <- suppressWarnings(bind_rows( (lapply(
        listOfdfqGISHIntMonocen, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfqGISHIntMonocen <- makeNumCols(dfqGISHIntMonocen)

    } # else

    # q marks of Holocen MUST NOt exist

    checkArmHolocenError(listOfdfqGISHInt,holocenNames)

  } #   if (exists("dfpGISHInt")){

  ##########################################3
  #
  #     gish whole
  #
  #########################################

if(exists("dfwholeGISHInt")) {

    listOfdfwholeGISHInt <- dfToListColumn(dfwholeGISHInt)

    ###########################################################################################################################3
    #
    # MONOCEN GISH TO P Q CEN
    #

    listOfdfwholeGISHMonocen<-listOfdfwholeGISHInt[which(names(listOfdfwholeGISHInt) %in% monocenNames)]

    if(length(listOfdfwholeGISHMonocen)==0) {
      remove(listOfdfwholeGISHMonocen)
    } else {
      listOfdfwholeGISHMonocen<-Filter(function(x) {nrow(x) >= 1}, listOfdfwholeGISHMonocen)

      #
      #   p part
      #

      listOfdfpGISHIntMonocen2 <- listOfdfwholeGISHMonocen

      # dfpGISHIntMonocen2 <- dplyr::bind_rows(listOfdfpGISHIntMonocen2, .id = "OTU")

      dfpGISHIntMonocen2 <- suppressWarnings(bind_rows( (lapply(
        listOfdfpGISHIntMonocen2, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfpGISHIntMonocen2 <- makeNumCols(dfpGISHIntMonocen2)

      dfpGISHIntMonocen2$chrRegion<-"p"
      dfpGISHIntMonocen2$chrRegionOrig<-"w"

      #
      #   q part
      #

      listOfdfqGISHIntMonocen2 <- listOfdfwholeGISHMonocen

      # dfqGISHIntMonocen2 <- dplyr::bind_rows(listOfdfqGISHIntMonocen2, .id = "OTU")

      dfqGISHIntMonocen2 <- suppressWarnings(bind_rows( (lapply(
        listOfdfqGISHIntMonocen2, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfqGISHIntMonocen2 <- makeNumCols(dfqGISHIntMonocen2)

      dfqGISHIntMonocen2$chrRegion<-"q"
      dfqGISHIntMonocen2$chrRegionOrig<-"w"

      #
      # cen part
      #

      listOfdfCenMarksInt2 <- listOfdfwholeGISHMonocen

      # dfCenMarksInt2 <- dplyr::bind_rows(listOfdfCenMarksInt2, .id = "OTU")

      dfCenMarksInt2<- suppressWarnings(bind_rows( (lapply(
        listOfdfCenMarksInt2, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) )
        , .id = "OTU") )

      dfCenMarksInt2 <- makeNumCols(dfCenMarksInt2)

      dfCenMarksInt2$chrRegion<-"cen"

      # dfCenMarksInt2$chrRegionOrig<-"w" leaving this hides w names completely in inline

      cendfs <- mget(ls(pattern = "^dfCenMarksInt" ) )

      if(length(cendfs) ) {
        # dfCenMarksInt <- suppressWarnings(dplyr::bind_rows(cendfs) )

        dfCenMarksInt<- suppressWarnings(bind_rows((lapply(
          cendfs, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

        dfCenMarksInt <- makeNumCols(dfCenMarksInt)

      }
      remove(listOfdfwholeGISHMonocen)
    } # whole to p q cen

    #
    # HOLOCEN
    #
    ###########################################################################################################

    listOfdfwholeGISHHolocen <- listOfdfwholeGISHInt[which(names(listOfdfwholeGISHInt) %in% holocenNames)]

    if(length(listOfdfwholeGISHHolocen)==0){
      remove(listOfdfwholeGISHHolocen)
    } else {

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
      corr_index <- which(names(listChrSize) %in% dfwholeGISHHolocen$OTU[i] )
      dfwholeGISHHolocen$r2[i] <- as.numeric(attr(listChrSize[[corr_index]],"r2"))
    }

    dfwholeGISHHolocen$markSize <- dfChrSizeInt[match(interaction(dfwholeGISHHolocen[c("OTU","chrName")] ),
                                                                  interaction(dfChrSizeInt[c("OTU","chrName") ] )
    ),]$chrSize

    dfwholeGISHHolocen$markSizeProtein<-dfwholeGISHHolocen$markSize-(dfwholeGISHHolocen$r2*2)

    dfwholeGISHHolocen$markPos <- 0
    dfwholeGISHHolocen$markPosProtein <- dfwholeGISHHolocen$markPos + dfwholeGISHHolocen$r2

    if (markDistType=="cen") {
        dfwholeGISHHolocen$markPos        <- dfwholeGISHHolocen$markPos + dfwholeGISHHolocen$markSize/2
        dfwholeGISHHolocen$markPosProtein <- dfwholeGISHHolocen$markPosProtein + dfwholeGISHHolocen$markSizeProtein/2
    }

    dfMarkPosInt5 <-dfwholeGISHHolocen
    remove(dfwholeGISHHolocen)

    } #     if(length(listOfdfwholeGISHHolocen)==0){

  }  #  end   if(exists("dfwholeGISHInt")){

#
#     transform cen. marks into inProtein, divisor applied
#

preserveIP<-character()

if(cenFormat=="inProtein" & exists("dfCenMarksInt") ) {

  dfMarkPosInt3 <- dfCenMarksInt

  remove(dfCenMarksInt)

  cenSize <- unique(dfChrSizeIntDivisor[match(interaction(dfMarkPosInt3[c("OTU","chrName")] )
                                           ,interaction(dfChrSizeIntDivisor[c("OTU","chrName") ] )
  ),]$centromereSize)

  for (row in 1:nrow(dfMarkPosInt3) ) {
    if(dfMarkPosInt3$chrRegion[row] == "cen" ) {
      dfMarkPosInt3$chrRegion[row]   <- "p"
      dfMarkPosInt3$markDistCen[row] <- -(cenSize/2)
    } else {
      dfMarkPosInt3$markDistCen[row] <- -(cenSize/2)
      preserveIP <- c(preserveIP,paste0("inProtein",dfMarkPosInt3$markName[row]) )
    }
  }

  dfMarkPosInt3$markSize    <- cenSize

  #
  #   mark colors
  #

  nameList <- unique(dfMarkPosInt3$markName)

  dfMarkPosInt3$markNameOld<-dfMarkPosInt3$markName

  dfMarkPosInt3$markName <- paste0("inProtein",dfMarkPosInt3$markName)

  if(!missing(bannedMarkName)){
    toBan <- nameList[which(nameList %in% bannedMarkName)]
    if(length(toBan)){
      bannedMarkName4 <- paste0("inProtein",toBan)
    }
  }

  if(!missing(forbiddenMark)){
    toBan <- NULL
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

  gishMonocenDfsP <- mget(ls(pattern = "^dfpGISHIntMonocen" ) )

  if(length(gishMonocenDfsP) ) {
    # MdfpGISHIntMonocen <- suppressWarnings(dplyr::bind_rows(gishMonocenDfsP) )

    MdfpGISHIntMonocen<- suppressWarnings(bind_rows((lapply(
      gishMonocenDfsP, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    MdfpGISHIntMonocen <- makeNumCols(MdfpGISHIntMonocen)

  }

  if(exists("MdfpGISHIntMonocen") ) {
    #
    #   divisor not used see 990
    #
    MdfpGISHIntMonocen <- markDistCenGISHfix(MdfpGISHIntMonocen,dfChrSizeInt
                                                  ,"shortArmSize",markDistType
                                                  ,listChrSize)
  } # p gish

  ############################################################################################
  # merge q

  gishMonocenDfsQ <- mget(ls(pattern = "^dfqGISHIntMonocen" ) )

  if(length(gishMonocenDfsQ) ) {

    MdfqGISHIntMonocen<- suppressWarnings(bind_rows((lapply(
      gishMonocenDfsQ, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    MdfqGISHIntMonocen <- makeNumCols(MdfqGISHIntMonocen)

  }

  if(exists("MdfqGISHIntMonocen") ) {
    #
    #   divisor not used see 990
    #
    MdfqGISHIntMonocen <- markDistCenGISHfix(MdfqGISHIntMonocen,dfChrSizeInt
                                                  ,"longArmSize",markDistType
                                                  ,listChrSize)
  } # q gish

  ##################################################################################################
  #
  #       merging p and q
  #
  ##################################################################################################

  gishMonocenDfsPQ <- mget(ls(pattern = "^Mdf" ) )

  if(length(gishMonocenDfsPQ) ) {
    # dfMarkPosInt2 <- suppressWarnings(dplyr::bind_rows(gishMonocenDfsPQ) )

    dfMarkPosInt2 <- suppressWarnings(bind_rows((lapply(
      gishMonocenDfsPQ, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    dfMarkPosInt2 <- makeNumCols(dfMarkPosInt2)
  }

  #
  #    merge dfMarkPosInt2 dfMarkPosInt  dfMarkPosInt3 dfMarkPosInt4 dfMarkPosInt5
  #

  mDfMarkPosI <- mget(ls(pattern = "^dfMarkPosInt" ) )


  if(length(mDfMarkPosI) ) {
    #
    #   rev gish must be first to be background color
    #
    dfMarkPosInt<- suppressWarnings(bind_rows(rev(lapply(
      mDfMarkPosI, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    dfMarkPosInt <- makeNumCols(dfMarkPosInt)

    # dfMarkPosInt <- suppressWarnings(dplyr::bind_rows(rev(mDfMarkPosI) ) )
  }

  #
  #     DF OF marks to list
  #

  if (exists("dfMarkPosInt") ) {

    dfMarkPosInt   <- unique(dfMarkPosInt)

    listMarkPosInt <- dfToListColumn(dfMarkPosInt)

    #
    #              monocen marks list creation
    #

    pLiMaPosMonocen <- listMarkPosInt[which(names(listMarkPosInt) %in% monocenNames)]

    if(length(pLiMaPosMonocen)==0){
      remove(pLiMaPosMonocen)
    } else {
      for (i in 1:length(pLiMaPosMonocen)) {

        #
        #   requires chrRegion
        #

        missingCol <-setdiff(c("chrRegion"),
                            colnames(pLiMaPosMonocen[[i]]) )

        if(length (missingCol )==0 ) {

          # preserve inProtein located at pcen and qcen
        if(length(preserveIP)){
           pLiMaPosMonocenIP  <- pLiMaPosMonocen[[i]][which(pLiMaPosMonocen[[i]]$chrRegion %in%
                                                                             c("pcen","qcen") &
                                                                            pLiMaPosMonocen[[i]]$markName %in%
                                                                            preserveIP),]
        } else {
          pLiMaPosMonocenIP <- data.frame()
        }

        pLiMaPosMonocenNoCen <- pLiMaPosMonocen[[i]][which(!pLiMaPosMonocen[[i]]$chrRegion %in%
                                                                             c("cen","pcen","qcen") ),]
        # inProtein must be after to overlap cen.

        if(nrow(pLiMaPosMonocenIP)>0) {
          pLiMaPosMonocen[[i]] <- rbind(pLiMaPosMonocenNoCen,pLiMaPosMonocenIP)
        } else if(nrow(pLiMaPosMonocenNoCen)>0) {
          pLiMaPosMonocen[[i]] <- pLiMaPosMonocenNoCen
        } else {
          pLiMaPosMonocen[[i]] <- data.frame()
        }

        } else {
          message(crayon::red("missing column chrRegion in dfMarkPos, unable to plot monocen. marks" ) )
        }
      } # for

      pLiMaPosMonocen<-Filter(function(x) {nrow(x) >= 1}, pLiMaPosMonocen)

      if(length(pLiMaPosMonocen)==0){
        remove(pLiMaPosMonocen)
      }
    } # else

    #
    #                holocen marks list creation
    #

    pLiMaPosHolocen <- listMarkPosInt[which(names(listMarkPosInt) %in% holocenNames)]
    if(length(pLiMaPosHolocen)==0){
      remove(pLiMaPosHolocen)
    }

} # end missing dfMarkPosInt

  #
  #   df of cen marks to list
  #

  if (exists("dfCenMarksInt")){ # when cen. non-inProtein

    #
    #   creation pLiMaPosDataCen
    #

    pLiMaPosDataCen <- dfToListColumn(dfCenMarksInt)

    pLiMaPosDataCen <- pLiMaPosDataCen[which(names(pLiMaPosDataCen) %in% monocenNames)]

    if(length(pLiMaPosDataCen)==0) {
      remove(pLiMaPosDataCen)
    } else {
      #
      #   remove columns without info.
      #
      for (i in 1:length(pLiMaPosDataCen)){
        pLiMaPosDataCen[[i]][pLiMaPosDataCen[[i]]==""]<-NA
        pLiMaPosDataCen[[i]]<-  pLiMaPosDataCen[[i]][, !apply(is.na(pLiMaPosDataCen[[i]]), 2, all)]
      } # for
    } # else
  } # end missing dfCenMarksInt

  #
  #   for each d.f. of dfmarkpos check columns
  #

  ############################################################################################################################
  #
  #   Monocen check marks
  #

  if(exists("pLiMaPosMonocen")){
    message(
      "\nChecking mandatory columns from dfMarkPos: chrName, markName, chrRegion,markDistCen\n (column OTU  is necessary if more than one species)\nmarkSize can be absent when cM style"
    )# cat

    for (i in 1:length(pLiMaPosMonocen ) ) {

      pLiMaPosMonocen[[i]][pLiMaPosMonocen[[i]]==""] <- NA
      pLiMaPosMonocen[[i]] <- pLiMaPosMonocen[[i]][, !apply(is.na(pLiMaPosMonocen[[i]]), 2, all)]

      #
      #   rename column markpos if necessary
      #

      if(!"markDistCen" %in% colnames(pLiMaPosMonocen[[i]]) & "markPos" %in% colnames(pLiMaPosMonocen[[i]])  ){
        message(crayon::red(
          paste(c("Column markPos in d.f. of marks of OTU",names(pLiMaPosMonocen)[[i]]
                  ,"renamed to markDistCen")))
        ) # mess
        colnames(pLiMaPosMonocen[[i]])[which(names(pLiMaPosMonocen[[i]])=="markPos")]<-"markDistCen"
      }

      #
      #   column error check
      #

      missingCol<-setdiff(c("chrName", "markName", "chrRegion","markDistCen"),
                    colnames(pLiMaPosMonocen[[i]]) )

      if(length (missingCol )>0 ) {
        message(crayon::red(paste(c("ERROR Missing columns in d.f. of marks of OTU"
                                    ,names(pLiMaPosMonocen)[[i]] ,":"
                                    ,missingCol) , sep="\n", collapse = " "
                                  )
        )
        ) # cat
        message(crayon::red(paste("\nERRORS PRESENT, see above, dfMarksPos of OTU"
                                  , names(pLiMaPosMonocen)[[i]]
                                  ,"REMOVED\n")
        ) ) #m
        pLiMaPosMonocen[[i]]<-NA
      } # fi setdiff
      #
      #   column without error
      #
      else { # if no error

        corr_index<- which(names(listChrSize) %in% names(pLiMaPosMonocen)[[i]] )

        divisor2 <- as.numeric(attr(listChrSize[[corr_index]],"divisor"))

        #
        #   careful divisor applied
        #

        #
        #  don't apply divisor to inProtein marks, check that $centromereSize behave similarly
        #

        selectionNoInProtein <- which(FALSE==pLiMaPosMonocen[[i]]$markName %in%
                                        grep("inProtein", pLiMaPosMonocen[[i]]$markName,
                                             value =TRUE) )

        pLiMaPosMonocen[[i]][selectionNoInProtein,]$markDistCen <-
          pLiMaPosMonocen[[i]][selectionNoInProtein,]$markDistCen/divisor2

          if("markSize" %in% colnames(pLiMaPosMonocen[[i]])){
              pLiMaPosMonocen[[i]][selectionNoInProtein,]$markSize <-
                pLiMaPosMonocen[[i]][selectionNoInProtein,]$markSize/divisor2
          }

        message(paste("\nOK marks of OTU",names(pLiMaPosMonocen)[[i]],"checked \n")
                 ) #m
        if(markDistType=="cen") { # this is from center

          #
          #   fix bug when markDistType is cen (center) but cM style of marks have NA in markSize column
          #

          if("markSize" %in% colnames(pLiMaPosMonocen[[i]])){
          pLiMaPosMonocen[[i]]$markDistCen <- psum(pLiMaPosMonocen[[i]]$markDistCen,
                                                          ( - pLiMaPosMonocen[[i]]$markSize/2),
                                                          na.rm=TRUE)
          }
        } # if
      } # else No Error
    } # for each data.frame of Marks of Monocen

  pLiMaPosMonocen<-pLiMaPosMonocen[!is.na(pLiMaPosMonocen)]

  # do as before with holo 27/09
} # fi pLiMaPosMonocen

  ##################################################################################################################
  #
  #   holocen check mark
  #

  if(exists("pLiMaPosHolocen")){
    message("\nChecking mandatory columns from dfMarkPos (without cen.): chrName, markName, markPos\n (column OTU  is necessary if more than one species)\nmarkSize column is not necessary for style of mark cM"
    ) # mess
    # pLiMaPosHolocen<-pLiMaPosHolocen1559
    # i<-1
    for (i in 1:length(pLiMaPosHolocen ) ) {

      pLiMaPosHolocen[[i]][pLiMaPosHolocen[[i]]==""] <- NA
      pLiMaPosHolocen[[i]]<-  pLiMaPosHolocen[[i]][, !apply(is.na(pLiMaPosHolocen[[i]]), 2, all)]

      #
      #   rename column markdistcen if necessary
      #

      if(!"markPos" %in% colnames(pLiMaPosHolocen[[i]]) & "markDistCen" %in% colnames(pLiMaPosHolocen[[i]])  ){
        message(crayon::red(paste(c("Columns markDistCen in d.f. of marks of OTU",names(pLiMaPosHolocen)[[i]]
                                    ,"renamed to markPos")))
        ) # mess
        colnames(pLiMaPosHolocen[[i]])[which(names(pLiMaPosHolocen[[i]])=="markDistCen")]<-"markPos"
      }

      #
      #   column error
      #

      if(length (setdiff(c("chrName", "markName", "markPos"),
                           colnames(pLiMaPosHolocen[[i]]) ) )>0 ) {
          message(crayon::red(paste(c("ERROR Missing columns:",
                                      # setdiff(c("chrName", "markName", "markPos","markSize"),
                                              setdiff(c("chrName", "markName", "markPos"),
                                              colnames(pLiMaPosHolocen[[i]]) ) ) , sep="\n", collapse = " " )
          )
          ) # cat
          message(crayon::red(paste("\nERRORS PRESENT, see above, dfMarksPos of OTU", names(pLiMaPosHolocen)[[i]] ,"REMOVED\n")
          ) ) #m
          # pLiMaPosHolocen<-pLiMaPosHolocen[-i]
          pLiMaPosHolocen[[i]]<-NA
        } else { # if no error
          message(paste("\nOK marks of OTU",names(pLiMaPosHolocen)[[i]],"checked \n")
          )
          #m
          if(any(is.na(pLiMaPosHolocen[[i]]$markPos))) {
            message(crayon::blue(paste("\nholocen. mark(s) without pos. might get unexpected results\n")
            ))
          }

          pLiMaPosHolocen[[i]]$markPos2 <- pLiMaPosHolocen[[i]]$markPos
          pLiMaPosHolocen[[i]]$markPosProtein2 <- pLiMaPosHolocen[[i]]$markPosProtein

        if(origin=="t") {

          pLiMaPosHolocen[[i]]$chrSize<-
            dfChrSizeIntDivisor[match(interaction( pLiMaPosHolocen[[i]][c("OTU", "chrName")]),
                                    interaction( dfChrSizeIntDivisor[c("OTU", "chrName")] )
                                    ),]$chrSize

          if(markDistType=="beg"){

            if("markSize" %in% colnames(pLiMaPosHolocen[[i]]) ) {
              pLiMaPosHolocen[[i]]$markPos <- psum(pLiMaPosHolocen[[i]]$chrSize,
                                                         - pLiMaPosHolocen[[i]]$markPos2,
                                                         - pLiMaPosHolocen[[i]]$markSize,
                                                         na.rm=TRUE)

              pLiMaPosHolocen[[i]]$markPosProtein <- psum(pLiMaPosHolocen[[i]]$chrSize,
                                                   - pLiMaPosHolocen[[i]]$markPosProtein2,
                                                   - pLiMaPosHolocen[[i]]$markSize,
                                                   na.rm=TRUE)



            } # markSize column exist

          } else if(markDistType=="cen") {
              if("markSize" %in% colnames(pLiMaPosHolocen[[i]])){
                pLiMaPosHolocen[[i]]$markPos<-psum( pLiMaPosHolocen[[i]]$chrSize,
                                                           - pLiMaPosHolocen[[i]]$markPos2,
                                                           (- pLiMaPosHolocen[[i]]$markSize/2),
                                                           na.rm=TRUE)
                pLiMaPosHolocen[[i]]$markPosProtein<-psum( pLiMaPosHolocen[[i]]$chrSize,
                                                    - pLiMaPosHolocen[[i]]$markPosProtein2,
                                                    (- pLiMaPosHolocen[[i]]$markSize/2),
                                                    na.rm=TRUE)

              } # col markSize exists
          } # cen

        } else if (origin=="b") { # if t else b


          if(markDistType=="cen") { # center
            if("markSize" %in% colnames(pLiMaPosHolocen[[i]])) {

            pLiMaPosHolocen[[i]]$markPos <- psum(pLiMaPosHolocen[[i]]$markPos2,
                                                        (- pLiMaPosHolocen[[i]]$markSize/2),
                                                        na.rm=TRUE)

            pLiMaPosHolocen[[i]]$markPosProtein <- psum(pLiMaPosHolocen[[i]]$markPosProtein2,
                                                 (- pLiMaPosHolocen[[i]]$markSize/2),
                                                 na.rm=TRUE)

            } # if col markSize exist
          } # cen
        } # origin b
      } # else No Error
    } # for each data.frame of Marks of Monocen

  pLiMaPosHolocen <- pLiMaPosHolocen[!is.na(pLiMaPosHolocen)]

  for (i in 1:length(pLiMaPosHolocen)) {

    corr_index<- which(names(listChrSize) %in% names(pLiMaPosHolocen)[[i]] )
    divisor2  <- as.numeric(attr(listChrSize[[corr_index]],"divisor"))

    # if(attr(pLiMaPosHolocen[i],"name") %in% MbNames ){
      pLiMaPosHolocen[[i]]$markPos <- pLiMaPosHolocen[[i]]$markPos/divisor2
      if("markSize" %in% colnames(pLiMaPosHolocen[[i]])){
        pLiMaPosHolocen[[i]]$markSize <- pLiMaPosHolocen[[i]]$markSize/divisor2
      }
    # } # if
  } # for
} # fi holocen exists

  ################################################################################################################################
  #
  #   cen Mark check
  #

  if(exists("pLiMaPosDataCen")) {
    message("\nChecking mandatory columns from dfCenMarks: chrName, markName\n (column OTU  is necessary if more than one species)\n")

    for (i in 1:length(pLiMaPosDataCen)){
      #
      #   columns with error
      #

    if(length(setdiff(c("chrName", "markName"),
                      colnames(pLiMaPosDataCen[[i]]) ) )>0 ){
      message(crayon::red(paste(c("ERROR Missing columns:",
                              setdiff(c("chrName", "markName"),
                                      colnames(pLiMaPosDataCen[[i]]) ),"in OTU", names(pLiMaPosDataCen)[[i]]   ), sep="\n", collapse = " " )
      )
      ) # cat
      message(crayon::red(paste("\nERRORS PRESENT, see above, dfCenMarks of OTU", names(pLiMaPosDataCen)[[i]] ,"REMOVED\n")
      ) ) #m
      pLiMaPosDataCen[[i]] <- NA

    } # fi

      #
      #   columns without error
      #

    else { # if no error
      message(paste("\nOK cen. marks of OTU",names(pLiMaPosDataCen)[[i]],"checked \n")
      ) # mess
    } # else
    } # for

  pLiMaPosDataCen<-pLiMaPosDataCen[!is.na(pLiMaPosDataCen)]

  } # fi   if(exists("pLiMaPosDataCen"))

  ##############################################################################################################
  #
  #   OTU cross check of d.fs
  #

  if(exists("pLiMaPosMonocen")){

    pLiMaPosMonocen<- filterExtraOTU(listChrSize,pLiMaPosMonocen)

  } # exists

  if(exists("pLiMaPosHolocen")){
    # message("\n####\ndfMarkPos exists, if error will be removed\n")

    pLiMaPosHolocen<- filterExtraOTU(listChrSize,pLiMaPosHolocen)

  } # exists

  #
  #     check chromosomes names  from d.f. marks to chr. size. d.f.
  #

  if(exists("pLiMaPosMonocen") ) {
    listOfChecksChr   <- checkNameChrDfMarks(listChrSize,pLiMaPosMonocen)

    listChrSize <- listOfChecksChr[[1]]

    pLiMaPosMonocen <-listOfChecksChr[[2]]

    if(length(pLiMaPosMonocen)==0){
      remove(pLiMaPosMonocen)
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

      if(exists("allMarkNames") ) {
        if(!length(allMarkNames)){
          remove(allMarkNames)
        }
      }

      # allMarkMaxSize first estimate

      if(length(listOfChecksChr[[4]])>0) {
        allMarkMaxSize <- max(listOfChecksChr[[4]], na.rm=TRUE)
      }
    }
  } # pLiMaPosMonocen


  if(exists("pLiMaPosHolocen") ) {

    # listChrSize<-listChrSize1780
    # pLiMaPosHolocen<-pLiMaPosHolocen1781

    listOfChecksChr<-checkNameChrDfMarks(listChrSize,pLiMaPosHolocen)
    # listOfChecksChr<-idiogramFISH:::checkNameChrDfMarks(listChrSize,pLiMaPosHolocen)

    listChrSize    <-listOfChecksChr[[1]]

    pLiMaPosHolocen<-listOfChecksChr[[2]]

    if(length(pLiMaPosHolocen)==0) {
      remove(pLiMaPosHolocen)
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

  if(exists("pLiMaPosDataCen") ) {
      listOfChecksChr  <- checkNameChrDfMarks(listChrSize,pLiMaPosDataCen)

      listChrSize<- listOfChecksChr[[1]]

      pLiMaPosDataCen<-listOfChecksChr[[2]]

      if(length(pLiMaPosDataCen)==0){
        remove(pLiMaPosDataCen)
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

      } # pLiMaPosDataCen 0
    } # pLiMaPosDataCen

  ###############################################################################
  #
  #   remake dfMarkPosInt (for per. mark) after filtering from lists
  #
  if(exists("allMarkNames") & markPer[1]!="" ) {
    markPer <- intersect(markPer,allMarkNames)
  }

  mlists <- ls(pattern = "^pLiMaPos" )

  if(length(mlists)) {
    plist <- lapply(mget(mlists ), function(x) unname(x) )
    #
    #   last dfMarkPosInt
    #
    # dfMarkPosInt <- suppressWarnings(dplyr::bind_rows(plist) )

    plist <- rbind.fill(lapply(plist, rbind.fill) )

    dfMarkPosInt <- makeNumCols(plist)
  }

  ###############################################################################
  #
  #   check compatibility of columns dfMarkColor
  #

  if(chrColor==""){
    chrColor<-"gray"
  } else {
    chrColor <- filter_colors(chrColor)
  }
  if(length(chrColor)==0){
    chrColor <- "white"
  }

  if(!missing(cenColor)) {

    if(length(is.na(cenColor)) ) {
      if (is.na(cenColor) ){
        cenColor2<-NULL
      } else if (cenColor==""){
        cenColor2 <- filter_colors(chrColor)
      } else if (cenColor=="NULL"){
        cenColor2<-NULL
      } else {
        cenColor2 <- filter_colors(cenColor)
      }
    } else if( is.null(cenColor) ){
      cenColor2<-NULL
    }
  } else {
    cenColor2 <- filter_colors(chrColor)
  }

  if(length(cenColor2)==0){
      cenColor2 <- filter_colors(chrColor)
  }

  if (!missing(mycolors) ) {

    mycolors2 <- filter_colors(mycolors)

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

    if( exists("consolMarkNames") & exists("dfMarkColorInt") ) {

      reservedfMarkColorInt <- dfMarkColorInt[which(FALSE==(dfMarkColorInt$markName %in% consolMarkNames) ) ,]

      if (nrow(reservedfMarkColorInt)==0) {
        remove(reservedfMarkColorInt)
      }

      dfMarkColorInt <- dfMarkColorInt[which(dfMarkColorInt$markName %in% consolMarkNames) ,]

      if (nrow(dfMarkColorInt)==0) {
        message(crayon::red("\nError in dfMarkColor markNames respect to Marks pos. data.frames, dfMarkColor REMOVED\n")
        ) # cat
        remove(dfMarkColorInt)
      }
    }

    if(exists("dfMarkColorInt") ) {

    message("\n####\nChecking mandatory columns from dfMarkColor: markName, markColor\n"
    ) #cat

    #
    #   create style column when missing
    #

    if(!"style" %in% colnames(dfMarkColorInt) ) {
      message(crayon::red(paste0("\ndfMarkColor style column missing, created with string: ",defaultStyleMark,"\n")
                          ) ) # m
      dfMarkColorInt$style <- defaultStyleMark    # "square" or user default
    }

    if (length( setdiff(c("markName", "markColor","style"),
                        colnames(dfMarkColorInt) ) )>0 ) {
      #
      #   removal
      #
      message(crayon::red(paste(c("ERROR Missing column:",
                              setdiff(c("markName", "markColor","style"),
                                      colnames(dfMarkColorInt) ) ) , sep="\n", collapse = " ")
      )
      )# cat
      remove(dfMarkColorInt)
      message(crayon::red("\nError in dfMarkColor, REMOVED\n") )

    } else {        # column names ok

      # missing color for constric cenStyle

      tryCatch(dfMarkColorInt[which(dfMarkColorInt$markColor==""),]$markColor <- NA, error= function(e){NA})

      tryCatch(bannedMarkName2 <- dfMarkColorInt[
        which(is.na(dfMarkColorInt$markColor) ) , ]$markName, error= function(e){NA} )

      tryCatch(dfMarkColorInt[which(is.na(dfMarkColorInt$markColor) &
                     dfMarkColorInt$style %in% "cenStyle"),]$markColor <- chrColor,
               error= function (e) NA )

      if(exists("consolMarkNames") ) {

        if ( length(setdiff(consolMarkNames,unique(dfMarkColorInt$markName) ) )>0 ) { # nrow not 0
          message("\nColors provided in to dfMarkColor are not enough, internal colors will be used.\n")
          dfMarkColorInt <- makedfMarkColor(dfMarkColorInt,consolMarkNames, c(chrColor,cenColor2) )
        } else { # nrow not 0
          message("\nCheck OK\n")
        }

      } else { # all Mark Names does not exist
        message(crayon::red("\nError in dfMarkColor Names respect to Marks data.frames, dfMarkColor REMOVED\n")
        )
        remove(dfMarkColorInt)
      } # else
    } # else column names ok
  } else if (!exists("mycolors2") ) { # if dfMarkColor not exist and missing mycolors

    if(exists("consolMarkNames")  ) {

        dfMarkColorInt <- makedfMarkColor(idiogramFISH::dfMarkColor
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

        dfMarkColorInt <- makedfMarkColorMycolors(consolMarkNames
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
      dfMarkColorIntCentro <- data.frame(markName="inProteinCentromere"
                                              ,markColor=cenColor2
                                              ,style="inProtein")
    }

    bToRemove <- c(bToRemove,"inProteinCentromere")


} # cenFormat Protein

  dfMarkColordfs <- mget(ls(pattern = "^dfMarkColorInt" ) )

  if(length(dfMarkColordfs) ) {

    # dfMarkColorInt <- suppressWarnings(dplyr::bind_rows(dfMarkColordfs) )

    dfMarkColorInt<- suppressWarnings(bind_rows((lapply(
      dfMarkColordfs, function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

    dfMarkColorInt <- makeNumCols(dfMarkColorInt)

  }

  #
  #   add border, remove cen if necessary
  #

  if(exists("dfMarkColorInt")) {

      conMNames <- mget(ls(pattern = "^allMarkNames" ) ) # mergewith inProtein

      if(length(conMNames) ) {
        consolMarkNames <- suppressWarnings(unlist(conMNames) )
      }

      #
      #   not used from mycolors
      #
      if(exists("mycolors2")){
        notUsed<- setdiff(mycolors2,unique(dfMarkColorInt$markColor) )
        if(length(notUsed)) {
          lenUnMarks<-length(dfMarkColorInt[which(is.na(dfMarkColorInt$markColor)),]$markColor)
          lenNotUsed<-length(notUsed)
          coMin<-min(lenNotUsed,lenUnMarks)
          if(coMin>0){
            dfMarkColorInt[which(is.na(dfMarkColorInt$markColor)),]$markColor[1:coMin]<-
            notUsed[1:coMin]
          }
        }
      }

     if(exists("markNamesCentromere")) {
       consolMarkNamesCen<-c(consolMarkNames,markNamesCentromere)
     } else {
       consolMarkNamesCen<-consolMarkNames
     }

      if(exists("reservedfMarkColorInt")) {
        dfMarkColorInt <- makedfMarkColor(dfMarkColorInt,consolMarkNamesCen, c(chrColor,cenColor2)
                                           ,reserveDF=reservedfMarkColorInt )
      } else {

        #   last makedfMarkColor
        #
        dfMarkColorInt <- makedfMarkColor(dfMarkColorInt,consolMarkNamesCen, c(chrColor,cenColor2)
                          )

      }

   if(length(cenColor2)==0){
     dfMarkColorInt <- dfMarkColorInt[
       which(FALSE==dfMarkColorInt$markName %in% "inProteinCentromere"),]
   }


   dfMarkColorInt$markBorderColor<-dfMarkColorInt$markColor

    if(colorBorderMark!="") {

      colorBorderMarkFiltered<-colorBorderMark[sapply(colorBorderMark, function(X) {
        tryCatch(is.matrix(col2rgb(X)),
                 error = function(e) {message(crayon::red(paste("Color",X,"invalid, removed from colorBorderMark") ) ); return(FALSE) })
      } )]

      colorBorderMarkFiltered<-colorBorderMarkFiltered[!is.na(colorBorderMarkFiltered)]

      if(length(colorBorderMarkFiltered)>0 ) {
        dfMarkColorInt$markBorderColor<-colorBorderMarkFiltered
      }

    } # colorBorderMark

    if(borderOfWhiteMarks){
      tryCatch(dfMarkColorInt[which(dfMarkColorInt$markColor %in% "white" &
                                           !dfMarkColorInt$style %in% "cenStyle"),]$markBorderColor<-"black",
               error= function (e) NA)
    }

  }

  #
  #   inline labels of arrows
  #

  if (exists("dfMarkColorInt") & legend=="inline") {
    dfMarkColorIntCopy <- dfMarkColorInt
    dfMarkColorIntArrowsLabels<-dfMarkColorIntCopy[which(dfMarkColorIntCopy$style %in% c("downArrow","upArrow") ),]
    remove(dfMarkColorIntCopy)
    if(nrow(dfMarkColorIntArrowsLabels)>0) {
      tryCatch(dfMarkColorIntArrowsLabels[which(dfMarkColorIntArrowsLabels$style %in% "downArrow"),]$style<-"cMLeft",error = function(e) {""})
      tryCatch(dfMarkColorIntArrowsLabels[which(dfMarkColorIntArrowsLabels$style %in% "upArrow"),]$style<-"cM",error = function(e) {""})
      #
      # adds space for new pseudomark
      #
      tryCatch(dfMarkColorIntArrowsLabels$markName <- paste0(dfMarkColorIntArrowsLabels$markName," "),error = function(e) {""} )
      #
      #   last dfMarkColorInt
      #

      # dfMarkColorInt <- dplyr::bind_rows(dfMarkColorInt,dfMarkColorIntArrowsLabels)

      dfMarkColorInt<- suppressWarnings(bind_rows((lapply(
        list(dfMarkColorInt,dfMarkColorIntArrowsLabels)
        , function(x){mutate(x, across(.cols=everything(), as.character) ) } ) ) ) )

      dfMarkColorInt <- makeNumCols(dfMarkColorInt)

    }
  }

  if (exists("copyDfMarkPosInt1") & exists("dfMarkColorInt") & cenFormat!="inProtein" ) {
    mWithW <- copyDfMarkPosInt1[which(copyDfMarkPosInt1$chrRegion %in% "w"),]$markName
    Wstyles<- dfMarkColorInt[which(dfMarkColorInt$markName %in% mWithW),]$style
    if(length(Wstyles)){
      if("inProtein" %in% Wstyles){
        message(crayon::magenta("You have used w (chrRegion) marks of 'inProtein' style, use cenFormat='inProtein'" ) )
      }
    }
  }

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

  if(exists("pLiMaPosMonocen") & exists("dfMarkColorInt")  ) {
    spelMaPosMonoArrLabs<-list()

    for (i in 1:length(pLiMaPosMonocen)){
      if(inherits(pLiMaPosMonocen[[i]], "data.frame")) {

        #
        #   data.frame of marks gains style
        #

        pLiMaPosMonocen[[i]]$style <- dfMarkColorInt$style[
          match(pLiMaPosMonocen[[i]]$markName, dfMarkColorInt$markName)]

        if (legend=="inline") {
          spelMaPosMonoArrLabs[[i]]<-pLiMaPosMonocen[[i]]
          spelMaPosMonoArrLabs[[i]]<-spelMaPosMonoArrLabs[[i]][which(spelMaPosMonoArrLabs[[i]]$style %in% c("downArrow","upArrow") ),]
          if(nrow(spelMaPosMonoArrLabs[[i]])>0) {
            tryCatch(spelMaPosMonoArrLabs[[i]][which(spelMaPosMonoArrLabs[[i]]$style %in% "downArrow"),]$style<-"cMLeft",error = function(e) {""})

            tryCatch(spelMaPosMonoArrLabs[[i]][which(spelMaPosMonoArrLabs[[i]]$style %in% "upArrow"),]$style<-"cM",error = function(e) {""})

            tryCatch({
              downleftp <- which(spelMaPosMonoArrLabs[[i]]$style %in% "cMLeft" &
                                 spelMaPosMonoArrLabs[[i]]$chrRegion %in% "p" )
              spelMaPosMonoArrLabs[[i]][downleftp,]$markDistCen<-
                spelMaPosMonoArrLabs[[i]][downleftp,]$markDistCen+
                spelMaPosMonoArrLabs[[i]][downleftp,]$markSize
                },    error = function(e) {""} )

            tryCatch({
              uprightq <- which(spelMaPosMonoArrLabs[[i]]$style %in% "cM" &
                                   spelMaPosMonoArrLabs[[i]]$chrRegion %in% "q" )
              spelMaPosMonoArrLabs[[i]][uprightq,]$markDistCen<-
                spelMaPosMonoArrLabs[[i]][uprightq,]$markDistCen+
                spelMaPosMonoArrLabs[[i]][uprightq,]$markSize
            },    error = function(e) {""} )

            tryCatch(spelMaPosMonoArrLabs[[i]]$markName <- paste0(spelMaPosMonoArrLabs[[i]]$markName," ")
                     ,error = function(e) {""} )

            pLiMaPosMonocen[[i]] <- dplyr::bind_rows(
              pLiMaPosMonocen[[i]],spelMaPosMonoArrLabs[[i]])
          } # arrows present
        } # inline

        tryCatch(pLiMaPosMonocen[[i]]$protruding <- dfMarkColorInt$protruding[match(pLiMaPosMonocen[[i]]$markName,
                                                                                                dfMarkColorInt$markName)]
                 ,error = function(e) {""}
                 )



      if(nrow(pLiMaPosMonocen[[i]][which(pLiMaPosMonocen[[i]]$style %in% "cenStyle"),])>0) {
        if(!"markSize" %in% colnames(pLiMaPosMonocen[[i]])){
          pLiMaPosMonocen[[i]]$markSize<-NA
        }
        for (m in 1:nrow(pLiMaPosMonocen[[i]][which(pLiMaPosMonocen[[i]]$style %in% "cenStyle"),] ) ) {
          if( is.na(pLiMaPosMonocen[[i]][which(pLiMaPosMonocen[[i]]$style %in% "cenStyle"),]$markSize[m]) ) {

         pLiMaPosMonocen[[i]][which(pLiMaPosMonocen[[i]]$style %in% "cenStyle"),]$markSize[m] <-
                dfChrSizeIntDivisor[match(interaction( pLiMaPosMonocen[[i]][which(
                pLiMaPosMonocen[[i]]$style %in% "cenStyle"),][m,c("OTU", "chrName")]),
                                      interaction( dfChrSizeIntDivisor[c("OTU", "chrName")] )
              ),]$centromereSize

            } # if
          } # for

        if(cenFormat=="inProtein") {
            if(nrow(pLiMaPosMonocen[[i]][which(pLiMaPosMonocen[[i]]$style=="cenStyle"),])>0 ){
              pLiMaPosMonocen[[i]][which(pLiMaPosMonocen[[i]]$style=="cenStyle"),]$style<-"inProtein"
              message(crayon::red("you tried to use cenStyle marks concomitantly with cenFormat='inProtein'
                                \ncenStyle marks are now inProtein marks"))
            }
        }
      }

      } # if data.frame
    } # for each monocen
  } # fi exists monocen

  if(exists("pLiMaPosHolocen") & exists("dfMarkColorInt")  ) {
    spelMaPosHoloArrLabs<-list()
    for (i in 1:length(pLiMaPosHolocen)) {
      if(inherits(pLiMaPosHolocen[[i]], "data.frame")) {

        pLiMaPosHolocen[[i]]$style<-dfMarkColorInt$style[match(pLiMaPosHolocen[[i]]$markName, dfMarkColorInt$markName)]

        if (legend=="inline"){
          spelMaPosHoloArrLabs[[i]]<-pLiMaPosHolocen[[i]]
          spelMaPosHoloArrLabs[[i]]<-spelMaPosHoloArrLabs[[i]][which(spelMaPosHoloArrLabs[[i]]$style %in% c("downArrow","upArrow") ),]
          if(nrow(spelMaPosHoloArrLabs[[i]])>0){
            tryCatch(spelMaPosHoloArrLabs[[i]][which(spelMaPosHoloArrLabs[[i]]$style %in% "downArrow"),]$style<-"cMLeft",error = function(e) {""})
            tryCatch(spelMaPosHoloArrLabs[[i]][which(spelMaPosHoloArrLabs[[i]]$style %in% "upArrow"),]$style<-"cM",error = function(e) {""})

            tryCatch({
              downleft <- which(spelMaPosHoloArrLabs[[i]]$style %in% "cMLeft" )

              spelMaPosHoloArrLabs[[i]][downleft,]$markPos <-
                spelMaPosHoloArrLabs[[i]][downleft,]$markPos+
                spelMaPosHoloArrLabs[[i]][downleft,]$markSize
            },    error = function(e) {""} )

            tryCatch(spelMaPosHoloArrLabs[[i]]$markName <- paste0(spelMaPosHoloArrLabs[[i]]$markName," "),error = function(e) {""} )
            pLiMaPosHolocen[[i]] <- dplyr::bind_rows(
              pLiMaPosHolocen[[i]],spelMaPosHoloArrLabs[[i]])
          }
        } # inline
        tryCatch(pLiMaPosHolocen[[i]]$protruding <- dfMarkColorInt$protruding[match(pLiMaPosHolocen[[i]]$markName,
                                                                                                dfMarkColorInt$markName)]
                 ,error = function(e) {""}
        )


      if(nrow(pLiMaPosHolocen[[i]][which(pLiMaPosHolocen[[i]]$style %in% "cenStyle"),])>0) {
        if(!"markSize" %in% colnames(pLiMaPosHolocen[[i]]) ) {
          pLiMaPosHolocen[[i]]$markSize<-NA
        }
        for (m in 1:nrow(pLiMaPosHolocen[[i]][which(pLiMaPosHolocen[[i]]$style %in% "cenStyle"),] ) ) {
          if( is.na(pLiMaPosHolocen[[i]][which(pLiMaPosHolocen[[i]]$style %in% "cenStyle"),]$markSize[m]) ) {
            pLiMaPosHolocen[[i]][which(pLiMaPosHolocen[[i]]$style %in% "cenStyle"),]$markSize[m]<-

              dfChrSizeIntDivisor[match(interaction( pLiMaPosHolocen[[i]][which(
                pLiMaPosHolocen[[i]]$style %in% "cenStyle"),][m,c("OTU", "chrName")]),
                interaction( dfChrSizeIntDivisor[c("OTU", "chrName")] )
              ),]$centromereSize

            } # if
          } # for

        if(cenFormat=="inProtein") {
          if(nrow(pLiMaPosHolocen[[i]][which(pLiMaPosHolocen[[i]]$style=="cenStyle"),])>0 ){
            pLiMaPosHolocen[[i]][which(pLiMaPosHolocen[[i]]$style=="cenStyle"),]$style<-"inProtein"
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

  if(!is.na(addMissingOTUAfter[1]) & addMissingOTUAfter[1]!= "" ){
    if (length(missOTUspacings) != length(addMissingOTUAfter) ){
      missOTUspacings<-rep(missOTUspacings, abs(length(addMissingOTUAfter)/length(missOTUspacings)) ) [1:length(addMissingOTUAfter)]
    }
    for (i in 1:length(addMissingOTUAfter)){
    listChrSize <- append(listChrSize,
                                rep(NA,missOTUspacings[[i]]),
                                which(names(listChrSize)==addMissingOTUAfter[[i]])
                                ) # append
    listChrSizeIndex <- append(listChrSizeIndex,
                          rep(NA,missOTUspacings[[i]]),
                          which(names(listChrSizeIndex)==addMissingOTUAfter[[i]])
    ) # append
    }
  } # fi

  if(!is.na(addMissingOTUBefore[1]) & addMissingOTUBefore[1]!= "" ) {
    if (length(missOTUspacings) != length(addMissingOTUBefore) ){
      missOTUspacings<-rep(missOTUspacings, abs(length(addMissingOTUBefore)/length(missOTUspacings)) ) [1:length(addMissingOTUBefore)]
    }
    for (i in 1:length(addMissingOTUBefore)){
      listChrSize <- append(listChrSize,
                                  rep(NA,missOTUspacings[[i]]),
                                  which(names(listChrSize)==addMissingOTUBefore[[i]] )-1
      ) # append
      listChrSizeIndex <- append(listChrSizeIndex,
                            rep(NA,missOTUspacings[[i]]),
                            which(names(listChrSizeIndex)==addMissingOTUBefore[[i]] )-1
      ) # append
    }
  } # fi

  #
  #     reverse
  #

  if(verticalPlot) {
    listChrSize <- rev(listChrSize)
    listChrSizeIndex <- rev(listChrSizeIndex)
  }

  if(revOTUs){
    listChrSize <-rev(listChrSize)
    listChrSizeIndex <- rev(listChrSizeIndex)
  }

  #
  #	change of size based on number of sps
  #

  if(propWidth){
    chrWidth  <- chrWidth/length(listChrSize)
    chrSpacing<- chrSpacing/length(listChrSize)
  }  # dfMarkPosSq to listMarkPosIntSq


  #######################################################
  #
  #   If Marks missing, rename duplicates of chrNames
  #
  #######################################################

  for (s in 1:length(listChrSize) ) {
    OTUname<-names(listChrSize[s])
    if (exists("pLiMaPosHolocen") ){
      OTUpLiMaPosHolocen<-pLiMaPosHolocen[which(names(pLiMaPosHolocen) %in% OTUname)]
      if(length(OTUpLiMaPosHolocen)==0){
        remove(OTUpLiMaPosHolocen)
      }
    }
    if (exists("pLiMaPosMonocen") ){
      OTUpLiMaPosMonocen<-pLiMaPosMonocen[which(names(pLiMaPosMonocen) %in% OTUname)]
      if(length(OTUpLiMaPosMonocen)==0){
        remove(OTUpLiMaPosMonocen)
      }
    }
    if (exists("pLiMaPosDataCen") ) {
      OTUpLiMaPosDataCen<-pLiMaPosDataCen[which(names(pLiMaPosDataCen) %in% OTUname)]
      if(length(OTUpLiMaPosDataCen)==0){
        remove(OTUpLiMaPosDataCen)
      }
    }
    mybooleanChrName <- !exists("OTUpLiMaPosHolocen") & !exists("OTUpLiMaPosMonocen") &
      !exists("OTUpLiMaPosDataCen")
    dfChromSize <- fixChrNameDupDF(listChrSize[s], mybooleanChrName)
    listChrSize[[s]]<-dfChromSize[[1]]
  }

  #####################
  #   total size of chr
  #####################

  #
  #   add column total to data.frames
  #

  listChrSize <- addChrSizeColumn(listChrSize)

  {
    totalLength<-lapply(listChrSize, function(x) tryCatch(x$chrSize, error=function(e) NA)  )
    ifelse(
      inherits(totalLength, "matrix"),
      totalLength <- base::split(totalLength, col(totalLength) )
      ,NA)
    normalizeToOne <- karHeight/max(unlist(totalLength) , na.rm=TRUE)
  }

  ##############################################################################
  # order by size
  ##############################################################################

    if(orderChr=="size") {
        orderlist<-lapply(totalLength, function(x) order(x, decreasing = TRUE) )
    } else if(orderChr=="name"){
        orderlist<-lapply(listChrSize, function(x) tryCatch(order(x$chrName), error=function(e) NA ) )
    } else if(orderChr=="original" | orderChr=="group" | orderChr=="chrNameUp") {
        orderlist<-lapply(listChrSize, function(x) tryCatch(1:max(order(x$chrName) ), error=function(e) NA ) )
    }

    ##################################################
    #
    #   add column of new chro index to data.frames
    #
    ##################################################
  listChrSize      <- addNeworderColumn(listChrSize, orderlist)

  listChrSizeIndex <- addNeworderColumn(listChrSizeIndex, orderlist)

    ###################################################
    #     groups
    ###################################################

    if("group" %in% colnames(dfChrSizeIntDivisor)) {
      message(crayon::blue("group column present - remove column if not using") )
      grouporderlist<-lapply(listChrSize, function(x) tryCatch(order(x$group), error=function(e) NA ) )

      if(orderChr=="group") {

        for (s in 1:length(listChrSize)){
          if(inherits(listChrSize[[s]], "data.frame")) {
            if(!anyNA(grouporderlist[[s]] ) ) {
              listChrSize[[s]]<-listChrSize[[s]][grouporderlist[[s]], ]
              listChrSize[[s]]$neworder<-1:nrow(listChrSize[[s]])

              listChrSizeIndex[[s]]<-listChrSizeIndex[[s]][grouporderlist[[s]], ]
              listChrSizeIndex[[s]]$neworder<-1:nrow(listChrSizeIndex[[s]])

            } else {
              message(crayon::blue(paste("Ordering by group was not possible for"
                                         , names(listChrSize)[s],"check that column")
                                   )
                      )
            }
          } # df
        } # end for

      } # order

    } # if group colname

  ###################################################
  #     order by chrNameUp
  ###################################################

  if("chrNameUp" %in% colnames(dfChrSizeIntDivisor)) {
    chrNameUpOrderlist<-lapply(listChrSize, function(x) tryCatch(order(x$chrNameUp), error=function(e) NA ) )

    if(orderChr=="chrNameUp") {

      for (s in 1:length(listChrSize)){
        if(inherits(listChrSize[[s]], "data.frame")) {
          if( !anyNA(chrNameUpOrderlist[[s]] ) ){
            listChrSize[[s]]<-listChrSize[[s]][chrNameUpOrderlist[[s]], ]
            listChrSize[[s]]$neworder<-1:nrow(listChrSize[[s]])
          } else {
            message(crayon::blue(paste("Ordering by chrNameUp was not possible for", names(listChrSize)[s],"check that column") )
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
#   important - add new indexer to df of marks - adds neworder column to listChrSizeCenType
#
##################################################

if(exists("pLiMaPosHolocen") ){
  pLiMaPosHolocen<-newOrderColumn(listChrSize,pLiMaPosHolocen)

} # end if presence of pLiMaPosHolocen order

if(exists("pLiMaPosMonocen") ) {

  pLiMaPosMonocen<-newOrderColumn(listChrSize,pLiMaPosMonocen)

} # end if presence of pLiMaPosMonocen

######################################################
#
#   important - add new indexer to d.f DataCen
#
######################################################

if (exists("pLiMaPosDataCen")){

  pLiMaPosDataCen<-newOrderColumn(listChrSize,pLiMaPosDataCen)

}  # end presence of dfCenMarksInt

#######################
#
#      main plot
#
#######################

{
  # Monocen
  num_species<-length(listChrSize)
}

# processing for main plot

# Monocen
{
    rownumber <-2

    chromosome_ns <- sapply(listChrSize, function(x) nrow(x) )

    # listChrSize ->     chromosome_ns ->     arms_number ->     armRepVector

    arms_number <- sapply(chromosome_ns, function(x) x*2)

    armRepVector<-lapply(arms_number, function(x) return(tryCatch( c(rep(rownumber,ceiling(x/rownumber) ) ), error=function(e) NA ) ))

    rownumber <-1

    chromRepVector <- lapply(chromosome_ns, function(x) return(tryCatch( c(rep(rownumber,ceiling(x/rownumber) ) ), error=function(e) NA ) ))

    #
    #   creating x y main coords fro chr.
    #

    ym <- xm <-  x <- list()
    # y <- list()
    segX0<-segX1<-segY0<-segY1<-list()
}
########################################################################################################################################
if(moveKarHor!="") {
  moveKarHor2<-tryCatch(intersect(moveKarHor,unique(dfChrSizeInt$OTU)),error=function(e) {
    "moveKarHor OTU not found"} )
} else {# if
  moveKarHor2<-""
}

if(karAnchorLeft != "" )  {
  karAnchorLeft2 <- karAnchorLeft
} else {# if
  karAnchorLeft2 <- ""
}

if(karAnchorRight != "") {
  karAnchorRight2 <- karAnchorRight
} else {# if
  karAnchorRight2 <- ""
}

if(is.numeric(moveAllKarValueY)) {
  moveAllKarValueY2 <- moveAllKarValueY
} else {
  moveAllKarValueY2 <-0
  message(crayon::blue("moveAllKarValueY must be numeric"))
}

for (s in 1:num_species) {

      if(inherits(listChrSize[[s]], "data.frame")) {

        #######################################################################################################

      if(attr(listChrSize[[s]], "cenType")=="monocen") {       # monocen

        if(attr(listChrSize[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
          chrSpacing2<-specialChrSpacing
        } else {
          chrWidth2<-chrWidth
          chrSpacing2<-chrSpacing
        }

        croxright<-  croxleft<-  croytop<-croybot <- list()


      for (i in 1:length(armRepVector[[s]] )) {

        centromereSize3 <- as.numeric(attr(listChrSize[[s]],"centromere"))

        if(cenFormat=="inProtein"){
          centromereSize3<-0
        }

        croybot[i]<-tryCatch(list(c(karHeight,
                                    rep( (karHeight-(listChrSize[[s]][,"longArmSize"]*normalizeToOne)[i]),2),
                                    karHeight
                          )#c
                        ), error=function(e) NA ) # list

        croytop[i]<-tryCatch( list(c( (karHeight+(centromereSize3*normalizeToOne)+(listChrSize[[s]][,"shortArmSize"]*normalizeToOne)[i] ),
                                       rep((karHeight+(centromereSize3*normalizeToOne)),2),
                                      (karHeight+(centromereSize3*normalizeToOne)+(listChrSize[[s]][,"shortArmSize"]*normalizeToOne)[i] )
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


        if(is.numeric(moveAllKarValueHor)) {
          crox <- crox + moveAllKarValueHor
        } else {
          message(crayon::blue("moveAllKarValueHor must be numeric"))
        }

      if(names(listChrSize)[s] %in% moveKarHor2 ) {
        crox <- crox + mkhValue

        if(anchor & verticalPlot) {
            x1 <-min(crox)- mkhValue
            x2 <-min(crox)- (chrWidth2*anchorHsizeF)
            y0<- ( (min(unlist(croytop) ) + max(unlist(croybot)) ) /2 ) +  karHeiSpace*(s-1) + moveAllKarValueY2
            yh<-y0+karHeiSpace*anchorVsizeF
            yl<-y0-karHeiSpace*anchorVsizeF
            segX0[[s]]<-c(x1+moveAnchorV,x1+moveAnchorV)
            segY0[[s]]<-c(y0,yh)
            segX1[[s]]<-c(x2+moveAnchorH,x1+moveAnchorV)
            segY1[[s]]<-c(y0,yl)
        }
      }

      #
      #   groups
      #

      if(circularPlot==FALSE & "group" %in% colnames(listChrSize[[s]])  ) {
        lens <- rle(listChrSize[[s]]$group)$lengths
        names(lens) <- rle(listChrSize[[s]]$group)$values
        clens  <- cumsum(lens)
        clens2 <- clens[!is.na(names(clens))]
        lens2  <- lens[!is.na(names(clens))]

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

      if(names(listChrSize)[s] %in% karAnchorLeft2 ) {
        # crox <- crox + mkhValue
        if(anchor & verticalPlot==FALSE) {
            x2 <- tryCatch(max(xm[[s-1]]),warning=function(w){0},error=function(e){0} ) + karSpaceHor + chrWidth2
            x1 <- tryCatch(max(xm[[s-1]]),warning=function(w){0},error=function(e){0} ) + chrWidth2
            x0 <- ((x2+x1)/2 )
            yh <- ( (min(unlist(croytop) ) + max(unlist(croybot)) ) /2 ) + moveAllKarValueY2
            yl <- yh - karHeiSpace * anchorVsizeF * 2
            segX0[[s]]<-c(x1+moveAnchorH+(anchorHsizeF*chrWidth2),x0+moveAnchorH)
            segY0[[s]]<-c(yh,yh)
            segX1[[s]]<-c(x2+moveAnchorH-(anchorHsizeF*chrWidth2),x0+moveAnchorH)
            segY1[[s]]<-c(yh,yl)
        }
      }

      if(names(listChrSize)[s] %in% karAnchorRight2 ) {
        if(anchor & verticalPlot==FALSE) {
            x2 <- max(xm[[s]]) + karSpaceHor + chrWidth2
            x1 <- max(xm[[s]]) + chrWidth2
            x0 <- (x2+x1)/2
            yh <- ( (min(unlist(croytop) ) + max(unlist(croybot)) ) /2 ) + moveAllKarValueY2
            yl <- yh - karHeiSpace * anchorVsizeF * 2
            segX0[[s]]<-c(x1+moveAnchorH+(anchorHsizeF*chrWidth2),x0+moveAnchorH)
            segY0[[s]]<-c(yh,yh)
            segX1[[s]]<-c(x2+moveAnchorH-(anchorHsizeF*chrWidth2),x0+moveAnchorH)
            segY1[[s]]<-c(yh,yl)
        }
      }

      #
      # create x
      #

      x[[s]] <- base::split(xm[[s]], row(xm[[s]]) )

      ifelse(any(is.na(x[[s]]) ), x[[s]]<-NA,"" ) # ifelseinloop

        if(verticalPlot | circularPlot) {
          ym[[s]] <- t(sapply (c(croybot,croytop ), function (x) {length (x) ; return (x)} ) ) + karHeiSpace*(s-1) + moveAllKarValueY2
        } else if (verticalPlot==FALSE) {
          ym[[s]] <- t(sapply (c(croybot,croytop ), function (x) {length (x) ; return (x)} ) ) + moveAllKarValueY2
        }
      } # fi is monocen

        ###########################################################################################              HOLOCEN

      if (attr(listChrSize[[s]], "cenType")=="holocen" ) {

        if(attr(listChrSize[[s]],"ytitle")=="cM"){
          chrWidth2  <-specialChrWidth
          chrSpacing2<-specialChrSpacing
        } else {
          chrWidth2  <-chrWidth
          chrSpacing2<-chrSpacing
        }

      croxright <-  croxleft <- croytop <- list()

      for (i in 1:length(chromRepVector[[s]])){

        croytop[i]<-tryCatch( list(c(   (karHeight/2+(listChrSize[[s]][,"chrSize"]*normalizeToOne)[i]   ), # 0+
                                        rep(karHeight/2,2),                                                              # 0,2
                                        (karHeight/2+(listChrSize[[s]][,"chrSize"]*normalizeToOne)[i]  )   # 0+
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


        if(is.numeric(moveAllKarValueHor)) {
          crox <- crox + moveAllKarValueHor
        } else {
          message(crayon::blue("moveAllKarValueHor must be numeric"))
        }


        if(names(listChrSize)[s] %in% moveKarHor2 ) {
          crox <- crox + mkhValue

          if(anchor & verticalPlot) {
              x1<-min(crox)-mkhValue
              x2<-min(crox)-chrWidth2
              y0<- ( (min(unlist(croytop) )+max(unlist(croytop)) ) /2 ) +  karHeiSpace*(s-1) + moveAllKarValueY2
              yh<-y0+karHeiSpace*anchorVsizeF
              yl<-y0-karHeiSpace*anchorVsizeF
              segX0[[s]]<-c(x1+moveAnchorV,x1+moveAnchorV)
              segY0[[s]]<-c(y0,yh)
              segX1[[s]]<-c(x2+moveAnchorH,x1+moveAnchorV)
              segY1[[s]]<-c(y0,yl)
          }
        }

      #
      #   horizontal anchor holocen
      #

      if(names(listChrSize)[s] %in% karAnchorLeft2 ) {
        # crox <- crox + mkhValue
        if(anchor & verticalPlot==FALSE) {
            x2 <- tryCatch(max(xm[[s-1]]),warning=function(w){0},error=function(e){0} ) + karSpaceHor + chrWidth2
            x1 <- tryCatch(max(xm[[s-1]]),warning=function(w){0},error=function(e){0} ) + chrWidth2
            x0 <- (x2+x1)/2
            yh<- ( (min(unlist(croytop) ) + max(unlist(croybot)) ) /2 ) + moveAllKarValueY2
            yl<-yh - karHeiSpace * anchorVsizeF * 2
            segX0[[s]]<-c(x1+moveAnchorH,x0+moveAnchorH)
            segY0[[s]]<-c(yh,yh)
            segX1[[s]]<-c(x2+moveAnchorH,x0+moveAnchorH)
            segY1[[s]]<-c(yh,yl)
        }
      }

      if(names(listChrSize)[s] %in% karAnchorRight2 ) {
        # crox <- crox + mkhValue
        if(anchor & verticalPlot==FALSE) {
            x2 <- max(xm[[s]]) + karSpaceHor + chrWidth2
            x1 <- max(xm[[s]]) + chrWidth2
            x0 <- (x2+x1)/2 # dont use x
            yh <- ( (min(unlist(croytop) ) + max(unlist(croybot)) ) /2 ) + moveAllKarValueY2
            yl <- yh - karHeiSpace * anchorVsizeF * 2
            segX0[[s]]<-c(x1+moveAnchorH,x0+moveAnchorH)
            segY0[[s]]<-c(yh,yh)
            segX1[[s]]<-c(x2+moveAnchorH,x0+moveAnchorH)
            segY1[[s]]<-c(yh,yl)
        }
      }

      #
      # group
      #

      if(circularPlot==FALSE & "group" %in% colnames(listChrSize[[s]])  ) {
        lens <- rle(listChrSize[[s]]$group)$lengths
        names(lens)<-rle(listChrSize[[s]]$group)$values
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

      if(verticalPlot | circularPlot) {
        ym[[s]] <- t(sapply ( croytop, function (x) {length (x) ; return (x)}) ) + (karHeiSpace * (s-1) ) + moveAllKarValueY2
      } else if (verticalPlot==FALSE) {
        ym[[s]] <- t(sapply ( croytop, function (x) {length (x) ; return (x)}) ) + moveAllKarValueY2
      }

    } # holocen if

  } # data.frame

  # message(crayon::green(paste0("main plot calc section end" ) ) )

} # for species

###################################################################################################

if (length(ym)==1){
  karSepar<-FALSE
}

#
#	reducing distance among OTUs
#

names(ym) <- names(listChrSize)[1:length(ym)]

ymCopyC<-ymCopy2<-ymCopy<-ym # important must stay here before modifying ym
#
#
# not useful when addMiss... present
#

if( (is.na(addMissingOTUAfter[1] ) | addMissingOTUAfter[1]== "") &
    ( is.na(addMissingOTUBefore[1]) | addMissingOTUBefore[1]== "")  ) {
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
    listChrSize[[s]]<-NA
  } # if
  attr(listChrSize[[s]],"groupPresence") <- 0
} # for

{
  areNA<-which(is.na(ym))
  listChrSizenoNA    <- removeNAFromList(listChrSize,areNA)

  for (s in 1:length(listChrSizeIndex)) {
    attr(listChrSizeIndex[[s]],"groupPresence") <- 0
  }


  listChrSizenoNAIndex <- removeNAFromList(listChrSizeIndex,areNA)

  xmnoNA <- removeNAFromList(xm,areNA)

    for (i in 1:length(xmnoNA)){
      attr(xmnoNA[[i]],"cenType") <- attr(listChrSizenoNA[[i]],"cenType")
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

  names(xm)<-names(listChrSize)[1:length(xm)]

  names(x)<-names(listChrSizenoNA)
  names(y)<-names(listChrSizenoNA)


for (s in 1:length(y) ) {

    lenCS<-length(listChrSizenoNA[[s]][,"chrName"] )

    for (c in 1:lenCS )  {
      attr(y[[s]][[c]], "chrName1") <- listChrSizenoNA[[s]][,"chrName"][c]
    }
    if (length(y[[s]])>lenCS ){
      for (c in 1:lenCS )  {
      attr(y[[s]][[c+lenCS]], "chrName1") <- listChrSizenoNA[[s]][,"chrName"][c]
      }
    }
   # } # mono holo check
}

} # chunk

    #
    #     mark percentages %
    #

if (exists("dfMarkPosInt") & markPer[1]!="" ) {
  perList <- perMark(dfMarkPosInt, listChrSizenoNAIndex)
} # exist

if (exists("dfMarkPosInt") & showMarkPos ) {
  posTib  <- posCalc(dfMarkPosInt, listChrSizenoNAIndex, bToRemove)
} # exist

{
  for(i in 1:length(listChrSizenoNA)){
    if(inherits(listChrSizenoNA[[i]], "data.frame")) {
      if(attr(listChrSizenoNA[[i]],"cenType")=="monocen"){
        attr(listChrSizenoNA[[i]],"positionnoNA") <- i
      }
    }
  }

  # holocenVector2<-integer()
  for(i in 1:length(listChrSizenoNA)){
    if(inherits(listChrSizenoNA[[i]], "data.frame")) {
      if(attr(listChrSizenoNA[[i]],"cenType")=="holocen"){
        # holocenVector2<-c(holocenVector2,i)
        attr(listChrSizenoNA[[i]],"positionnoNA") <- i
      }
    }
  }

}

for (s in 1:length(listChrSizenoNA) ) {
  attr(y[[s]], "positionnoNA")<- attr(listChrSizenoNA[[s]],"positionnoNA")
}

{
  monocenVector2<-integer()
  for(i in 1:length(listChrSize)){
    if(inherits(listChrSize[[i]], "data.frame")) {
      if(attr(listChrSize[[i]],"cenType")=="monocen"){
        monocenVector2<-c(monocenVector2,i)
        attr(listChrSize[[i]],"position") <- i
      }
    }
  }

  monocenNames2<-names(listChrSize)[monocenVector2]
  listChrSizeMonocen<-listChrSize[monocenVector2]

  if(length(listChrSizeMonocen)==0){
    remove(listChrSizeMonocen)
  }

  holocenVector2<-integer()
  for(i in 1:length(listChrSize)){
    if(inherits(listChrSize[[i]], "data.frame")) {
      if(attr(listChrSize[[i]],"cenType")=="holocen"){
        holocenVector2<-c(holocenVector2,i)
        attr(listChrSize[[i]],"position") <- i
      }
    }
  }
  holocenNames2<-names(listChrSize)[holocenVector2]
  listChrSizeHolocen<-listChrSize[holocenVector2]

  if(length(listChrSizeHolocen)==0){
    remove(listChrSizeHolocen)
  }
}

if(!missing(chrBorderColor)) {
  if (is.na(chrBorderColor) | chrBorderColor=="" ) {
    if(chrColor=="white"){
      chrBorderColor2 <- "black"
    } else {
      chrBorderColor2 <- chrColor
    }
  } else {
    chrBorderColor2 <- filter_colors(chrBorderColor)
  # chrBorderColor2<-chrBorderColor
  }
} else {
  if(chrColor=="white"){
    chrBorderColor2 <- "black"
  } else {
    chrBorderColor2 <- filter_colors(chrColor)
  }
} # else
if(length(chrBorderColor2)==0){
  chrBorderColor2 <- filter_colors(chrColor)
}

if(squareness < 1) {
  squareness <- 1
}

if(circularPlot){
  xlistNewChrSimple<-xlistNewChr<- xHortoVer(x)

  yInter<-intercalate(y,monocenNames)
  names(yInter)<-names(y)

  ylistNewChrSimple<-yVertoHor(yInter,monocenNames)
  names(ylistNewChrSimple)<-names(y)
  ylistNewChrSimpleOrig  <- ylistNewChrSimple

  ylistTransChrSimple<-transYList(ylistNewChrSimple,shrinkFactor,monocenNames)
  names(ylistTransChrSimple)<-names(ylistNewChrSimple)
  ylistTransChrSimpleOrig<-ylistTransChrSimple

  circleMapsOrig <- circleMaps <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor
                                                 ,ylistTransChrSimple
                                                 ,xlistNewChr,n,0,
                                                 chrWidth,rotation=rotation)


}

pts2<- pts <- seq(-pi/2, pi*1.5, length.out = n)
ptsl<- split(pts, sort(rep(1:4, each=length(pts)/4, len=length(pts))) )

###########################################################
#
#           plotting chr.
#
###########################################################

if(squareness > 20) {  ########## squareness > 20 #################

  if (chromatids) { # CHROMAT TRUE

      XSA<-YSA<-XLA<-YLA<-XHO1<-XHO2<-YHO1<-list()

      for (s in 1:length(y) ) {

        if(inherits(listChrSizenoNA[[s]], "data.frame")) {
          ########################################################

          if(attr(listChrSizenoNA[[s]], "cenType")=="monocen" ) {   ############# monocen ###########

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

            attr(YSA[[s]], "positionnoNA")<- attr(listChrSizenoNA[[s]],"positionnoNA")
            # attr(YLA[[s]], "positionnoNA")<- attr(listChrSizenoNA[[s]],"positionnoNA")

            # important integration of long arm and short arms info

            YSA[[s]][sapply(YSA[[s]], is.null)]<-YLA[[s]][!sapply(YLA[[s]], is.null)]

            XSA[[s]][sapply(XSA[[s]], is.null)]<-XLA[[s]][!sapply(XLA[[s]], is.null)]

            # short arm indices are in the second half of list, so YLA is a smaller list

            names(XSA)[s]<-names(YSA)[s]<-names(y[s])

            for (a in 1: length(YSA[[s]]) ) {
              attr(YSA[[s]][[a]],"chrName1") <- attr(y[[s]][[a]],"chrName1")
              names(YSA[[s]])[a]<- names(y[[s]][a])
              names(XSA[[s]])[a]<- names(y[[s]][a])
            }

        } else if (attr(listChrSizenoNA[[s]], "cenType")=="holocen" & holocenNotAsChromatids==FALSE ) { # if monocen else  #####  holo

            # mapXYchromatidHolo <- function(start,end,y,x,xModifier ){
            chrtXchrtYHolo<-mapXYchromatidHolo(1 ,
                                           (length(y[[s]]) ) ,
                              y[[s]],
                              x[[s]],
                              xModifierHolo
              )
              # 1 corresponds to right chrtd or outer
              # 2 to left inner
              XHO1[[s]]<-chrtXchrtYHolo$xCT1
              XHO2[[s]]<-chrtXchrtYHolo$xCT2

              YHO1[[s]]<-chrtXchrtYHolo$yCT1
              # YHO2[[s]]<-chrtXchrtYHolo$yCT2

              attr(YHO1[[s]], "positionnoNA")<- attr(listChrSizenoNA[[s]],"positionnoNA")

              names(YHO1)[s]<-names(XHO1)[s]<-names(XHO2)[s]<-names(y[s])

              for (a in 1: length(YHO1[[s]])){
                attr(YHO1[[s]][[a]],"chrName1") <- attr(y[[s]][[a]],"chrName1")

                names(YHO1[[s]])[a]<-names(XHO1[[s]])[a]<- names(XHO2[[s]])[a]<- names(y[[s]][a])
              }
           } # holo holocenNotAsChromatids F

        } # df
      } # for s

      YSA  <- YSA[lengths(YSA) != 0]
      XSA  <- XSA[lengths(XSA) != 0]
      YHO1 <- YHO1[lengths(YHO1) != 0]
      XHO1 <- XHO1[lengths(XHO1) != 0]
      XHO2 <- XHO2[lengths(XHO2) != 0]

    } # chromatids FALSE TRUE       sq > 20

  if(circularPlot==FALSE) {

    #####################################################################################################################
    if(callPlot){
      graphics::plot("", xlim = c( (#min(unlist(x), na.rm=TRUE)
        -1 * xlimLeftMod) ,
        (max(unlist(x), na.rm=TRUE) + xlimRightMod ) ),
        ylim = c( ylimBotMod * -1 , ( (max(unlist(y), na.rm = TRUE) ) + ylimTopMod) ) ,
        ylab = "", xaxt='n',
        xlab="", yaxt='n',main = NULL, frame.plot = FALSE, asp=asp, ...
      )
    }
    ######################################################################################################################
    horizPlot <- (karAnchorLeft!="" | karAnchorRight!="")  & verticalPlot==FALSE

    refKar <- (moveKarHor2!="" & verticalPlot) | horizPlot

    #
    #   anchor
    #

    if(anchor & refKar  )  {
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
      myI<-ifelse(moveKarHor2!="",1,2)

      lapply(1:length(segX0), function(s) mapply(function(x,y) graphics::points(x,
                                                                                y,
                                                                                pch=pchAnchor,
                                                                                bg="black"
      ),
      y=segY0[[s]][myI],
      x=segX0[[s]][myI]
      ) #m
      ) # l

      moveX <- ifelse(moveKarHor2!="",anchorTextMoveX,0)
      moveY <- ifelse(moveKarHor2!="",0,anchorTextMoveY)

      adj <- ifelse(moveKarHor2!="",1,0.5) # 1 right(vert. anchor), 0.5= centered (hor. anchor)

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
        posSegX<-ifelse(moveKarHor2!="",
                        min(unlist(segX0) ),
                        ifelse(!is.na(addMissingOTUAfter[1]) & addMissingOTUAfter[1] != ""  ,
                               max(unlist(segX1)  )
                               ,min(unlist(segX0) )
                        )
        ) + anchorTextMoveParenX

        posSegY <- ifelse(moveKarHor2!="", min(unlist(segY1) ), max(unlist(segY0)  )
        ) - anchorTextMoveParenY


        adj2 <- ifelse(verticalPlot, 0,1)

        graphics::text(posSegX + (anchorTextMoveX*sign1),
                       posSegY,
                       labels=anchorTextMParental
                       ,adj = adj2 # 0 left
                       ,cex = OTUTextSize
                       ,font   = ifelse( !missing(OTUfont),   OTUfont,   1)
                       ,family = ifelse( OTUfamily!="", OTUfamily, defaultFontFamily2)
        )
      }
    } # anchor

    #
    #   simplest plot
    #

    if (chromatids==FALSE) {

      lapply(1:length(y), function(s) mapply(function(x,y) graphics::polygon(x=x,
                                                                             y=y,
                                                                             col=chrColor,
                                                                             lwd=lwd.chr,
                                                                             border=chrBorderColor2),
                                             x=x[[s]],
                                             y=y[[s]]
      ) #m
      ) # l

    } # ct FALSE sqness > 20

    if (chromatids & holocenNotAsChromatids) {

      for (s in 1:length(y) ) {
        if(attr(listChrSizenoNA[[s]],"cenType")=="holocen"){

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
    } # if sq > 20

    if(chromatids) {

      #
      # monocen plot chromatid sq
      #

      if(length(YSA)>0) {

        # this is not only SA, were integrated



        lapply(1:length(YSA), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                                 col=chrColor,
                                                                                 lwd=lwd.chr,
                                                                                 border=chrBorderColor2),
                                                 x=XSA[[s]],
                                                 y=YSA[[s]]
        )#m
        ) #l

      } # if YSA

      # PLOT HOLOCEN SQ CHROMATIDS

      if(holocenNotAsChromatids==FALSE) {

        if(length(YHO1)>0) {



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
                                                  y=YHO1[[s]]
        )#m
        ) #l
      } # if YHO1
      } # hnac

    }

  } else { # circular false # circular TRUE             sq > 20

    #
    #   here
    #

    if(callPlot) {

      graphics::plot("",xlim=c( (min(unlist(circleMaps), na.rm=TRUE)-xlimLeftMod),
                                (max(unlist(circleMaps), na.rm=TRUE)+xlimRightMod )
      ),
      ylim = c( min (unlist(circleMaps), na.rm = TRUE) + ylimBotMod*-1 ,
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

  if(chromatids & holocenNotAsChromatids) {

    #
    # Only holocen (not chrtds)
    #

    if(length(monocenNames)){
      circleMapsHolo <- circleMaps[which(!names(circleMaps) %in% monocenNames)]
      drawPlot(circleMapsHolo,chrColor,lwd.chr,chrBorderColor2)
    } else {
      drawPlot(circleMaps,chrColor,lwd.chr,chrBorderColor2)
    }
  }

  if(chromatids) {

    if(length(YSA)>0) {

      xInter<-intercalate(XSA,monocenNames)
      names(xInter)<-names(XSA)

      xlistNewChr <- xHortoVer(xInter)

      # xlistNewChr<- xHortoVer(XSA)

      yInter<-intercalate(YSA,monocenNames)
      names(yInter)<-names(YSA)

      ylistNewChrSimple<-yVertoHor(yInter,monocenNames)
      names(ylistNewChrSimple)<-names(YSA)


      ylistTransChrSimple<-transYList(ylistNewChrSimple,shrinkFactor,monocenNames)
      names(ylistTransChrSimple)<-names(ylistNewChrSimple)


      circleMaps <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransChrSimple
                                   ,xlistNewChr,n,0,
                                   chrWidth,rotation=rotation)

      #
      #   plot chr
      #

      drawPlot(circleMaps,chrColor,lwd.chr,chrBorderColor2)

    }

    if(length(YHO1)>0 & holocenNotAsChromatids==FALSE) {


        # xlistNewChr<- xHortoVer(XHO1)
        xlistNewChr <- xChrtdMap(XHO1,x, 0)


        yInter<-intercalate(YHO1,monocenNames)
        names(yInter)<-names(YHO1)

        ylistNewChrSimple<-yVertoHor(yInter,monocenNames)
        names(ylistNewChrSimple)<-names(YHO1)



        ylistTransChrSimple01<-transYList(ylistNewChrSimple,shrinkFactor,monocenNames)
        names(ylistTransChrSimple01)<-names(ylistNewChrSimple)



        circleMaps <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransChrSimple01
                                     ,xlistNewChr,n,0,
                                     chrWidth,rotation=rotation)

        #
        #   plot chr
        #

        drawPlot(circleMaps,chrColor,lwd.chr,chrBorderColor2)

        # xlistNewChr<- xHortoVer(XHO2)
        xlistNewChr <- xChrtdMap(XHO2,x, 0)

        circleMaps <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransChrSimple01
                                     ,xlistNewChr,n,0,
                                     chrWidth,rotation=rotation)

        #
        #   plot chr
        #

        drawPlot(circleMaps,chrColor,lwd.chr,chrBorderColor2)

    }

  } else { # chrmtds FALSE

    # xlistNewChr<- xHortoVer(x)
    #
    # circleMaps <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransChrSimple
    #                              ,xlistNewChr,n,0,
    #                              chrWidth,rotation=rotation)

    #
    #   plot chr
    #


    drawPlot(circleMaps,chrColor,lwd.chr,chrBorderColor2)

    #
    #   add OTU names
    #
  }

} # cP true if else

} else {  # if squareness > 20 else       squareness <= 20

  #
  #   yMod creation
  #

  yMod <- y

  roundedY <- roundedX <- XSARO<-YSARO<-XLARO<-YLARO<-XHO1Ro<-YHO1Ro<-XHO2Ro<-YHO2Ro<-list()

for (s in 1:length(yMod) ) {

  if(inherits(listChrSizenoNA[[s]], "data.frame")) {

    if(attr(listChrSizenoNA[[s]], "cenType")=="monocen" ) {   ############ monocen ###########

      if(attr(listChrSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }

      r2<- chrWidth2/(squareness*2)

            shortxyCoords<- mapXY(( (length(yMod[[s]])/2)+1 ) , length(yMod[[s]]),
                                     y[[s]],yMod[[s]],x[[s]],
                                     yfactor,r2,
                                 ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]]
                                )

            longxyCoords <- mapXY( 1 , (length(yMod[[s]])/2 ) ,
                                    y[[s]], yMod[[s]] ,x[[s]],
                                    yfactor,r2,
                                  ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]]
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

            attr(roundedY[[s]], "positionnoNA") <- attr(listChrSizenoNA[[s]],"positionnoNA")
            attr(roundedY[[s]], "cenType")      <- attr(listChrSizenoNA[[s]],"cenType")

            for (a in 1: length(roundedY[[s]]) ) {
              names(roundedX[[s]])[a] <- names(roundedY[[s]])[a]<- names(y[[s]][a])
            }

            names(roundedX)[s]<-names(roundedY)[s]<-names(y[s])

      if (chromatids) { # chromatids FALSE TRUE
        chrtXchrtYSARo <- mapXYchromatidSARo(
          (length(y[[s]])/2) + 1 ,
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
        YSARO[[s]] <- chrtXchrtYSARo$RoundedSAChrty

        XLARO[[s]] <- chrtXchrtYLARo$RoundedLAChrtx
        YLARO[[s]] <- chrtXchrtYLARo$RoundedLAChrty

        attr(YSARO[[s]], "positionnoNA") <- attr(listChrSizenoNA[[s]],"positionnoNA")

        # important integration of long arm and short arms info
        # SARO NOW MEANS WHOLE CHR
        YSARO[[s]][sapply(YSARO[[s]], is.null)] <- YLARO[[s]][!sapply(YLARO[[s]], is.null)]

        XSARO[[s]][sapply(XSARO[[s]], is.null)] <- XLARO[[s]][!sapply(XLARO[[s]], is.null)]
        # short arm indices are in the second half of list, so YLA is a smaller list

        names(XSARO)[s]<-names(YSARO)[s]<-names(y[s])

        for (a in 1: length(YSARO[[s]]) ){
          names(YSARO[[s]])[a]<- names(y[[s]][a])
          names(XSARO[[s]])[a]<- names(y[[s]][a])
        }

      } # chromatids TRUE

    } else if (attr(listChrSizenoNA[[s]], "cenType")=="holocen") { # if monocen else  holocen #########################

      if(attr(listChrSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2 <- specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }

      r2<- chrWidth2/(squareness*2)

      xyCoords <- mapXY(1 , (length(yMod[[s]]) ) ,
                          y[[s]],
                          yMod[[s]] ,
                          x[[s]],
                          yfactor,r2,
                          ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]]
                      )

      roundedX[[s]] <- xyCoords$roundedX
      roundedY[[s]] <- xyCoords$roundedY

      attr(roundedY[[s]], "positionnoNA")<- attr(listChrSizenoNA[[s]],"positionnoNA")
      attr(roundedY[[s]], "cenType")     <- attr(listChrSizenoNA[[s]],"cenType")

      for (a in 1: length(roundedY[[s]])){
        names(roundedY[[s]])[a] <- names(y[[s]][a])
        names(roundedX[[s]])[a] <- names(y[[s]][a])
      }

      names(roundedX)[s] <- names(roundedY)[s] <- names(y[s])

      if ( chromatids==TRUE & holocenNotAsChromatids==FALSE ) { # chromatids FALSE TRUE

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

        attr(YHO1Ro[[s]], "positionnoNA")<- attr(listChrSizenoNA[[s]],"positionnoNA")
        attr(YHO2Ro[[s]], "positionnoNA")<- attr(listChrSizenoNA[[s]],"positionnoNA")

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

if(exists("YSARO")){
  YSARO<-YSARO[lengths(YSARO) != 0]
  XSARO<-XSARO[lengths(XSARO) != 0]
  if(length(YSARO)==0){
    remove(YSARO)
    remove(XSARO)
  }
}

if(exists("YHO1Ro") ){
  YHO1Ro <- YHO1Ro[lengths(YHO1Ro) != 0]
  XHO1Ro <- XHO1Ro[lengths(XHO1Ro) != 0]
  YHO2Ro <- YHO2Ro[lengths(YHO2Ro) != 0]
  XHO2Ro <- XHO2Ro[lengths(XHO2Ro) != 0]

  if(length(YHO1Ro)==0){
    remove(YHO1Ro)
    remove(YHO2Ro)
    remove(XHO1Ro)
    remove(XHO2Ro)
  }
}

if(exists("roundedY") ){
  roundedY<-roundedY[!is.na(roundedY)]
  roundedX<-roundedX[!is.na(roundedX)]
  if(length(roundedY)==0){
    remove(roundedY)
    remove(roundedX)
  }
}

if(circularPlot==FALSE) {

  ##################################################################################################################### < 20
  if(callPlot){
  graphics::plot("",xlim=c( ( #min(unlist(x), na.rm=TRUE)
                             -1*xlimLeftMod),(max(unlist(x), na.rm=TRUE)+xlimRightMod ) ),
                 ylim = c( ylimBotMod*-1 ,( (max(unlist(y), na.rm = TRUE) )+ylimTopMod) ) , ylab = "", xaxt='n',
                 xlab="", yaxt='n',main = NULL, frame.plot = FALSE, asp=asp, ...)
  }
  ######################################################################################################################
  horizPlot <- (karAnchorLeft!="" | karAnchorRight!="")  & verticalPlot==FALSE

  refKar <- (moveKarHor2!="" & verticalPlot) | horizPlot

  if (anchor & refKar) {

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
    myI<-ifelse(moveKarHor2!="",1,2)
    lapply(1:length(segX0), function(s) mapply(function(x,y) graphics::points(x,
                                                                              y,
                                                                              pch=pchAnchor,
                                                                              bg="black"
                                                                              ),
    y=segY0[[s]][myI],
    x=segX0[[s]][myI]
    ) #m
    ) # l

    moveX <- ifelse(moveKarHor2!="",anchorTextMoveX,0)
    moveY <- ifelse(moveKarHor2!="",0,anchorTextMoveY)
    adj <- ifelse(moveKarHor2!="",1,0.5)

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
      posSegX <- ifelse(moveKarHor2!="",
                      min(unlist(segX0) ),
                      ifelse(!is.na(addMissingOTUAfter[1]) & addMissingOTUAfter[1] != "",
                             max(unlist(segX1)  )
                             ,min(unlist(segX0) )
                      )
      ) + anchorTextMoveParenX

      posSegY <- ifelse(moveKarHor2!="", min(unlist(segY1) ), max(unlist(segY0)  )
                        ) - anchorTextMoveParenY

      adj2 <- ifelse(verticalPlot, 0,1) # adj2 <- 0

      graphics::text(posSegX + (anchorTextMoveX*sign1),
                     posSegY,
                     labels=anchorTextMParental
                     ,adj=adj2 # 0 left
                     ,cex=OTUTextSize
                     ,font=   ifelse( !missing(OTUfont),   OTUfont,   1
                                      )
                     ,family= ifelse( OTUfamily!="", OTUfamily, defaultFontFamily2
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
      if(exists("YSARO")) {

      lapply(1:length(YSARO), function(s) mapply(function(x,y) graphics::polygon(x=x, y=y,
                                                                               col=chrColor,
                                                                               lwd=lwd.chr,
                                                                               border=chrBorderColor2),
                                               x=XSARO[[s]],
                                               y=YSARO[[s]]
        )#m
       ) #l
      } # YHARO len

      if(exists("YHO1Ro")){ # PLOT HOLOCEN SQ CHROMATID



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
 } else {  # cP FALSE # cP TRUE circular - squareness < 20

   #
   #   x horizontal to vertical
   #

   if(callPlot){
     graphics::plot("",xlim=c( (min(unlist(circleMapsOrig), na.rm=TRUE)-xlimLeftMod),
                               (max(unlist(circleMapsOrig), na.rm=TRUE)+xlimRightMod )
     ),
     ylim = c( min (unlist(circleMapsOrig), na.rm = TRUE) +ylimBotMod*-1 ,
               ( (max(unlist(circleMapsOrig), na.rm = TRUE) )+ylimTopMod)
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

if(exists("roundedX") ) {

 xlistNewChr <- xHortoVer(roundedX)

 yInterLong<-intercalate(roundedY,monocenNames)
 names(yInterLong) <- names(y)

 ylistNewChrLong   <- yVertoHor(yInterLong,monocenNames )
 names(ylistNewChrLong) <-names(y)

 ylistTransChr     <- transYList(ylistNewChrLong,shrinkFactor,monocenNames)
 names(ylistTransChr) <-names(ylistNewChrLong)

 circleMapsRo<-applyMapCircle(radius,circleCenter,circleCenterY,separFactor
                              ,ylistTransChr
                              ,xlistNewChr
                              ,n,0,chrWidth,rotation=rotation)
}

if (chromatids==FALSE) {

    #
    #   plot chr
    #
    drawPlot(circleMapsRo,chrColor,lwd.chr,chrBorderColor2)

  } else if (chromatids) { # chrmtds FALSE TRUE

    ### copied
    if(holocenNotAsChromatids) {

      #
      # Only holocen (not chrtds) similar to chromatids=FALSE
      #

      if(length(monocenNames)){
        circleMapsRoHolo <- circleMapsRo[which(!names(circleMapsRo) %in% monocenNames)]
        drawPlot(circleMapsRoHolo,chrColor,lwd.chr,chrBorderColor2)
      } else {
        drawPlot(circleMapsRo,chrColor,lwd.chr,chrBorderColor2)
      }
    }

    if(exists("YSARO")) {


        xInter <- intercalate(XSARO,monocenNames)
        names(xInter) <- names(XSARO)

        xlistNewChr   <- xHortoVer(xInter)

        yInter<-intercalate(YSARO,monocenNames)
        names(yInter)<-names(YSARO)

        ylistNewChrSimple<-yVertoHor(yInter,monocenNames)
        names(ylistNewChrSimple)<-names(YSARO)

        ylistTransChrSimple<-transYList(ylistNewChrSimple,shrinkFactor,monocenNames)
        names(ylistTransChrSimple)<-names(ylistNewChrSimple)

        circleMaps <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor
                                     ,ylistTransChrSimple
                                     ,xlistNewChr,n,0,
                                     chrWidth,rotation=rotation)

        #
        #   plot chr
        #

        drawPlot(circleMaps,chrColor,lwd.chr,chrBorderColor2)

      }

      if(exists("YHO1Ro") & holocenNotAsChromatids==FALSE) {

        xlistNewChr   <- xChrtdMap(XHO1Ro,x, 0)

        yInter        <- intercalate(YHO1Ro,monocenNames)
        names(yInter) <- names(YHO1Ro)

        ylistNewChrSimple <- yVertoHor(yInter,monocenNames)
        names(ylistNewChrSimple) <- names(YHO1Ro)

        ylistTransChrSimple01_ro <- transYList(ylistNewChrSimple,shrinkFactor,monocenNames)
        names(ylistTransChrSimple01_ro) <- names(ylistNewChrSimple)

        circleMaps <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransChrSimple01_ro
                                     ,xlistNewChr,n,0,
                                     chrWidth,rotation=rotation)

        #
        #   plot chr
        #

        drawPlot(circleMaps,chrColor,lwd.chr,chrBorderColor2)

        # xlistNewChr<- xHortoVer(XHO2)
        xlistNewChr <- xChrtdMap(XHO2Ro,x, 0)

        circleMaps  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransChrSimple01_ro
                                     ,xlistNewChr,n,0,
                                     chrWidth,rotation=rotation)

        #
        #   plot chr
        #

        drawPlot(circleMaps,chrColor,lwd.chr,chrBorderColor2)

      }
    ### end copy


} # chrtd end

} # circularplot


} # squareness if squareness <20 end plot

############################################################

if(circularPlot){

  if(addOTUName) {

    firstXchrEach <- lapply(xlistNewChrSimple, `[[`, 1)
    firstYchrEach <- lapply(ylistTransChrSimpleOrig, `[[`, 1)


    circleMapsOTUname <- mapOTUnames(firstYchrEach
                                     , firstXchrEach
                                     # , ylistNewChrSimple
                                     , ylistNewChrSimpleOrig
                                     , n, radius
                                     , circleCenter, circleCenterY,separFactor,
                                     OTUlabelSpacing, chrWidth,rotation=rotation)

    addOTUnames(circleMapsOTUname,OTUTextSize,OTUsrt,OTUplacing,OTUfont2,OTUfamily2,
                circleCenter,OTULabelSpacerx,circleCenterY,OTULabelSpacery,
                OTUlegendHeight*normalizeToOne,radius,chrWidth,normalizeToOne
                ,OTUcentered,OTUjustif,separFactor,labelSpacing)
  }

  #
  # Plot chr. names
  #

  if(chrId!="none" & chrId!="") {
    listXChrCenter <- mapChrCenter(xlistNewChrSimple)
    listYChrCenter <- mapChrCenter(ylistTransChrSimpleOrig)
    names(listYChrCenter)<-names(ylistTransChrSimpleOrig)
    chrNames <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,
                               listYChrCenter,listXChrCenter,n,-chrLabelSpacing,chrWidth,
                               specialOTUNames=specialOTUNames,chrWFactor=chrWFactor,rotation=rotation)
    plotChrNames(chrNames, indexIdTextSize, chrId,monocenNames,chrColor)
  }
}

############################################################
#
#                     ruler calculate
#
############################################################

if(circularPlot==FALSE){
  if (ruler) {
    if (length( which(names(listChrSize) %in% monocenNames) )>0 ) {

    maxShortRound<-lapply(1:length(listChrSizeMonocen),
                            function(x) {
                            return(tryCatch(customCeiling(max(listChrSizeMonocen[[x]]$shortArmSize),
                                         ceilingFactor), error=function(e) NA ) )
                            }
                          )
    names(maxShortRound) <- names(listChrSizeMonocen)

    maxLongRound <-lapply(1:length(listChrSizeMonocen),
                          function(x)
                            tryCatch(customCeiling(max(listChrSizeMonocen[[x]]$longArmSize ),
                                  ceilingFactor), error=function(e) NA ) )


    fromZerotoMaxLong<-fromZerotoMaxShort<-list()

    for (i in 1:length(maxShortRound)) {

      corr_index <- which(names(listChrSize) %in% names(listChrSizeMonocen)[[i]] )
      divisor2   <- as.numeric(attr(listChrSize[[corr_index]],"divisor"))

      if ( attr(listChrSizeMonocen[[i]], "ytitle" )=="cM" ) {
        if(rulerIntervalcM!=0) {
          if (rulerIntervalcM > divisor2/11 ) {
            rulerInterval2 <-rulerIntervalcM/divisor2
          } else {
            message(crayon::green("rulerIntervalcM too small, using default"))
            rulerInterval2 <- 1
          }
        } else {
          rulerInterval2 <- 1
        }
      } else if( attr(listChrSizeMonocen[[i]], "ytitle" )=="Mb" ) {
        if( rulerIntervalMb != 0 ) {
          rulerIntervalMb2 <- rulerIntervalMb*1000000
          if (rulerIntervalMb2 > (divisor2/11) ) {
            rulerInterval2 <- rulerIntervalMb2/divisor2
          } else {
            message(crayon::green("rulerIntervalMb too small, using default"))
            rulerInterval2 <- 1
          }
        } else {
          rulerInterval2 <- 1
        }

      } else if(attr(listChrSizeMonocen[[i]], "ytitle" )=="notMb" ) {
        if( rulerInterval != 0 ) {
          rulerInterval2 <- rulerInterval/divisor2
        } else {
          rulerInterval2 <- 1
        }
      }

      if(rulerInterval2 > maxShortRound[[i]]){
        message(crayon::red(paste0("rulerInterval too big. Use smaller rulerInterval; rulerIntervalMb or rulerIntervalcM
                                   if you have Mb data or specialOTUNames, respectively" ) )
                )
      }

      fromZerotoMaxShort[[i]]<- tryCatch(seq(
        from = 0,
        to = (maxShortRound[[i]] +
        (rulerInterval2 * ifelse(maxShortRound[[i]] %% rulerInterval2>0,1,0) )
        - maxShortRound[[i]] %% rulerInterval2),
        by = rulerInterval2), error=function(e) NA
        ) # try

      fromZerotoMaxLong[[i]]<- tryCatch(seq(
        from = 0,
          to = (maxLongRound[[i]] + (rulerInterval2 * ifelse(maxLongRound[[i]] %% rulerInterval2 > 0,1,0) )
          - maxLongRound[[i]] %% rulerInterval2),
          by = rulerInterval2),
        error=function(e) NA
      ) # try

      if(!is.na(centromereSize)) {
        centromereSize2 <- centromereSize
      } else {
        #
        # auto cen. size
        #
        centromereSize2 <- divisor2
      }

      if(cenFormat=="inProtein"){
        centromereSize2 <- 0
      }

      attr(fromZerotoMaxShort[[i]],"centromere") <- (centromereSize2/divisor2)*cenFactor

      remove(rulerInterval2)

    } # for maxshortround

    if(collapseCen) {
      collapseF <- 1
    } else {
      collapseF <- 0
    }

    names(fromZerotoMaxShort) <- names(maxShortRound)

    ycoordLongRound <-lapply(1:length(fromZerotoMaxLong), function(x) {
      pos <- as.numeric(attr(listChrSizeMonocen[[x]],"position") )-1
      pos <- ifelse(verticalPlot,pos,0)
      cenNorm <- as.numeric(attr(fromZerotoMaxShort[[x]],"centromere"))*normalizeToOne
      unlist(
        lapply(1:length(fromZerotoMaxLong[[x]]), function(y)
          (karHeight -
             (fromZerotoMaxLong[[x]][y] * normalizeToOne) ) + (karHeiSpace*pos) + moveAllKarValueY2 + (cenNorm/2)*collapseF
        ) #l
      ) #u
    } # ycoordLongRound
    ) # l

    names(ycoordLongRound)<-(monocenNames2)

    ycoordShortRound  <- lapply(1:length(fromZerotoMaxShort), function(x){
      pos<-as.numeric(attr(listChrSizeMonocen[[x]],"position") )-1
      pos<-ifelse(verticalPlot,pos,0)
      cenNorm <- as.numeric(attr(fromZerotoMaxShort[[x]],"centromere"))*normalizeToOne
      unlist(
        lapply(1:length(fromZerotoMaxShort[[x]]), function(y)
          (karHeight + cenNorm +
             (fromZerotoMaxShort[[x]][y]*normalizeToOne) ) + (karHeiSpace*(pos) ) + moveAllKarValueY2 - (cenNorm/2)*collapseF
        ) # l
      ) # u
    }
    ) #l

    names(ycoordShortRound) <- monocenNames2

    if( (is.na(addMissingOTUAfter[1] ) | addMissingOTUAfter[1]== "") &
          ( is.na(addMissingOTUBefore[1]) | addMissingOTUBefore[1]== "")  ) {

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

    ycoordLongRound  <- ycoordLongRound[!is.na(ycoordLongRound)]
    ycoordShortRound <- ycoordShortRound[!is.na(ycoordShortRound)]

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

    rulerPlot(ycoordShortRound,listChrSize,listChrSizeMonocen,fromZerotoMaxShort,rulerNumberSize,rulerPos,ruler.tck,
              lwd.chr,moveKarHor2,mkhValue,useMinorTicks,miniTickFactor,verticalPlot,moveAllKarValueHor)

    ################

    # long arm ruler labels

    ################

    rulerPlot(ycoordLongRound,listChrSize,listChrSizeMonocen,fromZerotoMaxLong,rulerNumberSize,rulerPos,ruler.tck,
              lwd.chr,moveKarHor2,mkhValue,useMinorTicks,miniTickFactor,verticalPlot,moveAllKarValueHor)

    } # monocen

    ######################################################################################### holocen ruler


    if (length( which(names(listChrSize) %in% holocenNames) ) > 0 ) {

    maxChrRound<-lapply(1:length(listChrSizeHolocen),
                        function(x) {
                          return(tryCatch(
                        customCeiling(max(listChrSizeHolocen[[x]]$chrSize),
                        ceilingFactor),
                        error=function(e) NA )
                         )}
                       ) # l

    fromZerotoMaxChr<-list()

    for (i in 1:length(maxChrRound)) {

      corr_index<-which(names(listChrSize) %in% names(listChrSizeHolocen)[[i]] )
      divisor2<-as.numeric(attr(listChrSize[[corr_index]],"divisor"))

      if ( attr(listChrSizeHolocen[[i]], "ytitle" )=="cM" ) {
        if( rulerIntervalcM != 0 ) {
          if (rulerIntervalcM > divisor2/11 ) {
            rulerInterval2<-rulerIntervalcM/divisor2
          } else {
            message(crayon::green("rulerIntervalCM too small, using default"))
            rulerInterval2<-1
          }
        } else {
          rulerInterval2<-1
        }

      } else if( attr(listChrSizeHolocen[[i]], "ytitle" )=="Mb" ) {
        if( rulerIntervalMb != 0 ) {
          rulerIntervalMb2 <- rulerIntervalMb*1000000
          if (rulerIntervalMb2 > divisor2/11 ) {
            rulerInterval2<-rulerIntervalMb2/divisor2
          } else {
            message(crayon::green("rulerIntervalMb too small, using default"))
            rulerInterval2<-1
          }
        } else {
          rulerInterval2<-1
        }
      } else if(attr(listChrSizeHolocen[[i]], "ytitle" )=="notMb" ) {
        if( rulerInterval != 0 ) {
          rulerInterval2 <- rulerInterval/divisor2
        } else {
          rulerInterval2 <- 1
        }
      }

      if(rulerInterval2 > maxChrRound[[i]]){
        message(crayon::red(paste0("rulerInterval too big. Use smaller rulerInterval; or use rulerIntervalMb
        or rulerIntervalcM if you have Mb data or specialOTUNames, respectively" ) )
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
      pos<-as.numeric(attr(listChrSizeHolocen[[x]],"position") )-1
      pos<-ifelse(verticalPlot,pos,0)
      unlist(
        lapply(1:length(fromZerotoMaxChr[[x]]), function(y)
          (karHeight/2+(fromZerotoMaxChr[[x]][y]*normalizeToOne))+ (karHeiSpace*pos) + moveAllKarValueY2 )         # 0+(from
            ) # u
      }
    ) # l

    names(ycoordChrRound)<-holocenNames2

        if( (is.na(addMissingOTUAfter[1] ) | addMissingOTUAfter[1]== "") &
            ( is.na(addMissingOTUBefore[1]) | addMissingOTUBefore[1]== "")  ) {

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

    rulerPlot(ycoordChrRound,listChrSize,listChrSizeHolocen,fromZerotoMaxChr,rulerNumberSize,rulerPos,ruler.tck,
              lwd.chr,moveKarHor2,mkhValue,useMinorTicks,miniTickFactor,verticalPlot,moveAllKarValueHor)

  } # end holocen

    ###################################################3
    #   ADD TITLE OF RULER
    ###################################################

    par(las=1)

    # rulerTitle(ycoordShortRound,listChrSizeMonocen,MbUnit,specialyTitle,yTitle,xPosRulerTitle,rulerTitleSize)
    rulerTitle(xmnoNA,ymnoNA,chrSpacing,yPosRulerTitle,listChrSizenoNA,MbUnit
               ,specialyTitle,yTitle,xPosRulerTitle,rulerTitleSize,verticalPlot)


 }   # end rulers if
} # END     not    circular ruler

  ############################################################
  #
  #   groups line
  #
  ###########################################################

if(circularPlot==FALSE) {

    if("group" %in% colnames(dfChrSizeIntDivisor) ) {

      groupSegmentDistance <- 1 #ifelse(groupUp, 1, 2)

      chrIdCount  <- ifelse(groupUp | chrId=="none" | chrId=="" ,0, 1 ) # warn

    for (s in 1:length(xmnoNA)) {
      if(attr(listChrSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2<-chrWidth
      }

      # allowing not organized groups
      if("group" %in% colnames(listChrSizenoNA[[s]] ) ) {
      lens <- rle(listChrSizenoNA[[s]]$group)$lengths
      names(lens)<-rle(listChrSizenoNA[[s]]$group)$values

      ngroup<-length(lens)

      attr(listChrSizenoNA[[s]],"groupPresence") <- ngroup
      attr(listChrSizenoNAIndex[[s]],"groupPresence") <- ngroup

      for (g in 1: ngroup) {

        if(!is.na(names(lens)[g] ) ) {
        x0 <- xmnoNA[[s]][,3][ifelse(length(cumsum(lens)[g-1] )==0, # ifelseinloop
                               1,
                               # cumsum(table(listChrSizenoNA[[s]]$group) )[g-1]+1
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

###################################################################################
#                       chromosome names non-Circular
###################################################################################

chrNameDistance <-1

# original
if(circularPlot==FALSE){

  if(chrId=="original" | chrId=="simple"){

    for (s in 1:length(xmnoNA)) {

      if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

      if(chrId=="original"){
        mylabels<-listChrSizenoNA[[s]][,"chrName"]

        if(!missing(chrIdPatternRem)){
          mylabels<- sub(chrIdPatternRem,"",mylabels )
        }

      } else if (chrId=="simple"){
        mylabels <- 1:(nrow(xmnoNA[[s]])/armFactor)
      }

      if(attr(listChrSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2  <-specialChrWidth
      } else {
        chrWidth2<-chrWidth
      }

      if(as.numeric(attr(listChrSizenoNA[[s]],"groupPresence") ) > 0 & groupUp ) {
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

      if( attr(listChrSizenoNA[[s]], "ytitle" )=="Mb" ){
        unit<- classMbName
      } else if ( attr(listChrSizenoNA[[s]], "ytitle" )=="cM" ) {
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

chrIdCount  <- ifelse(chrId=="none" | chrId=="",0,1) # warn

chrSizeShow <- ifelse(chrSize==TRUE,1,0)

chrSizeMbpShow <- ifelse(chrSizeMbp==TRUE,1,0)

# perPresence <- ifelse(markPer != "",1,0)
perPresence    <- ifelse(markPer[1] == "",0,length(markPer))

# ifelse(markPer[1] == "",0,1:length(markPer)) # not work

perPresenceVec <- if(markPer[1] == ""){0} else {1:length(markPer)}

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

for (s in 1:length(listChrSizenoNA) ) {

  if("chrNameUp" %in% (colnames(listChrSizenoNA[[s]] ) ) ) {

    if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

    if(attr(listChrSizenoNA[[s]],"ytitle")=="cM"){
      chrWidth2  <-specialChrWidth
    } else {
      chrWidth2<-chrWidth
    }

    chrNamesUp<- listChrSizenoNA[[s]][,"chrNameUp"]

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
for (s in 1:length(listChrSizenoNA) ) {

  if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

  if(as.numeric(attr(listChrSizenoNA[[s]],"groupPresence") ) > 0 ) {
    ifelse(groupName,groupCount<-2,groupCount<-1)
  } else {
    groupCount=0
  } # end ifelse

  if(attr(listChrSizenoNA[[s]],"ytitle")=="cM"){
    chrWidth2 <- specialChrWidth
  } else {
    chrWidth2 <- chrWidth
  }

  if( attr(listChrSizenoNA[[s]], "ytitle" )=="Mb" ){
    unit<-MbUnit
  } else if ( attr(listChrSizenoNA[[s]], "ytitle" )=="cM" ) {
    unit<-specialyTitle
  } else {
    unit<-yTitle
  }

  divisor2<-as.numeric(attr(listChrSizenoNA[[s]],"divisor"))

  if ( attr(listChrSizenoNA[[s]], "ytitle" )=="cM" ) {
    labels<-listChrSizenoNA[[s]][,"chrSize"]*divisor2
  } else if ( attr(listChrSizenoNA[[s]], "ytitle" )=="Mb" ) {
    labels<-listChrSizenoNA[[s]][,"chrSize"]*divisor2/1e6
  } else  { # ytitle notmb
    labels<-listChrSizenoNA[[s]][,"chrSize"]*divisor2
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

  for (s in 1:length(listChrSizenoNA) ) {

    if("Mbp" %in% (colnames(listChrSizenoNA[[s]] ) ) ) {

    if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

    if(as.numeric(attr(listChrSizenoNA[[s]],"groupPresence") ) > 0 ) {
      ifelse(groupName,groupCount<-2,groupCount<-1)
    } else {
      groupCount=0
    } # end ifelse

    if(attr(listChrSizenoNA[[s]],"ytitle")=="cM"){
      chrWidth2  <-specialChrWidth
    } else {
      chrWidth2<-chrWidth
    }

    mbpLabel<-  format(round(listChrSizenoNA[[s]][,"Mbp"],nsmall),nsmall=nsmall )

    graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2) * nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/armFactor)]+(chrWidth2/2) ),
                   min(ymnoNA[[s]]) - ( ( (distTextChr/3) * (chrIdCount + chrSizeShow + chrSizeMbpShow + groupCount ) ) )  ,
                   labels = tryCatch( c("S (Mbp)", mbpLabel), error=function(e){NA} )
                   ,cex=indexIdTextSize
    ) # end graphics::text
    } else {
      message(crayon::blue(paste("Mbp column not found for species", names(listChrSizenoNA[s]) ) ) )
    }
  } # FOR


} # chrSize


#################################
# horizontal chromosome morphology categories
#################################

#
#   add CI
#

if(chrIndex=="both" | chrIndex == "CI" ) {

  for (s in 1:length(listChrSizenoNAIndex) ) {

    if(as.numeric(attr(listChrSizenoNAIndex[[s]],"groupPresence") ) > 0 ) {
      ifelse(groupName,groupCount<-2,groupCount<-1)
    } else {
      groupCount=0
    } # end ifelse

    if(attr(listChrSizenoNAIndex[[s]],"ytitle") == "cM"){
      chrWidth2 <- specialChrWidth
    } else {
      chrWidth2 <- chrWidth
    }

    if(attr(listChrSizenoNAIndex[[s]],"indexStatus")=="success") {

      graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos,
                       xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+(chrWidth2/2)
                       ),
                 min(ymnoNA[[s]]) - ( ( (distTextChr/3) * (chrIdCount+chrSizeShow+chrSizeMbpShow+
                                                             groupCount+indexCount-bothAddI ) )
                                      )
                 ,
                 labels = tryCatch(c("CI",listChrSizenoNAIndex[[s]][,"CI"] ),error=function(e){NA})
                 ,cex=indexIdTextSize
                ) # end graphics::text
   } # success
  } # FOR
} # BORH OR CI

#
#   add AR (radius)
#

if(chrIndex=="both" | chrIndex == "AR" ) {
for (s in 1:length(listChrSizenoNAIndex) ) {
  if(as.numeric(attr(listChrSizenoNAIndex[[s]],"groupPresence") ) > 0 ) {
    ifelse(groupName,groupCount<-2,groupCount<-1)
  } else{groupCount=0}
  if(attr(listChrSizenoNAIndex[[s]],"ytitle")=="cM"){
    chrWidth2  <-specialChrWidth
  } else {
    chrWidth2  <-chrWidth
  }

  if(attr(listChrSizenoNAIndex[[s]],"indexStatus")=="success") {

    ARLabel<-  listChrSizenoNAIndex[[s]][,"AR"]
    # ARLabel<-  format(round( listChrSizenoNAIndex[[s]][,"ARnum"],nsmall),nsmall=nsmall )

    graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth2/2),
                   rep( min(ymnoNA[[s]]) - ( ( (distTextChr/3) * (chrIdCount+chrSizeShow+chrSizeMbpShow+groupCount+indexCount) ) ) ,(nrow(xmnoNA[[s]])/2)+1 ),
                   labels = tryCatch(c("r", ARLabel ),error=function(e){NA})
                   ,cex=indexIdTextSize
    ) # end graphics::text
  } # success
} # FOR
} # fi BOTH OR AR


#
#   add Guerra and Levan
#

if(morpho=="both"){bothAdd=1} else {bothAdd=0}

#
#   add Guerra
#

if(morpho=="both" | morpho == "Guerra" ) {

for (s in 1:length(listChrSizenoNAIndex) ) {

  if(as.numeric(attr(listChrSizenoNAIndex[[s]],"groupPresence") ) > 0 ) {
    ifelse(groupName,groupCount<-2,groupCount<-1)
  } else{groupCount=0}
  if(attr(listChrSizenoNAIndex[[s]],"ytitle")=="cM"){
    chrWidth2  <-specialChrWidth
  } else {
    chrWidth2<-chrWidth
  }

  if(attr(listChrSizenoNAIndex[[s]],"indexStatus")=="success") {

  graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth2/2),
                 min(ymnoNA[[s]]) - ( (distTextChr/3)*(chrIdCount+chrSizeShow+chrSizeMbpShow+morphoCount+indexCount-bothAdd+groupCount) ),
                 labels = tryCatch(c("Guerra",listChrSizenoNAIndex[[s]][,"Guerra"]),error=function(e){NA})
                 ,cex=indexIdTextSize
  ) # end graphics::text
  } # if success
} # for
} # if guerra

#
#   add Levan
#

if(morpho=="both" | morpho == "Levan" ) {

  for (s in 1:length(listChrSizenoNAIndex) ) {

    if(as.numeric(attr(listChrSizenoNAIndex[[s]],"groupPresence") ) > 0 ) {
      ifelse(groupName,groupCount<-2,groupCount<-1)
    } else {groupCount=0}

    if(attr(listChrSizenoNAIndex[[s]],"ytitle")=="cM"){
      chrWidth2  <-specialChrWidth
    } else {
      chrWidth2<-chrWidth
    }

    if(attr(listChrSizenoNAIndex[[s]],"indexStatus")=="success") {

      graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/2)]+chrWidth2/2),
                     min(ymnoNA[[s]]) - ( ( (distTextChr/3)* (chrIdCount+chrSizeShow+chrSizeMbpShow+morphoCount+indexCount+groupCount) ) ) ,
                     #distVectorGue[decVector]
                     labels = tryCatch(c("Levan",listChrSizenoNAIndex[[s]][,"Levan"]),error=function(e){NA})
                     ,cex=indexIdTextSize
      ) # end graphics::text
    } # if success

  } # for
} # fi

  #
  #   add % Het
  #

markNotPresent <- rep(0,length(listChrSizenoNA))

  if(markPer[1]!="" & exists("allMarkNames") ) {

    for (s in 1:length(listChrSizenoNA) ) {

      if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

      if(as.numeric(attr(listChrSizenoNA[[s]],"groupPresence") ) > 0 ) {
        ifelse(groupName,groupCount<-2,groupCount<-1)
      } else {groupCount=0}

      if(attr(listChrSizenoNA[[s]],"ytitle")=="cM"){
        chrWidth2 <-specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }

      if(attr(listChrSizenoNAIndex[[s]],"indexStatus")=="success") {
        yForPer <- min(ymnoNA[[s]]) - ( ( (distTextChr/3)* (chrIdCount+chrSizeShow+chrSizeMbpShow+morphoCount+indexCount+groupCount+perPresenceVec) ) )
      } else {
        yForPer <- min(ymnoNA[[s]]) - ( ( (distTextChr/3)* (chrIdCount+chrSizeShow+chrSizeMbpShow+groupCount+perPresenceVec) ) )#+1
      }

      # fraction
      i = 1

      #
      #   for each mark
      #

      for (markP in markPer) {

        if(paste0(markP,"_per") %in% colnames(t(perList[[s]]))) {

        perValue <- t(perList[[s]])[,paste0(markP,"_per")] # was markPer

        if (perAsFraction==FALSE){
          perValue <- perValue*100
        }

        perValue[perValue==0]<-NA

        perValue <- format(round(perValue ,nsmall ),nsmall=nsmall)
        perValue[grep("NA",perValue)]<-""

        graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos,
                         xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/armFactor)]+chrWidth2/2 )
                         ,yForPer[i]
                         ,labels = tryCatch(c(paste0("% ",markP) # was markPer
                                              , perValue
                                              )
                                             ,error=function(e){""}
                                             )
                          ,cex=indexIdTextSize
          ) # end graphics::text
          i = i+1
        } else {# if band
          markNotPresent[s] <- markNotPresent[s]+1
        }
     } # for mark

    } # for species
  } # fi


#
#   add marks' pos
#

if(showMarkPos & exists("dfMarkPosInt") ) {
  for (s in 1:length(listChrSizenoNA) ) {

    if(attr(xmnoNA[[s]],"cenType")=="monocen") {armFactor<-2} else {armFactor<-1}

    if(as.numeric(attr(listChrSizenoNA[[s]],"groupPresence") ) > 0 ) {
        ifelse(groupName,groupCount<-2,groupCount<-1)
    } else {
        groupCount=0
    }

    if(attr(listChrSizenoNA[[s]],"ytitle")=="cM"){
      chrWidth2 <-specialChrWidth
    } else {
      chrWidth2 <- chrWidth
    }

    if(attr(listChrSizenoNAIndex[[s]],"indexStatus")=="success") {
      yForPos<-min(ymnoNA[[s]]) - ( ( (distTextChr/3)* (chrIdCount+chrSizeShow+chrSizeMbpShow+
                                                          morphoCount+indexCount+groupCount+
                                                          perPresence+posPresence-markNotPresent[s] ) ) )
    } else {
      yForPos<-min(ymnoNA[[s]]) - ( ( (distTextChr/3)* (chrIdCount+chrSizeShow+chrSizeMbpShow+
                                                          groupCount+perPresence+posPresence-
                                                          markNotPresent[s]) ) )
    }

    # fraction

    posValue <- tryCatch(lapply(t(posTib[[s]])[,1], sort), error=function(e){"no data"} )

    if(posValue[1] != "no data") {

      posValue <- unlist(lapply(posValue, function(x) tryCatch( (paste0(format(round(x , 2),nsmall=2), collapse="/") ), error=function(e){NA} ) ) )

      graphics::text(c(xmnoNA[[s]][,3][1]-(chrWidth/2)*nameChrIndexPos, xmnoNA[[s]][,3][1:(nrow(xmnoNA[[s]])/armFactor)]+chrWidth2/2 )
                     ,yForPos
                     ,labels = tryCatch(c(paste0("pos.")
                                          ,posValue
                                          )
                                          ,error=function(e){NA}
                     )
                     ,cex=indexIdTextSize
      ) # end graphics::text

    } # NO DATA

  } # for sps
} # fi

} # circular FALSE


#########################################################################
#                      add species names otu names
#########################################################################

if(!missing(notes)){
        if(inherits(notes,"data.frame") | inherits(notes,"data.table") | inherits(notes, "character") ) {
          notes2<-notes
        }
}

if(exists("notes2") ) {

        if (inherits(notes2, "character")) {
          if (file.exists(notes2)) {
            tryCatch(notes2Int <- read.csv(notes2, header = TRUE), error = function(e){"invalid notes file"} )
          }
        } else if ( !inherits(notes2, "data.frame") ) {
          message(crayon::red("notes2 is not a data.frame object or .csv filename in quotes") )
        }

        if(inherits(notes2, "data.frame") ) {
          if(!nrow(notes2)>0){
            remove(notes2)
          } else {
            notes2Int <- notes2
          }
        } else {
          remove(notes2)
        }

        if(exists("notes2Int") ){
          if(!inherits(notes2Int, "data.frame") ) {
            remove(notes2Int)
          } else {
            if(nrow(notes2Int)==0 ) {
              remove(notes2Int)
            }
          }
        }
}

if(!missing(leftNotes)){
if(inherits(leftNotes,"data.frame") | inherits(leftNotes,"data.table") | inherits(leftNotes, "character") ) {
  leftNotes2<-leftNotes
}
}

if(exists("leftNotes2") ) {

if (inherits(leftNotes2, "character")) {
  if (file.exists(leftNotes2)) {
    tryCatch(leftNotes2Int <- read.csv(leftNotes2, header = TRUE), error = function(e){"invalid leftNotes file"} )
  }
} else if ( !inherits(leftNotes2, "data.frame") ) {
  message(crayon::red("leftNotes2 is not a data.frame object or .csv filename in quotes") )
}

if(inherits(leftNotes2, "data.frame") ) {
  if(!nrow(leftNotes2)>0){
    remove(leftNotes2)
  } else {
    leftNotes2Int <- leftNotes2
  }
} else {
  remove(leftNotes2)
}

if(exists("leftNotes2Int") ){
  if(!inherits(leftNotes2Int, "data.frame") ) {
    remove(leftNotes2Int)
  } else {
    if(nrow(leftNotes2Int)==0 ) {
      remove(leftNotes2Int)
    }
  }
}
}

if(!missing(leftNotesUp)){
if(inherits(leftNotesUp,"data.frame") | inherits(leftNotesUp,"data.table") | inherits(leftNotesUp, "character") ) {
  leftNotesUp2<-leftNotesUp
}
}

if(exists("leftNotesUp2") ) {

if (inherits(leftNotesUp2, "character")) {
  if (file.exists(leftNotesUp2)) {
    tryCatch(leftNotesUp2Int <- read.csv(leftNotesUp2, header = TRUE), error = function(e){"invalid leftNotesUp file"} )
  }
} else if ( !inherits(leftNotesUp2, "data.frame") ) {
  message(crayon::red("leftNotesUp2 is not a data.frame object or .csv filename in quotes") )
}

if(inherits(leftNotesUp2, "data.frame") ) {
  if(!nrow(leftNotesUp2)>0){
    remove(leftNotesUp2)
  } else {
    leftNotesUp2Int <- leftNotesUp2
  }
} else {
  remove(leftNotesUp2)
}

if(exists("leftNotesUp2Int") ){
  if(!inherits(leftNotesUp2Int, "data.frame") ) {
    remove(leftNotesUp2Int)
  } else {
    if(nrow(leftNotesUp2Int)==0 ) {
      remove(leftNotesUp2Int)
    }
  }
}
}


if (circularPlot==FALSE){

        if(OTUasNote){

          addOTUName<-FALSE

          if(exists("notes2Int")){
            message(crayon::blurred("Warning: OTUasNote is TRUE, notes data.frame will be removed"))
          }
          notes2Int <- data.frame(OTU=unique(dfChrSizeIntDivisor$OTU), note=unique(dfChrSizeIntDivisor$OTU) )
        }

        if(OTUasLeftNote){

          addOTUName<-FALSE

          if(exists("leftNotesUp2Int")){
            message(crayon::blurred("Error: OTUasLeftNote is TRUE, leftNotesUp data.frame will be removed"))
          }
          leftNotesUp2Int <- data.frame(OTU=unique(dfChrSizeIntDivisor$OTU), note=unique(dfChrSizeIntDivisor$OTU) )
        }

        #
        #   ADD OTU NAME BELOW CHR.
        #

        if(addOTUName) {

          # message(crayon::green(paste0("OTU section start" ) ) )

          for (s in 1:length(xmnoNA) ) {
            if(as.numeric(attr(listChrSizenoNA[[s]],"groupPresence") ) > 0 ) {
              ifelse(groupName,groupCount<-2,groupCount<-1)
            } else {
              groupCount=0
            } # end ifelse

            if(attr(listChrSizenoNAIndex[[s]],"indexStatus")=="failure") {
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

            OTUcurrent<-names(listChrSizenoNA)[[s]]
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

            distFac<- (chrIdCount + chrSizeShow+ chrSizeMbpShow + morphoCount2 +
                         indexCount2 + groupCount + perPresence + posPresence +
                         2 - holocenDisCount - markNotPresent[s])


            graphics::text( min(xmnoNA[[s]] )
                            ,ydistance <- min(ymnoNA[[s]]) - ( (distTextChr/3) *
                                                 distFac )

                            ,labels = nameWithVar
                            # labels = paste("",names(listChrSizenoNA)[[s]] ),
                            ,cex=OTUTextSize
                            ,adj= 0 # justif 0 =left
                            ,font=   ifelse( !missing(OTUfont),   OTUfont,   1)
                            ,family= ifelse( OTUfamily!="", OTUfamily, defaultFontFamily2)
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

    for (i in 1:length(listChrSizenoNA) ) { # for each OTU

      if(attr(listChrSizenoNA[[i]],"cenType")=="monocen"){
            if(is.character(names(listChrSizenoNA)[[i]]  ) ){
              message(crayon::green(paste0(names(listChrSizenoNA)[[i]],":" ) ) # otu name:  Calc. (asymmetry)
              ) # mess
            }

        ind <- asymmetry(listChrSizenoNA[[i]])

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

     }  else if(attr(listChrSizenoNA[[i]],"cenType")=="holocen"){ #  if monocen
        if(is.character(names(listChrSizenoNA)[[i]]  ) ){
          message(crayon::green(paste0(names(listChrSizenoNA)[[i]],":" ) )
          ) # mess
        }
        ind<-asymmetryA2(listChrSizenoNA[[i]])
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

if(exists("listChrSizeMonocen") ) {

        #
        # cannot be based on listofmonocen because of addmissingotu
        #

        CentsList<-lapply(1:length(listChrSize), function(x) tryCatch(rep(0, nrow(listChrSize[[x]]) )
                                                                      , error=function(e) NA )
        ) # l
        names(CentsList)<-names(listChrSize)

        ycoordCents <-list()

        for (i in 1:length(CentsList)){
          khsFactor<-ifelse(verticalPlot,(i-1),0)
          corr_index <- which(names(listChrSize) %in% names(CentsList)[[i]] )

          centromereSize3 <- tryCatch(as.numeric(attr(listChrSize[[corr_index]],"centromere") ) , error=function(e) NA )

          ycoordCents[[i]] <- t(replicate(length(CentsList[[i]])
                                          , ( c( rep(  karHeight+  (karHeiSpace*khsFactor ), 2  ),
                                              (  (karHeight+  (karHeiSpace*khsFactor ) ) +
                                              (karHeight + (centromereSize3*normalizeToOne)  +(karHeiSpace*khsFactor) )
                                              ) /2
                                              ,rep(  karHeight + (centromereSize3*normalizeToOne)+(karHeiSpace*khsFactor) ,2  )
                                              ,   ( (karHeight + (karHeiSpace*khsFactor ) ) +
                                              (karHeight + (centromereSize3*normalizeToOne)+(karHeiSpace*khsFactor) )
                                              )/2
          ) # c
          ) + moveAllKarValueY2
          ) # r
          ) #t
        } # for
        names(ycoordCents)<-names(CentsList)

          if( (is.na(addMissingOTUAfter[1] ) | addMissingOTUAfter[1]== "") &
              ( is.na(addMissingOTUBefore[1]) | addMissingOTUBefore[1]== "")  ) {

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

        names(ycoordCentsS)<-names(listChrSize)

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
          if(names(listChrSizenoNA[s]) %in% monocenNames ){
            if(attr(listChrSizenoNA[[s]],"ytitle")=="cM"){
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

            names(xcoordCents)[s] <- names(listChrSizenoNA[s])
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

        cSbool<-if(is.numeric(centromereSize) & autoCenSize==FALSE  ) {
          if(centromereSize!=0) {
            TRUE
          } else {
            FALSE
          }
        } else if(autoCenSize){
          TRUE
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

                    names(xyLinesY1[[s]])[a]<- names(xyLinesY2[[s]])[a]<- names(ycoordCentsS[[s]][a])
                    names(xyLinesX1[[s]])[a]<- names(xyLinesX2[[s]])[a]<- names(xcoordCentsS[[s]][a])
                  }
                }

                names(xyLinesX2)<-names(xyLinesY2)<-names(xyLinesX1)<-names(xyLinesY1) <- names(ycoordCentsS)

              }
            }
          }
        } # roundedCen FALSE

        if(cenFormat=="rounded" ) {
          if(cSbool) {
            if(length(cenColor2=="white") ) {
              if(cenColor2=="white") {

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
                    names(xyRoundLinesY2[[s]])[a]<- names(xyRoundLinesY1[[s]])[a]<- names(ycoordCentsS[[s]][a])
                    names(xyRoundLinesX1[[s]])[a]<- names(xyRoundLinesX2[[s]])[a]<- names(xcoordCentsS[[s]][a])

                  }
                }

                names(xyRoundLinesX2)<-names(xyRoundLinesY2)<-names(xyRoundLinesX1)<-names(xyRoundLinesY1) <-
                  names(ycoordCentsS)
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

if(circularPlot) {

  ylistTransChrSimple<-ylistTransChrSimpleOrig

  if(cenFormat=="triangle" | cenFormat =="rounded") {

  spnamexcoord <- names(xcoordCentsS)[1]
  corrSp       <- which(names(x) %in% spnamexcoord)
  diffXRounded <- max(x[[corrSp]][[1]]) - max(xcoordCentsS[[1]][[1]]) # needed for all cen. marks


  if(cenFormat=="rounded"){
    ### new way of making cen.
    xlistNew        <- xHortoVerRoundCen(newLongxCen,diffXRounded)
    names(xlistNew) <- names(newLongxCen)

    yMarkPer        <- markMapPerCen(newLongyCen,y)
    names(yMarkPer) <- names(newLongyCen)

  } else if (cenFormat=="triangle") {  # ROUNDEDCEN FALSE

    xlistNew        <- xHortoVerRoundCen(xcoordCentsS,diffXRounded)
    names(xlistNew) <- names(xcoordCentsS)

    yMarkPer        <- markMapPerCen(ycoordCentsS,y)
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
} else if (circularPlot==FALSE) { # end cP ################################ cP FALSE

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

} # monocen exist listChrSizeMonocen


                            ##########################
                            #         MARKS          #
                            ##########################

##########################################################################################################3
#
#                           Marks monocen plot square marks
#
############################################################################################################

if (exists("pLiMaPosMonocen") & exists("dfMarkColorInt") ) {

  xMarkSq<-yMarkSq<-listMarkPosSq<-list()

  j<-1

  for (k in 1:length(pLiMaPosMonocen)) {
    currName <- names(pLiMaPosMonocen)[[k]]

    if( nrow(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style %in% c("square","squareLeft") ) , ] ) > 0 ){
      listMarkPosSq <- c(listMarkPosSq,list(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style %in%
                                                                         c("square","squareLeft") ),]))
      names(listMarkPosSq)[[j]] <- currName
      j<-j+1
    }
  }
  if(length(listMarkPosSq)>0){
    for (sm in 1:length(listMarkPosSq) ) {
      xMark1<-yMark1<-NULL

      corr_index<-which(names(listChrSize) %in% names(listMarkPosSq)[[sm]] )

      distCen <- attr(listChrSize[[corr_index]],"centromere")

      for (m in 1:nrow(listMarkPosSq[[sm]]) ) {
      # for (m in 1:2 ) {

        ifelse(listMarkPosSq[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
        ifelse(listMarkPosSq[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
        ifelse(listMarkPosSq[[sm]][m,"chrRegion"]=="q",endColumn<- 2, endColumn<- 1)
        ifelse(listMarkPosSq[[sm]][m,"chrRegion"]=="q",whichArm<- "long", whichArm<- "short")
        ifelse(listMarkPosSq[[sm]][m,"style"]=="square",squareSide<- "right", squareSide<- "left")

        ifelse(listMarkPosSq[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)

        rowIndex <- nrow(listChrSize[[corr_index]] ) * longORshort + (listMarkPosSq[[sm]][,"neworder"][m] )

        armStart<-ym[[corr_index]][rowIndex,column]
        armEnd  <-ym[[corr_index]][rowIndex,endColumn]


        armLength <- max(armStart,armEnd) - min(armStart,armEnd)

        # detect if whole arm

        mSizeOrig   <- listMarkPosSq[[sm]][m,"markSize"]
        mSize       <- mSizeOrig*normalizeToOne

        if(is.na(mSize)) {
          message(crayon::blue(paste0("you let square marks without size in a monocen. while using markDistCen (not NA)\n, maybe you want to mark all arm, use NA in markDistCen and use p|q|w in chrRegion. Now 0 will be used" ) ) )
          # listMarkPosSq[[sm]][m,"markSize"]<-0
          mSize<-0
        }

        #
        # correction for collapseCen
        #

        if(collapseCen) {
          markDistCen <- listMarkPosSq[[sm]][m,"markDistCen"] - distCen/2
          if(markDistCen < 0) {
            mSizeMod <- mSizeOrig + markDistCen
            mSizeNor <- mSize + markDistCen*normalizeToOne
            markDistCen <- 0
          } else {
            mSizeMod <- mSizeOrig
            mSizeNor <- mSize
          }
        } else {
          markDistCen <- listMarkPosSq[[sm]][m,"markDistCen"]
          mSizeMod <- mSizeOrig
          mSizeNor <- mSize
        }

        if(abs(sum(armLength, - mSizeNor) ) < efZero ) {
          wholeArm <- 'true'
        } else {
          wholeArm <- 'false'
        }

        yprox <- armStart +
          (markDistCen
           *normalizeToOne*mySign)

        yter <- armStart +
          ( sum( markDistCen
                 , mSizeMod, na.rm=TRUE ) *normalizeToOne*mySign )

        yMark1[[m]] <- if(longORshort==0) {c(yprox,yter,yter,yprox)} else {c(yter,yprox,yprox,yter)}

        attr(yMark1[[m]],"arm") <- listMarkPosSq[[sm]][m,"chrRegion"]

        # attr(yMark1[[m]],"armStart")<-armStart
        attr(yMark1[[m]],"rowIndex")<-rowIndex
        attr(yMark1[[m]],"wholeArm")<-wholeArm
        attr(yMark1[[m]],"whichArm")<-whichArm
        attr(yMark1[[m]],"squareSide")<-squareSide

        xMark1[[m]]<-xm[[corr_index]][listMarkPosSq[[sm]][,"neworder"][m],]
        attr(xMark1[[m]],"rowIndex")<-rowIndex

      }
      yMarkSq[[sm]]<-yMark1
      attr(yMarkSq[[sm]], "spname")<-names(listMarkPosSq)[[sm]]
      xMarkSq[[sm]]<-xMark1
      attr(xMarkSq[[sm]], "spname")<-names(listMarkPosSq)[[sm]]

    } # end for

    ########################
    #                             #
    #   add marks to plot monocen #
    #                             #
    ########################

chromatidsCheck_1<-chromatids==FALSE
chromatidsCheck_2<-chromatids

plotSqMarks(
  chromatidsCheck_1
  ,chromatidsCheck_2
  ,circularPlot
  ,squareness
  ,xMarkSq
  ,yMarkSq
  ,dfMarkColorInt
  ,lwd.marks2
  ,listMarkPosSq
  ,listChrSize
  ,specialChrWidth
  ,chrWidth
  ,yfactor
  ,markN
  ,ptsl
  ,bannedMarkName3
  ,y
  ,markLabelSize
  ,pattern
  ,separFactor
  ,labelSpacing
  ,circleCenter
  ,circleCenterY
  ,radius
  ,legend
  ,ylistTransChrSimple
  ,rotation
  ,labelOutwards
  ,x
  ,xModifierMono
  ,pts)

} else {remove(listMarkPosSq)}

  # square labels not centrom. (monocen.)

  booleanForsquareInlineLabel <- legend=="inline" & exists("dfMarkColorInt") &
    exists("listMarkPosSq") & circularPlot==FALSE

  if(booleanForsquareInlineLabel) {
    textLabel(xMarkSq,yMarkSq,listChrSize,listMarkPosSq,specialChrSpacing
              ,chrSpacing,markLabelSize,pattern,bannedMarkName3,
              markNewLine2=markNewLine,mylheight2=mylheight)
  }

} # if presence end painting marks

##################################################################################################
#
#                                painting Marks square holocen
#
##################################################################################################

if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") ) {
  # message(crayon::green(paste0("holocen. marks section start" ) ) )
  xMarkSq<-yMarkSq<-listMarkPosSq<-list()

  j<-1
  for (k in 1:length(pLiMaPosHolocen)) {
    currName<-names(pLiMaPosHolocen)[[k]]
    if(nrow(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style %in% c("square","squareLeft") ),])>0){
      listMarkPosSq <- c(listMarkPosSq,
                             list(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style %in% c("square","squareLeft")  ),])
                             )
      names(listMarkPosSq)[[j]]<-currName
      j<-j+1
    }
  }
  if(length(listMarkPosSq)>0) {
    for (sm in 1:length(listMarkPosSq)) {

      yMark1<-  xMark1<-NULL

      corr_index<-which(names(ym) %in% names(listMarkPosSq)[[sm]] )

      for (m in 1:nrow(listMarkPosSq[[sm]])){
        rowIndex<-(listMarkPosSq[[sm]][,"neworder"][m])
        chrStart <- ym[[corr_index]][rowIndex ,2]
        ifelse(listMarkPosSq[[sm]][m,"style"]=="square",squareSide<- "right", squareSide<- "left")

        if(!"markSize" %in% colnames(listMarkPosSq[[sm]])){
          listMarkPosSq[[sm]]$markSize<-NA
        }
        if(!"markPos" %in% colnames(listMarkPosSq[[sm]])){
          listMarkPosSq[[sm]]$markPos<-NA
        }
        mSize<-mPos<-NULL
        mSize <- listMarkPosSq[[sm]][m,"markSize"]*normalizeToOne
        mPos  <- listMarkPosSq[[sm]][m,"markPos"]

        if(is.na(mSize) ){
          message(crayon::blue(paste0("mark without size, unexpected results possible")))
        }
        if(is.na(mSize) & is.na(mPos ) ) {
          message(crayon::blue(paste0("\nyou let square marks without size nor pos. in holocen. Will use 0 as size\nand pos. maybe you want to add column chrRegion with w for whole chr. mark" ) )
          )
          listMarkPosSq[[sm]][m,"markPos"] <-0
          listMarkPosSq[[sm]][m,"markSize"]<-0
        }

        yinf <- chrStart +                        # was ysup
          (listMarkPosSq[[sm]][m,"markPos"]   *normalizeToOne)

        ysup <- chrStart +
          (  sum(listMarkPosSq[[sm]][m,"markPos"],listMarkPosSq[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

        yMark1[[m]]<-c(ysup,yinf,yinf,ysup)
        attr(yMark1[[m]],"rowIndex")  <-rowIndex
        attr(yMark1[[m]],"wholeArm")  <-'false' # this is to plot as mark not as arm see monocen.
        attr(yMark1[[m]],"squareSide")<-squareSide

        xMark1[[m]]<-xm[[corr_index]][listMarkPosSq[[sm]][,"neworder"][m],]
        attr(xMark1[[m]],"rowIndex")<-rowIndex

      }
      yMarkSq[[sm]]<-yMark1
      attr(yMarkSq[[sm]], "spname")<-names(listMarkPosSq)[[sm]]
      xMarkSq[[sm]]<-xMark1
      attr(xMarkSq[[sm]], "spname")<-names(listMarkPosSq)[[sm]]
    } # end for

    #####################
    #   add sq marks to plot holocen
    #####################


chromatidsCheck_1<- chromatids==FALSE | holocenNotAsChromatids
chromatidsCheck_2<- chromatids & holocenNotAsChromatids==FALSE

plotSqMarks(
  chromatidsCheck_1
  ,chromatidsCheck_2
  ,circularPlot
  ,squareness
  ,xMarkSq
  ,yMarkSq
  ,dfMarkColorInt
  ,lwd.marks2
  ,listMarkPosSq
  ,listChrSize
  ,specialChrWidth
  ,chrWidth
  ,yfactor
  ,markN
  ,ptsl
  ,bannedMarkName3
  ,y
  ,markLabelSize
  ,pattern
  ,separFactor
  ,labelSpacing
  ,circleCenter
  ,circleCenterY
  ,radius
  ,legend
  ,ylistTransChrSimple
  ,rotation
  ,labelOutwards
  ,x
  ,xModifierHolo
  ,pts)

} else {remove(listMarkPosSq)}
  #
  #   inline legend holocen
  #

  booleanForsquareInlineLabel<- legend=="inline" & exists("dfMarkColorInt") & exists("listMarkPosSq") & circularPlot==FALSE

  if(booleanForsquareInlineLabel) {
    textLabel(xMarkSq,yMarkSq,listChrSize,listMarkPosSq,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3
              ,markNewLine2=markNewLine,mylheight2=mylheight)
  }

} #   if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") )

# end square marks  #

#####################
#   marks CENTROMERE
#####################

#####################################
#   centromere with marks - plot cen marks
#####################################

if (exists("pLiMaPosDataCen") & exists("dfMarkColorInt") ) {

  xMarkCen<-yMarkCen<-list()

  listMarkPosCen<-pLiMaPosDataCen

  for (k in 1:length(listMarkPosCen)) {

    yMarkCen1  <- xMarkCen1 <- NULL
    corr_index <- which(names(xcoordCents) %in% names(listMarkPosCen)[[k]] )

    for (i in 1:nrow(listMarkPosCen[[k]])){

      rowIndex  <- listMarkPosCen[[k]][,"neworder"][i]
      chrRegion <- listMarkPosCen[[k]][,"chrRegion"][i]

      ysup <- max(ycoordCents[[corr_index]][rowIndex,] )
      yinf <- min(ycoordCents[[corr_index]][rowIndex,] )

      ymiddle <- (abs(ysup-yinf)/2) + yinf

      start <- ifelse(chrRegion %in% c("cen","qcen"),1,4)
      end   <- ifelse(chrRegion %in% c("cen","pcen"),6,3)

      yMarkCen1[[i]] <-c(yinf,yinf,ymiddle,ysup,ysup,ymiddle)

      yMarkCen1[[i]] <- yMarkCen1[[i]][start:end]

      attr(yMarkCen1[[i]],"rowIndex")  <- rowIndex
      attr(yMarkCen1[[i]],"chrRegion") <- chrRegion

      xMarkCen1[[i]] <- xcoordCents[[corr_index]][ rowIndex ,]
      xMarkCen1[[i]] <- xMarkCen1[[i]][start:end]

      attr(xMarkCen1[[i]],"rowIndex")  <- rowIndex
      attr(xMarkCen1[[i]],"chrRegion") <- chrRegion

    } # each mark

    yMarkCen[[k]]<-yMarkCen1
    xMarkCen[[k]]<-xMarkCen1

    attr(xMarkCen[[k]], "spname") <- attr(yMarkCen[[k]], "spname") <- names(listMarkPosCen)[[k]]
  } # each df


  if(!is.null(fixCenBorder)) {
    if (fixCenBorder){
      fixCenBorder2<-TRUE
    } else {
      fixCenBorder2<-FALSE
    }
  } else {
    fixCenBorder2<-FALSE
  }

  if(cenFormat=="rounded") {

    newLongyCenMarks <- newLongxCenMarks <- list()

    for (s in 1:length(yMarkCen) ) {

      xyCoordsCenMarks <- mapXYCen(1 , (length(yMarkCen[[s]]) ) ,
                                   yMarkCen[[s]],
                                   xMarkCen[[s]] ,
                                   ptsl[[1]],ptsl[[2]],ptsl[[3]],ptsl[[4]]
      )

      newLongxCenMarks[[s]] <- xyCoordsCenMarks$roundedX
      newLongyCenMarks[[s]] <- xyCoordsCenMarks$roundedY

      attr(newLongyCenMarks[[s]], "spname")<- attr(yMarkCen[[s]], "spname")
      attr(newLongxCenMarks[[s]], "spname")<- attr(xMarkCen[[s]], "spname")
    } # for s

  } # rC

  ###########################################
  #
  #   plotting labels inline CENTR. marks
  #
  ###########################################

  # cen labels
  booleanColorIntMarkCen<- legend=="inline" & exists("dfMarkColorInt") & exists("listMarkPosCen") & circularPlot==FALSE

  if(booleanColorIntMarkCen)  {
    textLabelCen(xMarkCen,yMarkCen,listChrSize,listMarkPosCen,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3)
  } # if

  if(circularPlot) {

    if (cenFormat=="triangle") {

      xlistNew        <- xHortoVerRoundCen(xMarkCen,diffXRounded)
      names(xlistNew) <- names(xMarkCen)

      yMarkPer        <- markMapPer(yMarkCen,y)

      ylistTransMark  <- transyListMark(yMarkPer,ylistTransChrSimple)

    } else if (cenFormat=="rounded") {

      xlistNew        <- xHortoVerRoundCen(newLongxCenMarks,diffXRounded)
      names(xlistNew) <- names(newLongxCenMarks)

      yMarkPer        <- markMapPer(newLongyCenMarks,y)

      ylistTransMark  <- transyListMark(yMarkPer,ylistTransChrSimple)
    }

    circleMapsMarksCen  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMark,xlistNew,n,0,chrWidth,
                                          rotation=rotation)

    drawCenMarks(circleMapsMarksCen,dfMarkColorInt
                 ,listMarkPosCen,lwd.chr,fixCenBorder2,chrColor)


    if(legend=="inline"){
      circleMapsMarksLabelCen  <- applyMapCircle(radius,circleCenter,circleCenterY,separFactor,ylistTransMark,xlistNew,n,
                                                 labelSpacing ,chrWidth,rotation=rotation)
      circLabelMark(bannedMarkName3,circleMapsMarksLabelCen,listMarkPosCen, markLabelSize,pattern,labelOutwards,circleCenter,circleCenterY)
    }

  } else { # non circular

    lwd.chrMod <- ifelse( gishCenBorder & cSbool ,
                          lwd.chr*hideCenLines,
                          lwd.chr)

    if (cenFormat=="triangle") {

      for (m in 1:length(xMarkCen)) {
          mapply(function(w,x,y,z) {
            graphics::polygon(
              x=x,
              y=y,
              col = w ,
              lwd=lwd.chrMod,
              border = ifelse(fixCenBorder2|gishCenBorder,
                              ifelse(fixCenBorder2,chrBorderColor2,w),
                              dfMarkColorInt$markBorderColor[match(z, dfMarkColorInt$markName)] # z outside
              ) # ifelse
            )},
            w=dfMarkColorInt$markColor[match(listMarkPosCen[[m]]$markName
                                             , dfMarkColorInt$markName)]
            ,x=xMarkCen[[m]]#[start:end]
            ,y=yMarkCen[[m]]#[start:end]

            ,z=listMarkPosCen[[m]]$markName # ifelse here gives error
          )
      }

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

      for (m in 1:length(newLongyCenMarks)) {
        mapply(function(w,x,y,z) graphics::polygon(
          x=x,
          y=y,
          col = w ,
          lwd=lwd.chrMod,
          border=ifelse(fixCenBorder2|gishCenBorder,
                        ifelse(fixCenBorder2,chrBorderColor2,w),
                        dfMarkColorInt$markBorderColor[match(z, dfMarkColorInt$markName)] # z outside
          ) # ifelse
        ), # p
        x=newLongxCenMarks[[m]],#[start:end]
        y=newLongyCenMarks[[m]],#[start:end]
        w=dfMarkColorInt$markColor[match(listMarkPosCen[[m]]$markName
                                         , dfMarkColorInt$markName)],
        z=listMarkPosCen[[m]]$markName # ifelse here gives error
        )
      }

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
#                             plotting monocen cenStyle marks
#
############################################################################################################

if (exists("pLiMaPosMonocen") & exists("dfMarkColorInt") ){

  yMarkLine<-xMarkRightLine<-xMarkLeftLine<-xMarkCenStyle<-yMarkCenStyle<-listMarkPosCenStyle<-list()

  j<-1

  for (k in 1:length(pLiMaPosMonocen)) {
    currName<-names(pLiMaPosMonocen)[[k]]
    if(nrow(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="cenStyle"),])>0){
      listMarkPosCenStyle <- c(listMarkPosCenStyle,list(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="cenStyle"),]))
      names(listMarkPosCenStyle)[[j]]<-currName
      j<-j+1
    }
  }
  if(length(listMarkPosCenStyle)>0) {

    for (sm in 1:length(listMarkPosCenStyle)) {
      yMark1<-yMark2<-xMark1<-xMark2Min<-xMark2Max<-NULL

      corr_index <- which(names(listChrSize) %in% names(listMarkPosCenStyle)[[sm]] )

      distCen <- attr(listChrSize[[corr_index]],"centromere")

      for (m in 1:nrow(listMarkPosCenStyle[[sm]]) ){
        ifelse(listMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
        ifelse(listMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
        ifelse(listMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
        rowIndex<- ( nrow(listChrSize[[corr_index]] ) * longORshort ) + listMarkPosCenStyle[[sm]][,"neworder"][m]
        armStart<-ym[[corr_index]][rowIndex,column]

        mSizeOrig <- listMarkPosCenStyle[[sm]][m,"markSize"]

        if(collapseCen) {
          markDistCen <- listMarkPosCenStyle[[sm]][m,"markDistCen"] - distCen/2
          if(markDistCen < 0) {
            mSizeMod <- mSizeOrig + markDistCen
            markDistCen <- 0
          } else {
            mSizeMod <- mSizeOrig
          }
        } else {
          markDistCen <- listMarkPosCenStyle[[sm]][m,"markDistCen"]
          mSizeMod <- mSizeOrig
        }

        yprox <- armStart + (markDistCen *normalizeToOne*mySign)

        yter <- armStart +  ( sum( markDistCen
                 , mSizeMod, na.rm=TRUE ) *normalizeToOne*mySign )

        ymiddle<- abs(yprox-yter)/2

        yMark1[[m]]<-if(longORshort==0) {
          c(yprox,yter,(yter+ymiddle),yprox,yter,(yter+ymiddle))
        } else {
          c(yter,yprox,(yprox+ymiddle),yter,yprox,(yprox+ymiddle))
        }

        attr(yMark1[[m]],"arm")<-listMarkPosCenStyle[[sm]][m,"chrRegion"]
        attr(yMark1[[m]],"rowIndex")<-rowIndex

        yMark2[[m]]<-if(longORshort==0) {c(yprox,yter)} else {c(yter,yprox)}
        attr(yMark2[[m]],"arm")<-listMarkPosCenStyle[[sm]][m,"chrRegion"]
        attr(yMark2[[m]],"rowIndex")<-rowIndex

        xMark1[[m]]<- xm[[corr_index]][ listMarkPosCenStyle[[sm]][,"neworder"][m],]

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
      attr(yMarkCenStyle[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]

      yMarkLine[[sm]]<- yMark2
      attr(yMarkLine[[sm]], "spname")    <-names(listMarkPosCenStyle)[[sm]]

      ##
      xMarkCenStyle[[sm]]<-xMark1
      attr(xMarkCenStyle[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]

      xMarkRightLine[[sm]]<-xMark2Max
      xMarkLeftLine[[sm]] <-xMark2Min

      attr(xMarkLeftLine[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]
      attr(xMarkRightLine[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]

      ##

    } # end for

    if(cenFormat=="rounded") {

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

    if(cenFormat=="triangle") {
      mimicCenPlotMark(squareness, xMarkCenStyle, yMarkCenStyle,
                       defCenStyleCol,
                       listMarkPosCenStyle,
                       chrWidth, #use for calc r2
                       specialChrWidth,
                       yfactor,
                       n,
                       lwd.mimicCen2,
                       listChrSize,
                       circularPlot,
                       y,
                       markLabelSize,
                       separFactor,
                       labelSpacing,circleCenter,circleCenterY,radius,
                       ylistTransChrSimple,
                       rotation=rotation,labelOutwards,
                       yMarkLine,xMarkRightLine,xMarkLeftLine,
                       x
                       ,pts=pts2)
    } else if(cenFormat=="rounded") {
      mimicCenPlotMark(squareness, newLongxCenMarksStyle, newLongyCenMarksStyle,
                       defCenStyleCol,
                       listMarkPosCenStyle,
                       chrWidth, #use for calc r2
                       specialChrWidth,
                       yfactor,
                       n,
                       lwd.mimicCen2,
                       listChrSize,
                       circularPlot,
                       y,
                       markLabelSize,
                       separFactor,
                       labelSpacing,circleCenter,circleCenterY,radius,
                       ylistTransChrSimple,
                       rotation=rotation,labelOutwards,
                       yMarkLine,xMarkRightLine,xMarkLeftLine,
                       x
                       ,cenFormat
                       ,pts=pts2)
    } # rC

    ######## new ######################################## inside cenStyle

    xMarkCenStyleBody<-yMarkCenStyleBody<-list()

      for (sm in 1:length(listMarkPosCenStyle) ) {

        yMark1body <- NULL
        xMark1body <- NULL

        corr_index<-which(names(listChrSize) %in% names(listMarkPosCenStyle)[[sm]] )

        distCen <- attr(listChrSize[[corr_index]],"centromere")

        for (m in 1:nrow(listMarkPosCenStyle[[sm]]) ){

          ifelse(listMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
          ifelse(listMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
          ifelse(listMarkPosCenStyle[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
          rowIndex<- ( nrow(listChrSize[[corr_index]] ) * longORshort ) + listMarkPosCenStyle[[sm]][,"neworder"][m]

          armStart<-ym[[corr_index]][rowIndex,column]

          mSizeOrig<- listMarkPosCenStyle[[sm]][m,"markSize"]

          if(collapseCen) {
            markDistCen <- listMarkPosCenStyle[[sm]][m,"markDistCen"] - distCen/2
            if(markDistCen < 0) {
              mSizeMod <- mSizeOrig + markDistCen
              markDistCen <- 0
            } else {
              mSizeMod <- mSizeOrig
            }
          } else {
            markDistCen <- listMarkPosCenStyle[[sm]][m,"markDistCen"]
            mSizeMod <- mSizeOrig
          }

          yprox <- armStart + (markDistCen*normalizeToOne*mySign)

          yter <- armStart +
            ( sum( markDistCen , mSizeMod, na.rm=TRUE ) *normalizeToOne*mySign )

          ymiddle<- abs(yprox-yter)/2

          yMark1body[[m]]<-if(longORshort==0) {
            c(yter,yter,(yter+ymiddle),yprox,yprox,(yter+ymiddle)) # in long arm yter is smaller
          } else {
            c(yprox,yprox,(yprox+ymiddle),yter,yter,(yprox+ymiddle))
          }

          attr(yMark1body[[m]],"arm")<-listMarkPosCenStyle[[sm]][m,"chrRegion"]
          attr(yMark1body[[m]],"rowIndex")<-rowIndex
          #
          fourX<-xm[[corr_index]][listMarkPosCenStyle[[sm]][,"neworder"][m],]

          meanXM1<- (min(fourX ) + max(fourX) )/2

          xMark1body[[m]] <- rep(c(min(fourX),max(fourX),meanXM1),2)

          attr(xMark1body[[m]],"rowIndex")<-rowIndex

        } # each mark

        yMarkCenStyleBody[[sm]]<-yMark1body
        attr(yMarkCenStyleBody[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]

        xMarkCenStyleBody[[sm]]<-xMark1body
        attr(xMarkCenStyleBody[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]

      } # for each df

      if (cenFormat=="rounded") {

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
                             dfMarkColorInt,
                             listMarkPosCenStyle,
                             chrWidth, #use for calc r2
                             specialChrWidth,
                             yfactor,
                             n,
                             lwd.mimicCen2,
                             listChrSize,
                             circularPlot,
                             y,
                             markLabelSize,
                             separFactor,
                             labelSpacing,circleCenter,circleCenterY,radius,
                             ylistTransChrSimple,
                             rotation=rotation,labelOutwards,
                             yMarkLine,xMarkRightLine,xMarkLeftLine,
                             x
                             ,lwd.chr
                             ,legend
                             ,pts=pts2)
    } else if(cenFormat=="rounded"){
      mimicCenPlotMarkInside(pattern,bannedMarkName3,squareness
                             ,newLongxCenMarksBodyStyle, newLongyCenMarksBodyStyle,
                             defCenStyleCol,
                             dfMarkColorInt,
                             listMarkPosCenStyle,
                             chrWidth, #use for calc r2
                             specialChrWidth,
                             yfactor,
                             n,
                             lwd.mimicCen2,
                             listChrSize,
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
                             ,cenFormat
                             ,pts=pts2)
    } # rC

  } else { remove(listMarkPosCenStyle) } #     if(length(listMarkPosCenStyle)>0)



      ###########################################
      #
      #   plotting labels inline mimicCENTR. monocen
      #
      ###########################################

      # cen labels
      booleanColorIntMimicMarkCen<- legend=="inline" & exists("dfMarkColorInt") &
  exists("listMarkPosCenStyle") & circularPlot==FALSE

      if(booleanColorIntMimicMarkCen)  {
        textLabelCen(xMarkCenStyleBody,yMarkCenStyleBody,listChrSize,listMarkPosCenStyle,specialChrSpacing,chrSpacing,markLabelSize,pattern,
                     bannedMarkName3)
      } # if
} # if exists monocen end  marks

##################################################################################################
#
#                                        painting Marks cenStyle holocen
#
##################################################################################################

if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") ) {
# message(crayon::green(paste0("holocen. marks section start" ) ) )
yMarkLine<-xMarkRightLine<-xMarkLeftLine<-xMarkCenStyle<-yMarkCenStyle<-listMarkPosCenStyle<-list()

j<-1
for (k in 1:length(pLiMaPosHolocen)) {
  currName<-names(pLiMaPosHolocen)[[k]]
  if(nrow(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="cenStyle"),])>0){
    listMarkPosCenStyle<-c(listMarkPosCenStyle,list(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="cenStyle"),]) )
    names(listMarkPosCenStyle)[[j]]<-currName
    j<-j+1
  }
}

if(length(listMarkPosCenStyle)>0){
  for (sm in 1:length(listMarkPosCenStyle)) {

    yMark1<-yMark2<-NULL
    xMark1<-xMark2Min<-xMark2Max<-NULL

    corr_index<-which(names(ym) %in% names(listMarkPosCenStyle)[[sm]] )

    for (m in 1:nrow(listMarkPosCenStyle[[sm]])){
      rowIndex<-(listMarkPosCenStyle[[sm]][,"neworder"][m])
      chrStart<-ym[[corr_index]][rowIndex ,2]
      yinf <- chrStart +                        # was ysup
        (listMarkPosCenStyle[[sm]][m,"markPos"]          *normalizeToOne)

      ysup <- chrStart +
        (  sum(listMarkPosCenStyle[[sm]][m,"markPos"],listMarkPosCenStyle[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

      ymiddle<- (yinf+ysup)/2

      yMark1[[m]]<-c(ysup,yinf,ymiddle,ysup,yinf,ymiddle)

      yMark2[[m]]<-c(yinf,ysup)

      attr(yMark1[[m]],"rowIndex")<-rowIndex
      attr(yMark2[[m]],"rowIndex")<-rowIndex

      fourX<-      xm[[corr_index]][listMarkPosCenStyle[[sm]][,"neworder"][m],]

      meanXM1<- (min(fourX ) + max(fourX) )/2

      xMark1[[m]] <- c(max(fourX),max(fourX),meanXM1, min(fourX),min(fourX),meanXM1 )

      xMark2Min[[m]]<-rep(min(xMark1[[m]]),2)
      xMark2Max[[m]]<-rep(max(xMark1[[m]]),2)

      attr(xMark1[[m]],"rowIndex")<-rowIndex
      attr(xMark2Min[[m]],"rowIndex")<-rowIndex
      attr(xMark2Max[[m]],"rowIndex")<-rowIndex

    }

    yMarkCenStyle[[sm]]<-yMark1
    attr(yMarkCenStyle[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]

    yMarkLine[[sm]]<- yMark2
    attr(yMarkLine[[sm]], "spname")    <-names(listMarkPosCenStyle)[[sm]]

    xMarkCenStyle[[sm]]<-xMark1
    xMarkRightLine[[sm]]<-xMark2Max
    xMarkLeftLine[[sm]] <-xMark2Min
    attr(xMarkCenStyle[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]
    attr(xMarkLeftLine[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]
    attr(xMarkRightLine[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]

  } # end for

  if(cenFormat=="rounded") {

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
                     listMarkPosCenStyle,
                     chrWidth, #use for calc r2
                     specialChrWidth,
                     yfactor,
                     n,
                     lwd.mimicCen2,
                     listChrSize,
                     circularPlot,
                     y,
                     markLabelSize,
                     separFactor,
                     labelSpacing,circleCenter,circleCenterY,radius,
                     ylistTransChrSimple,
                     rotation=rotation,labelOutwards,
                     yMarkLine,xMarkRightLine,xMarkLeftLine,
                     x
                     ,cenFormat
                     ,pts=pts2) #

  } else if(cenFormat=="triangle") {
    mimicCenPlotMark(squareness, xMarkCenStyle, yMarkCenStyle,
                     defCenStyleCol,
                     listMarkPosCenStyle,
                     chrWidth, #use for calc r2
                     specialChrWidth,
                     yfactor,
                     n,
                     lwd.mimicCen2,
                     listChrSize,
                     circularPlot,
                     y,
                     markLabelSize,
                     separFactor,
                     labelSpacing,circleCenter,circleCenterY,radius,
                     ylistTransChrSimple,
                     rotation=rotation,labelOutwards,
                     yMarkLine,xMarkRightLine,xMarkLeftLine,
                     x
                     ,pts=pts2) #
  }

  ######## new ######################################## inside cenStyle holocen

  xMarkCenStyleBody <- yMarkCenStyleBody<-list()

    for (sm in 1:length(listMarkPosCenStyle) ) {

      yMark1body <- NULL
      xMark1body <- NULL

      corr_index<-which(names(listChrSize) %in% names(listMarkPosCenStyle)[[sm]] )

      for (m in 1:nrow(listMarkPosCenStyle[[sm]]) ) {
        rowIndex<-(listMarkPosCenStyle[[sm]][,"neworder"][m])
        chrStart<-ym[[corr_index]][rowIndex ,2]

        yinf <- chrStart +                        # was ysup
          (listMarkPosCenStyle[[sm]][m,"markPos"]                                                          *normalizeToOne)

        ysup <- chrStart +
          (  sum(listMarkPosCenStyle[[sm]][m,"markPos"],listMarkPosCenStyle[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

        ymiddle<-(ysup+yinf)/2

        yMark1body[[m]]<-c(yinf,yinf,ymiddle,ysup,ysup,ymiddle)

        attr(yMark1body[[m]],"rowIndex")<-rowIndex

        fourX <-     xm[[corr_index]][listMarkPosCenStyle[[sm]][,"neworder"][m],]

        meanXM1<- (min(fourX ) + max(fourX) )/2

        # xMark1body[[m]] <- rep(c(min(fourX),max(fourX)),2)
        xMark1body[[m]] <- rep(c(min(fourX),max(fourX),meanXM1),2)


        attr(xMark1body[[m]],"rowIndex")<-rowIndex
      } # each mark

      yMarkCenStyleBody[[sm]]<-yMark1body
      attr(yMarkCenStyleBody[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]

      xMarkCenStyleBody[[sm]]<-xMark1body
      attr(xMarkCenStyleBody[[sm]], "spname")<-names(listMarkPosCenStyle)[[sm]]

    } # for each df

    if(cenFormat=="rounded") {

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

  if(cenFormat=="triangle") {
    mimicCenPlotMarkInside(pattern,bannedMarkName3,squareness, xMarkCenStyleBody, yMarkCenStyleBody ,
                           defCenStyleCol,
                           dfMarkColorInt,
                           listMarkPosCenStyle,
                           chrWidth, #use for calc r2
                           specialChrWidth,
                           yfactor,
                           n,
                           lwd.mimicCen2,
                           listChrSize,
                           circularPlot,
                           y,
                           markLabelSize,
                           separFactor,
                           labelSpacing,circleCenter,circleCenterY,radius,
                           ylistTransChrSimple,
                           rotation=rotation,labelOutwards,
                           yMarkLine,xMarkRightLine,xMarkLeftLine,
                           x,lwd.chr,legend
                           ,pts=pts2)
  } else if(cenFormat=="rounded") {

    mimicCenPlotMarkInside(pattern,bannedMarkName3,squareness, newLongxCenMarksBodyStyle, newLongyCenMarksBodyStyle,
                           defCenStyleCol,
                           dfMarkColorInt,
                           listMarkPosCenStyle,
                           chrWidth, #use for calc r2
                           specialChrWidth,
                           yfactor,
                           n,
                           lwd.mimicCen2,
                           listChrSize,
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
                           ,cenFormat
                           ,pts=pts2)
  } # rC

  ########

} else {  # if(length(listMarkPosCenStyle)>0){
  remove(listMarkPosCenStyle)
}

  ###########################################
  #
  #   plotting labels inline mimicCENTR. holocen
  #
  ###########################################

  # cen labels
  booleanColorIntMimicMarkCen<- legend=="inline" & exists("dfMarkColorInt") &
  exists("listMarkPosCenStyle") & circularPlot==FALSE

  if(booleanColorIntMimicMarkCen)  {
    textLabelCen(xMarkCenStyleBody,yMarkCenStyleBody,listChrSize,listMarkPosCenStyle,
              specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3)
  } # if

} #   if (exists("pLiMaPosHolocen") & ("dfMarkColorInt") )
# end cenStyle marks

##########################################################################################################3
#
#                           painting Marks monocen  ex protein marks
#
############################################################################################################

if (exists("pLiMaPosMonocen") & exists("dfMarkColorInt") ) {
    xMarkExProt_2nd<-yMarkExProt<-xMarkExProt<-listMarkPosExProt<-list()

        j<-1

        for (k in 1:length(pLiMaPosMonocen)) {
          currName<-names(pLiMaPosMonocen)[[k]]
          if(nrow(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="exProtein"),])>0){
            listMarkPosExProt<-c(listMarkPosExProt,list(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="exProtein"),]))
            names(listMarkPosExProt)[[j]]<-currName
            j<-j+1
          }
        }

        if(length(listMarkPosExProt)>0){
          for (sm in 1:length(listMarkPosExProt)) {
            yMark1<-xMark1<-xMark1_2nd<-NULL

            corr_index<-which(names(listChrSize) %in% names(listMarkPosExProt)[[sm]] )

            distCen <- attr(listChrSize[[corr_index]],"centromere")

            for (m in 1:nrow(listMarkPosExProt[[sm]]) ){
              ifelse(listMarkPosExProt[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1)
              squareSide<- "exProtein"
              ifelse(listMarkPosExProt[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
              ifelse(listMarkPosExProt[[sm]][m,"chrRegion"]=="q",mySign<- -1, mySign<- 1)
              rowIndex<- nrow(listChrSize[[corr_index]] ) * longORshort + (listMarkPosExProt[[sm]][,"neworder"][m])

              armStart<-ym[[corr_index]][rowIndex,column]

              mSizeOrig <- listMarkPosExProt[[sm]][m,"markSize"]

              if(collapseCen) {
                markDistCen <- listMarkPosExProt[[sm]][m,"markDistCen"] - distCen/2
                if(markDistCen < 0) {
                  mSizeMod <- mSizeOrig + markDistCen
                  markDistCen <- 0
                } else {
                  mSizeMod <- mSizeOrig
                }
              } else {
                markDistCen <- listMarkPosExProt[[sm]][m,"markDistCen"]
                mSizeMod <- mSizeOrig
              }

              yprox <- armStart + (markDistCen *normalizeToOne*mySign)

              yter <- armStart +
                ( sum( markDistCen, mSizeMod, na.rm=TRUE ) *normalizeToOne*mySign )

              # column produced by markDistCenGISHfix function

              if("markDistCenProtein" %in% colnames(listMarkPosExProt[[sm]] ) ) {

                if(!is.na(listMarkPosExProt[[sm]][m,"markDistCenProtein"] ) ) {

                  mSizeOrig <- listMarkPosExProt[[sm]][m,"markSizeProtein"]
                  r2 <- listMarkPosExProt[[sm]][m,"r2"]

                if(collapseCen) {
                  markDistCen <- listMarkPosExProt[[sm]][m,"markDistCenProtein"] - distCen/2
                  if(markDistCen < 0) {
                    mSizeMod <- mSizeOrig + markDistCen - r2
                    markDistCen <- 0 + r2
                  } else {
                    mSizeMod <- mSizeOrig
                  }
                } else {
                  markDistCen <- listMarkPosExProt[[sm]][m,"markDistCenProtein"]
                  mSizeMod <- mSizeOrig
                }

                  yprox <- armStart + (markDistCen * normalizeToOne*mySign)
                  yter  <- armStart +
                    ( sum( markDistCen, mSizeMod, na.rm=TRUE ) *normalizeToOne*mySign )
                }
              }

              if(longORshort==0) {
                ybase<-yter
                ysize<-abs(yprox-yter)
              } else {
                ybase<-yprox
                ysize<-abs(yter-yprox)
              }

              fourX <- xm[[corr_index]][listMarkPosExProt[[sm]][,"neworder"][m],]
              xsizeOrig <- xsize <-max(fourX)-min(fourX)

              if(longORshort==0) {
                # long arm
                yProtein<-c(0
                            , pMarkFac*3*(xsizeOrig*normalizeToOne)
                            ,ysize - (pMarkFac*3*(xsizeOrig*normalizeToOne) )
                            ,ysize
                            ,0
                )
                if(yProtein[3] < yProtein[2] ) {

                  yProtein[3] <- pMarkFac*3*(xsizeOrig*normalizeToOne)
                  yProtein[2] <- ysize - (pMarkFac*3*(xsizeOrig*normalizeToOne) )

                }
              } else {
                # short arm
                yProtein<-c(ysize
                            ,ysize - (pMarkFac*3*(xsizeOrig*normalizeToOne) )
                            , pMarkFac*3*(xsizeOrig*normalizeToOne)  # good!
                            ,0
                            ,ysize
                )
                if(yProtein[3] > yProtein[2] ) {

                  yProtein[2]<-pMarkFac*3*(xsizeOrig*normalizeToOne)
                  yProtein[3]<-ysize - (pMarkFac*3*(xsizeOrig*normalizeToOne) )

                }
              }

              yMark1[[m]]<- yProtein + ybase

              attr(yMark1[[m]],"arm") <-listMarkPosExProt[[sm]][m,"chrRegion"]
              attr(yMark1[[m]],"rowIndex")<-rowIndex
              attr(yMark1[[m]],"squareSide")<-squareSide

              #
              #   right side
              #

              xsize  <- xsize/2
              xWidth <- pMarkFac * xsizeOrig

              xPosWid <- startPos * xWidth
              xbase   <- min(fourX) + xsize + xPosWid

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

              xsize <- max(fourX)-min(fourX)

              xsize <- xsize/2
              # xsize<-xsize-xModifierMono

              #
              #   left side
              #

              xProtein <- c(0
                          , -xWidth
                          , -xWidth
                          ,0
                          ,0
              )

              xMark1_2nd[[m]] <- xbase + xProtein

              attr(xMark1_2nd[[m]],"rowIndex")<-rowIndex


            } # for each mark

            yMarkExProt[[sm]]<-yMark1
            attr(yMarkExProt[[sm]], "spname")<-names(listMarkPosExProt)[[sm]]

            xMarkExProt[[sm]]<-xMark1
            attr(xMarkExProt[[sm]], "spname")<-names(listMarkPosExProt)[[sm]]

            xMarkExProt_2nd[[sm]]<-xMark1_2nd
            attr(xMarkExProt_2nd[[sm]], "spname")<-names(listMarkPosExProt)[[sm]]

          } # end for

          ########################
          #                             #
          #   add marks to plot monocen #
          #                             #
          ########################

          arrowPlotMark(squareness, xMarkExProt, yMarkExProt,
                        dfMarkColorInt,
                        listMarkPosExProt,
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
          arrowPlotMark(squareness, xMarkExProt_2nd, yMarkExProt,
                        dfMarkColorInt,
                        listMarkPosExProt,
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
        } #     if(length(listMarkPosExProt)>0)

        else {
          remove(listMarkPosExProt)
        }

        booleanForProteinInlineLabel<- legend=="inline" & exists("dfMarkColorInt") & exists("listMarkPosExProt") &
          circularPlot==FALSE

        if(booleanForProteinInlineLabel) {
          textLabel(xMarkExProt,yMarkExProt,listChrSize,listMarkPosExProt
                    ,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3,
                    markNewLine2=markNewLine,mylheight2=mylheight)
        }

  } # if presence end painting marks

##################################################################################################
#
#                                   painting Marks ExProtein holocen
#
##################################################################################################

if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") ) {
        xMarkExProt_2nd<-xMarkExProt<-yMarkExProt<-listMarkPosExProt<-list()

        j<-1
        for (k in 1:length(pLiMaPosHolocen)) {
          currName<-names(pLiMaPosHolocen)[[k]]
          if(nrow(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="exProtein"),])>0){
            listMarkPosExProt<-c(listMarkPosExProt,list(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="exProtein"),]) )
            names(listMarkPosExProt)[[j]]<-currName
            j<-j+1
          }
        }

        if(length(listMarkPosExProt)>0){
          for (sm in 1:length(listMarkPosExProt)) {

            yMark1<-xMark1<-NULL
            xMark1_2nd<-NULL

            corr_index<-which(names(ym) %in% names(listMarkPosExProt)[[sm]] )

            for (m in 1:nrow(listMarkPosExProt[[sm]])) {
              squareSide<- "exProtein"
              rowIndex<-(listMarkPosExProt[[sm]][,"neworder"][m])
              chrStart<-ym[[corr_index]][rowIndex ,2]
              yinf <- chrStart +                        # was ysup
                (listMarkPosExProt[[sm]][m,"markPos"] *normalizeToOne)

              ysup <- chrStart +
                (  sum(listMarkPosExProt[[sm]][m,"markPos"]
                       ,listMarkPosExProt[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

              if("markPosProtein" %in% colnames(listMarkPosExProt[[sm]] ) ){
                if(!is.na(listMarkPosExProt[[sm]][m,"markPosProtein"] ) ) {
                  yinf <- chrStart +
                    (listMarkPosExProt[[sm]][m,"markPosProtein"]  *normalizeToOne)

                  ysup <- chrStart +
                    (  sum(listMarkPosExProt[[sm]][m,"markPosProtein"]
                           ,listMarkPosExProt[[sm]][m,"markSizeProtein"],na.rm=TRUE ) *normalizeToOne )
                }
              }

              ysize <- abs(ysup-yinf)

              fourX <- xm[[corr_index]][listMarkPosExProt[[sm]][,"neworder"][m],]

              xsizeOrig <- xsize <- max(fourX) - min(fourX)

              yProtein<-c(ysize
                          ,ysize - (pMarkFac*3*(xsizeOrig*normalizeToOne) )
                          ,pMarkFac*3*(xsizeOrig*normalizeToOne)
                          ,0
                          ,ysize
              )

              if(yProtein[3]>yProtein[2]) {
                yProtein[3] <- ysize - (pMarkFac*3*(xsizeOrig*normalizeToOne) )
                yProtein[2] <- pMarkFac*3*(xsizeOrig*normalizeToOne)
              }

              yMark1[[m]] <- yProtein + yinf

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
            attr(yMarkExProt[[sm]], "spname")<-names(listMarkPosExProt)[[sm]]
            xMarkExProt[[sm]]<-xMark1
            attr(xMarkExProt[[sm]], "spname")<-names(listMarkPosExProt)[[sm]]

              xMarkExProt_2nd[[sm]]<-xMark1_2nd
              attr(xMarkExProt_2nd[[sm]], "spname")<-names(listMarkPosExProt)[[sm]]

          } # end for

          #####################
          #   add ExProt marks to plot holocen
          #####################

          arrowPlotMark(squareness, xMarkExProt, yMarkExProt,
                        dfMarkColorInt,
                        listMarkPosExProt,
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

          arrowPlotMark(squareness, xMarkExProt_2nd, yMarkExProt,
                        dfMarkColorInt,
                        listMarkPosExProt,
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

        } #     if(length(listMarkPosExProt)>0){

        else {
          remove(listMarkPosExProt)
        }

        # square labels not centrom. (monocen.)
        booleanForProteinInlineLabel<- legend=="inline" & exists("dfMarkColorInt") & exists("listMarkPosExProt") & circularPlot==FALSE

        if(booleanForProteinInlineLabel) {
          textLabel(xMarkExProt,yMarkExProt,listChrSize,listMarkPosExProt
                    ,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3,
                    markNewLine2=markNewLine,mylheight2=mylheight)
        }

      } #   if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") )

      # end ExProteins


##############################################################################################
#
#                                inProtein marks monocen.
#
##############################################################################################

if (exists("pLiMaPosMonocen") & exists("dfMarkColorInt") ) {

  xMarkInProt_2nd<-yMarkInProt<-xMarkInProt<-listMarkPosInProt<-list()

  j<-1

  for (k in 1:length(pLiMaPosMonocen)) {
    currName<-names(pLiMaPosMonocen)[[k]]
    if(nrow(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="inProtein"),])>0){
      listMarkPosInProt<-c(listMarkPosInProt,list(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="inProtein"),]))
      names(listMarkPosInProt)[[j]]<-currName
      j<-j+1
    }
  }

  if(length(listMarkPosInProt)>0){

    for (sm in 1:length(listMarkPosInProt)) {

      yMark1<-xMark1<-xMark1_2nd<-NULL

      corr_index <- which(names(listChrSize) %in% names(listMarkPosInProt)[[sm]] )
      distCen    <- attr(listChrSize[[corr_index]],"centromere")

      for (m in 1:nrow(listMarkPosInProt[[sm]]) ){
        ifelse(listMarkPosInProt[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
        # longORshort<- 1

        squareSide<- "inProtein"
        ifelse(listMarkPosInProt[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
        # column<- 2
        ifelse(listMarkPosInProt[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
        # mySign<- 1

        rowIndex<- nrow(listChrSize[[corr_index]] ) * longORshort +
          (listMarkPosInProt[[sm]][,"neworder"][m])

        armStart  <- ym[[corr_index]][rowIndex,column]
        mSizeOrig <- listMarkPosInProt[[sm]][m,"markSize"]

        if(collapseCen) {
          markDistCen <- listMarkPosInProt[[sm]][m,"markDistCen"] - distCen/2
          if(markDistCen < 0) {
            mSizeMod <- mSizeOrig + markDistCen
            markDistCen  <- 0
          } else {
            mSizeMod <- mSizeOrig
          }
        } else {
          markDistCen <- listMarkPosInProt[[sm]][m,"markDistCen"]
          mSizeMod <- mSizeOrig
        }

        yprox <- armStart + (markDistCen*normalizeToOne*mySign)

        yter <- armStart +
          ( sum( markDistCen, mSizeMod, na.rm=TRUE ) *normalizeToOne*mySign )

        # column producen by function markDistCenGISHfix

        if("markDistCenProtein" %in% colnames(listMarkPosInProt[[sm]] ) ){
          if(!is.na(listMarkPosInProt[[sm]][m,"markDistCenProtein"] ) ) {

            mSizeOrig <- listMarkPosInProt[[sm]][m,"markSizeProtein"]
            r2 <- listMarkPosInProt[[sm]][m,"r2"]

            if(collapseCen) {
              markDistCen <- listMarkPosInProt[[sm]][m,"markDistCenProtein"] - distCen/2
              if(markDistCen < 0) {
                mSizeMod <- mSizeOrig + markDistCen - r2
                markDistCen <- 0 + r2
              } else {
                mSizeMod <- mSizeOrig
              }
            } else {
              markDistCen <- listMarkPosInProt[[sm]][m,"markDistCenProtein"]
              mSizeMod <- mSizeOrig
            }

            yprox <- armStart + (markDistCen * normalizeToOne*mySign)
            yter <- armStart +
              ( sum(markDistCen, mSizeMod, na.rm=TRUE ) *normalizeToOne*mySign )
          }
        }

        if(longORshort==0) {
          ybase<-yter
          ysize<-abs(yprox-yter)
        } else {
          ybase<-yprox
          ysize<-abs(yter-yprox)
        }

        fourX <- xm[[corr_index]][listMarkPosInProt[[sm]][,"neworder"][m],]
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

        pqcen <- attr(yMark1[[m]],"arm")       <- listMarkPosInProt[[sm]][m,"chrRegion"]
        markT <- attr(yMark1[[m]],"squareSide")<- squareSide
        attr(yMark1[[m]],"rowIndex")           <- rowIndex

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

        if(pqcen == "pcen" & markT == "inProtein"){
          m1 <- mean(yMark1[[m]][2:3])
          yMark1[[m]][3:4]<-m1
        } else if (pqcen == "qcen" & markT == "inProtein") {
          m1 <- mean(yMark1[[m]][2:3])
          yMark1[[m]][c(1,2,5)]<-m1
        }
      } # for each mark

      yMarkInProt[[sm]]<-yMark1
      attr(yMarkInProt[[sm]], "spname")<-names(listMarkPosInProt)[[sm]]

      xMarkInProt[[sm]]<-xMark1
      attr(xMarkInProt[[sm]], "spname")<-names(listMarkPosInProt)[[sm]]

      xMarkInProt_2nd[[sm]]<-xMark1_2nd
      attr(xMarkInProt_2nd[[sm]], "spname") <- names(listMarkPosInProt)[[sm]]

    } # end for sm


    ########################
    #                                       #
    #   add inProtein marks to plot monocen.#
    #                                       #
    ########################
    arrowPlotMark(squareness, xMarkInProt, yMarkInProt,
                  dfMarkColorInt,
                  listMarkPosInProt,
                  chrWidth, #use for calc r2
                  markN,
                  lwd.marks2,#lwd.chr,
                  circularPlot,
                  y,
                  x,
                  markLabelSize,
                  separFactor,
                  labelSpacing,circleCenter,circleCenterY,radius,
                  ylistTransChrSimple
                  ,rotation=rotation,
                  arrowheadWidthShrink)

    # plot second Protein when chrt
    arrowPlotMark(squareness, xMarkInProt_2nd, yMarkInProt,
                  dfMarkColorInt,
                  listMarkPosInProt,
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


  } #     if(length(listMarkPosInProt)>0)

  else {
    remove(listMarkPosInProt)
  }

  booleanForProteinInlineLabel<- legend=="inline" & exists("dfMarkColorInt") & exists("listMarkPosInProt") & circularPlot==FALSE

  if(booleanForProteinInlineLabel) {
    textLabel(xMarkInProt,yMarkInProt,listChrSize,listMarkPosInProt
              ,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3,
              markNewLine2=markNewLine,mylheight2=mylheight,xsize=xsize)
  }

} # if presence end painting marks

################################################################################################
#                                       inProtein holocen
################################################################################################

if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") ) {
  # message(crayon::green(paste0("holocen. marks section start" ) ) )
  xMarkInProt_2nd <-xMarkInProt<-yMarkInProt<-listMarkPosInProt<-list()

  j<-1
  for (k in 1:length(pLiMaPosHolocen)) {
    currName<-names(pLiMaPosHolocen)[[k]]
    if(nrow(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="inProtein"),]) > 0 ){
      listMarkPosInProt<-c(listMarkPosInProt,list(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="inProtein"),]) )
      names(listMarkPosInProt)[[j]]<-currName
      j<-j+1
    }
  }
  if(length(listMarkPosInProt)>0){
    for (sm in 1:length(listMarkPosInProt)) {

      xMark1_2nd<-yMark1<-xMark1<-NULL

      corr_index<-which(names(ym) %in% names(listMarkPosInProt)[[sm]] )

      for (m in 1:nrow(listMarkPosInProt[[sm]])) {
        squareSide<- "inProtein"
        rowIndex<-(listMarkPosInProt[[sm]][,"neworder"][m])
        chrStart<-ym[[corr_index]][rowIndex ,2]

        yinf <- chrStart +
          (listMarkPosInProt[[sm]][m,"markPos"] *normalizeToOne)

        ysup <- chrStart +
          (  sum(listMarkPosInProt[[sm]][m,"markPos"]
                 ,listMarkPosInProt[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

        if("markPosProtein" %in% colnames(listMarkPosInProt[[sm]] ) ){
          if(!is.na(listMarkPosInProt[[sm]][m,"markPosProtein"] ) ) {
            yinf <- chrStart +
              (listMarkPosInProt[[sm]][m,"markPosProtein"]  *normalizeToOne)

            ysup <- chrStart +
              (  sum(listMarkPosInProt[[sm]][m,"markPosProtein"]
                     ,listMarkPosInProt[[sm]][m,"markSizeProtein"],na.rm=TRUE ) *normalizeToOne )
          }
        }

        ysize <- abs(ysup-yinf)

        fourX <- xm[[corr_index]][listMarkPosInProt[[sm]][,"neworder"][m],]

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

        #
        #   right side
        #

        xsize <- xsize/2
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
      attr(yMarkInProt[[sm]], "spname")<-names(listMarkPosInProt)[[sm]]
      xMarkInProt[[sm]]<-xMark1
      attr(xMarkInProt[[sm]], "spname")<-names(listMarkPosInProt)[[sm]]

      # if(chromatids & ProteinsToSide & ProteinsBothChrt & circularPlot==FALSE & excHoloFrArrToSide==FALSE ) {
      xMarkInProt_2nd[[sm]]<-xMark1_2nd
      attr(xMarkInProt_2nd[[sm]], "spname")<-names(listMarkPosInProt)[[sm]]
      # }

    } # end for

    #####################
    #   add InProt marks to plot holocen
    #####################

    arrowPlotMark(squareness, xMarkInProt, yMarkInProt,
                  dfMarkColorInt,
                  listMarkPosInProt,
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

    arrowPlotMark(squareness, xMarkInProt_2nd, yMarkInProt,
                  dfMarkColorInt,
                  listMarkPosInProt,
                  chrWidth,
                  markN,
                  lwd.marks2,
                  circularPlot,
                  y,
                  x,
                  markLabelSize,
                  separFactor,
                  labelSpacing,circleCenter,circleCenterY,radius,
                  ylistTransChrSimple,rotation=rotation,
                  arrowheadWidthShrink) #

  } #     if(length(listMarkPosInProt)>0){

  else {
    remove(listMarkPosInProt)
  }

  # square labels not centrom. (monocen.)
  booleanForProteinInlineLabel<- legend=="inline" & exists("dfMarkColorInt") & exists("listMarkPosInProt") &
    circularPlot==FALSE

  if(booleanForProteinInlineLabel) {
    textLabel(xMarkInProt,yMarkInProt,listChrSize,listMarkPosInProt
              ,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3,
              markNewLine2=markNewLine,mylheight2=mylheight,xsize=xsize)
  }

} #   if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") )

## end infProtein

##########################################################################################################3
#
#                           painting Marks monocen    upArrow marks
#
############################################################################################################

arrowhead2 <- 1-arrowhead

if (exists("pLiMaPosMonocen") & exists("dfMarkColorInt") ) {
  yMarkUpAr<-xMarkUpAr<-listMarkPosUpAr<-list()
  xMarkUpAr_2nd<-list()

  j<-1

  for (k in 1:length(pLiMaPosMonocen)) {
    currName<-names(pLiMaPosMonocen)[[k]]
    if(nrow(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="upArrow"),])>0){
      listMarkPosUpAr<-c(listMarkPosUpAr,list(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="upArrow"),]))
      names(listMarkPosUpAr)[[j]]<-currName
      j<-j+1
    }
  }

  if(length(listMarkPosUpAr)>0){
    for (sm in 1:length(listMarkPosUpAr)) {
      yMark1<-NULL
      xMark1<-NULL
      xMark1_2nd<-NULL

      corr_index<-which(names(listChrSize) %in% names(listMarkPosUpAr)[[sm]] )
      distCen <- attr(listChrSize[[corr_index]],"centromere")

      for (m in 1:nrow(listMarkPosUpAr[[sm]]) ){
        ifelse(listMarkPosUpAr[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
        ifelse(listMarkPosUpAr[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
        ifelse(listMarkPosUpAr[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)
        rowIndex <- nrow(listChrSize[[corr_index]] ) * longORshort + (listMarkPosUpAr[[sm]][,"neworder"][m])
        armStart <- ym[[corr_index]][rowIndex,column]

        mSizeOrig<- listMarkPosUpAr[[sm]][m,"markSize"]

        if(collapseCen) {
          markDistCen <- listMarkPosUpAr[[sm]][m,"markDistCen"] - distCen/2
          if(markDistCen < 0) {
            mSizeMod <- mSizeOrig + markDistCen
            markDistCen <- 0
          } else {
            mSizeMod <- mSizeOrig
          }
        } else {
          markDistCen <- listMarkPosUpAr[[sm]][m,"markDistCen"]
          mSizeMod <- mSizeOrig
        }

        yprox <- armStart +   (markDistCen *normalizeToOne*mySign)

        yter <- armStart +
          ( sum( markDistCen, mSizeMod, na.rm=TRUE ) *normalizeToOne*mySign )

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

        attr(yMark1[[m]],"arm")<-listMarkPosUpAr[[sm]][m,"chrRegion"]
        attr(yMark1[[m]],"rowIndex")<-rowIndex

        fourX <- xm[[corr_index]][listMarkPosUpAr[[sm]][,"neworder"][m],]
        xsize<-max(fourX)-min(fourX)
        xbase<-min(fourX)

        if(arrowsToSide & chromatids==FALSE) {
          xsize <- xsize/2
          xbase <- xbase + xsize
        } else if (chromatids){
          xsize <- xsize/2
          xbase <- xbase + xsize
          xsize <- xsize - xModifierMono
          xbase <- xbase + xModifierMono
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

        if(chromatids & arrowsBothChrt){
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
      attr(yMarkUpAr[[sm]], "spname")<-names(listMarkPosUpAr)[[sm]]
      xMarkUpAr[[sm]]<-xMark1
      attr(xMarkUpAr[[sm]], "spname")<-names(listMarkPosUpAr)[[sm]]

      if(chromatids & arrowsBothChrt) {
        xMarkUpAr_2nd[[sm]]<-xMark1_2nd
        attr(xMarkUpAr_2nd[[sm]], "spname")<-names(listMarkPosUpAr)[[sm]]
      }

    } # end for

    ########################
    #                      #
    #   add marks to plot monocen #
    #                      #
    ########################
    # if(circularPlot==FALSE){
    arrowPlotMark(squareness, xMarkUpAr, yMarkUpAr,
                  dfMarkColorInt,
                  listMarkPosUpAr,
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
    if(chromatids & arrowsBothChrt) {
      arrowPlotMark(squareness, xMarkUpAr_2nd, yMarkUpAr,
                    dfMarkColorInt,
                    listMarkPosUpAr,
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


  } #     if(length(listMarkPosUpAr)>0)

  else {remove(listMarkPosUpAr)}

} # if presence end painting marks

##################################################################################################
#
#                                                 painting Marks upArrow holocen
#
##################################################################################################
if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") ) {
  # message(crayon::green(paste0("holocen. marks section start" ) ) )
  xMarkUpAr<-yMarkUpAr<-listMarkPosUpAr<-list()
  xMarkUpAr_2nd<- list()
  j<-1
  for (k in 1:length(pLiMaPosHolocen)) {
    currName<-names(pLiMaPosHolocen)[[k]]
    if(nrow(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="upArrow"),])>0){
      listMarkPosUpAr<-c(listMarkPosUpAr,list(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="upArrow"),]) )
      names(listMarkPosUpAr)[[j]]<-currName
      j<-j+1
    }
  }

  if(length(listMarkPosUpAr)>0){
    for (sm in 1:length(listMarkPosUpAr)) {

      yMark1<-xMark1<-NULL
      xMark1_2nd<-NULL

      corr_index<-which(names(ym) %in% names(listMarkPosUpAr)[[sm]] )

      for (m in 1:nrow(listMarkPosUpAr[[sm]])) {
        rowIndex<-(listMarkPosUpAr[[sm]][,"neworder"][m])
        chrStart<-ym[[corr_index]][rowIndex ,2]
        yinf <- chrStart +                        # was ysup
          (listMarkPosUpAr[[sm]][m,"markPos"]    *normalizeToOne)

        ysup <- chrStart +
          (  sum(listMarkPosUpAr[[sm]][m,"markPos"]
                 ,listMarkPosUpAr[[sm]][m,"markSize"],na.rm=TRUE ) * normalizeToOne )

        ysize <- abs(ysup-yinf)

        yArrow <- c(ysize,arrowhead2*ysize,
             arrowhead2*ysize,0,
             0,arrowhead2*ysize,
             arrowhead2*ysize,ysize
        )

        yMark1[[m]] <- yArrow + yinf

        attr(yMark1[[m]],"rowIndex")<-rowIndex

        fourX <- xm[[corr_index]][listMarkPosUpAr[[sm]][,"neworder"][m],]

        xsize <- max(fourX) - min(fourX)
        xbase <- min(fourX)

        if(arrowsToSide & c(chromatids==FALSE | holocenNotAsChromatids) & excHoloFrArrToSide==FALSE) {
          xsize <- xsize/2
          xbase <- xbase + xsize
        } else if (chromatids & holocenNotAsChromatids==FALSE){
          xsize <- xsize/2
          xbase <- xbase + xsize
          xsize <- xsize - xModifierHolo
          xbase <- xbase + xModifierHolo
        }

        xArrow<-c(.5*xsize, xsize-(arrowheadWidthShrink*xsize),
                  (1-shrinkArrow)*xsize,(1-shrinkArrow)*xsize,
             shrinkArrow*xsize,shrinkArrow*xsize,
             0+(arrowheadWidthShrink*xsize),.5*xsize
        )

        # +1 represents base (min)
        xMark1[[m]] <- xArrow + xbase
        attr(xMark1[[m]],"rowIndex")<-rowIndex

        if(chromatids & arrowsBothChrt & holocenNotAsChromatids==FALSE) {
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
      attr(yMarkUpAr[[sm]], "spname")<-names(listMarkPosUpAr)[[sm]]
      xMarkUpAr[[sm]]<-xMark1
      attr(xMarkUpAr[[sm]], "spname")<-names(listMarkPosUpAr)[[sm]]

      if(chromatids & arrowsBothChrt & holocenNotAsChromatids==FALSE ) {
        xMarkUpAr_2nd[[sm]]<-xMark1_2nd
        attr(xMarkUpAr_2nd[[sm]], "spname")<-names(listMarkPosUpAr)[[sm]]
      }

    } # end for

    #####################
    #   add UpAr marks to plot holocen
    #####################

    arrowPlotMark(squareness, xMarkUpAr, yMarkUpAr,
                  dfMarkColorInt,
                  listMarkPosUpAr,
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

    if(chromatids & arrowsBothChrt & holocenNotAsChromatids==FALSE) {
      arrowPlotMark(squareness, xMarkUpAr_2nd, yMarkUpAr,
                    dfMarkColorInt,
                    listMarkPosUpAr,
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

  } #     if(length(listMarkPosUpAr)>0){

  else {remove(listMarkPosUpAr)}

  #
  #   inline legend holocen
  #

} #   if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") )

    # end upArrow marks  #


    ## down arrows

    ##########################################################################################################3
    #
    #                           painting Marks monocen              downArrow marks
    #
    ############################################################################################################

    arrowhead2<-1-arrowhead
    if (exists("pLiMaPosMonocen") & exists("dfMarkColorInt") ){

      xMarkDwAr<-yMarkDwAr<-listMarkPosDwAr<-list()
      xMarkDwAr_2nd<-list()
      j<-1

      for (k in 1:length(pLiMaPosMonocen)) {
        currName<-names(pLiMaPosMonocen)[[k]]
        if(nrow(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="downArrow"),])>0){
          listMarkPosDwAr<-c(listMarkPosDwAr,list(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="downArrow"),]))
          names(listMarkPosDwAr)[[j]]<-currName
          j<-j+1
        }
      }

      if(length(listMarkPosDwAr)>0){
        for (sm in 1:length(listMarkPosDwAr)) {
          yMark1<-xMark1<-NULL
          yMark1_2nd<-xMark1_2nd<-NULL

          corr_index <- which(names(listChrSize) %in% names(listMarkPosDwAr)[[sm]] )
          distCen    <- attr(listChrSize[[corr_index]],"centromere")

          for (m in 1:nrow(listMarkPosDwAr[[sm]]) ){
            ifelse(listMarkPosDwAr[[sm]][m,"chrRegion"]=="q",longORshort<- 0,longORshort<- 1) # ifelseinloop
            ifelse(listMarkPosDwAr[[sm]][m,"chrRegion"]=="q",column<- 1, column<- 2)
            ifelse(listMarkPosDwAr[[sm]][m,"chrRegion"]=="q",mySign<- -1,mySign<- 1)

            rowIndex   <- nrow(listChrSize[[corr_index]] ) * longORshort + (listMarkPosDwAr[[sm]][,"neworder"][m])
            armStart   <- ym[[corr_index]][rowIndex,column]
            mSizeOrig  <- listMarkPosDwAr[[sm]][m,"markSize"]

              if(collapseCen) {
                markDistCen <- listMarkPosDwAr[[sm]][m,"markDistCen"] - distCen/2
                if(markDistCen < 0) {
                  mSizeMod <- mSizeOrig + markDistCen
                  markDistCen <- 0
                } else {
                  mSizeMod <- mSizeOrig
                }
              } else {
                markDistCen <- listMarkPosDwAr[[sm]][m,"markDistCen"]
                mSizeMod <- mSizeOrig
              }

            yprox <- armStart +     (markDistCen  *normalizeToOne*mySign)

            yter <- armStart +
              ( sum( markDistCen , mSizeMod, na.rm=TRUE ) *normalizeToOne*mySign )

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

            attr(yMark1[[m]],"arm")<-listMarkPosDwAr[[sm]][m,"chrRegion"]
            attr(yMark1[[m]],"rowIndex")<-rowIndex

            fourX <- xm[[corr_index]][listMarkPosDwAr[[sm]][,"neworder"][m],]

            xsize <- max(fourX)-min(fourX)

            xbase<-min(fourX)

            if(arrowsToSide & chromatids==FALSE) {
              xsize <- xsize/2
            } else if (chromatids){
              xsize <- xsize/2
              xsize <- xsize - xModifierMono
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

            if(chromatids & arrowsBothChrt) {
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
          attr(yMarkDwAr[[sm]], "spname")<-names(listMarkPosDwAr)[[sm]]
          xMarkDwAr[[sm]]<-xMark1
          attr(xMarkDwAr[[sm]], "spname")<-names(listMarkPosDwAr)[[sm]]

          if(chromatids & arrowsBothChrt) {
            xMarkDwAr_2nd[[sm]]<-xMark1_2nd
            attr(xMarkDwAr_2nd[[sm]], "spname")<-names(listMarkPosDwAr)[[sm]]
          }
        } # end for

        ###############################
        #                             #
        #   add marks to plot monocen #
        #                             #
        ###############################

        arrowPlotMark(squareness, xMarkDwAr, yMarkDwAr,
                      dfMarkColorInt,
                      listMarkPosDwAr,
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

        if(chromatids & arrowsBothChrt) {
          arrowPlotMark(squareness, xMarkDwAr_2nd, yMarkDwAr,
                        dfMarkColorInt,
                        listMarkPosDwAr,
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

      } #     if(length(listMarkPosDwAr)>0)

      else {remove(listMarkPosDwAr)}

    } # if presence end painting marks

    ##################################################################################################
    #
    #                                   painting Marks downArrow holocen
    #
    ##################################################################################################

    if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") ) {
      # message(crayon::green(paste0("holocen. marks section start" ) ) )
      xMarkDwAr<-yMarkDwAr<-listMarkPosDwAr<-list()
      xMarkDwAr_2nd<-list()

      j<-1
      for (k in 1:length(pLiMaPosHolocen)) {
        currName<-names(pLiMaPosHolocen)[[k]]
        if(nrow(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="downArrow"),])>0){
          listMarkPosDwAr<-c(listMarkPosDwAr,list(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="downArrow"),]) )
          names(listMarkPosDwAr)[[j]]<-currName
          j<-j+1
        }
      }

      if(length(listMarkPosDwAr)>0){
        for (sm in 1:length(listMarkPosDwAr)) {

          yMark1<-xMark1<-NULL

          corr_index<-which(names(ym) %in% names(listMarkPosDwAr)[[sm]] )

          for (m in 1:nrow(listMarkPosDwAr[[sm]])){
            rowIndex<-listMarkPosDwAr[[sm]][,"neworder"][m]
            chrStart<-ym[[corr_index]][rowIndex ,2]
            yinf <- chrStart +                        # was ysup
              (listMarkPosDwAr[[sm]][m,"markPos"]     *normalizeToOne)

            ysup <- chrStart +
              (  sum(listMarkPosDwAr[[sm]][m,"markPos"]
                     ,listMarkPosDwAr[[sm]][m,"markSize"],na.rm=TRUE ) *normalizeToOne )

            ysize <- abs(ysup-yinf)

            yArrow<-c(ysize,arrowhead*ysize,
                      arrowhead*ysize,0,
                      arrowhead*ysize,arrowhead*ysize,
                      ysize , ysize
            )

            yMark1[[m]] <- yArrow + yinf

            attr(yMark1[[m]],"rowIndex")<-rowIndex

            fourX <- xm[[corr_index]][listMarkPosDwAr[[sm]][,"neworder"][m],]

            xsize <- max(fourX) - min(fourX)
            xbase <- min(fourX)

            if(arrowsToSide & c(chromatids==FALSE | holocenNotAsChromatids) & excHoloFrArrToSide==FALSE) {
              xsize <- xsize/2
            } else if (chromatids & holocenNotAsChromatids==FALSE){
              xsize <- xsize/2
              xsize <- xsize - xModifierHolo
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
            if(chromatids & arrowsBothChrt & holocenNotAsChromatids==FALSE) {

              xsize <- max(fourX) - min(fourX)
              xbase <- min(fourX)

              xsize <- xsize/2
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
          attr(yMarkDwAr[[sm]], "spname")<-names(listMarkPosDwAr)[[sm]]
          xMarkDwAr[[sm]]<-xMark1
          attr(xMarkDwAr[[sm]], "spname")<-names(listMarkPosDwAr)[[sm]]

          if(chromatids & arrowsBothChrt & holocenNotAsChromatids==FALSE) {
            xMarkDwAr_2nd[[sm]]<-xMark1_2nd
            attr(xMarkDwAr_2nd[[sm]], "spname")<-names(listMarkPosDwAr)[[sm]]
          }

        } # end for

        #####################
        #   add DwAr marks to plot holocen
        #####################

        arrowPlotMark(squareness, xMarkDwAr, yMarkDwAr,
                      dfMarkColorInt,
                      listMarkPosDwAr,
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
        if(chromatids & arrowsBothChrt & holocenNotAsChromatids==FALSE) {

          arrowPlotMark(squareness, xMarkDwAr_2nd, yMarkDwAr,
                        dfMarkColorInt,
                        listMarkPosDwAr,
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

      } #     if(length(listMarkPosDwAr)>0){

      else {remove(listMarkPosDwAr)}

      #
      #   inline legend holocen
      #

    } #   if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") )

    # end downArrow marks  #

  ##########################################################################################################################

                            ##########################################
                            #
                            #   painting Marks monocen cM  & cMLeft style
                            #
                            ##########################################

if (exists("pLiMaPosMonocen") & exists("dfMarkColorInt") ) {

    yMarkcM<- xMarkcM<- listMarkPoscM<-list()

    j<-1

    for (k in 1:length(pLiMaPosMonocen)) {
      currName<-names(pLiMaPosMonocen)[[k]]
      if(nrow(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style %in%
                                        c("cM","cMLeft") ),])>0){
        listMarkPoscM<-c(listMarkPoscM,list(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style %in%
                                                                         c("cM","cMLeft") ),]))
        names(listMarkPoscM)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listMarkPoscM)>0){
      for (sm in 1:length(listMarkPoscM)) {

        yMark1<-xMark1<-NULL

        corr_index<- which(names(ym) %in% names(listMarkPoscM)[[sm]] )
        distCen   <- attr(listChrSize[[corr_index]],"centromere")

        for (m in 1:nrow(listMarkPoscM[[sm]]) ){

          ifelse(listMarkPoscM[[sm]][m,"style"]=="cM",squareSide<- "right", squareSide<- "cMLeft")

          if(squareSide=="right"){
            markSide<-2
            cMSign  <-1
          } else {
            markSide<-1 # left
            cMSign  <- -1
          }

          if("protruding" %in% colnames(listMarkPoscM[[sm]]) ){
            ifelse(is.na(listMarkPoscM[[sm]][m,"protruding"] ) ,
                   protruding2<-protruding,
                   protruding2<-listMarkPoscM[[sm]][m,"protruding"]
                   )
          } else {
            protruding2<-protruding
          }

          ifelse(listMarkPoscM[[sm]][m,"chrRegion"]=="q",longORshort<-0,longORshort<-1)
          ifelse(listMarkPoscM[[sm]][m,"chrRegion"]=="q",column<-1,column<-2)
          ifelse(listMarkPoscM[[sm]][m,"chrRegion"]=="q",mySign<--1,mySign<-1)

          rowIndex <- nrow(listChrSize[[corr_index]])*longORshort+(listMarkPoscM[[sm]][,"neworder"][m])

          if(collapseCen) {
            markDistCen <- listMarkPoscM[[sm]][m,"markDistCen"] - distCen/2
          } else {
            markDistCen <- listMarkPoscM[[sm]][m,"markDistCen"]
          }

          ypos <-ym[[corr_index]][ rowIndex  ,column]+
            (markDistCen*normalizeToOne*mySign)

          yMark1[[m]]<-c(ypos,ypos)
          attr(yMark1[[m]],"rowIndex") <- rowIndex

          xMark1[[m]]<-xm[[corr_index]][listMarkPoscM[[sm]][,"neworder"][m],][c(1,3)]

          initOrig1<-xMark1[[m]][1]
          initOrig2<-xMark1[[m]][2]

          if(cMBeginCenter){
            xMark1[[m]][markSide] <- xMark1[[m]][markSide] +   ( (xMark1[[m]][1] - xMark1[[m]][2])  / 2)*cMSign
          }

          if(chromatids ) {
            xMark1[[m]][markSide] <- xMark1[[m]][markSide] +
              cMSign*( (xMark1[[m]][1] - xMark1[[m]][2])  / 2) + xModifierMono*cMSign
          }

          if(squareSide=="right"){
            xMark1[[m]][1] <- xMark1[[m]][1] +   ( (xMark1[[m]][1] - initOrig2  )  * protruding2) # 1st protruding
          } else {
            xMark1[[m]][2] <- xMark1[[m]][2] -   ( (initOrig1 - xMark1[[m]][2])  * protruding2) # left
          }

          attr(xMark1[[m]],"rowIndex")<-rowIndex
          attr(yMark1[[m]],"squareSide")<-squareSide
        }
        yMarkcM[[sm]]<-yMark1
        attr(yMarkcM[[sm]], "spname")<-names(listMarkPoscM)[[sm]] # added 1.14
        xMarkcM[[sm]]<-xMark1
        attr(xMarkcM[[sm]], "spname")<-names(listMarkPoscM)[[sm]]
      } # end for

      #####################
      #   add marks to plot
      #####################

      cMPlotMark(bannedMarkName3,xMarkcM, yMarkcM,y, x, dfMarkColorInt,listMarkPoscM, lwd.cM2,circularPlot,
        radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,markN,labelSpacing,chrWidth,
        ylistTransChrSimple,rotation=rotation,labelOutwards)

    } #     if(length(listMarkPoscM)>0){
      else {
      remove(listMarkPoscM)
      }

  #
  #   labels cM inline monocen.
  #

booleanColorIntMarkcM<- exists ("dfMarkColorInt") & exists ("listMarkPoscM") & circularPlot==FALSE

if(booleanColorIntMarkcM)  {
      textLabel(xMarkcM,yMarkcM,listChrSize,listMarkPoscM,specialChrSpacing,chrSpacing,markLabelSize,pattern,bannedMarkName3
                ,markNewLine2=markNewLine,mylheight2=mylheight)
}

} # if presence end painting marks
                            ########################################
                            #
                            #   painting Marks cM & cMleft holocen
                            #
                            ########################################

if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") ) {

  xMarkcM<-yMarkcM<-listMarkPoscM<-list()

    j<-1
    for (k in 1:length(pLiMaPosHolocen)) {
      currName<-names(pLiMaPosHolocen)[[k]]
      if(nrow(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style %in%
                                         c("cM","cMLeft") ),])>0) {
        listMarkPoscM<-c(listMarkPoscM,list(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style %in%
                                                                         c("cM","cMLeft")),]) )
        names(listMarkPoscM)[[j]]<-currName
        j<-j+1
      }
    }

    if(length(listMarkPoscM)>0) {

      for (sm in 1:length(listMarkPoscM)) {

        yMark1<-NULL
        xMark1<-NULL

        for (m in 1:nrow(listMarkPoscM[[sm]])){

          ifelse(listMarkPoscM[[sm]][m,"style"]=="cM",squareSide<- "right", squareSide<- "cMLeft")
          if(squareSide=="right"){
            markSide<-2
            cMSign  <-1

          } else {
            markSide<-  1 # left
            cMSign  <- -1
          }

          if("protruding" %in% colnames(listMarkPoscM[[sm]]) ){
            ifelse(is.na(listMarkPoscM[[sm]][m,"protruding"] ) ,
                   protruding2<-protruding,
                   protruding2<-listMarkPoscM[[sm]][m,"protruding"]
            )
          } else {
            protruding2<-protruding
          }

          corr_index<-which(names(ym) %in% names(listMarkPoscM)[[sm]] )
          rowIndex<-(listMarkPoscM[[sm]][,"neworder"][m])
          ypos<-  ym[[corr_index]][ rowIndex , 2]+
            (listMarkPoscM[[sm]][m,"markPos"]   *normalizeToOne*1)

          yMark1[[m]]<-c(ypos,ypos)
          attr(yMark1[[m]],"rowIndex")<-rowIndex

          xMark1[[m]]<-xm[[corr_index]][ rowIndex , ][c(1,3)]

          initOrig2<-xMark1[[m]][2]
          initOrig1<-xMark1[[m]][1] # left

          if(cMBeginCenter){
            xMark1[[m]][markSide] <- xMark1[[m]][markSide] +   ( (xMark1[[m]][1] - xMark1[[m]][2])  / 2)*cMSign
          }

          if(chromatids & holocenNotAsChromatids==FALSE) {
            xMark1[[m]][markSide] <- xMark1[[m]][markSide] +
              cMSign*( (xMark1[[m]][1] - xMark1[[m]][2])  / 2) + xModifierHolo*cMSign
          }

          if(squareSide=="right"){
            xMark1[[m]][1] <- xMark1[[m]][1] + ((xMark1[[m]][1] - initOrig2)* protruding2)
          } else {
            xMark1[[m]][2] <- xMark1[[m]][2] - ((initOrig1 - xMark1[[m]][2])* protruding2) # left
          }
          attr(xMark1[[m]],"rowIndex")<-rowIndex
          attr(yMark1[[m]],"squareSide")<-squareSide

        }
        yMarkcM[[sm]]<-yMark1

        attr(yMarkcM[[sm]], "spname")<-names(listMarkPoscM)[[sm]] # added 1.14
        xMarkcM[[sm]]<-xMark1
        attr(xMarkcM[[sm]], "spname")<-names(listMarkPoscM)[[sm]]
      } # end for

      #####################
      #   add marks to plot
      #####################

      cMPlotMark(bannedMarkName3,xMarkcM, yMarkcM, y, x, dfMarkColorInt,listMarkPoscM, lwd.cM2,circularPlot,
                 radius,circleCenter,circleCenterY,separFactor,markLabelSize,pattern,markN,labelSpacing,chrWidth,
                 ylistTransChrSimple,rotation=rotation,labelOutwards)

    } #     if(length(listMarkPoscM)>0){

    else { # length = 0
      remove(listMarkPoscM)
    }
    # message(crayon::green(paste0("holocen. marks section end" ) ) )

  #
  #   inline legend cM holocen
  #

  booleanColorIntMarkcM<- exists ("dfMarkColorInt") & exists ("listMarkPoscM") & circularPlot==FALSE

  if(booleanColorIntMarkcM)  {
    textLabel(xMarkcM,yMarkcM,listChrSize,listMarkPoscM,specialChrSpacing,chrSpacing,markLabelSize,pattern
              ,bannedMarkName3
              ,markNewLine2=markNewLine,mylheight2=mylheight)
  } # if
} # if pLiMaPosHolocen

                                                            #
                                                            #         DOTS
                                                            #

  ##########################################################################################################################
  #
  # dot style of marks                        monocen dots
  #
  ##########################################################################################################################
  if (exists("pLiMaPosMonocen") & exists("dfMarkColorInt") ) {

    listMarkPosCr<-  yMarkCr<-xMarkCr<-rad<-radX<-  colBorderCr<-colCr<-list()

    j<-1

    for (k in 1:length(pLiMaPosMonocen)) {
      currName<-names(pLiMaPosMonocen)[[k]]
      if(nrow(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="dots"),])>0){
      listMarkPosCr<-c(listMarkPosCr,list(pLiMaPosMonocen[[k]][which(pLiMaPosMonocen[[k]]$style=="dots"),]) )
      names(listMarkPosCr)[[j]]<-currName
      j<-j+1
      }
    }


    if(length(listMarkPosCr)>0) {

    for (k in 1:length(listMarkPosCr) ) {

      colBorderCr1<-colCr1<-rad1<-rad1X<-yMarkCr1<-xMarkCr1<-NULL

      corr_index<-which(names(ym) %in% names(listMarkPosCr)[[k]] )
      distCen   <- attr(listChrSize[[corr_index]],"centromere")

      for (i in 1:nrow(listMarkPosCr[[k]])){
        # i=1
        ifelse(listMarkPosCr[[k]][i,"chrRegion"]=="q",longORshort<-0,longORshort<-1) #ifelseinloop
        ifelse(listMarkPosCr[[k]][i,"chrRegion"]=="q",column<-1,column<-2)
        ifelse(listMarkPosCr[[k]][i,"chrRegion"]=="q",mySign<--1,mySign<-1)

        rowIndex <- nrow(listChrSize[[corr_index]])*longORshort+(listMarkPosCr[[k]][,"neworder"][i])
        armStart <- ym[[corr_index]][ rowIndex,column]
        mSizeOrig<- listMarkPosCr[[k]][i,"markSize"]

        if(collapseCen) {
          markDistCen <- listMarkPosCr[[k]][i,"markDistCen"] - distCen/2
          if(markDistCen < 0) {
            mSizeMod <- mSizeOrig + markDistCen
            markDistCen <- 0
          } else {
            mSizeMod <- mSizeOrig
          }
        } else {
          markDistCen <- listMarkPosCr[[k]][i,"markDistCen"]
          mSizeMod <- mSizeOrig
        }

        ysupCr <- armStart +   (markDistCen     *normalizeToOne*mySign)

        yinfCr <- armStart +
          ( sum(markDistCen,mSizeMod,na.rm=TRUE)*normalizeToOne*mySign)

        yMarkCr1[[i]]<-rep(list(mean(c(yinfCr,ysupCr))),2)
        attr(yMarkCr1[[i]], "rowIndex") <- rowIndex

        xBoundaries <- xm[[corr_index]][listMarkPosCr[[k]][,"neworder"][i],3:2]
        xBoundariesMm <-(xBoundaries[2]-xBoundaries[1])
        xBoundariesQuar <- xBoundariesMm/4

        if(chromatids) {

          xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar - (xModifierMono/2) ), list(xBoundaries[1]+ 3*xBoundariesQuar + (xModifierMono/2) ) )
        } else {
          xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar),list(xBoundaries[1]+ 3*xBoundariesQuar ) ) # default
        }

        attr(xMarkCr1[[i]], "rowIndex")<- rowIndex

        rad1[[i]]<-rep(list(abs(ysupCr-yinfCr)/2 ),2)
        attr(rad1[[i]], "rowIndex")<- rowIndex

        rad1X[[i]]<-rep(list(xBoundariesMm/2),2)
        attr(rad1X[[i]], "rowIndex")<- rowIndex

        colCr1[[i]] <- rep(list(dfMarkColorInt$markColor[match(listMarkPosCr[[k]]$markName[i]
                                                               , dfMarkColorInt$markName)]),2)
        colBorderCr1[[i]] <- rep(list(dfMarkColorInt$markBorderColor[match(listMarkPosCr[[k]]$markName[i]
                                                                           , dfMarkColorInt$markName)]),2)

      }
      yMarkCr[[k]]<-yMarkCr1
      attr(yMarkCr[[k]], "spname")<-names(listMarkPosCr)[[k]] # added 1.14
      xMarkCr[[k]]<-xMarkCr1
      attr(xMarkCr[[k]], "spname")<-names(listMarkPosCr)[[k]] # added 1.14

      rad[[k]]<-rad1
      attr(rad[[k]], "spname") <-names(listMarkPosCr)[[k]] # added 1.16.4

      radX[[k]]<-rad1X
      attr(radX[[k]], "spname") <-names(listMarkPosCr)[[k]] # added 1.16.4

      colCr[[k]]<-colCr1
      colBorderCr[[k]]<-colBorderCr1

    } # end for k OTU

    #####################
    #   add to plot MarkCrs DOTS monocen
    #####################

      plotDotMarks(bannedMarkName3,xMarkCr,yMarkCr, rad, radX, colCr,markN,xfactor,colBorderCr,circularPlot, y,x,radius,circleCenter,circleCenterY,separFactor,
                   chrWidth,listMarkPosCr,markLabelSize,pattern,labelSpacing,useOneDot,legend,ylistTransChrSimple,rotation=rotation,
                   labelOutwards,dotsAsOval)


    } #     if(length(listMarkPosCr)>0){

   else {
     remove(listMarkPosCr)
   }

    #
    #         WRITE INLINE LEGENDS WHEN DOTS
    #

    booleanDotsLabels<- legend=="inline" & exists("dfMarkColorInt") & exists("listMarkPosCr") & circularPlot==FALSE

    if (booleanDotsLabels){
      textLabelDots(xMarkCr,yMarkCr,listChrSize,listMarkPosCr,specialChrSpacing,
                    chrSpacing,markLabelSize,pattern,bannedMarkName3,xBoundariesQuar)
    }

  } # end painting MarkCrs



  ##############################################################################
  #
  #                                            dot style of marks holocen
  #
  ##############################################################################

  if (exists("pLiMaPosHolocen") & exists("dfMarkColorInt") ){
    # message(crayon::green(paste0("holocen. dot marks section start" ) ) )
    listMarkPosCr<-list()

    colBorderCr<-colCr<-rad<-radX<-xMarkCr<-yMarkCr<-list()
    j<-1

    for (k in 1:length(pLiMaPosHolocen)) {
      currName<-names(pLiMaPosHolocen)[[k]]

      if(nrow(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="dots"),])>0){
        listMarkPosCr<-c(listMarkPosCr,list(pLiMaPosHolocen[[k]][which(pLiMaPosHolocen[[k]]$style=="dots"),]) )
        names(listMarkPosCr)[[j]]<-currName
        j<-j+1
      }
    }


    if(length(listMarkPosCr)>0){

    for (k in 1:length(listMarkPosCr) ) {

      yMarkCr1<-xMarkCr1<-rad1<-rad1X<-colCr1<-colBorderCr1<-NULL

      for (i in 1:nrow(listMarkPosCr[[k]])){

        corr_index<-which(names(ym) %in% names(listMarkPosCr)[[k]] )

        rowIndex <- (listMarkPosCr[[k]][,"neworder"][i])
        chrStart <- ym[[corr_index]][ rowIndex , 2]
        ysupCr<- chrStart +
          (listMarkPosCr[[k]][i,"markPos"] *normalizeToOne*1)
        yinfCr<- chrStart +
          ( sum(listMarkPosCr[[k]][i,"markPos"], listMarkPosCr[[k]][i,"markSize"], na.rm=TRUE )*normalizeToOne)
        yMarkCr1[[i]]<-rep(list(mean(c(yinfCr,ysupCr))),2)
        attr(yMarkCr1[[i]], "rowIndex")<- rowIndex

        xBoundaries<-xm[[corr_index]][listMarkPosCr[[k]][,"neworder"][i],3:2]
        xBoundariesMm  <-(xBoundaries[2]-xBoundaries[1])
        xBoundariesQuar<- xBoundariesMm/4

        if(chromatids & holocenNotAsChromatids==FALSE){
          xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar - (xModifierHolo/2) ), list(xBoundaries[1]+ 3*xBoundariesQuar + (xModifierHolo/2) ) )
        } else {
          xMarkCr1[[i]]<-c(list(xBoundaries[1]+xBoundariesQuar),list(xBoundaries[1]+3*xBoundariesQuar) ) # default
        }

        attr(xMarkCr1[[i]], "rowIndex")<- rowIndex

        rad1[[i]]<-rep(list(abs(ysupCr-yinfCr)/2 ),2)
        attr(rad1[[i]], "rowIndex")<- rowIndex

        rad1X[[i]]<-rep(list(xBoundariesMm/2),2)
        attr(rad1X[[i]], "rowIndex")<- rowIndex

        colCr1[[i]] <- rep(list(dfMarkColorInt$markColor[match(listMarkPosCr[[k]]$markName[i], dfMarkColorInt$markName)]),2)
        colBorderCr1[[i]] <- rep(list(dfMarkColorInt$markBorderColor[
          match(listMarkPosCr[[k]]$markName[i], dfMarkColorInt$markName)]),2)
      }

      yMarkCr[[k]]<-yMarkCr1
      attr(yMarkCr[[k]], "spname")<-names(listMarkPosCr)[[k]] # added 1.14
      xMarkCr[[k]]<-xMarkCr1
      attr(xMarkCr[[k]], "spname")<-names(listMarkPosCr)[[k]] # added 1.14
      rad[[k]]<-rad1
      attr(rad[[k]], "spname")<-names(listMarkPosCr)[[k]] # added 1.16.4
      radX[[k]]<-rad1X
      attr(radX[[k]], "spname")<-names(listMarkPosCr)[[k]] # added 1.16.4

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
    chrWidth,listMarkPosCr,markLabelSize,pattern,labelSpacing,useOneDot,legend,ylistTransChrSimple, rotation=rotation,
    labelOutwards,dotsAsOval)

    } # if(length(listMarkPosCr)>0){

  else {remove(listMarkPosCr)}

    #
    #     write inline legend dots holocen
    #

    booleanDotsLabels<- legend=="inline" & exists("dfMarkColorInt") & exists("listMarkPosCr") & circularPlot==FALSE

    if (booleanDotsLabels){
      textLabelDots(xMarkCr,yMarkCr,listChrSize,listMarkPosCr,specialChrSpacing,
                    chrSpacing,markLabelSize,pattern,bannedMarkName3,xBoundariesQuar)
    }

  } # end painting MarkCrs

  ##############################################
  #
  #   labels to the right
  #
  ##############################################

  xNoNA <- x # removeNAFromList(x,areNA)

  yNoNA <- removeNAFromList(y,areNA)

  if(exists("dfMarkColorInt") ){

    dfMarkColorIntNocM <- dfMarkColorInt[which(!dfMarkColorInt$style %in% c("cM","cMLeft") ),]

    # remove bannedMarkNames

    if(length(bannedMarkName3) ) {
      dfMarkColorIntNocM <- dfMarkColorIntNocM[
        which(! (dfMarkColorIntNocM$markName %in% bannedMarkName3) ) ,]
    }


    if(length(dfMarkColorIntNocM)==0){
      remove(dfMarkColorIntNocM)
    }

    #
    # allow cM if cen. because they are not shown as cM , they must be aside if desired
    #

    if(exists("cenMarkNames") ) {
      dfMarkColorIntcMnoCen <- dfMarkColorInt[which(dfMarkColorInt$style    %in% c("cM","cMLeft") &
                                                              dfMarkColorInt$markName %in% cenMarkNames ) , ]
      if( exists("dfMarkColorIntNocM") ) {
        dfMarkColorIntNocM<-dplyr::bind_rows(
          dfMarkColorIntNocM,dfMarkColorIntcMnoCen)
      } else {
        dfMarkColorIntNocM<-dfMarkColorIntcMnoCen
      }
    }

    if(remSimiMarkLeg){
      dfMarkColorIntNocM$markName <- sub(pattern,"",dfMarkColorIntNocM$markName )
      dfMarkColorIntNocM<-dfMarkColorIntNocM[!duplicated(dfMarkColorIntNocM$markName),]
    }

    if(nrow(dfMarkColorIntNocM)>0 )  {

      if(!exists("allMarkMaxSize") ){
        message(crayon::green(paste0("Seems you didn't provide markSize (NA), if you want cM style, set it in the dfMarkColor data.frame" ) ) )

        allMarkMaxSize<-1
      }

      if(circularPlot){
        maxx<-max(unlist(circleMapsOrig), na.rm=TRUE )
      } else {
        maxx<-(max(unlist(xNoNA)) )
      }

      if(legend=="aside" ){
        plotlabelsright(maxx,yNoNA, markLabelSpacer,chrWidth,dfMarkColorIntNocM,allMarkMaxSize,normalizeToOne,
                              markLabelSize,xfactor,legendWidth, legendHeight, n, pattern,legendYcoord,useOneDot,
                        dotsAsOval,circularPlot)
      } # if aside

    } # is df


if(legend=="inline" & bMarkNameAside & !missing(bannedMarkName) ) {

      dfMarkColorIntBanned <- dfMarkColorInt[which( dfMarkColorInt$markName %in% bannedMarkName ) ,]

        dfMarkColorIntBanned <- dfMarkColorIntBanned[
          which(! dfMarkColorIntBanned$markName %in% bannedMarkNameFor ) ,]

    if(nrow(dfMarkColorIntBanned) >0 ) {
    #
    dfMarkColorIntBanned <- dfMarkColorIntBanned[which(!dfMarkColorIntBanned$style %in% c("cM","cMLeft") ),]

    # remove bannedMarkNames with cenStyle style

    if(exists("bannedMarkName2") ) {
      if(length(bannedMarkName2) ) {
        dfMarkColorIntBanned <- dfMarkColorIntBanned[
          which(! (dfMarkColorIntBanned$markName %in% bannedMarkName2) ) ,]
      }
    }

    if(length(dfMarkColorIntBanned)==0){
      remove(dfMarkColorIntBanned)
    }

    #
    # allow cM if cen. because they are not shown as cM , they must be aside if desired
    #

    if(exists("cenMarkNames")  ) {

      dfMarkColorIntBannedcMnoCen <- dfMarkColorInt[which(dfMarkColorInt$style    %in% c("cM","cMLeft") &
                                                                      dfMarkColorInt$markName %in% cenMarkNames &
                                                                      dfMarkColorInt$markName %in% bannedMarkName) , ]
      if( exists("dfMarkColorIntBanned") ) {
        dfMarkColorIntBanned<-dplyr::bind_rows(
          dfMarkColorIntBanned,dfMarkColorIntBannedcMnoCen)
      } else {
        dfMarkColorIntBanned<-dfMarkColorIntBannedcMnoCen
      }

    }

    if(remSimiMarkLeg){
      dfMarkColorIntBanned$markName <- sub(pattern,"",dfMarkColorIntBanned$markName )
      dfMarkColorIntBanned<-dfMarkColorIntBanned[!duplicated(dfMarkColorIntBanned$markName),]
    }

    if(nrow(dfMarkColorIntBanned)>0 )  {

      if(!exists("allMarkMaxSize") ){
        message(crayon::green(paste0("Seems you didn't provide markSize (NA), if you want cM style, set it in the dfMarkColor data.frame" ) ) )

        allMarkMaxSize<-1
      }


      if(circularPlot) {
        maxx<-max(unlist(circleMapsOrig), na.rm=TRUE )
      } else {
        maxx<-(max(unlist(xNoNA)) )
      }
      #
      #   dots legend wrong, in triangle and rounded
      #

      plotlabelsright(maxx,yNoNA, markLabelSpacer,chrWidth,dfMarkColorIntBanned,allMarkMaxSize,normalizeToOne,
                      markLabelSize,xfactor,legendWidth, legendHeight, n, pattern,legendYcoord,useOneDot,
                      dotsAsOval,circularPlot)

    } # is df
    #
    } # exists dfMarkColorIntBanned

  } # inline

  } # if dfmarkcolor
  #################################
  #
  # add notes (to the right)
  #
  #################################

if(circularPlot==FALSE) {

  if(exists("notes2Int")) {

    if(missing(OTUfont)){
      OTUfont<-1
    }
    if(OTUfamily==""){
      OTUfamily<-NA
    }
    addNotes(notes2Int,
             listChrSizenoNA, groupName,indexCount,morphoCount,
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

  if(exists("leftNotes2Int")){

    addNotes(leftNotes2Int,listChrSizenoNA, groupName,indexCount,morphoCount,
                 xmnoNA,ymnoNA,distTextChr,chrIdCount, leftNotesPosX,leftNotesPosY,
                 leftNotesTextSize,defaultFontFamily2,
                 FALSE,
                 NA, NA,
                 downNote=TRUE,rightN=FALSE,
             leftNoteFont,
             leftNoteFontUp,
             noteFont,
             parseTypes,parseStr2lang)
  } # fi notes missing

  if(exists("leftNotesUp2Int")){

    if(missing(OTUfont)){
      OTUfont<-1
    }

    addNotes(leftNotesUp2Int,listChrSizenoNA, groupName,indexCount,morphoCount,
             xmnoNA,ymnoNA,distTextChr,chrIdCount, leftNotesUpPosX,leftNotesUpPosY,
             leftNotesUpTextSize,defaultFontFamily2,
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
