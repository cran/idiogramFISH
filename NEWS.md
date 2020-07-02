
<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

# idiogramFISH 1.15.3

01-07-2020

main changes:

  - Better plotting of GISH with chromatids
  - change in messages when missing data
  - chr. in groups are closer
  - parsing of citrus names of chromosomes F<sub>L</sub><sup>+</sup> and
    F<sub>L</sub><sup>0</sup> automatic
  - helper functions for plotting Citrus karyotypes:
      - `citrusSize`, `citrusMarkPos`, `markOverCMA`

param: (`plotIdiograms`)

  - `efZero` threshold for checking if \!= 0
  - `orderChr`, order of chr. Replaces `orderBySize` - deprecated.
    Values = `size`, `original`, `name`, `group`
  - `orderBySize` - deprecated
  - `notesLeft` note position to the left when `TRUE`
  - `notesPosY` y axis modify notes position
  - `chrIdPatternRem` regex pattern to remove from chr. names

# idiogramFISH 1.15.1

02-06-2020

  - introducing ‘chromatids’
  - new rounded style of centromere added (default).
  - better naming of `w` position marks when `inline`
  - changed logic of `cenStyle` coloring

param:

  - `chromatids` show separated chromatids
  - `holocenNotAsChromatids` do not use chromatids in holocen.
  - `arrowsBothChrt` prints arrow marks in both chromatids
  - `excHoloFrArrToSide` excludes holocentrics from arrowsToSide config.
  - `xModifier` separation among chromatids
  - `xModMonoHoloRate` shrink holocen. separ among chromatids with this
    quotient.
  - `remSimiMarkLeg` remove “duplicated” name of labels when presence of
    pseudoduplicates arising from `pattern`
  - `bannedMarkName` remove this mark name from labels (legends)
  - `defCenStyleCol` color for external part of marks with `cenStyle`
  - `roundedCen` rounded centromere
  - `lwd.mimicCen` line width for `cenStyle` marks
  - `squareness` new name for `roundness` (deprecated)

# idiogramFISH 1.14.11

23-04-2020

  - `genBankReadIF` function, now allows duplicated field names
  - `cMLeft` style of mark added
  - `cM` and `cMLeft`styles are used as `inline` type of `legend` for
    arrows (`upArrow`,`downArrow`)
  - A new column `protruding` can be added to `dfMarkColor` data.frame
    to define aspect of `cM` marks
  - `namesToColumns` new function to avoid overlap of mark names, for
    holoc. and monoc.

params (`namesToColumns`):

  - `marksDf` data.frame of marks
  - `dfChrSize` data.frame, size of chr. same as plot.
  - `markType` of type “downArrow”,“upArrow”,“cM”,“cMLeft”
  - `amountofSpaces` numeric, number of spaces for each column
  - `colNumber` numeric, number of columns
  - `protruding` numeric, same as plot, equivalent to cM protruding
  - `protrudingInt` numeric, spacing of columns in terms of width of
    chr. percent 1 = 100%.
  - `circularPlot` same as plot
  - `rotation` same as plot
  - `defaultStyleMark` if some data in column style missing fill with
    this one
  - `orderBySize` same as in plot.
  - `halfModUp` when plotting several chromosomes in a circular plot,
    corrects for alignment problems of “upArrows”, “cM” labels.
  - `halfModDown` when plotting several chromosomes in a circular plot,
    corrects for alignment problems of “downArrows”, “cMLeft” labels.
  - `rotatMod` for circ. plots, when rotation diff. from `0`, corrects
    alignment of labels.

params:

  - `cMBeginCenter` modifies start position of `cM` and `cMLeft` marks
  - `arrowsToSide` arrows are plotted near chr. margin

# idiogramFISH 1.14.7

27-03-2020

  - Compatibility with `rentrez` downloaded data
  - Better reading of `join` from genBank data
  - new styles of mark: `cenStyle` to add constrictions anywhere;
    `upArrow` (clockwise in circular plot); `downArrow` (anti-clockwise
    in circular plot)
  - fixed bug when `legend="inline"` in circular plots

params:

  - `rulerTitleSize`: Font size of units (title)
  - `arrowhead`: proportion of head of arrow - length
  - `shrinkArrow`: proportion to shrink body of arrow - width
  - `arrowheadWidthShrink`: proportion to shrink arrowhead - width

# idiogramFISH 1.14.2

26-02-2020

  - Introducing circular plots `circularPlot=TRUE` and other params. for
    circular plot
  - function `genBankReadIF` to read plasmid or prokaryote data. Uses
    tidyr.  
  - function `swapChrRegionDfSizeAndMarks` to swap arm size and marks
  - tolerance when column `markSize` absent

params:

  - `legendYcoord`: modify mark legend Y pos (for common plot also)
  - `callPlot`: call plot.new or use your device (when FALSE)

params: (circularPlot=TRUE)

  - `shrinkFactor`: size of chr. in fraction of circle
  - `separFactor`: separ among kar.
  - `labelSpacing`: among label and chr.
  - `chrLabelSpacing`: chr. label space
  - `OTUlabelSpacing`: OTU name space
  - `radius`: radius
  - `OTUsrt`: angle of OTU name text
  - `OTUplacing`: add number and legend instead of OTU name
  - `useOneDot`: one dot instead of two
  - `circleCenter`: X coordinate
  - `circleCenterY`: Y coordinate
  - `OTULabelSpacerx`: modify OTU name pos.
  - `OTULabelSpacery`: modify OTU name pos.
  - `OTUcentered`: OTU name centered
  - `OTUjustif`: OTU name justif.
  - `OTUlegendHeight`: separ. of OTU names when `OTUplacing`

# idiogramFISH 1.13.8

05-02-2020

  - Fixed bug when plotting several OTU with groups
  - cen. marks allowed also when `centromereSize = 0`
  - improvement in automatic scale of ruler.
  - Added the “cM” style of mark, with custom `protruding`
  - `centromereSize` is automatic (when absent), as well as
    `rulerInterval`

params:

  - `lwd.cM`: thickness of cM marks
  - `OTUfont`: style of font of OTU name
  - `OTUfamily`: font family for OTU names
  - `lwd.chr`: affects ruler too.
  - `defaultFontFamily`: modify font of texts.
  - Custom default style of mark with `defaultStyleMark`
  - `fixCenBorder` affects cen. marks also.
  - `chrBorderColor` for adding optionally chr. border color.
  - `cenColor` defaults to `chrColor` now.
  - `colorBorderMark` forces custom color in border of marks.
  - `borderOfWhiteMarks`, if `TRUE`, when mark is white, its border is
    black.
  - `ceilingFactor` number of significative digits to consider when
    rounding ruler max. value.
  - `MbThreshold` created (substitutes `MbThresholds`)
  - added option to modify ruler intervals for Mb, and cM independently
    with params: `rulerIntervalMb`, `rulerIntervalcM`
  - other added parameters: `defaultStyleMark`, `protruding`,
    `ceilingFactor`, `rulerInterval`, `threshold`, `MbUnit`,
    `specialChrWidth`, `specialChrSpacing`, `specialOTUNames`,
    `specialyTitle`
  - OTUs passed to `specialOTUNames`, can have special:
    `specialChrWidth`, `specialChrSpacing`, and `specialyTitle`. Useful
    for e.g. cM.
  - Allowed customization of ruler (`ceilingFactor`, `rulerInterval`)
  - Allowed custom ruler title `MbUnit`, `specialyTitle`, `yTitle`.
    `yTitle` is the common (micrometers). `specialyTitle` is for OTUs in
    `specialOTUNames` (e.g. “cM”), and `MbUnit` when data in millions
    and OTU is not in `specialOTUNames`

# idiogramFISH 1.12.1

06-01-2020

  - Fixed bug of absence of chr. indices when monocen. and holocen.
    together
  - Added functionality to print each index separately
  - Added functionality to print groups below chr. name
  - DOI added
  - minor vignette corrections

# idiogramFISH 1.11.1

12 12 2019

  - Added functionality for fixing y x aspect ratio (roundness
    proportion) using `asp = 1` only
  - Use of `dotRoundCorr` discouraged, requires `useXYfactor = TRUE`
  - Fixed misplacement of marks when `origin="t"` or `markDistType =
    "cen"`
  - Added functionality for plotting karyotypes in micrometers and bases
    together, see monocen. vignette

# idiogramFISH 1.9.1

29 11 2019

  - Fixed bug when centromere=0 when several karyotypes
  - Added rounded vertices for `centromere > 0`
  - Added functionality for plotting GISH.

# idiogramFISH 1.8.3

14 11 2019

  - Fixed dependencies
  - Fixed size of dots of legend

# idiogramFISH 1.8.1

29 10 2019

  - Added parameters for adding notes to the right of karyotype.
  - Improvement in messages when plotting.

# idiogramFISH 1.7.1

20 10 2019

  - Cen. marks don’t need another data.frame. Can be present in main
    marks data.frame
  - Allowed dup. names for not ordered chr. names (and no marks)

# idiogramFISH 1.6.3

13 10 2019

  - More tolerance when allowing duplicated chr. names when no marks.
  - Documentation changes, new examples.

# idiogramFISH 1.6.1

02 10 2019

  - Added support to plot monocen. and holocen. together  
  - Function `plotIdiogramsHolo` deprecated

# idiogramFISH 1.5.1

27 09 2019

  - Added support to plot alongside phylogenies
  - Allow some karyotypes to appear without indexes when error in long /
    short classif.
  - Fixed bug in naming of OTUs.
  - Vignettes corrections
  - Fix references of packages in vignettes when package not
    installed.  
  - Added support for vignettes in devel. in R-32 bits

# idiogramFISH 1.2.1

17 09 2019

  - Fixed bug in armRatioCI that impacts all other functions.
  - Added support for groups
  - Added human karyotype
  - Added rounded vertices when `centromereSize =0`
  - You don’t have to use dfMarkColor data.frame, is not mandatory now.
  - You can use (optionally) a character vector to pass colors.
  - Package has default colors now.
