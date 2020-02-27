
<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

# idiogramFISH 1.14.2

26-02-2020

  - function `genBankReadIF` to read plasmid or prokaryote data. Uses
    tidyr.  
  - function `swapChrRegionDfSizeAndMarks` to swap arm size and marks
  - tolerance when column `markSize` absent
  - Added circular plot `circularPlot=TRUE` and other params. for
    circular plot:
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
  - `callPlot`: call plot.new or use your device (when FALSE)
  - `OTULabelSpacerx`: modify OTU name pos
  - `OTULabelSpacery`: modify OTU name pos
  - `OTUcentered`: OTU name centered
  - `OTUjustif`: OTU name justif.
  - `OTUlegendHeight`: separ. of OTU names when `OTUplacing`
  - `legendYcoord`: modify mark legend Y pos (for common plot also)

# idiogramFISH 1.13.8

05-02-2020

  - `lwd.cM`: thickness of cM marks
  - `OTUfont`: style of font of OTU name
  - `OTUfamily`: font family for OTU names
  - `lwd.chr`: affects ruler too.
  - `defaultFontFamily`: modify font of texts.
  - Fixed bug when plotting several OTU with groups
  - `fixCenBorder` affects cen. marks also.
  - `chrBorderColor` for adding optionally chr. border color.
  - `cenColor` defaults to `chrColor` now.
  - `colorBorderMark` forces custom color in border of marks.
  - `borderOfWhiteMarks`, if `TRUE`, when mark is white, its border is
    black.
  - cen. marks allowed also when `centromereSize = 0`
  - `centromereSize` is automatic (if absent), as well as
    `rulerInterval`
  - `ceilingFactor` number of significative digits to consider when
    rounding ruler max. value.
  - improvement in automatic scale of ruler. `MbThreshold` created
    (substitutes `MbThresholds`)
  - added option to modify ruler intervals for Mb, and cM independently
    with params: `rulerIntervalMb`, `rulerIntervalcM`
  - other added parameters: `defaultStyleMark`, `protruding`,
    `ceilingFactor`, `rulerInterval`, `threshold`, `MbUnit`,
    `specialChrWidth`, `specialChrSpacing`, `specialOTUNames`,
    `specialyTitle`
  - params: OTUs passed to `specialOTUNames`, can have special:
    `specialChrWidth`, `specialChrSpacing`, and `specialyTitle`. Useful
    for e.g. cM.
  - Added the “cM” style of mark, with custom `protruding`
  - Custom default style of mark with `defaultStyleMark`
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
