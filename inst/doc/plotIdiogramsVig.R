## ---- results="hide", message=FALSE, warning=FALSE, eval=TRUE------------

#load package
library(idiogramFISH) 

## ------------------------------------------------------------------------
# Example dataframe written in R, use: (column OTU is optional if only 1 OTU)
mydfChrSize<-read.table(text=
"            OTU chrName shortArmSize longArmSize 
1 \"Species one\"   1     1.5         2.0  
2 \"Species one\"   2     2.0         2.5  
3 \"Species one\"   3     1.0         1.5
4 \"Species one\"   X     2.0         3.5"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

# just to show it here
knitr::kable(mydfChrSize) 

## ------------------------------------------------------------------------
# From scratch:
mydfMarkColor<-read.table(text=
"  markName markColor  style
1       5S       red   dots
2      45S     green square
3     DAPI      blue square
4      CMA    yellow square"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

knitr::kable(mydfMarkColor) 

## ------------------------------------------------------------------------
# We will use column OTU if dataframe because chromosome size df has it
mydfOfMarks<-read.table(text=
"            OTU chrName markName markArm markSize markDistCen
1 \"Species one\"      1       5S       p      0.5         0.5
2 \"Species one\"      1      45S       q        1         0.5
3 \"Species one\"      X      45S       p        1         1.0
4 \"Species one\"      3     DAPI       q        1         1.0"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

knitr::kable(mydfOfMarks) 

## ------------------------------------------------------------------------
# We will use column OTU because dataframe of chromosome size has it
mydfOfCenMarks<-read.table(text=
"             OTU chrName markName
1  \"Species one\"     1     DAPI
2  \"Species one\"     X      CMA"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

knitr::kable(mydfOfCenMarks)

## ----example_M1, echo=TRUE, results="hide", fig.width=7, fig.height=4.5, message=FALSE----
# library(idiogramFISH)

plotIdiograms(mydfChrSize, mydfMarkColor, mydfOfMarks, mydfOfCenMarks,
              dotRoundCorr=2, chrWidth=4.5, chrSpacing = 4,
              karSpacing=1.6,
              indexIdTextSize=.7, OTUTextSize=.7,
              markLabelSize=.7, 
              rulerPos=-1.9, ruler.tck=-0.02, rulerNumberPos=.5, rulerNumberSize=.7
)

## ------------------------------------------------------------------------
data(bigdfOfChrSize, bigdfOfMarks, bigdfDataCen, dfMarkColor)

# Chromosome sizes
knitr::kable(bigdfOfChrSize)

# Mark characteristics, does not require OTU
knitr::kable(dfMarkColor)

# Mark position
knitr::kable(bigdfOfMarks)

# Centromeric Marks only
knitr::kable(bigdfDataCen)

## ----example_M3, echo=TRUE, results="hide", fig.width=6, fig.height=13, message=FALSE----
# library(idiogramFISH)

plotIdiograms(bigdfOfChrSize, dfMarkColor, bigdfOfMarks, bigdfDataCen,
                        karHeight=1.2,karSpacing=2.2,
                        dotRoundCorr = .5, distTextChr=.5,
                        indexIdTextSize=.7, OTUTextSize=.9,
                        legend="aside",markLabelSize=.7, markLabelSpacer=1,
                        morpho=FALSE, xlimRightMod=2,
                        ruler=TRUE,rulerPos=-.9, rulerPosMod=3, ruler.tck=-0.004, 
                        rulerNumberPos=.4, rulerNumberSize=.4,
                        )

