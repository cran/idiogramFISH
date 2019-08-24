## ---- results="hide", message=FALSE, warning=FALSE, eval=TRUE------------

#load package
library(idiogramFISH) 

## ------------------------------------------------------------------------
# Example dataframe written in R, use: (column OTU is optional if only 1 OTU)
mydfChrSizeHolo<-read.table(text=
"            OTU chrName chrSize  
1 \"Species one\"   1     6.5      
2 \"Species one\"   2     5.0      
3 \"Species one\"   3     4.0    
4 \"Species one\"   X     3.0    "  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

# just to show it here
knitr::kable(mydfChrSizeHolo) 

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
mydfMarkPosHolo<-read.table(text=
"             OTU  chrName markName markPos markSize
1 \"Species one\"       3       5S     1.0      0.5
2 \"Species one\"       3     DAPI     2.0      0.5
3 \"Species one\"       1      45S     2.0      0.5
4 \"Species one\"       2     DAPI     2.0      0.5
5 \"Species one\"       4      CMA     2.0      0.5
6 \"Species one\"       4       5S     0.5      0.5"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

knitr::kable(mydfMarkPosHolo) 

## ----parinit, echo=FALSE-------------------------------------------------
opar <- par(no.readonly = TRUE)      # make a copy of current settings
on.exit(suppressWarnings(par(opar)) )

## ----example, echo=TRUE, results="hide", fig.width=6, fig.height=3.5, message=FALSE----
# library(idiogramFISH)
par(mar=c(1,4,1,1)) # bottom left top right
plotIdiogramsHolo(mydfChrSizeHolo, mydfMarkColor, mydfMarkPosHolo, 
                        dotRoundCorr=2,
                        chrWidth = 2, chrSpacing = 2,
                        indexIdTextSize=1, OTUTextSize=1, karHeight = 1.4,
                        legend="aside" ,markLabelSize=1,
                        addOTUName=FALSE,
                        rulerNumberSize=1, rulerPos=-2.2,ruler.tck=-0.03,rulerNumberPos=.9,
                        xlimLeftMod=2.2,  xlimRightMod=10, ylimBotMod=.1
                        )

## ------------------------------------------------------------------------
data(bigdfChrSizeHolo, dfMarkColor, bigdfMarkPosHolo)

# Chromosome sizes
knitr::kable(bigdfChrSizeHolo)

# Mark characteristics, does not require OTU
knitr::kable(dfMarkColor)

# Mark position
knitr::kable(bigdfMarkPosHolo)

## ----example3, echo=TRUE, results="hide", fig.width=6, fig.height=6, message=FALSE----
library(idiogramFISH)
par(mar=c(1,1,1,1))
plotIdiogramsHolo(bigdfChrSizeHolo, dfMarkColor, bigdfMarkPosHolo, MarkDistanceType="cen", 
                  roundness=6, dotRoundCorr=1, karHeight = 1, karSpacing = 1.7,
                  chrWidth = 2, chrSpacing = 2,
                  indexIdTextSize=.9,
                  markLabelSize=.9,
                  rulerNumberSize=.9,
                  OTUTextSize=1.2,
                  reduDistKar=FALSE,
                  chrId="simple", xlimRightMod=2,
                  ylimBotMod=.4)

## ------------------------------------------------------------------------
# using previous dataframes
bigdfChrSizeHolo$chrSize<-bigdfChrSizeHolo$chrSize*980000
bigdfMarkPosHolo$markPos<-bigdfMarkPosHolo$markPos*980000
bigdfMarkPosHolo$markSize<-bigdfMarkPosHolo$markSize*980000

## ----example4, echo=TRUE, results="hide", fig.width=6, fig.height=6, message=FALSE----
library(idiogramFISH)
par(mar=c(1,1,1,1))
plotIdiogramsHolo(bigdfChrSizeHolo, dfMarkColor, bigdfMarkPosHolo, MarkDistanceType="cen", 
                  Mb=TRUE, # <- THIS IS NEW
                  roundness=6, dotRoundCorr=1, karHeight = 1, karSpacing = 1.7,
                  chrWidth = 2, chrSpacing = 2,
                  karIndex = FALSE,
                  markLabelSize=.9,
                  rulerNumberSize=.9, rulerPos = 0,
                  OTUTextSize=1.2,
                  reduDistKar=FALSE,
                  chrId="simple", xlimRightMod=2,
                  xlimLeftMod = 3,
                  ylimBotMod=.4)

## ----returntooldpar, echo=FALSE------------------------------------------
suppressWarnings(par(opar) )

