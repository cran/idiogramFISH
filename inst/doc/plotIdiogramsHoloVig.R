## ---- results="asis", echo=FALSE, message=FALSE--------------------------
# <!-- pkgdown --> 
# <!-- jquery --><script src="jquery.min.js" crossorigin="anonymous"></script>
myfile<-"jquery.min.js"
if(file.exists(myfile)){
cat(paste0('<script src="',myfile,'" crossorigin="anonymous"></script> <!-- # -->'))
}
# <!-- clipboard.js --><script src="clipboard.min.js"  crossorigin="anonymous"></script>
myfile<-"clipboard.min.js"
if(file.exists(myfile)){
cat(paste0('<script src="',myfile,'"crossorigin="anonymous"></script>'))
}
# <!-- Font Awesome icons --><link rel="stylesheet" href="all.minMod.css"  crossorigin="anonymous">
myfile<-"all.minMod.css"
if(file.exists(myfile)){
cat(paste0('<link rel="stylesheet" href="',myfile,'"  crossorigin="anonymous">'))
}
# <!-- Bootstrap --><link rel="stylesheet" href="bootstrap.minO.css" crossorigin="anonymous">
myfile<-"bootstrap.minO.css"
if(file.exists(myfile)){
cat(paste0('<link rel="stylesheet" href="',myfile,'"  crossorigin="anonymous">'))
}
# <!-- # <script src="bootstrap.min.js"  crossorigin="anonymous"></script> -->
myfile<-"bootstrap.min.js"
if(file.exists(myfile)){
cat(paste0('<script src="',myfile,'" crossorigin="anonymous"></script> <!-- # -->'))
}
myfile<-"pkgdown2.js"
if(file.exists(myfile)){
cat(paste0('<script src="',myfile,'"></script> <!-- # -->'))
}

## ---- results="hide", message=FALSE, warning=FALSE, eval=TRUE------------

#load package
library(idiogramFISH) 

## ------------------------------------------------------------------------
# Example data.frame written in R, use: (column OTU is optional if only 1 OTU)
mydfChrSizeHolo<-read.table(text=
"            OTU chrName chrSize  
1 \"Species one\"   1     6.5      
2 \"Species one\"   2     5.0      
3 \"Species one\"   3     4.0    
4 \"Species one\"   X     3.0    "  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F-------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(mydfChrSizeHolo) , full_width = F
                           , font_size = 10)

## ---- eval=FALSE---------------------------------------------------------
#  setwd("~/folder/subfolder")

## ---- eval=FALSE---------------------------------------------------------
#  mydfChrSize<-read.csv("somefile.csv")

## ---- eval=FALSE---------------------------------------------------------
#  colnames(mydfChrSize)<-c("OTU", "chrName","chrSize")

## ---- eval=FALSE---------------------------------------------------------
#  packageVersion("idiogramFISH")

## ------------------------------------------------------------------------
# From scratch:
mydfMarkColor<-read.table(text=
"  markName markColor  style
1       5S       red   dots
2      45S     green square
3     DAPI      blue square
4      CMA    yellow square"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F-------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(mydfMarkColor) , full_width = F
                           , font_size = 10)

## ---- eval=FALSE---------------------------------------------------------
#  colnames(mydfMarkColor)<-c("markName", "markColor","style") # if style column not present it will be filled with "square"

## ------------------------------------------------------------------------
# We will use column OTU if data.frame because chromosome size df has it
mydfMarkPosHolo<-read.table(text=
"             OTU  chrName markName markPos markSize
1 \"Species one\"       3       5S     1.0      0.5
2 \"Species one\"       3     DAPI     2.0      0.5
3 \"Species one\"       1      45S     2.0      0.5
4 \"Species one\"       2     DAPI     2.0      0.5
5 \"Species one\"       4      CMA     2.0      0.5
6 \"Species one\"       4       5S     0.5      0.5"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F-------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(mydfMarkPosHolo) , full_width = F
                           , font_size = 10)

## ---- eval=FALSE---------------------------------------------------------
#  colnames(mydfMarkColor)<-c("OTU", "chrName","markName","markPos","markSize")

## ----parinit, echo=FALSE-------------------------------------------------
opar <- par(no.readonly = TRUE)      # make a copy of current settings
on.exit(suppressWarnings(par(opar)) )

## ----example, echo=TRUE, results="hide", fig.width=6, fig.height=3.5, message=FALSE----
# library(idiogramFISH)
par(mar=c(1,4,1,1)) # bottom left top right

plotIdiogramsHolo(dfChrSize=mydfChrSizeHolo,  # data.frame of chr. sizes
                  dfMarkColor=mydfMarkColor,  # df of mark style
                  dfMarkPos=mydfMarkPosHolo,  # df of mark positions
                  karHeight = 1.4,            # vertical size of karyotype plus spacing
                  addOTUName=FALSE,           # add OTU names
                  
                  chrWidth = 2,               # width of chromosomes
                  chrSpacing = 2,             # space among chromosomes
                  indexIdTextSize=1,          # font size of chr. name and indices 
                  
                  dotRoundCorr=2,             # correction factor for roundness of dots
                  legend="aside" ,            # position of legend, not "inline"
                  markLabelSize=1,            # font size of legend
                  
                  rulerNumberSize=1,          # font size of ruler
                  rulerPos=-2.2,              # position of ruler
                  ruler.tck=-0.03,            # size and orient. of tick of ruler
                  rulerNumberPos=.9,          # ruler's number position

                  xlimLeftMod=2.2,            # modify xlim left argument
                  xlimRightMod=10,            # modify xlim right argument
                  ylimBotMod=.1               # modify ylim bottom argument
                  )

## ---- eval=FALSE---------------------------------------------------------
#  unique(dfMarkPosHolo$markName)

## ---- eval=FALSE---------------------------------------------------------
#  par(mar=c(1,4,1,1))
#  plotIdiogramsHolo(dfChrSize=dfChrSizeHolo,
#                    dfMarkPos=dfMarkPosHolo,
#                    mycolors = c("green","yellow","blue","red"),  # optional
#                    dotRoundCorr=2.5, chrWidth=2.5,
#                    indexIdTextSize=1,
#                    legend="aside" ,markLabelSize=1,
#                    addOTUName=F,
#                    rulerNumberSize=1, rulerPos=-.7,ruler.tck=-0.04,rulerNumberPos=.9,
#                    xlimLeftMod=1,  xlimRightMod=10, ylimBotMod=.2
#  )

## ------------------------------------------------------------------------
data(bigdfChrSizeHolo)

## ---- echo=F-------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(bigdfChrSizeHolo) , full_width = F
                           , font_size = 10)

## ------------------------------------------------------------------------
data(dfMarkColor) 

## ---- echo=F-------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(dfMarkColor) , full_width = F
                           , font_size = 10)

## ------------------------------------------------------------------------
data(bigdfMarkPosHolo)

## ---- echo=F-------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(bigdfMarkPosHolo) , full_width = F
                           , font_size = 10)

## ----example3, echo=TRUE, results="hide", fig.width=6, fig.height=6, message=FALSE----
library(idiogramFISH)
par(mar=c(1,1,1,1))

plotIdiogramsHolo(dfChrSize=bigdfChrSizeHolo, # df of chr. sizes
                  dfMarkColor=dfMarkColor,    # df of mark style
                  dfMarkPos=bigdfMarkPosHolo, # df of marks' position
                  
                  MarkDistanceType="cen",     # distance to mark (center of mark)
                  roundness=6,                # vertices roundness of chr. and marks 
                  dotRoundCorr=1,             # correction of dots roundness
                  
                  karHeight = 1,              # rel. karyotype height
                  karSpacing = 1.7,           # karyotype height including spacing
                  reduDistKar=FALSE,          # reduce vertical space among karyotypes of karSpacing
                  
                  chrId="simple",             # numbering of chr., not using "original" name
                  chrWidth = 2,               # chr. width
                  chrSpacing = 2,             # space among chr.
                  
                  indexIdTextSize=.9,         # font size of chr names and indices
                  markLabelSize=.9,           # font size of legends
                  rulerNumberSize=.9,         # font size of ruler
                  OTUTextSize=1.2,            # font size of OTUs' names
                  
                  xlimRightMod=2,             # modify xlim right argument
                  ylimBotMod=.4)              # modify ylim bottom argument

## ------------------------------------------------------------------------
# using previous data.frames
bigdfChrSizeHolo$chrSize<-bigdfChrSizeHolo$chrSize*980000
bigdfMarkPosHolo$markPos<-bigdfMarkPosHolo$markPos*980000
bigdfMarkPosHolo$markSize<-bigdfMarkPosHolo$markSize*980000

## ----example4, echo=TRUE, results="hide", fig.width=6, fig.height=6, message=FALSE----

par(mar=c(1,1,1,1))

plotIdiogramsHolo(dfChrSize=bigdfChrSizeHolo,    # df of chr. sizes
                  dfMarkColor=dfMarkColor,       # df of mark style
                  dfMarkPos=bigdfMarkPosHolo,    # df of mark positions
                  
                  MarkDistanceType="cen",        # distance to mark is to its center
                  Mb=TRUE, # <- THIS IS NEW      # distances provided are in Mbs
                  ylabline = -1, # <-THIS IS NEW # if Mb=TRUE modifies position of y axis title (Mb)
                  roundness=6,                   # vertices roundness of chr. and marks 
                  dotRoundCorr=1,                # correction of dots roundness
                  
                  karHeight = 1,                 # rel. karyotype height
                  karSpacing = 1.7,              # karyotype height including spacing
                  reduDistKar=FALSE,             # reduce spacing in karSpacing
                  
                  chrId="simple",                # chr. names not "original"
                  chrWidth = 2,                  # chr. width
                  chrSpacing = 2,                # space among chr.
                  indexIdTextSize=.9,            # font size of chr names and indices
                  karIndex = FALSE,              # add karyotype asymmetry index
                  
                  markLabelSize=.9,              # font size of legend
                  rulerNumberSize=.9,            # font size of ruler
                  rulerPos = 0,                  # position of ruler
                  OTUTextSize=1.2,               # font size of OTU names

                  xlimRightMod=2,                # modify right argument of xlim
                  xlimLeftMod = 3,               # modify left argument of xlim
                  ylimBotMod=.4)                 # modify bottom argument of ylim

## ----returntooldpar, echo=FALSE------------------------------------------
suppressWarnings(par(opar) )

