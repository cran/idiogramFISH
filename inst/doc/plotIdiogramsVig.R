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
mydfChrSize<-read.table(text=
"            OTU chrName shortArmSize longArmSize 
1 \"Species one\"   1     1.5         2.0  
2 \"Species one\"   2     2.0         2.5  
3 \"Species one\"   3     1.0         1.5
4 \"Species one\"   X     2.0         3.5"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F-------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(mydfChrSize) , full_width = F
                           , font_size = 10)

## ---- eval=FALSE---------------------------------------------------------
#  setwd("~/folder/subfolder")

## ---- eval=FALSE---------------------------------------------------------
#  mydfChrSize<-read.csv("somefile.csv")

## ---- eval=FALSE---------------------------------------------------------
#  colnames(mydfChrSize)<-c("OTU", "chrName","shortArmSize","longArmSize")

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
                           , font_size = 10
                          # , bootstrap_options = c("striped", "hover", "condensed") 
                          )

## ---- eval=FALSE---------------------------------------------------------
#  colnames(mydfMarkColor)<-c("markName", "markColor","style")

## ------------------------------------------------------------------------
# We will use column OTU if data.frame because chromosome size df has it
mydfOfMarks<-read.table(text=
"            OTU chrName markName markArm markSize markDistCen
1 \"Species one\"      1       5S       p      0.5         0.5
2 \"Species one\"      1      45S       q        1         0.5
3 \"Species one\"      X      45S       p        1         1.0
4 \"Species one\"      3     DAPI       q        1         1.0"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F-------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(mydfOfMarks) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed")
                          )

## ---- eval=FALSE---------------------------------------------------------
#  colnames(mydfMarkColor)<-c("OTU", "chrName","markName","markArm","markSize","markDistCen")

## ------------------------------------------------------------------------
# We will use column OTU because data.frame of chromosome size has it
mydfOfCenMarks<-read.table(text=
"             OTU chrName markName
1  \"Species one\"     1     DAPI
2  \"Species one\"     X      CMA"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F-------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(mydfOfCenMarks) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed")
                          )

## ----example_M1, echo=TRUE, results="hide", fig.width=7, fig.height=4.5, message=FALSE----
# fig.width=7, fig.height=4.5

plotIdiograms(dfChrSize=mydfChrSize,      # chr. size data.frame
              dfMarkPos=mydfOfMarks,      # mark position df (not cen.)
              dfCenMarks=mydfOfCenMarks,  # cen. marks df
              dfMarkColor=mydfMarkColor,  # mark style df
              roundness=3,                # vertices roundness  
              dotRoundCorr=2,             # correction of roundness of dots 
              
              chrWidth=4.5,               # width of chr.
              chrSpacing = 4,             # space among chr.
              karSpacing=1.6,             # vertical size of karyotype including spacer
              
              indexIdTextSize=.7,         # font size fo chr name and indices
              OTUTextSize=.7,             # font size of OTUs
              markLabelSize=.7,           # font size of mark legends
              
              rulerPos=-1.9,              # ruler position
              ruler.tck=-0.02,            # ticks of ruler size and orientation
              rulerNumberPos=.5,          # position of numbers in ruler
              rulerNumberSize=.7,         # font size of ruler numbers
              
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
)

## ----example_M1cen0, echo=TRUE, results="hide", fig.width=7, fig.height=4.5, message=FALSE----
# fig.width=7, fig.height=4.5

plotIdiograms(dfChrSize=mydfChrSize,      # chr. size df
              dfMarkPos=mydfOfMarks,      # mark position df (not cen.)
              dfMarkColor=mydfMarkColor,  # mark style df

              # dfCenMarks=mydfOfCenMarks,  # cen. marks df, NOT AVAILABLE FOR centromereSize = 0
              centromereSize = 0,         # <- HERE
              
              roundness=3,                # vertices roundness  
              dotRoundCorr=2,             # correction of roundness of dots 
              chrWidth=4.5,               # width of chr.
              chrSpacing = 4,             # space among chr.
              karSpacing=1.6,             # vertical size of karyotype including spacer
              indexIdTextSize=.7,         # font size fo chr name and indices
              OTUTextSize=.7,             # font size of OTUs
              markLabelSize=.7,           # font size of mark legends
              rulerPos=-1.9,              # ruler position
              ruler.tck=-0.02,            # ticks of ruler size and orientation
              rulerNumberPos=.5,          # position of numbers in ruler
              rulerNumberSize=.7,         # font size of ruler numbers
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
)

## ---- eval=FALSE---------------------------------------------------------
#  unique(c(dfOfMarks$markName,dfOfCenMarks$markName) )

## ---- eval=FALSE---------------------------------------------------------
#  charVectorCol <- c("tomato3","darkolivegreen4","dfsd","blue","green")
#  
#  plotIdiograms(dfChrSize=dfOfChrSize,
#                dfMarkPos=dfOfMarks,
#                chrColor = "gray",
#                cenColor = "gray",
#                dfCenMarks=dfOfCenMarks,
#  
#                mycolors = charVectorCol,
#  
#                dotRoundCorr=2, chrWidth=2.5, chrSpacing = 2.5,
#                karSpacing=1.6,
#                indexIdTextSize=1,
#                markLabelSize=1,
#                rulerPos=-1.9, ruler.tck=-0.02, rulerNumberPos=.5, rulerNumberSize=1
#  )

## ------------------------------------------------------------------------
data(bigdfOfChrSize)

## ---- echo=F-------------------------------------------------------------
# kableExtra::scroll_box(height = "300px", width="400px",
           # kableExtra::add_header_above(
                            kableExtra::kable_styling(knitr::kable(bigdfOfChrSize) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed"),
                 
) 
# ) 
# )

## ------------------------------------------------------------------------
data(bigdfOfMarks)

## ---- echo=F-------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(dfMarkColor) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed")
                          )

## ------------------------------------------------------------------------
data("bigdfOfMarks")

## ---- echo=F-------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(bigdfOfMarks) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed")
                          )

## ------------------------------------------------------------------------
data("bigdfDataCen")

## ---- echo=F-------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(bigdfDataCen) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed")
                          )

## ----example_M3, echo=TRUE, results="hide", fig.width=6, fig.height=13, message=FALSE----
# library(idiogramFISH)

plotIdiograms(dfChrSize=bigdfOfChrSize,  # chr sizes
              dfMarkColor=dfMarkColor,   # mark characteristics, optional in dev version. see above. 
              dfMarkPos=bigdfOfMarks,    # mark positions (no cen. marks)
              dfCenMarks=bigdfDataCen,   # cen. marks df
              karHeight=1.2,             # karyotype rel. height
              karSpacing=2.2,            # karyotype vertical size with spacing
              
              dotRoundCorr = .5,         # correction factor for dot marks
              distTextChr=.5,            # distance of chr. to text
              indexIdTextSize=.7,        # font size of indices and chr. name
              OTUTextSize=.9,            # font size of OTU names
              legend="aside",            # position of legend not "inline"
              markLabelSize=.7,          # font size of legend
              markLabelSpacer=1,         # distance from chr. to legend
              morpho=FALSE,              # add chr. morphology
              
              ruler=TRUE,                # add ruler
              rulerPos=-.9,              # position of ruler
              rulerPosMod=3,             # modify position of ruler
              ruler.tck=-0.004,          # size and orient. of ticks in ruler
              rulerNumberPos=.4,         # position of numbers of ruler
              rulerNumberSize=.4,        # font size of ruler
              
              xlimRightMod=2,            # modify xlim right argument
              ylimBotMod = 0,            # modify ylim bottom argument
              ylimTopMod = -.3           # modify ylim top argument
              )

