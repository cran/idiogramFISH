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
#  # if style column is not present it will be filled with "square"

## ------------------------------------------------------------------------
# We will use column OTU if data.frame because chromosome size df has it
mydfOfMarks<-read.table(text=
"            OTU chrName markName markArm markSize markDistCen
1 \"Species one\"      1       5S       p      0.5         0.5
2 \"Species one\"      1      45S       q        1         0.5
3 \"Species one\"      X      45S       p        1         1.0
4 \"Species one\"      3     DAPI       q        1         1.0
5 \"Species one\"      1     DAPI      cen
6 \"Species one\"      X      CMA      cen", header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F-------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(mydfOfMarks) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed")
                          )

## ---- eval=FALSE---------------------------------------------------------
#  colnames(mydfMarkColor)<-c("OTU", "chrName","markName","markArm","markSize","markDistCen")

## ----example_M1, echo=TRUE, results="hide", fig.width=7, fig.height=4.5, message=FALSE,dev='svg'----
# fig.width=7, fig.height=4.5

svg("mydfChrSize.svg",width=13,height=8 )
# png("mydfChrSize.png", width=600, height=400)
plotIdiograms(dfChrSize=mydfChrSize,      # chr. size data.frame
              dfMarkPos=mydfOfMarks,      # mark position df (inc. cen.)

              dfMarkColor=mydfMarkColor,  # mark style df
              roundness=3,                # vertices roundness  
              dotRoundCorr=2.5,           # correction of roundness of dots 
              
              chrWidth=4.5,               # width of chr.
              chrSpacing = 4,             # space among chr.
              karHeiSpace=1.6,            # vertical size of karyotype including spacer
              
              indexIdTextSize=1,          # font size fo chr name and indices
              OTUTextSize=1.2,            # font size of OTUs
              markLabelSize=1,            # font size of mark legends
              
              rulerPos=-1.9,              # ruler position
              ruler.tck=-0.02,            # ticks of ruler size and orientation
              rulerNumberPos=.5,          # position of numbers in ruler
              rulerNumberSize=1,          # font size of ruler numbers
              
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
              
              # ,xlimRightMod = 12
              # ,legend="aside"           # <- TRY THIS
              # ,legendWidth = 1.2
              # ,markLabelSpacer = 4
)
dev.off()

## ---- results="asis", comment=NA, echo=FALSE-----------------------------
# cat(paste0("![](mydfChrSize.png)" ) )
cat(paste0("![](mydfChrSize.svg)" ) )

## ----example_M1cen0, echo=TRUE, results="hide", fig.width=7, fig.height=4.5, message=FALSE,dev='svg'----
# fig.width=7, fig.height=4.5

png("mydfChrSize2.png", width=600, height=400)
plotIdiograms(dfChrSize   = mydfChrSize,  # chr. size df
              dfMarkColor = mydfMarkColor,# mark style df
              dfMarkPos   = mydfOfMarks,  # mark position df
              
              # cen. marks NOT AVAILABLE for centromereSize = 0
              centromereSize = 0,         # <- HERE
              
              roundness=3,                # vertices roundness  
              dotRoundCorr=2.5,           # correction of roundness of dots 
              chrWidth=4.5,               # width of chr.
              chrSpacing = 4,             # space among chr.
              karHeiSpace=1.6,            # vertical size of karyotype including spacer
              indexIdTextSize=.7,         # font size fo chr name and indices
              OTUTextSize=.7,             # font size of OTUs
              markLabelSize=.7,           # font size of mark legends
              rulerPos=-1.9,              # ruler position
              ruler.tck=-0.02,            # ticks of ruler size and orientation
              rulerNumberPos=.5,          # position of numbers in ruler
              rulerNumberSize=.7,         # font size of ruler numbers
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
              
              # ,markLabelSpacer=5
              # ,xlimRightMod = 16        #   TRY THIS
              # ,legend="aside"
              # ,legendWidth = 1.5
)
dev.off()

## ---- results="asis", comment=NA, echo=FALSE-----------------------------
cat(paste0("![](mydfChrSize2.png)" ) )

## ---- eval=FALSE---------------------------------------------------------
#  unique(c(dfOfMarks$markName,dfOfCenMarks$markName) )

## ---- eval=FALSE, dev='svg'----------------------------------------------
#  
#  charVectorCol <- c("tomato3","darkolivegreen4","dfsd","blue","green")
#  png("dfOfChrSize.png", width=600, height=400)
#  par(mar=rep(0,4))
#  
#  plotIdiograms(dfChrSize = dfOfChrSize,     # d.f. of chr. sizes
#                dfMarkPos = dfOfMarks2,      # d.f. of marks' positions
#                chrColor  = "gray",          # chr. color
#                cenColor  = "gray",          # cen. color
#  
#                mycolors = charVectorCol,    # colors to use
#  
#                OTUTextSize = 1,             # font size for OTU names
#                dotRoundCorr=2,              # correction of roundness of vert. and dots
#                chrWidth=2.5,                # rel. chr. width
#                chrSpacing = 2.5,            # rel. horizontal chr. spacing
#                karHeiSpace=1.6,             # karyotype height including spacing
#                indexIdTextSize=1,           # font size for chr. indexes and chr. name
#                markLabelSize=1,             # font size for labels (legend)
#  
#                rulerPos=-1.9,               # ruler position
#                ruler.tck=-0.02,             # ruler tick orientation and length
#                rulerNumberPos=.5,           # rulers' numbers position
#                rulerNumberSize=1            # ruler font size
#  )
#  dev.off()

## ---- eval=F-------------------------------------------------------------
#  head(bigdfOfChrSize,30)

## ---- echo=F-------------------------------------------------------------
# kableExtra::scroll_box(height = "300px", width="400px",
           # kableExtra::add_header_above(
                            kableExtra::kable_styling(knitr::kable(head(bigdfOfChrSize,30)) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed"),
                 
) 
# ) 
# )

## ---- width=45-----------------------------------------------------------
data("dfMarkColor")

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

## ----example_M3, echo=TRUE, results="hide", fig.width=6, fig.height=13, message=FALSE, dev='png'----
# fig.width=6, fig.height=13
# png("bigdfOfChrSize.png", width=650, height=1300)
plotIdiograms(dfChrSize  =bigdfOfChrSize,# chr sizes
              dfMarkColor=dfMarkColor,   # mark characteristics, optional in dev version. see above. 
              dfMarkPos  =bigdfOfMarks,  # mark positions (inc. cen. marks)
                                         # cen. marks in bigdfOfMarks
              karHeight=1.2,             # karyotype rel. height
              karHeiSpace=2.2,           # karyotype vertical size with spacing
              karSepar = TRUE,           # modify vertical separation of kar.
              amoSepar = 1.2,            # Vertical separation of kar. when karSpear = TRUE
              
              dotRoundCorr = .5,         # correction factor for dot marks
              distTextChr=.5,            # distance of chr. to text
              morpho=FALSE,              # add chr. morphology
              indexIdTextSize=.6,        # font size of indices and chr. name
              OTUTextSize=.9,            # font size of OTU names
              
              legend="aside",            # position of legend not "inline"
              markLabelSize=.7,          # font size of legend
              markLabelSpacer=1,         # distance from chr. to legend
              legendHeight = 2,          # height of labels
              
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
# dev.off() # for png()

## ---- results="asis", comment=NA, echo=FALSE, eval=FALSE-----------------
#  cat(paste0("![](bigdfOfChrSize.png)" ) )

