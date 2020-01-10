## ---- results="asis", echo=FALSE, message=FALSE-------------------------------
# <!-- pkgdown --> 
# <!-- jquery --><script src="js/jquery.min.js" crossorigin="anonymous"></script>
myfile<-"js/jquery.min.js"
if(file.exists(myfile)){
cat(paste0('<script src="',myfile,'" crossorigin="anonymous"></script> <!-- # -->'))
}
# <!-- clipboard.js --><script src="js/clipboard.min.js"  crossorigin="anonymous"></script>
myfile<-"js/clipboard.min.js"
if(file.exists(myfile)){
cat(paste0('<script src="',myfile,'"crossorigin="anonymous"></script>'))
}
# <!-- Font Awesome icons --><link rel="stylesheet" href="css/all.minMod.css"  crossorigin="anonymous">
myfile<-"css/all.minMod.css"
if(file.exists(myfile)){
cat(paste0('<link rel="stylesheet" href="',myfile,'"  crossorigin="anonymous">'))
}
# <!-- Bootstrap --><link rel="stylesheet" href="css/bootstrap.minO.css" crossorigin="anonymous">
myfile<-"css/bootstrap.minO.css"
if(file.exists(myfile)){
cat(paste0('<link rel="stylesheet" href="',myfile,'"  crossorigin="anonymous">'))
}
# <!-- # <script src="js/bootstrap.min.js"  crossorigin="anonymous"></script> -->
myfile<-"js/bootstrap.min.js"
if(file.exists(myfile)){
cat(paste0('<script src="',myfile,'" crossorigin="anonymous"></script> <!-- # -->'))
}
myfile<-"js/pkgdown2.js"
if(file.exists(myfile)){
cat(paste0('<script src="',myfile,'"></script> <!-- # -->'))
}

## ---- results="hide", message=FALSE, warning=FALSE, eval=TRUE-----------------

#load package
library(idiogramFISH) 

## -----------------------------------------------------------------------------
# Example data.frame written in R, use: (column OTU is optional if only 1 OTU)
mydfChrSizeHolo<-read.table(text=
"            OTU chrName chrSize  
\"Species one\"   1     6.5      
\"Species one\"   2     5.0      
\"Species one\"   3     4.0
\"Species one\"   4     4.0
\"Species one\"   X     3.0    "  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)


## ---- echo=F------------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(mydfChrSizeHolo) , full_width = F
                           , font_size = 10)

## ---- eval=FALSE--------------------------------------------------------------
#  setwd("~/folder/subfolder")

## ---- eval=FALSE--------------------------------------------------------------
#  mydfChrSize<-read.csv("somefile.csv")

## ---- eval=FALSE--------------------------------------------------------------
#  colnames(mydfChrSize)<-c("OTU", "chrName","chrSize")

## ---- eval=FALSE--------------------------------------------------------------
#  packageVersion("idiogramFISH")

## -----------------------------------------------------------------------------
# From scratch:
mydfMarkColor<-read.table(text=
"  markName markColor  style
1       5S       red   dots
2      45S     green square
3     DAPI      blue square
4      CMA    yellow square"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F------------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(mydfMarkColor) , full_width = F
                           , font_size = 10)

## ---- eval=FALSE--------------------------------------------------------------
#  colnames(mydfMarkColor)<-c("markName", "markColor","style")
#  # if style column not present it will be filled with "square"

## -----------------------------------------------------------------------------
# We will use column OTU if data.frame because chromosome size df has it
mydfMarkPosHolo<-read.table(text=
"             OTU  chrName markName markPos markSize chrRegion
\"Species one\"       4        B      NA       NA        w           # whole chromosome mark, use 'w' in col. chrRegion 
\"Species one\"       3     DAPI     2.0      0.5
\"Species one\"       1      45S     2.0      0.5
\"Species one\"       2     DAPI     2.0      0.5
\"Species one\"       X      CMA     2.0      0.5
\"Species one\"       X       5S     0.5      0.5
\"Species one\"       X       5S     0.5      0.5"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F------------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(mydfMarkPosHolo) , full_width = F
                           , font_size = 10)

## ---- eval=FALSE--------------------------------------------------------------
#  colnames(mydfMarkColor)<-c("OTU", "chrName","markName","markPos","markSize")

## ----parinit, echo=FALSE------------------------------------------------------
opar <- par(no.readonly = TRUE)      # make a copy of current settings
on.exit(suppressWarnings(par(opar)) )

## ----example, echo=TRUE, results="hide", fig.width=13.5, fig.height=8, message=FALSE, dev='png'----
# library(idiogramFISH)
# svg("mydfChrSizeHolo.svg",width=13.5,height=8 )
# png("mydChrSizeHolo.png", width=600, height=400)
par(mar=c(0,4,1,1)) # bottom left top right

# function `plotIdiogramsHolo` deprecated after ver. > 1.5.1

plotIdiograms(dfChrSize  = mydfChrSizeHolo,# data.frame of chr. sizes
              dfMarkColor= mydfMarkColor,  # df of mark style
              dfMarkPos  = mydfMarkPosHolo,# df of mark positions
              addOTUName=FALSE,            # add OTU names
              
              rulerPos=-1,                 # position of ruler
              ruler.tck=-0.01,             # size and orient. of tick of ruler
              rulerNumberPos=.9,           # ruler's number position
              
              xlimLeftMod=2.2,             # modify xlim left argument
              ylimBotMod=-2                # modify ylim bottom argument
              ,legendWidth=1               # width of legend
              ,legendHeight=.7             # height of legend item 
              ,asp=1                       # y x aspect
)
# dev.off() # closes png or svg

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
# cat(paste0("![](mydChrSizeHolo.png)" ) )
# cat(paste0("![](mydfChrSizeHolo.svg)" ) )

## ---- eval=FALSE,dev='svg'----------------------------------------------------
#  unique(dfMarkPosHolo$markName)

## ---- eval=FALSE--------------------------------------------------------------
#  par(mar=c(1,4,1,1))
#  # function plotIdiogramsHolo was deprecated
#  plotIdiograms(dfChrSize = dfChrSizeHolo, # d.f. of chr. size
#                dfMarkPos  = dfMarkPosHolo, # d.f. of marks' positions
#                mycolors   = c("green","yellow","blue","red"),  # colors for marks
#  
#                addOTUName=F,               # add OTU name
#                rulerPos=-.7,               # ruler position
#                ruler.tck=-0.01,            # ruler ticks size and orient.
#                rulerNumberPos=.9,          # position of numbers of ruler
#                xlimLeftMod=1,              # modify left xlim arg.
#                xlimRightMod=3,             # modify right xlim arg.
#                ylimBotMod=.2               # modify bottom ylim
#                ,asp=1                      # y x aspect
#  )

## -----------------------------------------------------------------------------
data(bigdfChrSizeHolo)

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(bigdfChrSizeHolo) , full_width = F
                           , font_size = 10, position = "center")

## -----------------------------------------------------------------------------
data(dfMarkColor) 

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(dfMarkColor) , full_width = F
                           , font_size = 10)

## -----------------------------------------------------------------------------
data(bigdfMarkPosHolo)

## ---- echo=F------------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(bigdfMarkPosHolo) , full_width = F
                           , font_size = 10)

## ----example3, echo=TRUE, results="hide", fig.width=6, fig.height=6, message=FALSE,dev='svg'----
library(idiogramFISH)

# fig.width=6, fig.height=6
png("bigdfChrSizeHolo.png", width=600, height=600)
par(mar=c(1,1,1,1))

plotIdiograms(dfChrSize=bigdfChrSizeHolo, # df of chr. sizes
              dfMarkColor=dfMarkColor,    # df of mark style
              dfMarkPos=bigdfMarkPosHolo, # df of marks' position
              
              markDistType="cen",         # measure towards center of mark
              roundness=6,                # vertices roundness of chr. and marks 
              
              karHeiSpace = 4,            # karyotype height including spacing
              karSepar=TRUE,              # reduce vertical space among karyotypes 
              amoSepar = 1,               # separation among karyotypes
              distTextChr=.5,             # distance text to chr.
              
              legendWidth = 1             # width of legend labels
              
              ,chrId="simple",            # numbering of chr., not using "original" name
              
              indexIdTextSize=.9,         # font size of chr names and indices
              markLabelSize=.9,           # font size of legends
              rulerNumberSize=.9,         # font size of ruler
              ruler.tck= -.004,           # tick length and orient.
              
              ylimBotMod=.4               # modify ylim bottom argument
              ,asp=1                      # y x aspect
)
dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
cat(paste0("![](bigdfChrSizeHolo.png)" ) )

## -----------------------------------------------------------------------------
# transform previous data.frames for simplicity
bigdfChrSizeHoloMb <- bigdfChrSizeHolo
bigdfChrSizeHoloMb$chrSize <- bigdfChrSizeHoloMb$chrSize * 980000
bigdfMarkPosHoloMb <- bigdfMarkPosHolo
bigdfMarkPosHoloMb$markPos <- bigdfMarkPosHoloMb$markPos * 980000
bigdfMarkPosHoloMb$markSize<- bigdfMarkPosHoloMb$markSize * 980000

## ----example4, echo=TRUE, results="hide", fig.width=6, fig.height=6, message=FALSE,dev='svg'----

png("bigdfChrSizeHolo2.png", width=700, height=600)
par(mar=c(1,1,1,1))

plotIdiograms(dfChrSize=bigdfChrSizeHoloMb,  # df of chr. sizes
              dfMarkColor=dfMarkColor,       # df of mark style
              dfMarkPos=bigdfMarkPosHoloMb,  # df of mark positions
              
              markDistType="cen",            # distance to mark is to its center
              ylabline = -8, # <- NEW        # modifies position of y axis title (Mb)
              roundness=4,                   # vertices roundness of chr. and marks 
              distTextChr = .5,              # separ. chr. to text
              
              karHeight = 2,                 # rel. karyotype height
              karHeiSpace = 4,               # karyotype height including spacing
              karSepar=TRUE,                 # reduce spacing among karyotypes 
              amoSepar = 1,                  # depends on karSepar, amount of sep.
              
              chrId="simple",                # chr. names not "original"
              indexIdTextSize=.9,            # font size of chr names and indices
              karIndex = FALSE,              # do not add karyotype asymmetry index
              
              markLabelSize=.9,              # font size of legend
              rulerNumberSize=.9,            # font size of ruler
              rulerPos = 0,                  # position of ruler
              ruler.tck= -.004,              # ruler tick length and orient.
              
              legendWidth = 1.2,             # width of legends
              
              xlimLeftMod = 1,               # modify left argument of xlim
              ylimBotMod=.4                  # modify bottom argument of ylim
              ,asp=1)                        # y x aspect
dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
cat(paste0("![](bigdfChrSizeHolo2.png)" ) )

## ----returntooldpar, echo=FALSE-----------------------------------------------
suppressWarnings(par(opar) )

## ---- comment=NA, echo=F------------------------------------------------------
cat(paste0("parentalAndHybHoloChrSize" ) )

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(parentalAndHybHoloChrSize) , full_width = F
                           , font_size = 10)

## ---- comment=NA, echo=F------------------------------------------------------
cat(paste0("dfAlloParentMarksHolo" ) )

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(dfAlloParentMarksHolo) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed")
                          )

## ----example_M1, echo=TRUE, results="hide", fig.width=8, fig.height=7, message=FALSE,dev='png'----

# svg("gish.svg",width=8,height=7 )
plotIdiograms(dfChrSize = parentalAndHybHoloChrSize,  # d.f. of chr. sizes
              dfMarkPos = dfAlloParentMarksHolo,      # d.f. of marks' positions
              chrColor  = "gray",          # chr. color
              cenColor  = NULL,            # cen. color when GISH
              
              karHeight = 3,               # karyotype heigth without spacing
              karHeiSpace=5,               # karyotype height including spacing
              distTextChr = 0.8,           # separation among chr. and text
              
              rulerPos=-1.8,               # ruler position
              ruler.tck=-0.01,             # ruler tick orientation and length
              rulerNumberSize=0.7          # ruler font size
              ,legend=""                   # no legend
              
              ,xlimRightMod = 0            # xlim right arg. modif.
              ,asp=1                       # y x aspect
)
# dev.off()

