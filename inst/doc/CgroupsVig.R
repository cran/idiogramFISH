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
# Example data.frame written in R, use
dfwithgroups<-read.table(text="
      chrName shortArmSize longArmSize group
1        1            3           5     1
2        1            3.2         5.5   1
3        1            3.5         4.8   1
4        4            1           3     NA
5        5            3           5     NA
6        X            4           6     NA", header=T, stringsAsFactors=F)

## ---- echo=F-------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(dfwithgroups) , full_width = F
                           , font_size = 10)

## ----example_G1, echo=TRUE, results="hide", fig.width=6, fig.height=5, message=FALSE,dev='png'----

plotIdiograms(dfChrSize=dfwithgroups, # chr sizes
              orderBySize = TRUE,     # order chr. by size
              chrColor = "gray",      # chr. color  
              cenColor = "gray",      # centromere color
              chrWidth=2.5,           # chromosome width
              chrSpacing = 2.5,       # space among chromosomes
              karHeiSpace=1,          # vertical size of karyotypes
              karHeight = 1,          # vertical size of karyotypes including spacing
              
              distTextChr = 0.6,      # distance from chr. to its name
              chrId="original",       # chr. names as in you data.frame
              indexIdTextSize=1,      # font size of chr. name and indices
              chrIndex = FALSE,       # do not add chr. indices 
              morpho=FALSE,           # do not add chr. morphology
              karIndex = FALSE,       # do not add karyotype indices
              
              rulerPos=-2.2,          # position of ruler
              ruler.tck=-0.02,        # size and orientation of tick of ruler
              rulerNumberPos=.5,      # position of numbers of ruler
              rulerNumberSize=.7,     # font size of ruler

              ylimBotMod = 1,         # modify ylim bottom argument
              xlimRightMod = 0        # modify xlim right argument
)

## ------------------------------------------------------------------------
dfwithHetero<-read.table(text="
       chrName shortArmSize longArmSize group
1        1A           3           5     1
2        1B           3           5     1
4        2            1           3     NA
5        3            3           5     NA
6        4            4           6     NA", header=TRUE, stringsAsFactors=FALSE)

## ---- echo=F-------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(dfwithHetero) , full_width = F
                           , font_size = 10)

## ------------------------------------------------------------------------
dfOfMarksHetero<-read.table(text=
"     chrName markName markArm markSize markDistCen
1       1A       5S       p        1         0.9
2       1B      45S       p        1         0.9
3       2       CMA       q        1         1.0
4       3      DAPI       q        1         1.0", header=TRUE, stringsAsFactors=FALSE)

## ---- echo=F-------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(dfOfMarksHetero) , full_width = F
                           , font_size = 10)

## ----example_G2, echo=TRUE, results="hide", fig.width=7, fig.height=4.5, message=FALSE, dev='png'----

svg("dfwithHetero.svg",width=13.5,height=8 )
plotIdiograms(dfChrSize=dfwithHetero,    # chr sizes
              dfMarkPos=dfOfMarksHetero, # position of marks
              chrColor = "gray",         # colors of chr.
              cenColor = "gray",         # colors of centromeres
              
              chrId="original",          # chr. name in df.
              indexIdTextSize=1,         # font size of chr. name and indices
              chrIndex = FALSE,          # do not add chr. indices
              morpho=FALSE,              # do not add chr. morphologies  
              karIndex = FALSE,          # do not add karyotype indices
              
              dotRoundCorr=2,            # correction factor of roundness of dots
              markLabelSize=1,           # font size of legends
              MarkDistanceType="cen",    # mark distance is to center of mark
              
              chrWidth=2.5,              # chr. width
              chrSpacing = 2.5,          # space among chr.
              orderBySize = FALSE,       # do not order chr. by size
              karHeiSpace=1.2,           # vertical size of karyotypes including spacing
              
              rulerPos=-1.9,             # position of rulers
              ruler.tck=-0.02,           # size and orientation of ticks of ruler
              rulerNumberPos=.5,         # position of numbers of ruler
              rulerNumberSize=1,         # font size of ruler numbers
              
              ylimBotMod = -.2,          # modify ylim bottom argument
              xlimRightMod = 7           # modify xlim right argument
              , legend="aside"
              , markLabelSpacer = 2
              , legendWidth = 1
)
dev.off()

## ---- results="asis", comment=NA, echo=FALSE-----------------------------
# cat(paste0("![](mydfChrSizeHolo.png)" ) )
cat(paste0("![](dfwithHetero.svg)" ) )

## ------------------------------------------------------------------------
data("dfChrSizeHolo")
data("dfMarkPosHolo")
dfMarkPosHoloHetero<-dfMarkPosHolo
dfMarkPosHoloHetero$chrName<-c(3,3,"1A",2,"1B","1B")
dfChrSizeHoloHetero<-dfChrSizeHolo
dfChrSizeHoloHetero$chrName<-c("1A","1B",2,3)

# Adding the group column
dfChrSizeHoloHetero$group<-c(1,1,NA,NA)

## ----example_G3, echo=TRUE, results="hide", fig.width=7, fig.height=4.5, message=FALSE,dev='png'----
plotIdiograms(dfChrSize=dfChrSizeHoloHetero, # df of chr sizes
              orderBySize = FALSE,           # do not order chr. by size
              dfMarkPos=dfMarkPosHoloHetero, # df of position of marks
              dotRoundCorr=3,                # correction of roundness of dots
              
              chrWidth = 2.3,                # chr. width
              chrSpacing = 2,                # chr. spacing
              karIndex = FALSE,              # do not add karyotype asymmetry index
              indexIdTextSize=1,             # font size of chr. name and indices
              karHeight = 1.4,               # vertical size of karyotypes
              addOTUName=FALSE,              # do not add name of OTUs
              distTextChr = 0.6,             # distance from chr. to its name
              
              legend="aside",                # position of legend to the right
              markLabelSize=1,               # font size of legend
              markLabelSpacer=4,             # space among chr. and legend
              
              rulerNumberSize=1,             # font size of ruler
              rulerPos=-2.2,                 # position of ruler
              ruler.tck=-0.03,               # size and orientation of ticks
              rulerNumberPos=.9,             # position of rulers' numbers
              
              xlimLeftMod=2.2,               # modify left argument of xlim
              xlimRightMod=15,               # modify right argument of xlim
              ylimBotMod=.3                  # modifiy bottom argument of ylim
              ,legendWidth=1                 # width of legend
)

## ------------------------------------------------------------------------
dfChrSizeHoloGroup<-data.frame(chrName=c(1,1,1,1,2,3,4), 
                               chrSize=c(3.1,3.2,3.3,3.4,4,5,6), 
                               group=c(1,1,1,1,NA,NA,NA) 
                               )

## ---- echo=F-------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(dfChrSizeHoloGroup) , full_width = F
                           , font_size = 10)

## ----example_G4, echo=TRUE, results="hide", fig.width=7, fig.height=4.5, message=FALSE,dev='png'----
plotIdiograms(dfChrSize=dfChrSizeHoloGroup, # data.frame of chr sizes
              orderBySize = FALSE,          # do not order chr. by size
              karIndex = FALSE,             # do not add karyotype indices
              chrWidth = 2,                 # chr. width
              chrSpacing = 2,               # space among chr.
              indexIdTextSize=1,            # font size of chr. names and indices
              OTUTextSize=1,                # font size of OTU names
              karHeight = 1.4,              # karyotype rel. height
              distTextChr = 0.6,            # distance from chr. to its name
              
              rulerNumberSize=1,            # font size of ruler
              rulerPos=-2.2,                # position of ruler
              ruler.tck=-0.03,              # size and orientation of ticks of ruler
              rulerNumberPos=.9,            # position of number of ruler
              
              xlimLeftMod=2.2,              # modify left argument of xlim
              xlimRightMod=10,              # modify right argument of xlim
              ylimBotMod=.3                 # modify bottom argument of ylim
)

