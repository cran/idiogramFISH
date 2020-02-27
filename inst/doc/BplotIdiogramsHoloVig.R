## ----setup, include=FALSE-----------------------------------------------------
#Create myheader.html
if(Sys.info()['sysname']=="Windows"){
res<-!as.logical(system(paste("ping", "www.google.com")) )
  if(res){
  fileConn <- file("myheader.html")
  writeLines('<script src="https://kit.fontawesome.com/af0a13599b.js" crossorigin="anonymous"></script>', fileConn)
  close(fileConn)
  }
} else {
  fileConn <- file("myheader.html")
  writeLines('<script src="https://kit.fontawesome.com/af0a13599b.js" crossorigin="anonymous"></script>', fileConn)
  close(fileConn)
}

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

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
# version of manual
v<-sub("Version: ","",readLines("../DESCRIPTION")[3])
# v<-tryCatch(suppressWarnings(rvcheck:::check_github_gitlab("ferroao/idiogramFISH", "gitlab")$latest_version), error=function(e) NA )
pkg<-"idiogramFISH"
link<-tryCatch(suppressWarnings(badger::badge_custom("Documentation", paste(pkg,v), "cornflowerblue") ), error=function(e) NA )
  if(!is.na(link)){
  svglink<-gsub("\\[|\\]|!|\\(|\\)","", link)
  manual_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svglink) ), error=function(e) NA )
    if (!is.na(manual_cont)){
    manual_contFile <- "../man/figures/manual.svg"
    writeLines(manual_cont, con = manual_contFile)
    manual_contFile <- normalizePath(manual_contFile)
    knitr::include_graphics(manual_contFile)
    # cat(paste0("&nbsp;![''](",knitr::include_graphics(manual_contFile),")" ) )
    }
  }
} # rcurl

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

## ----example, echo=TRUE, results="hide", fig.width=13.5, fig.height=6, message=FALSE, dev='png'----
# library(idiogramFISH)
# svg("mydfChrSizeHolo.svg",width=13.5,height=6 )
# png("mydChrSizeHolo.png", width=600, height=300)
par(mar=c(0,4,0,1)) # bottom left top right

# function `plotIdiogramsHolo` deprecated after ver. > 1.5.1

plotIdiograms(dfChrSize  = mydfChrSizeHolo,# data.frame of chr. sizes
              dfMarkColor= mydfMarkColor,  # df of mark style
              dfMarkPos  = mydfMarkPosHolo,# df of mark positions
              addOTUName=FALSE,            # add OTU names
              
              xlimLeftMod= 1,              # modify xlim left argument
              ylimTopMod= -1,              # modify ylim top argument
              ylimBotMod= -2               # modify ylim bottom argument
              ,rulerPos = 0
              ,ruler.tck = -0.01
              
              ,legendWidth=1               # width of legend
              ,legendHeight=.7             # height of legend item 
              #,asp=1                      # y x aspect
)
# dev.off() # closes png or svg

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
# cat(paste0("![](mydChrSizeHolo.png)" ) )
# cat(paste0("![](mydfChrSizeHolo.svg)" ) )

## ---- eval=FALSE--------------------------------------------------------------
#  unique(dfMarkPosHolo$markName)

## ---- eval=FALSE--------------------------------------------------------------
#  # par(mar=c(1,4,1,1))
#  par(mar=rep(0,4))
#  
#  plotIdiograms(dfChrSize = dfChrSizeHolo, # d.f. of chr. size
#                dfMarkPos  = dfMarkPosHolo, # d.f. of marks' positions
#                mycolors   = c("green","yellow","blue","red"),  # colors for marks
#  
#                addOTUName=FALSE,           # do not add OTU name
#                ruler=FALSE,                # do not add ruler
#                xlimLeftMod=1,              # modify left xlim arg.
#                xlimRightMod=3,             # modify right xlim arg.
#                ylimBotMod=.2               # modify bottom ylim
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

## ----example3, echo=TRUE, results="hide", fig.width=6, fig.height=6, message=FALSE,dev='png'----
library(idiogramFISH)

# fig.width=6, fig.height=6
png("bigdfChrSizeHolo.png", width=600, height=600)
# par(mar=c(1,1,1,1))
par(mar=rep(0,4)) 

plotIdiograms(dfChrSize=bigdfChrSizeHolo, # chr. size data.frame
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
              
              rulerPos=0,
              rulerNumberSize=.9,         # font size of ruler
              ruler.tck= -.004,           # tick length and orient.
              
              ylimBotMod=.4               # modify ylim bottom argument
              #,asp=1                     # y x aspect
)
dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
cat(paste0("![](bigdfChrSizeHolo.png)" ) )

## -----------------------------------------------------------------------------
# transform previous data.frames for simplicity
bigdfChrSizeHoloMb <- bigdfChrSizeHolo
bigdfChrSizeHoloMb$chrSize <- bigdfChrSizeHoloMb$chrSize * 98000000
bigdfMarkPosHoloMb <- bigdfMarkPosHolo
bigdfMarkPosHoloMb$markPos <- bigdfMarkPosHoloMb$markPos * 98000000
bigdfMarkPosHoloMb$markSize<- bigdfMarkPosHoloMb$markSize * 98000000

## ----example4, echo=TRUE, results="hide", fig.width=6, fig.height=6, message=FALSE,dev='png'----

png("bigdfChrSizeHolo2.png", width=700, height=600)
# par(mar=c(1,1,1,1))
par(mar=rep(0,4)) 

plotIdiograms(dfChrSize=bigdfChrSizeHoloMb,  # chr. size data.frame
              dfMarkColor=dfMarkColor,       # df of mark style
              dfMarkPos=bigdfMarkPosHoloMb,  # df of mark positions
              
              markDistType="cen",            # distance to mark is to its center
              roundness=4,                   # vertices roundness of chr. and marks 
              distTextChr = .5,              # separ. chr. to text
              
              karHeight = 2,                 # rel. karyotype height
              karHeiSpace = 4,               # karyotype height including spacing
              karSepar=TRUE,                 # reduce spacing among karyotypes 
              amoSepar = 1,                  # depends on karSepar, amount of sep.
              
              chrId="simple",                # chr. names not "original"
              indexIdTextSize=.9,            # font size of chr names and indices
              karIndex = FALSE,              # do not add karyotype asymmetry index
              
              rulerNumberSize=.9,            # font size of ruler
              rulerPos = 0,                  # position of ruler
              ruler.tck= -.004,              # ruler tick length and orient.
              ylabline = -6, # <- NEW        # modifies position of ruler title (Mb)
              
              markLabelSize=.9,              # font size of legend
              legendWidth = 1.2,             # width of legends
              
              xlimLeftMod = 1,               # modify left argument of xlim
              ylimBotMod=.4                  # modify bottom argument of ylim
              # ,asp=1                       # y x aspect
              )                     
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
par(mar=c(0,0,0,0)) 
plotIdiograms(dfChrSize = parentalAndHybHoloChrSize,  # d.f. of chr. sizes
              dfMarkPos = dfAlloParentMarksHolo,      # d.f. of marks' positions
              chrColor  = "gray",          # chr. color
              cenColor  = NULL,            # cen. color when GISH
              
              karHeight = 3,               # karyotype height without spacing
              karHeiSpace=5,               # karyotype height including spacing
              distTextChr = 0.8            # separation among chr. and text
              
              ,ruler=FALSE                 # no ruler
              ,legend=""                   # no legend
              
              ,xlimRightMod = 0            # xlim right arg. modif.
              #,asp=1                      # y x aspect
)
# dev.off()

## ----example_G3, echo=TRUE, results="hide", fig.width=7, fig.height=4.5, message=FALSE,dev='png'----
data("dfChrSizeHolo")
data("dfMarkPosHolo")
dfMarkPosHoloHetero<-dfMarkPosHolo
dfMarkPosHoloHetero$chrName<-c(3,3,"1A",2,"1B","1B")
dfMarkPosHoloHetero$OTU<-"heteromorphic"

dfChrSizeHoloHetero<-dfChrSizeHolo
dfChrSizeHoloHetero$chrName<-c("1A","1B",2,3)
dfChrSizeHoloHetero$OTU<-"heteromorphic"

# Adding the group column
dfChrSizeHoloHetero$group<-c(1,1,NA,NA)

## -----------------------------------------------------------------------------
dfChrSizeHoloGroup<-data.frame(OTU="Species name", 
                               chrName=c(1,1,1,1,2,3,4), 
                               chrSize=c(3.1,3.2,3.3,3.4,4,5,6), 
                               group=c(1,1,1,1,NA,NA,NA) 
                               )

## ---- echo=F------------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(dfChrSizeHoloGroup) , full_width = F
                           , font_size = 10)

## ----example_G4, echo=TRUE, results="hide", fig.width=7, fig.height=4.5, message=FALSE,dev='png'----
par(mar=rep(0,4)) 
mergedChrSize<-plyr::rbind.fill(dfChrSizeHoloGroup,dfChrSizeHoloHetero)

plotIdiograms(dfChrSize=mergedChrSize,      # data.frame of chr. sizes
              dfMarkPos=dfMarkPosHoloHetero,# d.f. of marks
              orderBySize = FALSE,          # do not order chr. by size
              karIndex = FALSE,             # do not add karyotype indices
              addOTUName = TRUE,            # add OTU name
              karHeiSpace = 4,              # height of kar. with spacing
              
              ruler=FALSE,                  # no ruler
              
              xlimLeftMod=-1,               # modify left argument of xlim
              xlimRightMod=0,               # modify right argument of xlim
              ylimBotMod=1.3                # modify bottom argument of ylim
)

