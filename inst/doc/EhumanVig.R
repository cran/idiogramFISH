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
knitr::opts_chunk$set(echo = TRUE)

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

## ----otus, message=FALSE, results="hide"--------------------------------------
library(idiogramFISH)
# Chromosome sizes for human
head(humChr) 

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(head(humChr) ), full_width = F
                           , font_size = 10)

## ---- message=FALSE-----------------------------------------------------------
FstRow<-length(which(humChr$group %in% c("A","B") ) )        # groups in 1st row
SndRow<-length(which(humChr$group %in% "C" ) )               # groups in second row
TrdRow<-length(which(humChr$group %in% c("D","E") ) )        # groups in third row
FrtRow<-length(which(humChr$group %in% c("F","G","sex") ) )  # groups in forth row

OTUdf<-data.frame(OTU=c(rep("otu1",FstRow),
                        rep("otu2",SndRow),
                        rep("otu3",TrdRow),
                        rep("otu4",FrtRow)
), stringsAsFactors=FALSE # c
) # df

OTUdf$chrName<-humChr$chrName

humChr$OTU<-OTUdf$OTU

## ----chunk2, results="hide"---------------------------------------------------
# data.frame with marks' position
head(humMarkPos)  

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(head(humMarkPos) ), full_width = F
                           , font_size = 10)

## ---- echo=T------------------------------------------------------------------
humMarkPos$OTU<-OTUdf$OTU[match(humMarkPos$chrName, OTUdf$chrName)]

## ---- results="hide"----------------------------------------------------------
head(humMarkColor)

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(head(humMarkColor )) , full_width = F
                           , font_size = 10)

## ----hkaryo, echo=TRUE,  message=FALSE, results="hide", fig.width=10, fig.height=28, dev='png'----
# fig.width=10, fig.height=28
par(mar=rep(0,4))
plotIdiograms(humChr,                     # data.frame of chromosome size (in package)
              dfMarkPos = humMarkPos,     # df of mark positions  (in package)
              dfMarkColor = humMarkColor, # df of mark characteristics (in package)
              
              addOTUName = FALSE,         # do not add name of OTU
              karHeight = 6,              # vertical size of kar.
              karHeiSpace = 7,            # vertical spacing among OTU
              chrWidth = .4,              # chr. width
              chrSpacing = .6,            # space among chr.
              
              amoSepar = 2,               # reduce distance among OTUs
              karIndex = FALSE,           # do not add karyotype indices
              distTextChr = 1.5,          # distance from chr. to text.
              
              chrColor = "black",         # chr. color
              chrIndex = "",              # do not add chromosome indices
              morpho = "",                # do not add morphological categories

              centromereSize = 0,         # apparent centromere size
              
              roundness = 10,             # roundness of chr. and marks
              legend = "inline",          # mark labels next to chr.
              markLabelSize = .5,         # size of legend font
              colorBorderMark = "black",  # force color of border of marks 
              pattern= "chr[0-9XY]+",     # REGEX pattern to remove from name of marks
              indexIdTextSize = 2,        # font size of chr name and indices
              lwd.chr=.5,                 # width of chr and mark borders
              
              ruler= FALSE,

              xlimRightMod = 0,           # space to the right of karyotype
              ylimBotMod = -.6            # modify ylim of bottom
              # ,asp=1                    # y x aspect
              )

## ----translo2, results="hide"-------------------------------------------------
chrt13q14q<-robert(humChr,humMarkPos,13,14,"q","q")

# which produces a list of two data.frames:

# 1. chr. sizes
dfChrSizeDer<-chrt13q14q$dfChrSizeDer
# remove the group column
dfChrSizeDer<-dfChrSizeDer[ , !(names(dfChrSizeDer) %in% "group")]

# 2. marks' positions
dfMarkPosDer<-chrt13q14q$dfMarkPosDer

head(dfMarkPosDer)

## ---- echo=F------------------------------------------------------------------

kableExtra::kable_styling(knitr::kable(head(dfMarkPosDer) ) , full_width = F
                           , font_size = 10)

## ----hsel2, echo=TRUE, fig.width=6, fig.height=6, message=FALSE, results="hide",dev='png'----

par(mar=c(0,2,.5,0))

plotIdiograms(dfChrSizeDer,               # data.frame of chromosome size
              dfMarkPos = dfMarkPosDer,   # df of mark positions
              dfMarkColor = humMarkColor, # df of mark characteristics (in package)

              addOTUName = FALSE,         # do not add name of OTU
              karIndex = FALSE,           # do not add karyotype indices
              morpho = "",                # do not add morphological categories
              chrIndex = "",              # do not add chromosome indices
              
              chrColor = "black",         # chr. color
              chrWidth = 1,               # chromosome width
              karHeight = 9,              # kar. height without space
              centromereSize = 0,         # apparent centromere size
              roundness = 7,              # roundness of chr. and marks

              markLabelSize = .5,         # size of legend font
              legend = "inline",          # mark labels next to chr.
              pattern= "chr[0-9XY]+",     # REGEX pattern to remove from name of marks
              distTextChr = 6,            # distance from chr. to text.
              indexIdTextSize = 2,        # font size of chr name and indices
              lwd.chr=.5,                 # width of chr and mark borders
              colorBorderMark = "black",  # force color of border of marks 
              
              ruler= FALSE
              
              ,xlimLeftMod = 4            # space to the right of karyotype
              # ,asp=1                    # aspect ratio y x
)

