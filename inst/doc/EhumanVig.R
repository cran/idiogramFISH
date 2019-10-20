## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

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

## ----otus, message=FALSE, results="hide"---------------------------------
library(idiogramFISH)
# Chromosome sizes for human
head(humChr)  

## ---- echo=F-------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(head(humChr) ), full_width = F
                           , font_size = 10)

## ----otus2, message=FALSE------------------------------------------------
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

## ----chunk2, results="hide"----------------------------------------------
# data.frame with marks' position
head(humMarkPos)  

## ---- echo=F-------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(head(humMarkPos) ), full_width = F
                           , font_size = 10)

## ---- echo=T-------------------------------------------------------------
humMarkPos$OTU<-OTUdf$OTU[match(humMarkPos$chrName, OTUdf$chrName)]

## ---- results="hide"-----------------------------------------------------
head(humMarkColor)

## ---- echo=F-------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(head(humMarkColor )) , full_width = F
                           , font_size = 10)

## ----hkaryo, echo=TRUE,  message=FALSE, results="hide", fig.width=10, fig.height=28, dev='png'----
# fig.width=10, fig.height=28

plotIdiograms(humChr,                     # data.frame of chromosome size (in package)
              humMarkPos,                 # df of mark positions  (in package)
              dfMarkColor = humMarkColor, # df of mark characteristics (in package)
              
              addOTUName = FALSE,         # do not add name of OTU
              karHeiSpace = 1.7,          # vertical spacing among OTU
              amoSepar = 7,               # reduce distance among OTUs
              karIndex = FALSE,           # do not add karyotype indices
              
              chrColor = "black",         # chr. color
              chrWidth = 1,               # chromosome width
              chrSpacing = 2,             # space among chromosomes
              chrIndex = FALSE,           # add chromosome indices
              
              morpho = FALSE,             # add morphological categories
              dotRoundCorr = .5,          # correcting factor for dots
              centromereSize = 0,         # apparent centromere size
              roundness = 7,              # roundness of chr. and marks
              markLabelSize = .5,         # size of legend font
              pattern= "chr[0-9XY]+",     # REGEX pattern to remove from name of marks
              indexIdTextSize = 2,        # font size of chr name and indices
              lwd.chr=.5,                 # width of chr and mark borders
              
              rulerNumberSize = .9,       # font size of ruler
              rulerNumberPos = 2,         # position of ruler
              xlimRightMod = 0,           # space to the right of karyotype
              ylimBotMod = -.6            # modify ylim of bottom
              )

## ----translo-------------------------------------------------------------
# extract 13 data
humChr13<-humChr[which(humChr$chrName %in% 13),]
humMarkPos13<-humMarkPos[which(humMarkPos$chrName %in% 13),]

# extract 21 data
humChr21<-humChr[which(humChr$chrName %in% 21),]
humMarkPos21<-humMarkPos[which(humMarkPos$chrName %in% 21),]

# Making derivative data.frame of Marks

# remove p arm from 21
humMarkPos21Der<-humMarkPos21[humMarkPos21$markArm=="q",]
humMarkPos21Der$markArm<-"p"

# remove p arm from 13
humMarkPos13Der<-humMarkPos13[humMarkPos13$markArm=="q",]

# rename fragments
humMarkPos21Der$chrName<-"t(13;21)"
humMarkPos13Der$chrName<-"t(13;21)"

# merge fragments of Marks
humMarkPosDer<-rbind(humMarkPos21Der,humMarkPos13Der)

# Making derivative data.frame of chr. size

humChrDer<-humChr13
humChrDer$shortArmSize<-humChr21$longArmSize
humChrDer$chrName<-"t(13;21)"

# Make data.frame of chr. to plot
humChr1321der<-rbind(humChr13, humChrDer , humChr21)
humChr1321der<-humChr1321der[,c("chrName","shortArmSize","longArmSize"),]

# marks for them, together:
humMarkPos1321Der<-rbind(humMarkPos13, humMarkPos21, humMarkPosDer)
humMarkPos1321Der$OTU<-NULL

## ----hsel, echo=TRUE, fig.width=6, fig.height=6, message=FALSE, results="hide",dev='png'----

plotIdiograms(humChr1321der,               # data.frame of size of chr.
              humMarkPos1321Der,           # df of position of marks
              dfMarkColor = humMarkColor,  # df of style of marks
              
              addOTUName = FALSE,          # do not add OTU name
              centromereSize=0,            # apparent size of centromere
              roundness=9,                 # roundness of vertices of chr. and marks
              chrColor = "black",          # chr. color
              chrWidth = 1,                # width of chr.
              chrSpacing = 1,              # space among chr.
              karHeiSpace = 2,             # karyotype height including spacing
              
              chrIndex = FALSE,            # do not add chr. indices
              karIndex = FALSE,            # do not add karyotype indices
              morpho = FALSE,              # do not add chr. morphology
              indexIdTextSize = 1.5,       # font size of chr name
              markLabelSize = .5,          # font size of chr. mark labels
              pattern="chr[0-9]+",         # REGEX pattern to remove from mark names
              lwd.chr=.9,                  # width of chr and mark borders
              
              rulerNumberSize = .9,        # ruler font size
              rulerNumberPos = 2,          # ruler number position
              ruler.tck = -.03,            # tick of ruler, orientation and size
              
              xlimRightMod=2,              # modify xlim right argument
              ylimBotMod = -.15)           # modify ylim bottom argument

## ----translo2, results="hide"--------------------------------------------
chrt13q14q<-robert(humChr,humMarkPos,13,14,"q","q")

# which produces a list of two data.frames:

dfChrSizeDer<-chrt13q14q$dfChrSizeDer
dfMarkPosDer<-chrt13q14q$dfMarkPosDer

head(dfMarkPosDer)

## ---- echo=F-------------------------------------------------------------

kableExtra::kable_styling(knitr::kable(head(dfMarkPosDer) ) , full_width = F
                           , font_size = 10)

## ----hsel2, echo=TRUE, fig.width=6, fig.height=6, message=FALSE, results="hide",dev='png'----

par(mar=c(0,2,.5,0))

plotIdiograms(dfChrSizeDer,               # data.frame of chromosome size (in package)
              dfMarkPosDer,               # df of mark positions  (in package)
              dfMarkColor = humMarkColor, # df of mark characteristics (in package)

              addOTUName = FALSE,         # do not add name of OTU
              karHeiSpace = 1.2,          # vertical spacing among OTU
              karIndex = FALSE,           # do not add karyotype indices

              chrColor = "black",         # chr. color
              chrWidth = .6,              # chromosome width
              chrSpacing = 2,             # space among chromosomes
              chrIndex = FALSE,           # do not add chromosome indices

              morpho = FALSE,             # do not add morphological categories
              dotRoundCorr = .5,          # correcting factor for dots
              centromereSize = 0,         # apparent centromere size
              roundness = 7,              # roundness of chr. and marks
              markLabelSize = .5,         # size of legend font
              pattern= "chr[0-9XY]+",     # REGEX pattern to remove from name of marks
              distTextChr = 1,            # distance from chr. to text.
              indexIdTextSize = 2,        # font size of chr name and indices
              lwd.chr=.5,                 # width of chr and mark borders

              rulerNumberSize = .9,       # font size of ruler
              rulerNumberPos = 1,         # position of ruler
              
              xlimLeftMod = 4,            # space to the right of karyotype
              xlimRightMod = 5,           # space to the right of karyotype
              ylimBotMod = .7,            # modify bottom ylim 
              ylimTopMod =.1              # modify top ylim
)

