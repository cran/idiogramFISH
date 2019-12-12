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

## ---- message=FALSE, echo=FALSE------------------------------------------
packageCheck<-all(unlist(invisible(lapply(c("ggtree","grid","ggpubr","phytools","treeio","plyr"), function(pkg) requireNamespace(pkg, quietly=TRUE)  ) ) ) )

## ---- message=FALSE, eval=packageCheck-----------------------------------
require(ggplot2)
require(phytools)
require(ggpubr)
require(grid)   #pushViewport
require(ggtree)
# list.files(system.file('extdata', package = 'my_package') )

# find path of iqtree file
iqtreeFile    <- system.file("extdata", "eightSpIqtree.treefile", package = "idiogramFISH")

# load file as phylo object
iqtreephylo   <- read.newick(iqtreeFile) # phytools

# transform tree
iqtreephyloUM <- force.ultrametric(iqtreephylo, method= "extend") # phytools


## ---- message=FALSE, eval=packageCheck-----------------------------------
ggtreeOf8 <- ggtree(iqtreephyloUM) + geom_tiplab(size=6)

## ---- message=FALSE,eval=packageCheck------------------------------------
gbuil2      <-  ggplot_build(ggtreeOf8)       # get ggplot_built
gtgbuild    <-  ggplot_gtable(gbuil2)         # get gtable from ggplot_built
gtgbuild$layout$clip[gtgbuild$layout$name == "panel"] <- "off"                # modify gtable
ggtreeOf8b   <- as_ggplot(gtgbuild)            # back to ggplot
gtgbuildgg2 <- ggtreeOf8b +  theme(plot.margin = unit(c(1,9.5,3,1.5), "cm") ) # top right bottom left - modify margins

## ---- message=FALSE,eval=packageCheck------------------------------------
ggtreeOf8TIPS<-ggtreeOf8$data[which(ggtreeOf8$data$isTip),]
desiredOrder  <- rev(ggtreeOf8TIPS[order(ggtreeOf8TIPS$y),]$label)

## ---- message=FALSE,eval=packageCheck------------------------------------
# make a vector without missing OTUs
desiredFiltered <- intersect(desiredOrder,allChrSizeSample$OTU)

# establish desired order
allChrSizeSample$OTU <- factor(allChrSizeSample$OTU, levels = desiredFiltered)

# order
allChrSizeSample     <- allChrSizeSample[order(allChrSizeSample$OTU),]

## ---- message=FALSE,eval=packageCheck------------------------------------
# Establish position of OTUs before missing data OTUs
matchres <- match(desiredOrder,desiredFiltered)
matchres[is.na(matchres)] <- "R"
reps     <- rle(matchres)
posOTUsBeforeMissing      <- as.numeric(matchres[which(matchres=="R")-1][which(matchres[which(matchres=="R")-1]!="R")] )

# This are the OTUs that come before missing chr. data OTUs
BeforeMissing             <- desiredFiltered[posOTUsBeforeMissing]

# This is the amount of missing OTUs, spaces to add (ghost karyotypes)
valuesOfMissRepsBefore    <- reps$lengths[which(reps$values=="R")]

## ---- message=FALSE, comment=NA, results="hide",eval=packageCheck--------
# plot to png file
png(file="firstplot.png" ,width=962,height=962 )

par(omi=rep(0,4) , mar=c(0,1,2,1), 
    mfrow=c(1,2) )   # one row two columns
par(fig=c(0,.3,0,1)) # location of left ghost plot
plot.new()           # ghost plot to the left
par(fig=c(.3,1,0,1)) # location of right plot

plotIdiograms(allChrSizeSample,                # data.frame of Chr. Sizes
              allMarksSample,                  # d.f. of Marks (inc. cen. marks) 
              dfMarkColor =  mydfMaColor,      # d.f. of mark characteristics
              
              roundness = 4,                   # roundness of vertices
              lwd.chr=.5,                      # width of lines
              orderBySize = FALSE,             # don't order chr. by size
              centromereSize = 1.3,            # apparent cen. size
              chrWidth =.75,                   # width of chr.
              chrSpacing = .25,                # horizontal spacing of chr.
              indexIdTextSize=.4,              # font size of indices and chr. names
              
              karHeight = 4.8,                 # karyotype vertical relative size without spacing
              karHeiSpace = 6.5,               # karyotype vertical relative size with spacing
              
              nameChrIndexPos=4,               # move the name of chr. indexes to left
              morpho=TRUE,                     # add chr. morphology
              chrIndex = TRUE,                 # add chr. indices
              karIndex = TRUE,                 # add karyotype indices
              
              markLabelSpacer = 0              # spaces from rightmost chr. to legend
              
              ,ylimTopMod = -.1                # modify ylim top margin
              ,ylimBotMod=1.6                  # modify ylim bottom margin
              
              ,rulerPos = -1                   # position of rulers
              ,rulerNumberSize = .35           # font size of ruler number
              ,rulerNumberPos = .4             # position of ruler numbers
              ,ruler.tck = -.004               # tick size and orient.
              ,asp=1                           # y x aspect
              
              ,addMissingOTUAfter = BeforeMissing          # OTUs after which there are ghost karyotypes - empty spaces
              ,missOTUspacings    = valuesOfMissRepsBefore # number of ghost karyotypes
)

# plot to the left the ggtree
pushViewport(viewport(layout = grid.layout(1, 2)))
pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
print(gtgbuildgg2,newpage=F) 

# close png
dev.off()

## ---- results="asis", comment=NA, echo=FALSE-----------------------------
if(packageCheck){
# cat(paste0("![](",myt,")"))
cat(paste0("![](firstplot.png)" ) )
# cat(paste0("![](firstplot.svg)" ) )
} else {
cat(paste0("![](firstplot2.png)" ) )
# img1_path <- "../man/figures/firstplot2.svg"
# if(file.exists(img1_path)) {
# cat(paste0("<img src=",img1_path," width=\"100%\">") )
# }
}

## ---- message=FALSE, eval=packageCheck-----------------------------------
require(ggplot2)
require(phytools)
require(ggpubr)
require(grid)   #pushViewport
require(ggtree)
require(treeio)

# find path of iqtree file
revBayesFile    <- system.file("extdata", "revBayesTutorial.tree", package = "idiogramFISH")

# load file as phylo object
revBayesPhylo   <- read.beast(revBayesFile) # ggtree or treeio

# transform tree
revBayesPhyloUM <- force.ultrametric(revBayesPhylo@phylo, method= "extend") # phytools


## ---- message=FALSE,eval=packageCheck------------------------------------
is_tip           <- revBayesPhyloUM$edge[,2] <= length(revBayesPhyloUM$tip.label)
ordered_tips     <- revBayesPhyloUM$edge[is_tip, 2]
desiredorderRevB <- rev(revBayesPhyloUM$tip.label[ordered_tips])

## ---- message=FALSE,eval=packageCheck------------------------------------
allChrSizeSampleHolo <- allChrSizeSample
allChrSizeSampleHolo <- allChrSizeSampleHolo[,c("OTU","chrName","longArmSize")]
colnames(allChrSizeSampleHolo)[which(names(allChrSizeSampleHolo)=="longArmSize")]<-"chrSize"

allMarksSampleHolo   <- allMarksSample
allMarksSampleHolo   <- allMarksSampleHolo[which(allMarksSampleHolo$chrRegion!="cen"),]
allMarksSampleHolo   <- allMarksSampleHolo[c("OTU","chrName","markName","markDistCen","markSize")]
colnames(allMarksSampleHolo)[which(names(allMarksSampleHolo)=="markDistCen")] <- "markPos"
allMarksSampleHolo[which(allMarksSampleHolo$markName=="5S"),]$markSize <- .5

## ---- message=FALSE,eval=packageCheck------------------------------------
# make a vector without missing OTUs
desiredFiltered <- intersect(desiredorderRevB,allChrSizeSampleHolo$OTU)

# establish desired order
allChrSizeSampleHolo$OTU <- factor(allChrSizeSampleHolo$OTU, levels = desiredFiltered)

# order
allChrSizeSampleHolo <- allChrSizeSampleHolo[order(allChrSizeSampleHolo$OTU),]

## ---- message=FALSE,eval=packageCheck------------------------------------
# Establish position of OTUs before missing data OTUs
matchres <- match(desiredorderRevB,desiredFiltered)
matchres[is.na(matchres)]   <- "R"
reps     <- rle(matchres)
posOTUsBeforeMissing        <- as.numeric(matchres[which(matchres=="R")-1][which(matchres[which(matchres=="R")-1]!="R")] )

# This are the OTUs that come before missing chr. data OTUs
BeforeMissingPlot2          <- desiredFiltered[posOTUsBeforeMissing]

# This is the amount of missing OTUs, spaces to add (ghost karyotypes)
valuesOfMissRepsBeforePlot2 <- reps$lengths[which(reps$values=="R")]

## ---- message=FALSE, comment=NA, results="hide",eval=packageCheck--------
# plot to png file
png(file=paste0("secondplot.png" ),width=962,height=700)

{
par(omi=rep(0,4) , mar=c(0,0,0,0), mfrow=c(1,2))
par(fig=c(0,.3,0,1)) 
par(mar=c(2,0,2,0)) # b l t r

plot(revBayesPhyloUM)
par(fig=c(0.3,1,0,1), new=TRUE)
par(mar=c(0,0,0,0)) # b l t r

# Function plotIdiogramsHolo deprecated after ver. 1.5.1 

plotIdiograms(allChrSizeSampleHolo,               # chr. size data.frame
              allMarksSampleHolo,                 # data.frame of marks' positions
              dfMarkColor =  mydfMaColor,         # d.f. of mark characteristics
              
              roundness = 4,                      # vertices roundness
              karHeight = 2.8,                    # karyotype height
              karHeiSpace = 4.5,                  # vertical size of kar. including spacing

              karIndex = TRUE,                    # add karyotype index
              indexIdTextSize=.4                  # font size of indices and chr. names
              
              ,addMissingOTUAfter = BeforeMissingPlot2           # add ghost OTUs after these names
              ,missOTUspacings    = valuesOfMissRepsBeforePlot2  # how many ghosts, respectively
              ,lwd.chr=.5                         # line width

              ,markLabelSpacer = 0                # dist. of legend to rightmost chr.
              ,legendWidth = 2.3                  # width of square or dots of legend
              
              ,rulerPos = - 1                     # position of ruler
              ,rulerNumberSize = .35              # font size of number of ruler
              ,rulerNumberPos = .4                # position of ruler number
              ,ruler.tck=-.004

              ,ylimTopMod = 0                     # modify ylim of top
              ,ylimBotMod = 0                     # modify ylim of bottom
              ,asp=1                              # y x aspect
)
}

# close png
dev.off()

## ---- results="asis", comment=NA, echo=FALSE-----------------------------
if(packageCheck){
# myt2<-normalizePath(myt2)
# cat(paste0("![](",myt2,")"))
cat(paste0("![](secondplot.png)" ) )
} else {
cat(paste0("![](secondplot2.png)" ) )
# img1_path <- "../man/figures/secondplot2.svg"
# if(file.exists(img1_path)) {
# cat(paste0("<img src=",img1_path," width=\"100%\">") )
# }
}

## ---- message=FALSE,eval=packageCheck------------------------------------

# Select this OTU from the monocen.
monosel<-c("Species_F","Species_C","Species_A")
# chr.
allChrSizeSampleSel  <- allChrSizeSample [which(allChrSizeSample$OTU  %in% monosel  ),]
# marks
allMarksSampleSel    <- allMarksSample   [which(allMarksSample$OTU    %in% monosel  ),]

# Select the others from the holocen.
holosel    <- setdiff(unique(allChrSizeSampleHolo$OTU),monosel)
# chr.
allChrSizeSampleHoloSel <- allChrSizeSampleHolo[which(allChrSizeSampleHolo$OTU %in% holosel  ),]
# marks
allMarksSampleHoloSel   <- allMarksSampleHolo  [which(allMarksSampleHolo$OTU   %in% holosel  ),]

# merge chr d.fs
mixChrSize <- plyr::rbind.fill(allChrSizeSampleSel,allChrSizeSampleHoloSel)

# merge marks' d.fs
mixMarks   <- plyr::rbind.fill(allMarksSampleSel,allMarksSampleHoloSel)

## ---- message=FALSE,eval=packageCheck------------------------------------
# make a vector without missing OTUs
desiredFiltered <- intersect(desiredorderRevB, mixChrSize$OTU)

# establish desired order
mixChrSize$OTU <- factor(mixChrSize$OTU, levels = desiredFiltered)

# order data.frame
mixChrSize <- mixChrSize[order(mixChrSize$OTU),]

# Establish position of OTUs before missing data OTUs
matchres <- match(desiredorderRevB,desiredFiltered)
matchres[is.na(matchres)]   <- "R"
reps     <- rle(matchres)
posOTUsBeforeMissing        <- as.numeric(matchres[which(matchres=="R")-1][which(matchres[which(matchres=="R")-1]!="R")] )

# This are the OTUs that come before missing chr. data OTUs
BeforeMissingPlot2          <- desiredFiltered[posOTUsBeforeMissing]

# This is the amount of missing OTUs, spaces to add (ghost karyotypes)
valuesOfMissRepsBeforePlot2 <- reps$lengths[which(reps$values=="R")]

## ---- message=FALSE, comment=NA, results="hide",eval=packageCheck--------
# plot to png file
png(file=paste0("thirdplot.png" ),width=1100,height=1000)
{
  par(omi=rep(0,4) , mar=c(0,0,0,0), mfrow=c(1,2))
  par(fig=c(0,.25,0,1)) 
  par(mar=c(1,0,0,0))
  
  plot(revBayesPhyloUM)
  par(fig=c(0.25,1,0,1), new=TRUE)
  par(mar=c(0,0,0,0))
  
plotIdiograms(mixChrSize,                         # chr. size data.frame
              mixMarks,                           # data.frame of marks' positions (inc. cen. marks)
              dfMarkColor = mydfMaColor,          # d.f. of mark characteristics
              
              origin="b",                         # position measured from bottom of chr.
              
              karHeight = 2.8,                    # vertical size of kar. including spacing
              karHeiSpace = 4.5,                  # vertical size of kar. including spacing
              roundness = 5,                      # vertices roundness
              chrSpacing = .25,                   # horizontal spacing among chr.
              
              karIndex = TRUE                     # add karyotype index
              ,indexIdTextSize=.4                 # font size of indices and chr. names
              
              ,addMissingOTUAfter = BeforeMissingPlot2           # add ghost OTUs after these names
              ,missOTUspacings    = valuesOfMissRepsBeforePlot2  # how many ghosts, respectively
              ,lwd.chr=.5                         # line width
              
              ,markLabelSpacer = 0                # dist. of legend to rightmost chr.
              ,legendWidth = 2                    # width of square or dots of legend
              
              ,ylimTopMod = -2                    # modify ylim of top
              ,ylimBotMod = -2                    # modify ylim of bottom
              
              ,rulerPos = -1                      # position of ruler
              ,rulerNumberSize = .35              # font size of number of ruler
              ,rulerNumberPos = .4                # position of ruler number
              ,ruler.tck=-.004
              ,asp = 1                            # y x aspect
              
)
}
# close png
dev.off()

## ---- results="asis", comment=NA, echo=FALSE, eval=TRUE------------------
if(packageCheck){
cat(paste0("![](thirdplot.png)" ) )
} else {
cat(paste0("![](thirdplot2.png)" ) )
# img1_path <- "../man/figures/logo.svg"
# if(file.exists(img1_path)) {
# cat(paste0("<img src=",img1_path," width=\"100%\">") )
# }
}

