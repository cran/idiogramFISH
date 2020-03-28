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

## ---- echo=TRUE, results="hide", fig.width=10, fig.height=7, message=FALSE,dev='png', eval=TRUE----
{
  require(plyr)
  dfOfChrSize$OTU   <- "Species mono"
  dfChrSizeHolo$OTU <- "Species holo"

  monoholoCS <- plyr::rbind.fill(dfOfChrSize,dfChrSizeHolo)

  dfOfMarks2$OTU     <-"Species mono"
  dfMarkPosHolo$OTU <-"Species holo"

  monoholoMarks <- plyr::rbind.fill(dfOfMarks2,dfMarkPosHolo)
  monoholoMarks[which(monoholoMarks$markName=="5S"),]$markSize<-.5
}

library(idiogramFISH)

plotIdiograms(dfChrSize  = monoholoCS,   # data.frame of chr. size
              dfMarkColor= dfMarkColor,  # df of mark style
              dfMarkPos  = monoholoMarks,# df of mark positions, includes cen. marks

              roundness =5,              # vertices roundness
              addOTUName = TRUE,         # add OTU names
              distTextChr = .5,          # separ. among chr. and text and among chr. name and indices

              chrId="original",          # use original name of chr.
              OTUTextSize = .7,          # size of OTU name

              legendHeight= 1,           # height of legend labels
              legendWidth = 1,           # width of legend labels
              # ,legend="inline"
              fixCenBorder = TRUE,       # use chrColor as border color of cen. or cen. marks

              xlimLeftMod=1,             # modify xlim left argument of plot
              xlimRightMod=2,            # modify xlim right argument of plot
              ylimBotMod= .2             # modify ylim bottom argument of plot
              
              # GRAPHICAL PARAMETERS FOR CIRCULAR PLOT
  
              ,circularPlot = T          # circularPlot
              ,shrinkFactor = .9         # percentage 1 = 100% of circle with chr.
              ,circleCenter = 3          # X coordinate of circleCenter (affects legend pos.)
              ,chrLabelSpacing = .9      # chr. names spacing
              
              ,OTUsrt = 0                # angle for OTU name (or number)
              ,OTUplacing = "number"     # Use number and legend instead of name. See OTUcentered
              ,OTUjustif = 0             # OTU names justif. left.
              ,OTULabelSpacerx = -1.5    # modify position of OTU label, when OTUplacing="number" or "simple"
              ,OTUlegendHeight = 1.5     # space among OTU names when in legend - OTUplacing
)

## ---- echo=TRUE, results="hide", fig.width=10, fig.height=7, message=FALSE,dev='png', eval=TRUE----

# First swap short and long arms to show the same rotation of the article

listradfs<-swapChrRegionDfSizeAndMarks(traspadf,traspaMarks,c("3","6","7","9","12") )

# Create marks' characteristics

dfMarkColor5S25S<-read.table(text="    markName markColor  style
        5S       black dots
       25S       white dots"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

plotIdiograms(dfChrSize = listradfs$dfChrSize,  # d.f. of chr. sizes
              dfMarkPos = listradfs$dfMarkPos,  # d.f. of marks' positions
              dfMarkColor = dfMarkColor5S25S,   # d.f. of mark characteristics
              cenColor  = "black",              # cen. color 
              roundness = 5,                    # corner roundness
              chrWidth = 1,                     # chr. width
              orderBySize = FALSE               # do not order chr. by size

              ,addOTUName = FALSE               # do not add OTU name
              ,legendHeight = 2.5               # labels separ. y axis
              
              # circular plot parameters
              ,circularPlot=TRUE                   
              ,radius=5                         # basic radius
              ,useOneDot=FALSE                  # use two dots in dot marks
              ,chrLabelSpacing = 1              # chr. name spacing
              ,rotation = 0.1                   # anti-clockwise start site in x*pi radians, from top (0)
              ,shrinkFactor = .95               # % of circle use
)

## ---- echo=TRUE, fig.width=9, fig.height=9, message=FALSE,dev='png', eval=TRUE,collapse=TRUE, comment="#"----

# data from: https://www.ncbi.nlm.nih.gov/nuccore/NZ_CP009939.1

#install.packages("rentrez")
library(rentrez)
# search string
bcereus <- "Bacillus cereus strain 03BB87 plasmid pBCN, complete sequence"
bcereus_search <- rentrez::entrez_search(db="nuccore", term = bcereus)
# get summaries
esummaries<-rentrez::entrez_summary(db = "nuccore", id = bcereus_search$ids)

# download plasmid data
# From the entrez formats:
# https://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.T._valid_values_of__retmode_and/
# idiogramFISH can read only:
rentrezDownload  <- rentrez::entrez_fetch(db="nuccore", 
                                   id = bcereus_search$ids[1], 
                                   rettype="gbwithparts", 
                                   retmode = "text")

mylist<-genBankReadIF(rentrezDownload)

# data.frames in mylist
names(mylist)

# mylist$source
# View(mylist$gbdfMain)
# View(mylist$gbdfAssemblyMeta)
# mylist$gbdfAnnoMeta
# View(mylist$CDS)
# View(mylist$gene)

# Authors of plasmid sequence
paste(mylist$gbdfMain[which(mylist$gbdfMain$field=="AUTHORS"),][1,2] )

# create plasmid size data data.frame
myPlasmiddf <- data.frame(chrName=1, chrSize=mylist$source$end)
myPlasmiddf$OTU<-mylist$gbdfMain[which(mylist$gbdfMain$field=="DEFINITION"),]$value
myPlasmiddf$OTU<-gsub(", complete sequence.","",myPlasmiddf$OTU)

# Creating mark info data.frame

mylistSel<- mylist[which(names(mylist) %in% "gene")]
mylistSelDF <- dplyr::bind_rows(mylistSel, .id="feature")

mylistSelDF$markPos <-pmin(as.numeric(mylistSelDF$begin),as.numeric(mylistSelDF$end) )
mylistSelDF$markSize<-abs(as.numeric(mylistSelDF$end)-as.numeric(mylistSelDF$begin) )
mylistSelDF$markName<-mylistSelDF$locus_tag
# orientation of arrows
mylistSelDF$style<-ifelse(mylistSelDF$isComplement,"downArrow","upArrow")

# Replace codes with names
mylistSelDF[which(!is.na(mylistSelDF$gene) ),]$markName<-
  mylistSelDF[which(!is.na(mylistSelDF$gene) ),]$gene



# subset columns
marksDf<-mylistSelDF[,c("markName","markPos","markSize","style"),]

# add spaces before name
distantNames<-unlist(lapply(marksDf$markName, 
                            function(x) gsub("(.*)",paste0(paste0(rep(" ",20),collapse=""),"\\1"),x) ) )
# add spaces after name
distantNames2<-unlist(lapply(marksDf$markName, 
                             function(x) gsub("(.*)",paste0("\\1",paste0(rep(" ",20), collapse="")),x) ) )

# Replace names
for (i in seq(1,nrow(marksDf ), by=2) )  { marksDf$markName[i]<-distantNames[i]}
for (i in seq(2,nrow(marksDf ), by=2) )  { marksDf$markName[i]<-distantNames2[i]}

# add mandatory column
marksDf$chrName<-1

# add marker for start pos.
marksDf<-rbind(marksDf,c(paste0("START",paste0(rep(" ",20), collapse="")),1,NA,"square",1) )

# add column - name of plasmid
marksDf$OTU <- myPlasmiddf$OTU

# create mark general data data.frame
markStyle2 <-markStyle  <- idiogramFISH:::makedfMarkColorMycolors(unique(marksDf$markName),
                                                                  c("black","forestgreen","cornflowerblue") )
# 1st plot with cM style of marks
markStyle$style<-"cM"

# arrows
markStyle2$style <- marksDf$style[match(markStyle2$markName, marksDf$markName)]

# prefix to remove from marks
mypattern<-sub("([[:alnum:]]+_).*","\\1",trimws(marksDf$markName[1]) )

library(idiogramFISH)
par(mar=rep(0,4))
plotIdiograms(dfChrSize = myPlasmiddf,  # plasmid size d.f.
              dfMarkPos = marksDf,      # mark pos d.f.
              dfMarkColor = markStyle,  # mark style d.f.
              
              roundness = 21,           # corners not rounded
              chrWidth = .1,            # chr. width
              chrId="",                 # no chr. name
              
              markLabelSize=.5,         # font size of labels
              pattern=mypattern,        # remove pattern from mark names
              protruding=.5,            # modify cM marks size
              
              ylimBotMod = 0,           # modify plot size
              ylimTopMod = 0, 
              xlimLeftMod = 2, 
              
              # circular params.
              circularPlot = TRUE,      # circular
              shrinkFactor = 1,         # use 100% of circle
              labelSpacing = 1.5,       # label spacing from chr.
              rotation=0,               # begin plasmid in top
              labelOutwards = TRUE,     # label projected based on mark angle
              OTUjustif = 0.5,          # OTU name justif. centered.
              OTUplacing = "simple"     # plasmid name place. See OTUcentered
)

# overlap arrow style of marks with second plot

# plot over previous plot
plotIdiograms(dfChrSize = myPlasmiddf, dfMarkPos = marksDf, 
              circularPlot = TRUE,
              shrinkFactor = 1, roundness = 21, chrWidth = .1, labelSpacing = 2, chrId="",
              ylimBotMod = 0, ylimTopMod = 0, rotation=0, legend="", 
              addOTUName = FALSE,       # do not add OTU name, see above
              callPlot = FALSE          # do not create a new plot
              ,dfMarkColor = markStyle2 # d.f. of mark style downArrow and upArrow
              ,shrinkArrow = 0.1        # Arrow without protruding head
)



## ---- echo=TRUE, results="asis", fig.width=9, fig.height=9, message=FALSE,dev='png', eval=FALSE----
#  library(idiogramFISH)
#  
#  # Option 1: Download prokaryote genome data from:
#  # https://www.ncbi.nlm.nih.gov/nuccore/NC_014248.1
#  # Choose Customize View -> Basic Features -> genes, CDS
#  # Send To -> File -> Create File
#  
#  # Use your file name:
#  data.gb <- "nostoc.gb" # 5 Mbytes
#  
#  # Option 2: Download with rentrez package
#  library(rentrez)
#  # search string
#  nostoc <- "'Nostoc azollae' 0708, complete genome"
#  nostoc_search <- rentrez::entrez_search(db="nuccore", term=nostoc)
#  # get summaries
#  esummaries<-rentrez::entrez_summary(db="nuccore", id=nostoc_search$ids)
#  # select only perfect matches
#  select<-numeric()
#  for (i in 1:length(esummaries)){
#    if(esummaries[[i]]$title ==nostoc){ select<-c(select,i) }
#  }
#  select
#  # 3 8
#  
#  # download chr. data
#  data.gb  <- rentrez::entrez_fetch(db="nuccore",
#                                        id=nostoc_search$ids[select][1],
#                                        rettype="gbwithparts",
#                                        retmode = "text")
#  # START:
#  mylist<-genBankReadIF(data.gb)
#  names(mylist)
#  # "gbdfMain"     "gbdfAnnoMeta" "source"       "gene"         "CDS"          "tRNA"
#  # "regulatory"   "ncRNA"        "rRNA"         "misc_feature" "tmRNA"
#  
#  # Authors of sequence
#  paste(mylist$gbdfMain[which(mylist$gbdfMain$field=="AUTHORS"),][1,2] )
#  # [1] "Ran,L., Larsson,J., Vigil-Stenman,T., Nylander,J.A., Ininbergs,K.,;
#  # Zheng,W.W., Lapidus,A., Lowry,S., Haselkorn,R. and Bergman,B."
#  
#  # create chr. size data data.frame
#  myProkaryotedf <- data.frame(chrName=1, chrSize=mylist$source$end)
#  myProkaryotedf$OTU<-mylist$gbdfMain[which(mylist$gbdfMain$field=="DEFINITION"),]$value
#  myProkaryotedf$OTU<-gsub(", complete genome.","",myProkaryotedf$OTU)
#  
#  # Creating mark info data.frame excluding some features
#  mylistSel  <- mylist[which(names(mylist) %in%
#                               setdiff( names(mylist) , c("gbdfMain","gbdfAnnoMeta","source","CDS") ) )]
#  # or:
#  # mylistSel<- mylist[which(names(mylist) %in% "CDS")]
#  
#  mylistSelDF<-dplyr::bind_rows(mylistSel, .id="feature")
#  
#  mylistSelDF$markPos <-pmin(as.numeric(mylistSelDF$begin),as.numeric(mylistSelDF$end) )
#  mylistSelDF$markSize<-abs(as.numeric(mylistSelDF$end)-as.numeric(mylistSelDF$begin) )
#  mylistSelDF$markName<-mylistSelDF$locus_tag
#  
#  # orientation of arrows	
#  mylistSelDF$style<-ifelse(mylistSelDF$isComplement,"downArrow","upArrow")
#  
#  dim(mylistSelDF)
#  
#  # Replace codes with names
#  mylistSelDF[which(!is.na(mylistSelDF$gene) ),]$markName<-
#    mylistSelDF[which(!is.na(mylistSelDF$gene) ),]$gene
#  
#  marksDf<-mylistSelDF[,c("markName","markPos","markSize","feature","isJoin","style"),]
#  
#  # Separate names in 4 columns
#  distantNames1<-unlist(lapply(marksDf$markName, function(x)
#    gsub("(.*)",paste0("\\1",paste0(rep(" ",25*3), collapse = "")),x) ) )
#  distantNamesCenter2<-unlist(lapply(marksDf$markName, function(x)
#    gsub("(.*)",paste0(paste0(rep(" ",25), collapse = ""),"\\1",paste0(rep(" ",25*2), collapse = "")),x) ) )
#  distantNamesCenter3<-unlist(lapply(marksDf$markName, function(x)
#    gsub("(.*)",paste0(paste0(rep(" ",25*2), collapse = ""),"\\1",paste0(rep(" ",25), collapse = "")),x) ) )
#  distantNames4<-unlist(lapply(marksDf$markName, function(x)
#    gsub("(.*)",paste0(paste0(rep(" ",25*3), collapse = ""),"\\1"),x ) ) )
#  
#  for (i in seq(1,nrow(marksDf), by=4) )  { marksDf$markName[i]<-distantNames1[i]}
#  for (i in seq(2,nrow(marksDf), by=4) )  { marksDf$markName[i]<-distantNamesCenter2[i]}
#  for (i in seq(3,nrow(marksDf), by=4) )  { marksDf$markName[i]<-distantNamesCenter3[i]}
#  for (i in seq(4,nrow(marksDf), by=4) )  { marksDf$markName[i]<-distantNames4[i]}
#  
#  # add marker for start
#  colnames(marksDf)
#  marksDf<-rbind(marksDf,c("START",1,NA,"start",FALSE,"square") )
#  
#  # add mandatory column
#  marksDf$chrName<-1
#  
#  # add column OTU, when in main data.frame
#  marksDf$OTU <- myProkaryotedf$OTU
#  
#  unique(marksDf$feature)
#  # [1] "gene"         "tRNA"         "regulatory"   "ncRNA"        "rRNA"         "misc_feature" "tmRNA"        "start"
#  unique(marksDf$isJoin)
#  # [1] "FALSE"
#  
#  # create mark general data data.frame
#  markStyle  <- idiogramFISH:::makedfMarkColorMycolors(unique(marksDf$markName),
#                                                       c("black","forestgreen","cornflowerblue") )
#  # change some colors depending on feature
#  markStyle[which(markStyle$markName %in%
#                    marksDf[which(marksDf$feature %in% c("tRNA","tmRNA") ),]$markName
#  ),]$markColor<-"magenta"
#  markStyle[which(markStyle$markName %in%
#                    marksDf[which(marksDf$feature %in% c("regulatory","ncRNA") ),]$markName
#  ),]$markColor<-"tomato3"
#  markStyle[which(markStyle$markName %in%
#                    marksDf[which(marksDf$feature %in% "rRNA" ),]$markName
#  ),]$markColor<-"red2"
#  markStyle[which(markStyle$markName %in%
#                    marksDf[which(marksDf$feature %in% "misc_feature" ),]$markName
#  ),]$markColor<-"lightsalmon"
#  
#  # or:
#  # When isJoin is TRUE (CDS feature included)
#  # markStyle[which(markStyle$markName %in%
#  #                   marksDf[which(marksDf$isJoin==TRUE),]$markName
#  # ),]$markColor<-"red"
#  
#  # duplicate mark style data.frame for arrow styles
#  markStyle2 <- markStyle
#  
#  # cM style d.f.
#  markStyle$style<-"cM"
#  
#  # downArrow, upArrow styles, using them instead of squares, increases plot size in bytes	
#  markStyle2$style <- marksDf$style[match(markStyle2$markName, marksDf$markName)]
#  
#  # prefix to remove from mark names
#  mypattern<-sub("([[:alnum:]]+_).*","\\1",trimws(marksDf$markName[1]) )
#  
#  # png("NOSTOC2.png", width=9500, height=9500) #  14 Mbytes
#  pdf("NOSTOC2.pdf",   width=130,  height=130)  #  20 Mb with arrows
#  # svg("NOSTOC2.svg", width=130,  height=130)  # 140 Mb with arrows
#  
#  par(mar=rep(0,4))
#  plotIdiograms(dfChrSize = myProkaryotedf,  # chr. data d.f.
#                dfMarkPos = marksDf,         # mark pos d.f.
#                dfMarkColor = markStyle,     # mark style d.f. style cM
#  
#                roundness = 21,           # corners not rounded
#                n=100,                    # number of vertices in rounded items.
#                chrWidth = .02,           # chr. width
#                chrId="",                 # no chr. name
#  
#                markLabelSize=1,          # font size of labels
#                protruding=.5,            # modify cM marks size
#                pattern= mypattern,       # remove pattern from mark names
#  
#                ylimBotMod = -.5,         # modify plot size
#                ylimTopMod = -.5,
#                xlimLeftMod = .3,
#                xlimRightMod = .3,
#  
#                # circular plot params.
#                circularPlot = TRUE,      # circular
#                shrinkFactor = 1,         # use 100% of circle
#                labelSpacing = 1.2,       # label spacing from chr.
#                rotation=0,               # begin chr. in top
#                labelOutwards = TRUE      # label projected based on mark angle
#                ,OTUjustif = 0.5          # OTU name centered
#                ,OTUplacing = "simple"    # location of OTU name, see OTUcentered
#                ,radius = .1              # radius of circle
#                ,OTUTextSize = 10         # font size of OTU name
#  )
#  # plot over previous plot arrow style
#  plotIdiograms(dfChrSize = myProkaryotedf, dfMarkPos = marksDf,
#                dfMarkColor = markStyle2, # style downArrow and upArrow
#                circularPlot = TRUE,
#                shrinkFactor = 1, roundness = 21,   chrWidth = .02, chrId="",
#                ylimBotMod = -.5, ylimTopMod = -.5, xlimLeftMod = .3,
#                xlimRightMod = .3, radius=.1, n=100, rotation=0,
#                arrowhead = .65,          # proportion of head of arrow
#                legend="",                # do not add legend for marks
#                addOTUName = FALSE,       # do not add. OTU name
#                callPlot = FALSE          # plot over previous plot
#  )
#  dev.off()

## ---- results="asis", comment=NA, echo=FALSE, eval=TRUE-----------------------
    nostocFile1 <- "../man/figures/nostoc.jpg"
    nostocFile1 <- normalizePath(nostocFile1)
    knitr::include_graphics(nostocFile1)  
    nostocFile2 <- "../man/figures/nostocSmall.jpg"
    nostocFile2 <- normalizePath(nostocFile2)
    knitr::include_graphics(nostocFile2)

