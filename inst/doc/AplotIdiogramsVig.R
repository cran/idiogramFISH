## ---- echo=F, warning=FALSE, error=FALSE, comment=NA--------------------------
if(length(rmarkdown::pandoc_version()<2)>0) { # solaris workaround
if(rmarkdown::pandoc_version()<2){
  message(crayon::red("\nMissing pandoc version > 2. Vignette may fail because it uses lua filter for multiple bibliographies
                      \nMore info:
                      \nhttps://stat.ethz.ch/pipermail/r-package-devel/2019q2/004127.html
                      \nhttps://stat.ethz.ch/pipermail/r-package-devel/2020q1/004814.html
                      \nLua filters are supported by rmarkdown
                      \nhttps://cran.r-project.org/web/packages/rmarkdown/vignettes/lua-filters.html
                      \nLua filters are an old characteristic of pandoc
                      \nhttps://pandoc.org/lua-filters.html
                      \nInstall pandoc >2 or try with package installr (if in Windows):
                      \nhttps://github.com/jgm/pandoc/releases")
          )# me
  if(Sys.info()['sysname']=="Windows"){
    message("pandoc > 2 not available, see online vignettes")
    # system("powershell -Command \"$pwd.Path\" ")
    # shell("powershell -Command \"(gc index.Rmd) -replace 'pandoc_args', '#pandoc_args' | Out-File -encoding ASCII index.Rmd ")
    # shell("powershell -Command \"(gc DphylogenyVig.Rmd) -replace 'pandoc_args', '#pandoc_args' | Out-File -encoding ASCII DphylogenyVig.Rmd ")
    
    # remove vignettes with lua filter
    shell("del index.Rmd")
    shell("del DphylogenyVig.Rmd")
    shell("del EhumanVig.Rmd")         # pandoc error Could not determine mime type for `https://kit.fontawesome.com/af0a13599b.js'
    shell("del AplotIdiogramsVig.Rmd") # pandoc error Could not determine mime type for `https://kit.fontawesome.com/af0a13599b.js'
    
    #
    #    create new index.Rmd
    #
      shell("@echo off")
      shell('@echo --- > index.Rmd')
      shell('@echo title: "1. Plotting monocentric chromosomes" >> index.Rmd')
      shell('@echo author: "Fernando Roa" >> index.Rmd')
      shell('@echo date: "23 08 2019" >> index.Rmd')
      shell('@echo output: >> index.Rmd')
      shell('@echo   prettydoc::html_pretty: >> index.Rmd')
      shell('@echo     theme: leonids >> index.Rmd')
      shell('@echo     highlight: github >> index.Rmd')
      shell('@echo     toc: true >> index.Rmd')
      shell('@echo     toc_depth: 1 >> index.Rmd')
      shell('@echo     number_sections: true >> index.Rmd')
      shell('@echo vignette: ^> >> index.Rmd')
      shell("@echo   %\\VignetteIndexEntry{1. Plotting monocentric chromosomes} >> index.Rmd")
      shell('@echo   %\\VignetteEngine{knitr::rmarkdown} >> index.Rmd')
      shell('@echo   %\\VignetteEncoding{UTF-8} >> index.Rmd')
      shell('@echo --- >> index.Rmd')
      shell('@echo(     >> index.Rmd')
      shell('@echo pandoc ^> 2 not available, visit https://ferroao.gitlab.io/idiogramfishhelppages >> index.Rmd')
    #
    #   create new DphylogenyVig.Rmd
    #
      shell('@echo --- > DphylogenyVig.Rmd')
      shell('@echo title: "4. Plotting alongside phylogeny" >> DphylogenyVig.Rmd')
      shell('@echo author: "Fernando Roa" >> DphylogenyVig.Rmd')
      shell('@echo date: "23 08 2019" >> DphylogenyVig.Rmd')
      shell('@echo output: >> DphylogenyVig.Rmd')
      shell('@echo   prettydoc::html_pretty: >> DphylogenyVig.Rmd')
      shell('@echo     theme: leonids >> DphylogenyVig.Rmd')
      shell('@echo     highlight: github >> DphylogenyVig.Rmd')
      shell('@echo     toc: true >> DphylogenyVig.Rmd')
      shell('@echo     toc_depth: 1 >> DphylogenyVig.Rmd')
      shell('@echo     number_sections: true >> DphylogenyVig.Rmd')
      shell('@echo vignette: ^> >> DphylogenyVig.Rmd')
      shell("@echo   %\\VignetteDphylogenyVigEntry{4. Plotting alongside phylogeny} >> DphylogenyVig.Rmd")
      shell('@echo   %\\VignetteEngine{knitr::rmarkdown} >> DphylogenyVig.Rmd')
      shell('@echo   %\\VignetteEncoding{UTF-8} >> DphylogenyVig.Rmd')
      shell('@echo --- >> DphylogenyVig.Rmd')
      shell('@echo(     >> DphylogenyVig.Rmd')
      shell('@echo pandoc ^> 2 not available, visit https://ferroao.gitlab.io/idiogramfishhelppages >> DphylogenyVig.Rmd')
      
    #
    #   create new EhumanVig.Rmd
    #
      shell('@echo --- > EhumanVig.Rmd')
      shell('@echo title: "5. Human" >> EhumanVig.Rmd')
      shell('@echo author: "Fernando Roa" >> EhumanVig.Rmd')
      shell('@echo date: "23 08 2019" >> EhumanVig.Rmd')
      shell('@echo output: >> EhumanVig.Rmd')
      shell('@echo   prettydoc::html_pretty: >> EhumanVig.Rmd')
      shell('@echo     theme: leonids >> EhumanVig.Rmd')
      shell('@echo     highlight: github >> EhumanVig.Rmd')
      shell('@echo     toc: true >> EhumanVig.Rmd')
      shell('@echo     toc_depth: 1 >> EhumanVig.Rmd')
      shell('@echo     number_sections: true >> EhumanVig.Rmd')
      shell('@echo vignette: ^> >> EhumanVig.Rmd')
      shell("@echo   %\\VignetteEhumanVigEntry{5. Human} >> EhumanVig.Rmd")
      shell('@echo   %\\VignetteEngine{knitr::rmarkdown} >> EhumanVig.Rmd')
      shell('@echo   %\\VignetteEncoding{UTF-8} >> EhumanVig.Rmd')
      shell('@echo --- >> EhumanVig.Rmd')
      shell('@echo(     >> EhumanVig.Rmd')
      shell('@echo pandoc ^> 2 not available, visit https://ferroao.gitlab.io/idiogramfishhelppages >> EhumanVig.Rmd')
      
    #
    #   create new AplotIdiogramsVig.Rmd
    #
      shell('@echo --- > AplotIdiogramsVig.Rmd')
      shell('@echo title: "1. Plotting I" >> AplotIdiogramsVig.Rmd')
      shell('@echo author: "Fernando Roa" >> AplotIdiogramsVig.Rmd')
      shell('@echo date: "23 08 2019" >> AplotIdiogramsVig.Rmd')
      shell('@echo output: >> AplotIdiogramsVig.Rmd')
      shell('@echo   prettydoc::html_pretty: >> AplotIdiogramsVig.Rmd')
      shell('@echo     theme: leonids >> AplotIdiogramsVig.Rmd')
      shell('@echo     highlight: github >> AplotIdiogramsVig.Rmd')
      shell('@echo     toc: true >> AplotIdiogramsVig.Rmd')
      shell('@echo     toc_depth: 1 >> AplotIdiogramsVig.Rmd')
      shell('@echo     number_sections: true >> AplotIdiogramsVig.Rmd')
      shell('@echo vignette: ^> >> AplotIdiogramsVig.Rmd')
      shell("@echo   %\\VignetteAplotIdiogramsVigEntry{1. Plotting I} >> AplotIdiogramsVig.Rmd")
      shell('@echo   %\\VignetteEngine{knitr::rmarkdown} >> AplotIdiogramsVig.Rmd')
      shell('@echo   %\\VignetteEncoding{UTF-8} >> AplotIdiogramsVig.Rmd')
      shell('@echo --- >> AplotIdiogramsVig.Rmd')
      shell('@echo(     >> AplotIdiogramsVig.Rmd')
      shell('@echo pandoc ^> 2 not available, visit https://ferroao.gitlab.io/idiogramfishhelppages >> AplotIdiogramsVig.Rmd')
      
    } # if windows
  } # pandoc < 2
} # len

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
# Example data.frame to write in R, use: (column OTU is optional if only 1 OTU)
mydfChrSize<-read.table(text=
"            OTU chrName shortArmSize longArmSize 
  \"Species one\"   1     1.5         2.0  
  \"Species one\"   2     2.0         2.5  
  \"Species one\"   3     1.0         1.5
  \"Species one\"   B     2.0         3.5"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F------------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(mydfChrSize) , full_width = F
                           , font_size = 10)

## ---- eval=FALSE--------------------------------------------------------------
#  setwd("~/folder/subfolder")

## ---- eval=FALSE--------------------------------------------------------------
#  mydfChrSize<-read.csv("somefile.csv")

## ---- eval=FALSE--------------------------------------------------------------
#  bigdfOfChrSize <- edit(bigdfOfChrSize, edit.row.names = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  colnames(mydfChrSize)<-c("OTU", "chrName","shortArmSize","longArmSize")

## -----------------------------------------------------------------------------
# From scratch:
mydfMarkColor<-read.table(text=
" markName markColor  style
        5S       red   dots
       45S     green square
      DAPI      blue     cM    # <- new style
       CMA    yellow square
\"B mark\"     black square"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F------------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(mydfMarkColor) , full_width = F
                           , font_size = 10
                          # , bootstrap_options = c("striped", "hover", "condensed") 
                          )

## ---- eval=FALSE--------------------------------------------------------------
#  colnames(mydfMarkColor)<-c("markName", "markColor","style")
#  # if style column is not present it will be filled with "square"

## -----------------------------------------------------------------------------
# We will use column OTU if data.frame because chromosome size df has it
mydfOfMarks<-read.table(text=
"            OTU chrName markName chrRegion markSize markDistCen
\"Species one\"      1      45S       p       NA         NA     # no measure means whole arm
\"Species one\"      1       5S       q      0.5         0.5
\"Species one\"      B  \"B mark\"    w       NA         NA     # w for whole chromosome
\"Species one\"      2     45S        p        1         1.0
\"Species one\"      3     DAPI       q        1         1.0
\"Species one\"      1     DAPI       cen
\"Species one\"      3      CMA       cen", header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(mydfOfMarks) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed")
                          )

## ---- eval=FALSE--------------------------------------------------------------
#  colnames(mydfMarkColor)<-c("OTU", "chrName","markName","chrRegion","markSize","markDistCen")

## -----------------------------------------------------------------------------
# We will use column note to add a note to the right of the karyotype of the OTU in column OTU
notesdf<-read.table(text=
"            OTU    note
\"Species one\"   \"Author notes\"  ", header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ----example_M1, echo=TRUE, results="hide", fig.width=12, fig.height=6, message=FALSE, dev='png'----
# svg("mydfChrSize.svg",width=12,height=6 )

par(mar = c(0, 0, 0, 0))

plotIdiograms(dfChrSize= mydfChrSize,     # chr. size data.frame
              dfMarkPos= mydfOfMarks,     # mark position data.frame (inc. cen.)
              dfMarkColor=mydfMarkColor,  # mark style d.f.
              
              distTextChr = .7,           # separation among text and chr names and ind.              
              orderBySize = FALSE,        # do not order chr. by size
              karHeiSpace=1.6,            # vertical size of karyotype including spacer
              
              fixCenBorder = TRUE         # use chrColor as border color of cen. or cen. marks
              ,legendWidth = .8           # legend item width
              ,legendHeight = .5          # legend item height
              ,markLabelSpacer = 2        # legend spacer
              
              ,rulerPos=0,                # ruler position
              ruler.tck=-0.01,            # ticks of ruler size and orientation
              
              notes=notesdf               # data.frame with notes NEW
              ,notesTextSize = 1.3        # font size of notes
              ,notesPos = .2              # space from chr. (right) to note
              
              ,ylimBotMod = 1             # modify ylim bottom argument
              ,ylimTopMod = 0             # modify ylim top argument
              ,xlimLeftMod = 2            # modify left xlim
              ,xlimRightMod = 3           # modify right xlim
              ,lwd.cM = 2                 # thickness of cM marks 

)
# dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
# cat(paste0("![](mydfChrSize.png)" ) )
# cat(paste0("![](mydfChrSize.svg)" ) )

## ----example_M1cen0, echo=TRUE, results="hide", fig.width=4.5, fig.height=4.5, message=FALSE,dev='png'----

png("mydfChrSize2.png", width=550, height=550)
par(mar = c(0, 0, 0, 0))
plotIdiograms(dfChrSize   = bigdfOfChrSize[1:8,],  # chr. size data.frame
              dfMarkColor = mydfMarkColor,# mark style df
              dfMarkPos   = bigdfOfMarks, # mark position df
              
              centromereSize = 0,         # <- HERE
              
              roundness=3,                # vertices roundness  
              chrSpacing = .7,            # space among chr.
              karHeight = 2,              # karyotype rel. height 
              karHeiSpace=4,              # vertical size of karyotype including spacer
              amoSepar= 2.5,              # separation among karyotype
              
              indexIdTextSize=.8,         # font size of chr. name and indices
              karIndexPos = .1,           # position of kar. index
              markLabelSize=.7,           # font size of mark legends
              fixCenBorder = FALSE,       # do not use chrColor as border color of cen. or cen. marks
              distTextChr = .8,           # separation among chr. and ind.
              
              rulerPos= 0,                # ruler position
              ruler.tck=-0.01,            # ticks of ruler size and orientation
              
              xlimLeftMod = 2,            # modify xlim left argument 
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
              ,lwd.cM = 2                 # thickness of cM marks 
              )
dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
cat(paste0("![](mydfChrSize2.png)" ) )

## ---- eval=FALSE--------------------------------------------------------------
#  unique(mydfOfMarks$markName)

## ---- echo=TRUE, results="hide", fig.width=10, fig.height=4.5, message=FALSE,dev='png', eval=TRUE----

charVectorCol <- c("tomato3","darkolivegreen4","dfsd","blue","green")
png("dfOfChrSizeVector.png", width=1000, height=450)
par(mar=rep(0,4))

# Modify size of kar. to use rulerInterval and ceilingFactor (>= 1.13)  
quo<-9
dfOfChrSize2<-dfOfChrSize
dfOfChrSize2$shortArmSize<-dfOfChrSize$shortArmSize/quo
dfOfChrSize2$longArmSize<-dfOfChrSize$longArmSize/quo
dfOfMarks2b<-dfOfMarks2
dfOfMarks2b$markSize<-dfOfMarks2$markSize/quo
dfOfMarks2b$markDistCen<-dfOfMarks2$markDistCen/quo

plotIdiograms(dfChrSize = dfOfChrSize2,    # d.f. of chr. sizes
              dfMarkPos = dfOfMarks2b,     # d.f. of marks' positions
              defaultStyleMark = "cM",     # forces "cM" style in d.f dfMarkColor (exc. 5S)
              
              mycolors = charVectorCol,    # colors to use
              
              distTextChr = .5,            # separ. text and chr.
              
              markLabelSize=.7,            # font size for labels (legend)
              lwd.cM=2,                    # width of cM marks
              legendWidth=0.9,             # legend item width
              
              rulerPos= 0,                 # ruler position
              ruler.tck=-0.01,             # ruler tick orientation and length
              rulerNumberSize=.5           # ruler font size
              
              ,xlimRightMod = 1            # modify xlim right arg.
)
dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
cat(paste0("![](dfOfChrSizeVector.png)" ) )

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

              karHeiSpace = 3,           # karyotype height inc. spacing
              karIndexPos = .2,          # move karyotype index
              
              chrId="original",          # use original name of chr.
              OTUTextSize = .7,          # size of OTU name

              legendHeight= 1,           # height of legend labels
              legendWidth = 1,           # width of legend labels
              # ,legend="inline"
              fixCenBorder = TRUE,       # use chrColor as border color of cen. or cen. marks

              rulerPos= 0,               # position of ruler
              ruler.tck=-0.02,           # size and orientation of ruler ticks
              rulerNumberPos=.9,         # position of numbers of rulers

              xlimLeftMod=1,             # modify xlim left argument of plot
              xlimRightMod=2,            # modify xlim right argument of plot
              ylimBotMod= .2             # modify ylim bottom argument of plot
              
              # GRAPHICAL PARAMETERS FOR CIRCULAR PLOT
  
              ,circularPlot = T          # circularPlot
              ,shrinkFactor = .9         # percentage 1 = 100% of circle with chr.
              ,circleCenter = 3          # X coordinate of circleCenter (affects legend pos.)
              ,chrLabelSpacing = .9      # chr. names spacing
              
              ,OTUsrt = 0                # angle for OTU name (or nuber)
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
              cenColor  = "black",              # cen. color for GISH
              roundness = 5,                    # corner roundness
              chrWidth = 1,                     # chr width
              orderBySize = F                   # do not order chr. by size

              ,addOTUName = F                   # do not add OTU name
              ,legendHeight = 2.5               # labels separ y axis
              
              # circular plot parameters
              ,circularPlot=TRUE                   
              ,radius=5                         # basic radius
              ,useOneDot=F                      # use two dots
              ,chrLabelSpacing = 1              # chr nama spacing
              ,rotation = .1                    # anti-clockwise rotation
              ,shrinkFactor = .95               # % of circle use
)

## ---- echo=TRUE, results="asis", fig.width=9, fig.height=9, message=FALSE,dev='png', eval=TRUE----

# Load data .gb downloaded from: https://www.ncbi.nlm.nih.gov/nuccore/NZ_CP009939.1
filename    <- system.file("extdata", "sequence.gb", package = "idiogramFISH")

mylist<-genBankReadIF(filename)
names(mylist)

# Authors of plasmid sequence
paste(mylist$gbdfMain[which(mylist$gbdfMain$field=="AUTHORS"),][1,2] )
# mylist$gbdfSourceMeta
# View(mylist$gbdfMain)
# View(mylist$gbdfAssemblyMeta)
# mylist$gbdfAnnoMeta
# View(mylist$gbdfCDS)
# View(mylist$gene)

# create plasmid size data data.frame
myPlasmiddf <- data.frame(chrName=1, chrSize=mylist$source$end)
myPlasmiddf$OTU<-mylist$gbdfMain[which(mylist$gbdfMain$field=="DEFINITION"),]$value
myPlasmiddf$OTU<-gsub(", complete sequence.","",myPlasmiddf$OTU)

# Creating mark info data.frame
mylist$gene$markPos <-pmin(as.numeric(mylist$gene$begin),as.numeric(mylist$gene$end) )
mylist$gene$markSize<-abs(as.numeric(mylist$gene$end)-as.numeric(mylist$gene$begin) )
mylist$gene$markName<-mylist$gene$locus_tag

# Replace codes with names
mylist$gene[which(!is.na(mylist$gene$gene) ),]$markName<-
  mylist$gene[which(!is.na(mylist$gene$gene) ),]$gene

marksDf<-mylist$gene[,c("markName","markPos","markSize"),]
# manually move away some names 

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
marksDf<-rbind(marksDf,c(paste0("START",paste0(rep(" ",20), collapse="")),1,NA,1))

# add column - name of plasmid
marksDf$OTU <- myPlasmiddf$OTU

# create mark general data data.frame
markStyle2 <-markStyle  <- idiogramFISH:::makedfMarkColorMycolors(unique(marksDf$markName),
                                                     c("black","forestgreen","cornflowerblue") )
# 1st plot with cM style of marks
markStyle$style<-"cM"

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

# overlap square style of marks with second plot

# plot over previous plot
plotIdiograms(dfChrSize = myPlasmiddf, dfMarkPos = marksDf, dfMarkColor = markStyle2, circularPlot = T,
              shrinkFactor = 1, roundness = 21, chrWidth = .1, labelSpacing = 2, chrId="",
              ylimBotMod = 0, ylimTopMod = 0, rotation=0, legend="", 
              addOTUName = FALSE,       # do not add OTU name, see above
              callPlot = FALSE          # do not create a new plot
)



## ---- echo=TRUE, results="asis", fig.width=9, fig.height=9, message=FALSE,dev='png', eval=FALSE----
#  
#  library(idiogramFISH)
#  # Download prokaryote genome from:
#  # https://www.ncbi.nlm.nih.gov/nuccore/NC_014248.1
#  # Choose Customize View -> Basic Features -> genes, CDS
#  # Send To -> File -> Create File
#  
#  # Use your file name:
#  # filename2<- "nostoc.gb" # 5 Mbytes
#  mylist<-genBankReadIF(filename2) # Wait 6 seconds to load ...
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
#  # Creating mark info data.frame
#  mylistSel<-mylist[which(names(mylist) %in%
#                            setdiff( names(mylist) , c("gbdfMain","gbdfAnnoMeta","source","CDS") ) )]
#  mylistSelDF<-dplyr::bind_rows(mylistSel, .id="feature")
#  
#  mylistSelDF$markPos <-pmin(as.numeric(mylistSelDF$begin),as.numeric(mylistSelDF$end) )
#  mylistSelDF$markSize<-abs(as.numeric(mylistSelDF$end)-as.numeric(mylistSelDF$begin) )
#  mylistSelDF$markName<-mylistSelDF$locus_tag
#  
#  # Replace codes with names
#  mylistSelDF[which(!is.na(mylistSelDF$gene) ),]$markName<-
#    mylistSelDF[which(!is.na(mylistSelDF$gene) ),]$gene
#  
#  marksDf<-mylistSelDF[,c("markName","markPos","markSize","feature"),]
#  
#  # manually move away some names
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
#  marksDf<-rbind(marksDf,c("START",1,NA,"start"))
#  
#  # add mandatory column
#  marksDf$chrName<-1
#  
#  # add column OTU, when in main data.frame
#  marksDf$OTU <- myProkaryotedf$OTU
#  
#  unique(marksDf$feature)
#  
#  # create mark general data data.frame
#  markStyle  <- idiogramFISH:::makedfMarkColorMycolors(unique(marksDf$markName),
#                                                       c("black","forestgreen","cornflowerblue") )
#  
#  markStyle[which(markStyle$markName %in%
#                    marksDf[which(marksDf$feature %in% c("tRNA","tmRNA") ),]$markName
#                  ),]$markColor<-"magenta"
#  markStyle[which(markStyle$markName %in%
#                    marksDf[which(marksDf$feature %in% c("regulatory","ncRNA") ),]$markName
#                  ),]$markColor<-"tomato3"
#  markStyle[which(markStyle$markName %in%
#                    marksDf[which(marksDf$feature %in% "rRNA" ),]$markName
#                  ),]$markColor<-"red2"
#  markStyle[which(markStyle$markName %in%
#                    marksDf[which(marksDf$feature %in% "misc_feature" ),]$markName
#                  ),]$markColor<-"lightsalmon"
#  
#  # duplicate mark style data.frame for square style
#  markStyle2 <- markStyle
#  
#  # cM style d.f.
#  markStyle$style<-"cM"
#  
#  # prefix to remove from mark names
#  mypattern<-sub("([[:alnum:]]+_).*","\\1",trimws(marksDf$markName[1]) )
#  
#  # png("NOSTOC2.png", width=9500, height=9500) #  14 Mbytes
#  pdf("NOSTOC2.pdf", width=130, height=130)     #   6 Mb
#  # svg("NOSTOC2.svg", width=130, height=130)   # 100 Mb
#  
#  par(mar=rep(0,4))
#  plotIdiograms(dfChrSize = myProkaryotedf,  # chr. data d.f.
#                dfMarkPos = marksDf,         # mark pos d.f.
#                dfMarkColor = markStyle,     # mark style d.f.
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
#  # plot over previous plot square style
#  plotIdiograms(dfChrSize = myProkaryotedf, dfMarkPos = marksDf, dfMarkColor = markStyle2, circularPlot = T,
#                shrinkFactor = 1, roundness = 21,   chrWidth = .02, chrId="",
#                ylimBotMod = -.5, ylimTopMod = -.5, xlimLeftMod = .3,
#                xlimRightMod = .3, radius=.1, n=100, rotation=0,
#                legend="",                # do not add legend for marks
#                addOTUName = FALSE,       # do not add. OTU name
#                callPlot = FALSE          # plot over previous plot
#  )
#  dev.off()

## ---- results="asis", comment=NA, echo=FALSE, eval=TRUE-----------------------
    nostocFile1 <- "../man/figures/nostoc.jpg"
    nostocFile1 <- normalizePath(nostocFile1)
    knitr::include_graphics(nostocFile1)  
    nostocFile2 <- "../man/figures/nostoSmall.jpg"
    nostocFile2 <- normalizePath(nostocFile2)
    knitr::include_graphics(nostocFile2)

## ---- eval=F------------------------------------------------------------------
#  head(bigdfOfChrSize,15)

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(head(bigdfOfChrSize,15)) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed"),
                 
) 

## ---- width=45----------------------------------------------------------------
data("dfMarkColor")

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(dfMarkColor) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed")
                          )

## -----------------------------------------------------------------------------
data("bigdfOfMarks")

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(bigdfOfMarks) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed")
                          )

## ----example_M3, echo=TRUE, results="hide", fig.width=6, fig.height=13, message=FALSE, dev='png'----
par(mar = c(0, 0, 0, 0))
plotIdiograms(dfChrSize  =bigdfOfChrSize,# chr. sizes
              dfMarkColor=dfMarkColor,   # mark characteristics, optional in dev version. see above. 
              dfMarkPos  =bigdfOfMarks,  # mark positions (inc. cen. marks)

              karHeight=2.5,             # karyotype rel. height
              karHeiSpace=6,             # karyotype vertical size with spacing
              chrWidth = .35,            # chr. width
              amoSepar = 2,              # Vertical separation of kar. when karSepar = TRUE
              
              roundness = 10,            # roundness of chr. vertices
              distTextChr=.8,            # distance of chr. to text
              chrIndex = "AR",           # add arm ratio only. For v. >=1.12
              morpho="Guerra",           # add chr. morphology by Guerra, see above. For v. >=1.12
              indexIdTextSize=.6,        # font size of indices and chr. name
              OTUTextSize=.9,            # font size of OTU names
              
              markLabelSize=.7,          # font size of legend
              fixCenBorder = TRUE,       # use chrColor as border color of cen. or cen. marks
              legendHeight = 2,          # height of labels
              
              rulerPos=-1,               # position of ruler
              # rulerPosMod=3,           # modify position of ruler
              ruler.tck=-0.004,          # size and orient. of ticks in ruler
              rulerNumberPos=.4,         # position of numbers of ruler
              rulerNumberSize=.4,        # font size of ruler

              xlimRightMod = 3,          # modify xlim left argument 
              xlimLeftMod = 2,           # modify xlim left argument 
              ylimBotMod = 0,            # modify ylim bottom argument
              ylimTopMod = -.3           # modify ylim top argument
              #,asp=1                    # y x aspect ratio
)

# dev.off() # for png()

## ---- results="asis", comment=NA, echo=FALSE, eval=FALSE----------------------
#  cat(paste0("![](bigdfOfChrSize.png)" ) )

## ---- comment=NA, echo=F------------------------------------------------------
cat(paste0("parentalAndHybChrSize" ) )

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(parentalAndHybChrSize) , full_width = F
                           , font_size = 10)

## ---- comment=NA, echo=F------------------------------------------------------
cat(paste0("dfAlloParentMarks" ) )

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(dfAlloParentMarks) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed")
                          )

## -----------------------------------------------------------------------------
# We will use column note to add a note to the right of the karyotype of the OTU in column OTU
notesdf2<-read.table(text=
"           OTU                note
\"Parental 1\"     \"Parental One\"  
\"Parental 2\"     \"Parental Two\"  
\"Allopolyploid\"  Allopolyploid  ", header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

## ---- echo=TRUE, results="hide", fig.width=7, fig.height=9, message=FALSE,dev='png'----

# svg("gish.svg",width=7,height=9 )
#png("parentalAndHybChrSize.png", width=700, height=900)
par(mar=rep(0,4) )
plotIdiograms(dfChrSize = parentalAndHybChrSize,  # d.f. of chr. sizes
              dfMarkPos = dfAlloParentMarks,      # d.f. of marks' positions
              cenColor  = NULL,            # cen. color for GISH
              
              karHeiSpace=5,               # karyotype height including spacing
              karSepar = FALSE,            # equally sized (height) karyotypes
              
              rulerPos=-.7,                # ruler position
              ruler.tck= -0.006,           # ruler tick orientation and length
              rulerNumberSize=.4           # ruler font size
              
              ,legend=""                   # no legend
              
              ,notes=notesdf2              # data.frame with notes NEW
              #,OTUasNote=TRUE             # TRY THIS (OTU name to the right)
              ,notesTextSize = 1.3         # font size of notes
              ,notesPos = 1.5              # space from chr. (right) to note
              
              ,ylimBotMod = 1              # ylim bottom argument mod.
)
# dev.off()

## ---- echo=TRUE, results="hide", fig.width=10, fig.height=10, message=FALSE,dev='png', warning=FALSE----
#fig.width=10, fig.height=10
# modify data in millions to hundreds of millions of Mb
bigdfOfChrSize3_100Mb<-bigdfOfChrSize3Mb[1:8,]
bigdfOfChrSize3_100Mb$chrSize<-bigdfOfChrSize3_100Mb$chrSize*100

bigdfOfMarks3_100Mb<-bigdfOfMarks3Mb
bigdfOfMarks3_100Mb$markPos<-bigdfOfMarks3_100Mb$markPos*100
bigdfOfMarks3_100Mb$markSize<-bigdfOfMarks3_100Mb$markSize*100

# merge data.frames in micrometers and number of bases
mixedThreeSpChrSize <- plyr::rbind.fill(bigdfOfChrSize[1:8,], bigdfOfChrSize3_100Mb)
# sort by OTU name
mixedThreeSpChrSize <- mixedThreeSpChrSize[order(mixedThreeSpChrSize$OTU),]
 
# merge marks in micrometers and bases
mixedThreeSpMarks <- plyr::rbind.fill(bigdfOfMarks , bigdfOfMarks3_100Mb)

par(mar=rep(0,4))
plotIdiograms(dfChrSize   = mixedThreeSpChrSize,  # chr. size data.frame
              dfMarkPos   = mixedThreeSpMarks,    # mark position df
              
              chrWidth=.6,                # width of chr.
              chrSpacing = .6,            # space among chr.
              karHeight = 3,              # kar. height without interspace
              karHeiSpace = 5,            # vertical size of karyotype including spacer
              amoSepar =2,                # separ. among kar.
              
              indexIdTextSize=.6,         # font size of chr. name and indices
              markLabelSize=.7,           # font size of mark legends
              distTextChr = .65,          # separation among chr. names and indices
              
              legendWidth = 1.5           # legend items width
              ,fixCenBorder = TRUE        # use chrColor as border color of cen. or cen. marks
              
              ,ylabline = -8              # position of Mb (title) in ruler               
              ,rulerPos= 0,               # ruler position
              ruler.tck=-0.005,           # ticks of ruler size and orientation
              rulerNumberPos =.7,         # position of numbers in ruler
              rulerNumberSize=.7,         # font size of ruler numbers
              rulerInterval = 1.5,        # ruler interval for micrometeres
              rulerIntervalMb = 150000000,# ruler interval for Mb
              ceilingFactor = 1,          # affects rounding for ruler max. value 
              
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
              #,asp=1                     # aspect of plot
)

## ---- eval=F------------------------------------------------------------------
#  head(mixedThreeSpChrSize,6)

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(head(mixedThreeSpChrSize,6)) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed"),
                 
) 

## ---- eval=F------------------------------------------------------------------
#  mixedThreeSpMarks[which(mixedThreeSpMarks$OTU %in% c("Species 1","Species 1 genome") ),]

## ---- echo=F------------------------------------------------------------------
options("scipen"=10)  
kableExtra::kable_styling(knitr::kable(mixedThreeSpMarks[which(mixedThreeSpMarks$OTU %in% c("Species 1","Species 1 genome") ),] 
 ) , full_width = F
                           , font_size = 10
                          , bootstrap_options = c("striped", "hover", "condensed"),
                 
) 

## ---- echo=TRUE, results="hide", fig.width=7, fig.height=7, message=FALSE,dev='png', warning=FALSE, eval=FALSE----
#  # fig.width=7, fig.height=7
#  bigdfOfChrSize3_100Mb<-bigdfOfChrSize3Mb
#  bigdfOfChrSize3_100Mb$chrSize<-bigdfOfChrSize3Mb$chrSize*33
#  
#  bigdfOfMarks3_100Mb<-bigdfOfMarks3Mb
#  bigdfOfMarks3_100Mb$markPos<-bigdfOfMarks3_100Mb$markPos*33
#  bigdfOfMarks3_100Mb$markSize<-bigdfOfMarks3_100Mb$markSize*33
#  
#  par(mar=rep(0,4))
#  plotIdiograms(dfChrSize   = bigdfOfChrSize3_100Mb,  # chr. size data.frame
#                dfMarkPos   = bigdfOfMarks3_100Mb,    # mark position df
#  
#                chrWidth=.6,                # width of chr.
#                chrSpacing = .6,            # space among chr.
#                karHeight = 3,              # kar. height without interspace
#                karHeiSpace = 5,            # vertical size of karyotype including spacer
#                amoSepar =2,                # separ. among kar.
#  
#                indexIdTextSize=.6,         # font size of chr. name and indices
#                markLabelSize=.7,           # font size of mark legends
#                distTextChr = .65,          # separation among chr. names and indices
#  
#                fixCenBorder = TRUE         # use chrColor as border color of cen. or cen. marks
#                ,legendWidth = 1.5          # legend items width
#  
#                ,ylabline = -2              # position of Mb (title) in ruler
#                ,rulerPos= 0,               # ruler position
#                ruler.tck=-0.005,           # ticks of ruler size and orientation
#                rulerNumberPos =.7,         # position of numbers in ruler
#                rulerNumberSize=.7,         # font size of ruler numbers
#                rulerInterval = 1.5,        # ruler interval for micrometeres
#                rulerIntervalMb = 50000000, # ruler interval for Mb
#  
#                ylimBotMod = 0.4,           # modify ylim bottom argument
#                ylimTopMod = 0              # modify ylim top argument
#  
#                           ####  NEW    #####
#                ,threshold = 90             # this will allow to not to shrink data greater than 350 Mb
#  )

## ---- echo=TRUE, results="hide", fig.width=10, fig.height=10, message=FALSE,dev='png', warning=FALSE----
#fig.width=10, fig.height=10
# merge data.frames in micrometers and cM
bigdfOfChrSize3cM<-bigdfOfChrSize3Mb[1:8,]
bigdfOfChrSize3cM$chrSize<-bigdfOfChrSize3cM$chrSize/100000
mixedThreeSpChrSize <- plyr::rbind.fill(bigdfOfChrSize[1:8,], bigdfOfChrSize3cM)

# sort by OTU name
mixedThreeSpChrSize <- mixedThreeSpChrSize[order(mixedThreeSpChrSize$OTU),]

# create data with cM. markSize col. is not necessary because style is cM
bigdfOfMarks3cM<-bigdfOfMarks3Mb
bigdfOfMarks3cM$markPos<-bigdfOfMarks3Mb$markPos/100000
bigdfOfMarks3cM$markSize<-NA
# As we want only the cM idiograms to be plotted as cM (lines), change mark names 
bigdfOfMarks3cM$markName<-paste0("cM",bigdfOfMarks3cM$markName)

# d.f of all marks
mixedThreeSpMarks <- plyr::rbind.fill(bigdfOfMarks , bigdfOfMarks3cM)

# create a data.frame with mark characteristics
mixedDfMarkStyle  <- idiogramFISH:::makedfMarkColorMycolors(unique(mixedThreeSpMarks$markName), 
                                                            c("red","green","blue","yellow")
                                                            )

# mark names of cM marks with "cM" style (lines): not dots, not squares
mixedDfMarkStyle[which(mixedDfMarkStyle$markName %in% 
                         grep("cM", mixedDfMarkStyle$markName, value=TRUE) ) ,]$style<-"cM"

par(mar=rep(0,4))
plotIdiograms(dfChrSize   = mixedThreeSpChrSize,  # chr. size data.frame
              dfMarkPos   = mixedThreeSpMarks,    # mark position data.frame
              dfMarkColor = mixedDfMarkStyle,     # mark style data.frame
              
              chrWidth=.6,                # width of chr.
              chrSpacing = .7,            # space among chr.
              
              specialOTUNames = bigdfOfMarks3cM$OTU, # OTUs in this object will have different ruler units 
              specialyTitle = "cM",       # ruler title for specialOTUNames
              specialChrWidth = .2,       # modify chr width of OTUs in specialOTUNames
              specialChrSpacing = 1.1,    # modify chr spacing of OTUs in specialOTUNames

              karHeight = 3,              # kar. height without interspace
              karHeiSpace = 5,            # vertical size of karyotype including spacer
              amoSepar =2,                # separ. among kar.
              
              indexIdTextSize=.6,         # font size of chr. name and indices
              distTextChr = .65,          # separation among chr. names and indices
              
              protruding = 1,             # extension of cM mark type
              pattern = "cM",             # regex pattern to remove from mark names
              markLabelSize=.7            # font size of mark legends
              ,legendWidth = 2            # legend items width
              ,fixCenBorder = TRUE        # use chrColor as border color of cen. or cen. marks
              ,lwd.cM = 2                 # thickness of cM marks 
              
              ,ylabline = -8              # position of Mb or cM (title) in ruler               
              ,rulerPos= 0,               # ruler position
              ruler.tck=-0.005,           # ticks of ruler size and orientation
              rulerNumberPos =.7,         # position of numbers in ruler
              rulerNumberSize=0.7,        # font size of ruler numbers
              rulerIntervalcM = 12,       # ruler interval for OTU in specialOTUnames and MbThreshold not met
              ceilingFactor = 1,          # affects max. value in ruler. See also rulerInterval
              
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
)

## -----------------------------------------------------------------------------
# Example data.frame written in R, use
dfwithgroups<-read.table(text="
      chrName shortArmSize longArmSize group
1        1            3           5     1
2        1            3.2         5.5   1
3        1            3.5         4.8   1
4        4            1           3     NA
5        5            3           5     NA
6        X            4           6     NA", header=TRUE, stringsAsFactors=F)

## ---- echo=F------------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(dfwithgroups) , full_width = F
                           , font_size = 10)

## -----------------------------------------------------------------------------
dfwithHetero<-read.table(text="
       chrName shortArmSize longArmSize group
1        1A           3           5     1
2        1B           3           5     1
4        2            1           3     NA
5        3            3           5     NA
6        4            4           6     NA", header=TRUE, stringsAsFactors=FALSE)

## ---- echo=F------------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(dfwithHetero) , full_width = F
                           , font_size = 10)

## -----------------------------------------------------------------------------
dfOfMarksHetero<-read.table(text=
"     chrName markName chrRegion markSize markDistCen
1       1A       5S       p        1         0.9
2       1B      45S       p        1         0.9
3       2       CMA       q        1         1.0
4       3      DAPI       q        1         1.0", header=TRUE, stringsAsFactors=FALSE)

## ---- echo=F------------------------------------------------------------------
# just to show it here
kableExtra::kable_styling(knitr::kable(dfOfMarksHetero) , full_width = F
                           , font_size = 10)

## ----example_G2, echo=TRUE, results="hide", fig.width=13.5, fig.height=8, message=FALSE, dev='png'----

# svg("dfwithHetero.svg",width=13.5,height=8 )
par(mar=rep(0,4)) 

dfwithHetero$OTU<-"hetero"
dfwithgroups$OTU<-"first"
both<-plyr::rbind.fill(dfwithHetero,dfwithgroups)
dfOfMarksHetero$OTU<-"hetero"
plotIdiograms(dfChrSize=both,    # chr. sizes
              dfMarkPos=dfOfMarksHetero, # position of marks
              karHeiSpace = 4,
              
              chrId="original",          # chr. name in df.
              chrIndex = "",             # do not add chr. indices
              morpho="",                 # do not add chr. morphologies  
              karIndex = FALSE,          # do not add karyotype indices
              distTextChr = .8,          # distance from text to chr.
              
              markDistType="cen",        # mark position measured to center of mark
              orderBySize = FALSE,       # do not order chr. by size
              
              ruler=FALSE                # do not plot ruler
              
              ,ylimBotMod = 1            # modify ylim bottom argument
              ,legendWidth = 1           # width of legend
)
# dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
# cat(paste0("![](mydChrSizeHolo.png)" ) )
# cat(paste0("![](dfwithHetero.svg)" ) )

