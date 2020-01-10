## ---- echo=F, warning=FALSE, error=FALSE, comment=NA--------------------------
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
  shell("del index.Rmd")
  shell("del DphylogenyVig.Rmd")
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
  } # if windows
} # pandoc < 2

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
      DAPI      blue square
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

## ----example_M1, echo=TRUE, results="hide", fig.width=13, fig.height=8, message=FALSE, dev='png'----
# svg("mydfChrSize.svg",width=13,height=8 )

# par(mar = c(0, 0, 0, 0), omi=rep(0,4), oma=rep(0,4) )

plotIdiograms(dfChrSize= mydfChrSize,     # chr. size data.frame
              dfMarkPos= mydfOfMarks,     # mark position data.frame (inc. cen.)
              dfMarkColor=mydfMarkColor,  # mark style d.f.
              roundness=3,                # vertices roundness  
              distTextChr = .7,           # separation among text and chr names and ind.              
              orderBySize = FALSE,        # do not order chr. by size
              karHeiSpace=1.6,            # vertical size of karyotype including spacer
              
              legendWidth = .8            # legend item width
              ,legendHeight = .5          # legend item height
              ,markLabelSpacer = 2        # legend spacer
              
              ,rulerPos=-1,               # ruler position
              ruler.tck=-0.01,            # ticks of ruler size and orientation
              
              notes=notesdf               # data.frame with notes NEW
              ,notesTextSize = 1.3        # font size of notes
              ,notesPos = .2              # space from chr. (right) to note
              
              ,ylimBotMod = 0.4           # modify ylim bottom argument
              ,ylimTopMod = 0             # modify ylim top argument
              ,xlimLeftMod = 2            # modify left xlim
              ,xlimRightMod = 3           # modify right xlim
              ,asp=1                      # y x aspect ratio

)
# dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
# cat(paste0("![](mydfChrSize.png)" ) )
# cat(paste0("![](mydfChrSize.svg)" ) )

## ----example_M1cen0, echo=TRUE, results="hide", fig.width=7, fig.height=4.5, message=FALSE,dev='png'----

png("mydfChrSize2.png", width=700, height=950)
plotIdiograms(dfChrSize   = bigdfOfChrSize[1:14,],  # chr. size df
              dfMarkColor = mydfMarkColor,# mark style df
              dfMarkPos   = bigdfOfMarks, # mark position df
              
              # cen. marks NOT AVAILABLE for centromereSize = 0
              centromereSize = 0,         # <- HERE
              roundness=3,                # vertices roundness  
              chrSpacing = .7,            # space among chr.
              karHeight = 3,              # karyotype rel. height 
              karHeiSpace=5,              # vertical size of karyotype including spacer
              amoSepar= 2.5,              # separation among karyotype
              indexIdTextSize=.8,         # font size fo chr name and indices
              karIndexPos = .1,           # position of kar. index
              markLabelSize=.7,           # font size of mark legends
              distTextChr = .8,           # separation among chr. and ind.
              
              rulerPos=-.8,               # ruler position
              ruler.tck=-0.01,            # ticks of ruler size and orientation
              
              xlimLeftMod = 2,            # modify xlim left argument 
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
              ,asp=1                      # y x aspect
)
dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
cat(paste0("![](mydfChrSize2.png)" ) )

## ---- eval=FALSE--------------------------------------------------------------
#  unique(c(dfOfMarks$markName,dfOfCenMarks$markName) )

## ---- eval=FALSE, dev='svg'---------------------------------------------------
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
#                distTextChr = .5             # separ. text and chr.
#                ,karHeiSpace=1.6,            # karyotype height including spacing
#                markLabelSize=.7,            # font size for labels (legend)
#  
#                rulerPos=-.5,                # ruler position
#                ruler.tck=-0.01,             # ruler tick orientation and length
#                rulerNumberSize=.5           # ruler font size
#                ,asp=1                       # y x aspect
#                ,xlimRightMod = 1            # modify xlim right arg.
#  )
#  dev.off()

## ---- eval=F------------------------------------------------------------------
#  head(bigdfOfChrSize,30)

## ---- echo=F------------------------------------------------------------------
kableExtra::kable_styling(knitr::kable(head(bigdfOfChrSize,30)) , full_width = F
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

plotIdiograms(dfChrSize  =bigdfOfChrSize,# chr sizes
              dfMarkColor=dfMarkColor,   # mark characteristics, optional in dev version. see above. 
              dfMarkPos  =bigdfOfMarks,  # mark positions (inc. cen. marks)
              # cen. marks in bigdfOfMarks

              karHeight=2.5,             # karyotype rel. height
              karHeiSpace=6,             # karyotype vertical size with spacing
              amoSepar = 2,              # Vertical separation of kar. when karSepar = TRUE
              
              centromereSize = 1,        # apparent size of cen.
              roundness = 10,            # roundness of chr. vertices
              distTextChr=.8,            # distance of chr. to text
              chrIndex = "AR",           # add arm ratio only. For v. >=1.12
              morpho="Guerra",           # add chr. morphology by Guerra, see above. For v. >=1.12
              indexIdTextSize=.6,        # font size of indices and chr. name
              OTUTextSize=.9,            # font size of OTU names
              
              markLabelSize=.7,          # font size of legend
              legendHeight = 2,          # height of labels
              
              ruler=TRUE,                # add ruler
              rulerPos=-.9,              # position of ruler
              rulerPosMod=3,             # modify position of ruler
              ruler.tck=-0.004,          # size and orient. of ticks in ruler
              rulerNumberPos=.4,         # position of numbers of ruler
              rulerNumberSize=.4,        # font size of ruler

              xlimRightMod = 3,          # modify xlim left argument 
              ylimBotMod = 0,            # modify ylim bottom argument
              ylimTopMod = -.3           # modify ylim top argument
              ,asp=1                     # y x aspect ratio
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
plotIdiograms(dfChrSize = parentalAndHybChrSize,  # d.f. of chr. sizes
              dfMarkPos = dfAlloParentMarks,      # d.f. of marks' positions
              cenColor  = NULL,            # cen. color 
              
              karHeiSpace=5,               # karyotype height including spacing
              karSepar = FALSE,            # equally sized (height) karyotypes
              
              rulerPos=-1.2,               # ruler position
              ruler.tck= -0.006,           # ruler tick orientation and length
              rulerNumberSize=.4           # ruler font size
              
              ,legend=""                   # no legend
              ,asp=1                       # y x aspect ratio
              
              ,notes=notesdf2              # data.frame with notes NEW
              #,OTUasNote=TRUE             # TRY THIS (OTU name to the right)
              ,notesTextSize = 1.3         # font size of notes
              ,notesPos = 1.5              # space from chr. (right) to note
              
              ,ylimBotMod = 1              # ylim bottom argument mod.
)
# dev.off()

## ---- echo=TRUE, results="hide", fig.width=10, fig.height=14, message=FALSE,dev='png', warning=FALSE----

# merge data.frames in micrometers and number of bases
mixedThreeSpChrSize <- plyr::rbind.fill(bigdfOfChrSize[1:14,], bigdfOfChrSize3Mb)
# sort by OTU name
mixedThreeSpChrSize <- mixedThreeSpChrSize[order(mixedThreeSpChrSize$OTU),]
 
# merge marks in micrometers and bases
mixedThreeSpMarks <- plyr::rbind.fill(bigdfOfMarks , bigdfOfMarks3Mb)

plotIdiograms(dfChrSize   = mixedThreeSpChrSize,  # chr. size df
              dfMarkPos   = mixedThreeSpMarks,    # mark position df
              centromereSize = 1,         # cen. size
              roundness=5,                # vertices roundness
              
              chrWidth=.6,                # width of chr.
              chrSpacing = .6,            # space among chr.
              karHeight = 3,              # kar. height without interspace
              karHeiSpace = 5,            # vertical size of karyotype including spacer
              amoSepar =2,                # separ. among kar.
              
              indexIdTextSize=.6,         # font size fo chr name and indices
              markLabelSize=.7,           # font size of mark legends
              distTextChr = .65,          # separation among chr. names and indices
              
              legendWidth = 1.5           # legend items width
              ,ylabline = -7              # position of Mb (title) in axis               
              
              ,rulerPos=-.9,              # ruler position
              ruler.tck=-0.005,           # ticks of ruler size and orientation
              rulerNumberPos =.7,         # position of numbers in ruler
              rulerNumberSize=.5,         # font size of ruler numbers
              
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
              ,asp=1                      # aspect of plot
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

