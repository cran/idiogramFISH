## ---- echo=F, warning=FALSE, error=FALSE, comment=NA--------------------------
if(Sys.info()['sysname']=="Darwin") {
      # system("rm index.Rmd")
      # system("rm DphylogenyVig.Rmd")
      # system("rm EhumanVig.Rmd")         
      # system("rm AplotIdiogramsVig.Rmd")
      # system("rm BcircularVig.Rmd")
    
      system('echo "---" > index.Rmd')
      system('echo "title: \'Credits\'" >> index.Rmd')
      system('echo "author: \'Fernando Roa\'" >> index.Rmd')
      system('echo "date: \'23 08 2019\'" >> index.Rmd')
      system('echo "output:" >> index.Rmd')
      system('echo "  prettydoc::html_pretty:" >> index.Rmd')
      system('echo "    theme: leonids" >> index.Rmd')
      system('echo "    highlight: github" >> index.Rmd')
      system('echo "    toc: true" >> index.Rmd')
      system('echo "    toc_depth: 1" >> index.Rmd')
      system('echo "    number_sections: true" >> index.Rmd')
      system('echo "vignette: >" >> index.Rmd')
      system('echo "  %\\VignetteIndexEntry{Credits}" >> index.Rmd')
      system('echo "  %\\VignetteEngine{knitr::rmarkdown}" >> index.Rmd')
      system('echo "  %\\VignetteEncoding{UTF-8}" >> index.Rmd')
      system('echo "---" >> index.Rmd')
      system('echo ""    >> index.Rmd')
      system('echo "visit https://ferroao.gitlab.io/idiogramfishhelppages" >> index.Rmd')
      
      system('echo "---" > AplotIdiogramsVig.Rmd')
      system('echo "title: \'1. Plotting chromosomes\'" >> AplotIdiogramsVig.Rmd')
      system('echo "author: \'Fernando Roa\'" >> AplotIdiogramsVig.Rmd')
      system('echo "date: \'23 08 2019\'" >> AplotIdiogramsVig.Rmd')
      system('echo "output:" >> AplotIdiogramsVig.Rmd')
      system('echo "  prettydoc::html_pretty:" >> AplotIdiogramsVig.Rmd')
      system('echo "    theme: leonids" >> AplotIdiogramsVig.Rmd')
      system('echo "    highlight: github" >> AplotIdiogramsVig.Rmd')
      system('echo "    toc: true" >> AplotIdiogramsVig.Rmd')
      system('echo "    toc_depth: 1" >> AplotIdiogramsVig.Rmd')
      system('echo "    number_sections: true" >> AplotIdiogramsVig.Rmd')
      system('echo "vignette: >" >> AplotIdiogramsVig.Rmd')
      system('echo "  %\\VignetteAplotIdiogramsVigEntry{1. Plotting chromosomes}" >> AplotIdiogramsVig.Rmd')
      system('echo "  %\\VignetteEngine{knitr::rmarkdown}" >> AplotIdiogramsVig.Rmd')
      system('echo "  %\\VignetteEncoding{UTF-8}" >> AplotIdiogramsVig.Rmd')
      system('echo "---" >> AplotIdiogramsVig.Rmd')
      system('echo ""    >> AplotIdiogramsVig.Rmd')
      system('echo "visit https://ferroao.gitlab.io/idiogramfishhelppages" >> AplotIdiogramsVig.Rmd')
      
      system('echo "---" > BcircularVig.Rmd')
      system('echo "title: \'2. Circular Plots\'" >> BcircularVig.Rmd')
      system('echo "author: \'Fernando Roa\'" >> BcircularVig.Rmd')
      system('echo "date: \'23 08 2019\'" >> BcircularVig.Rmd')
      system('echo "output:" >> BcircularVig.Rmd')
      system('echo "  prettydoc::html_pretty:" >> BcircularVig.Rmd')
      system('echo "    theme: leonids" >> BcircularVig.Rmd')
      system('echo "    highlight: github" >> BcircularVig.Rmd')
      system('echo "    toc: true" >> BcircularVig.Rmd')
      system('echo "    toc_depth: 1" >> BcircularVig.Rmd')
      system('echo "    number_sections: true" >> BcircularVig.Rmd')
      system('echo "vignette: >" >> BcircularVig.Rmd')
      system('echo "  %\\VignetteAplotIdiogramsVigEntry{2. Circular Plots}" >> BcircularVig.Rmd')
      system('echo "  %\\VignetteEngine{knitr::rmarkdown}" >> BcircularVig.Rmd')
      system('echo "  %\\VignetteEncoding{UTF-8}" >> BcircularVig.Rmd')
      system('echo "---" >> BcircularVig.Rmd')
      system('echo ""    >> BcircularVig.Rmd')
      system('echo "visit https://ferroao.gitlab.io/idiogramfishhelppages" >> BcircularVig.Rmd')
      
      system('echo "---" > DphylogenyVig.Rmd')
      system('echo "title: \'3. Phylogeny\'" >> DphylogenyVig.Rmd')
      system('echo "author: \'Fernando Roa\'" >> DphylogenyVig.Rmd')
      system('echo "date: \'23 08 2019\'" >> DphylogenyVig.Rmd')
      system('echo "output:" >> DphylogenyVig.Rmd')
      system('echo "  prettydoc::html_pretty:" >> DphylogenyVig.Rmd')
      system('echo "    theme: leonids" >> DphylogenyVig.Rmd')
      system('echo "    highlight: github" >> DphylogenyVig.Rmd')
      system('echo "    toc: true" >> DphylogenyVig.Rmd')
      system('echo "    toc_depth: 1" >> DphylogenyVig.Rmd')
      system('echo "    number_sections: true" >> DphylogenyVig.Rmd')
      system('echo "vignette: >" >> DphylogenyVig.Rmd')
      system('echo "  %\\VignetteIndexEntry{3. Phylogeny}" >> DphylogenyVig.Rmd')
      system('echo "  %\\VignetteEngine{knitr::rmarkdown}" >> DphylogenyVig.Rmd')
      system('echo "  %\\VignetteEncoding{UTF-8}" >> DphylogenyVig.Rmd')
      system('echo "---" >> DphylogenyVig.Rmd')
      system('echo ""    >> DphylogenyVig.Rmd')
      system('echo "visit https://ferroao.gitlab.io/idiogramfishhelppages" >> DphylogenyVig.Rmd')
      
     system('echo "---" > EhumanVig.Rmd')
     system('echo "title: \'4. Human\'" >> EhumanVig.Rmd')
     system('echo "author: \'Fernando Roa\'" >> EhumanVig.Rmd')
     system('echo "date: \'23 08 2019\'" >> EhumanVig.Rmd')
     system('echo "output:" >> EhumanVig.Rmd')
     system('echo "  prettydoc::html_pretty:" >> EhumanVig.Rmd')
     system('echo "    theme: leonids" >> EhumanVig.Rmd')
     system('echo "    highlight: github" >> EhumanVig.Rmd')
     system('echo "    toc: true" >> EhumanVig.Rmd')
     system('echo "    toc_depth: 1" >> EhumanVig.Rmd')
     system('echo "    number_sections: true" >> EhumanVig.Rmd')
     system('echo "vignette: >" >> EhumanVig.Rmd')
     system('echo "  %\\VignetteEhumanVigEntry{4. Human}" >> EhumanVig.Rmd')
     system('echo "  %\\VignetteEngine{knitr::rmarkdown}" >> EhumanVig.Rmd')
     system('echo "  %\\VignetteEncoding{UTF-8}" >> EhumanVig.Rmd')
     system('echo "---" >> EhumanVig.Rmd')
     system('echo ""    >> EhumanVig.Rmd')
     system('echo "visit https://ferroao.gitlab.io/idiogramfishhelppages" >> EhumanVig.Rmd')
    
}
if(length(rmarkdown::pandoc_version()<2)>0) { # solaris workaround
if(rmarkdown::pandoc_version()<2) {
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
      shell('@echo title: "Credits" >> index.Rmd')
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
      shell("@echo   %\\VignetteIndexEntry{Credits} >> index.Rmd")
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
      shell('@echo title: "1. Plotting chromosomes" >> AplotIdiogramsVig.Rmd')
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
      shell("@echo   %\\VignetteAplotIdiogramsVigEntry{1. Plotting chromosomes} >> AplotIdiogramsVig.Rmd")
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
  if(!is.na(link)) { 
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
        5S      red       dots
       45S      green     square
     gene1      orange    upArrow    
     gene2      salmon    downArrow
     gene3      \"#056522\" cMLeft    
      DAPI      blue      cM   
\"cB mark\"     black     cenStyle   
       CMA      yellow    square
\"B mark\"      black     square"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

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
\"Species one\"      1      45S       p       NA         NA     # no measure means whole arm for square marks
\"Species one\"      1       5S       q      0.5         0.5
\"Species one\"      B  \"B mark\"    w       NA         NA     # w for whole chromosome for square marks
\"Species one\"      B  \"cB mark\"   q       NA         1.0    
\"Species one\"      2     45S        p        1         1.0
\"Species one\"      2     gene1      q      0.5         1.0
\"Species one\"      2     gene2      q      0.5         2.0
\"Species one\"      3     DAPI       q       NA          1
\"Species one\"      3     gene3      p       NA         .5 
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

## ---- echo=TRUE, results="hide", fig.width=12, fig.height=6, message=FALSE, dev='png'----
# svg("mydfChrSize.svg",width=12,height=6 )

par(mar = c(0, 0, 0, 0))

plotIdiograms(dfChrSize= mydfChrSize,     # chr. size data.frame
              dfMarkPos= mydfOfMarks,     # mark position data.frame (inc. cen.)
              dfMarkColor=mydfMarkColor,  # mark style d.f.
              
              chrSpacing=.6,              # separ. among chr.
              distTextChr = .7,           # separation among text and chr. names and ind.        
              orderChr = "name",        # order chr. by name
              karHeiSpace=1.6             # vertical size of karyotype including spacer
              
              ,arrowhead = .5             # proportion of head of arrow
              
              ,fixCenBorder = TRUE        # use chrColor as border color of cen. or cen. marks
              ,legendWidth = .8           # legend item width
              ,legendHeight = .5          # legend item height
              ,markLabelSpacer = 2        # legend spacer
              ,lwd.mimicCen = 2.5         # constric. mark line width
              
              ,rulerPos=0                 # ruler position
              ,ruler.tck=-0.01            # ticks of ruler size and orientation
              ,ylabline = -6              # ruler units pos.
              
              ,notes=notesdf              # data.frame with notes NEW
              ,notesTextSize = 1.3        # font size of notes
              ,notesPos = .2              # space from chr. (right) to note
              
              ,ylimBotMod = 1             # modify ylim bottom argument
              ,ylimTopMod = 0             # modify ylim top argument
              ,xlimLeftMod = 2            # modify left xlim
              ,xlimRightMod = 3           # modify right xlim
              ,lwd.cM = 2                 # thickness of cM marks 
              ,pattern = "^c"             # regex pattern to remove from mark names
              ,remSimiMarkLeg=TRUE        # remove pseudoduplicated mark names from legend (same after pattern removal)
              # ,legend="inline"            # legend next to chr.
              # ,bannedMarkName = "cB mark" # remove from legends
)
# dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
# cat(paste0("![](mydfChrSize.png)" ) )
# cat(paste0("![](mydfChrSize.svg)" ) )

## ----cen0, echo=TRUE, results="hide", fig.width=4.5, fig.height=4.5, message=FALSE,dev='png'----

png("mydfChrSize2.png", width=550, height=550)
par(mar = c(0, 0, 0, 0))
plotIdiograms(dfChrSize   = bigdfOfChrSize[1:8,],  # chr. size data.frame
              dfMarkColor = mydfMarkColor,# mark style df
              dfMarkPos   = bigdfOfMarks, # mark position df
              
              centromereSize = 0,         # <- HERE
              
              squareness=3,               # vertices squareness  
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
              ylabline = -3,              # ruler units pos.
              
              xlimLeftMod = 2,            # modify xlim left argument 
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
              ,lwd.cM = 2                 # thickness of cM marks 
              )
dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
cat(paste0("![](mydfChrSize2.png)" ) )
# cat(paste0("![](mydfChrSize2.jpg)" ) )

## ---- eval=FALSE--------------------------------------------------------------
#  unique(mydfOfMarks$markName)

## ---- echo=TRUE, results="hide", fig.width=10, fig.height=4.5, message=FALSE,dev="png", eval=TRUE----

charVectorCol <- c("tomato3","darkolivegreen4","dfsd","blue","green")

# Modify size of kar. to use rulerInterval and ceilingFactor (>= 1.13)  
quo<-9
dfOfChrSize2<-dfOfChrSize
dfOfChrSize2$shortArmSize<-dfOfChrSize$shortArmSize/quo
dfOfChrSize2$longArmSize<-dfOfChrSize$longArmSize/quo
dfOfMarks2b<-dfOfMarks2
dfOfMarks2b$markSize<-dfOfMarks2$markSize/quo
dfOfMarks2b$markDistCen<-dfOfMarks2$markDistCen/quo

png("dfOfChrSizeVector.png", width=1000, height=450)
par(mar=rep(0,4))
plotIdiograms(dfChrSize = dfOfChrSize2,    # d.f. of chr. sizes
              dfMarkPos = dfOfMarks2b,     # d.f. of marks' positions
              defaultStyleMark = "cM",     # forces "cM" style in d.f dfMarkColor (exc. 5S)
              
              mycolors = charVectorCol,    # colors to use
              
              distTextChr = .5,            # separ. text and chr.
              
              markLabelSize=.7,            # font size for labels (legend)
              lwd.cM=2,                    # width of cM marks
              legendWidth=0.9,             # legend item width
              legendHeight=.5,
              
              rulerPos= 0,                 # ruler position
              ruler.tck=-0.01,             # ruler tick orientation and length
              rulerNumberSize=.5           # ruler font size
              ,ylabline = -8               # ruler units pos.
              
              ,xlimRightMod = 1            # modify xlim right arg.
)
dev.off() # close png

## ----hidethiscran, results="asis", comment=NA, echo=FALSE, eval=TRUE----------
cat(paste0("![](dfOfChrSizeVector.png)" ) )

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

## ----example, echo=TRUE, results="hide", fig.width=13.5, fig.height=6, message=FALSE, dev="png", eval=TRUE----
# library(idiogramFISH)
# svg("mydfChrSizeHolo.svg",width=13.5,height=6 )
# png("mydChrSizeHolo.png", width=600, height=300)

par(mar=c(0,4,0,1)) # bottom left top right

plotIdiograms(dfChrSize  = mydfChrSizeHolo,# data.frame of chr. sizes
              dfMarkColor= mydfMarkColor,  # df of mark style
              dfMarkPos  = mydfMarkPosHolo,# df of mark positions
              addOTUName=FALSE,            # add OTU names
              
              xlimLeftMod= 1,              # modify xlim left argument
              ylimTopMod= -1,              # modify ylim top argument
              ylimBotMod= -2               # modify ylim bottom argument
              ,rulerPos = 0                # ruler position
              ,ruler.tck = -0.01           # ruler ticks size and orient.
              
              ,legendWidth=1               # width of legend
              ,legendHeight=.7             # height of legend item 
              ,xModifier=0.01              # separation among chromatids
)
# dev.off() # closes png or svg

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
# cat(paste0("![](mydChrSizeHolo.png)" ) )
# cat(paste0("![](mydfChrSizeHolo.svg)" ) )

## ---- echo=TRUE, comment="#", fig.width=6, fig.height=3, message=FALSE,dev='png', collapse=TRUE----
unique(dfMarkPosHolo$markName)
par(mar=rep(0,4)) 
plotIdiograms(dfChrSize = dfChrSizeHolo, # d.f. of chr. size
              dfMarkPos  = dfMarkPosHolo, # d.f. of marks' positions
              mycolors   = c("green","yellow","blue","red"),  # colors for marks

              addOTUName=FALSE,           # do not add OTU name
              ruler=FALSE,                # do not add ruler
              xlimLeftMod=1,              # modify left xlim arg.
              xlimRightMod=3,             # modify right xlim arg.
              ylimBotMod=.2               # modify bottom ylim
              ,chromatids=FALSE           # do not show separ. chromatids
)

## -----------------------------------------------------------------------------
# mark general characteristics' data.frame:
mydfMarkColor2<-read.table(text=
"  markName markColor  style
1       5S       red   dots
2      45S     green   square
3     DAPI      blue   square
4      CMA    yellow   square
5     c45S     green   cenStyle # <- simulate Cen.
6      c5S       red   cenStyle
7       cB     black   cenStyle
8   constr        NA   cenStyle
9        B     black   square"  ,  header=TRUE, stringsAsFactors=FALSE,fill=TRUE)

# add new marks to data.frame of marks' position
mydfMarkPosHolo2<-plyr::rbind.fill(mydfMarkPosHolo,data.frame(OTU="Species one",
                                                             chrName=1:4,
                                                             markName=c("c45S","c5S","constr","cB"), # <- use new mark
                                                             markPos=2.5,
                                                             markSize=NA 
                                                             )
                  )


## ---- echo=TRUE, results="hide", fig.width=13.5, fig.height=6, message=FALSE, dev='png'----

# png("mydChrSizeHolo.png", width=600, height=300)
par(mar=c(0,4,0,1)) # bottom left top right

plotIdiograms(dfChrSize  = mydfChrSizeHolo,# data.frame of chr. sizes
              dfMarkColor= mydfMarkColor2,  # df of mark style
              dfMarkPos  = mydfMarkPosHolo2,# df of mark positions
              addOTUName=FALSE,            # add OTU names
              
              xlimLeftMod= 1,              # modify xlim left argument
              ylimTopMod= -1,              # modify ylim top argument
              ylimBotMod= -2               # modify ylim bottom argument
              ,rulerPos = 0
              ,ruler.tck = -0.01
              
              ,legendWidth=1               # width of legend
              ,legendHeight=.7             # height of legend item 
              ,lwd.mimicCen=2.5            # line width of const. mark
              ,pattern="^c"                # regex pattern to remove from mark names
              ,remSimiMarkLeg = TRUE       # remove pseudoduplicated mark names (got equal after pattern removal)
              # ,legend="inline"             # legends inline
              # ,bannedMarkName = c("c45S","cB") # do not use this names from labels
)
# dev.off() # closes png or svg

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
# png("bigdfOfChrSize.png", width=650, height=1300)
par(mar = c(0, 0, 0, 0))
plotIdiograms(dfChrSize  =bigdfOfChrSize,# chr. sizes
              dfMarkColor=dfMarkColor,   # mark characteristics, optional in dev version. see above. 
              dfMarkPos  =bigdfOfMarks,  # mark positions (inc. cen. marks)

              karHeight=2.5,             # karyotype rel. height
              karHeiSpace=6,             # karyotype vertical size with spacing
              chrWidth = .35,            # chr. width
              amoSepar = 2,              # Vertical separation of kar. when karSepar = TRUE
              
              squareness = 10,           # squareness of chr. vertices
              distTextChr=.8,            # distance of chr. to text
              chrIndex = "AR",           # add arm ratio only. For v. >=1.12
              nameChrIndexPos = 3,
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
              ylabline = -1,             # ruler units pos.
              rulerTitleSize=.5,         # ruler font size of units (title)

              xlimRightMod = 3,          # modify xlim left argument 
              xlimLeftMod = 2,           # modify xlim left argument 
              ylimBotMod = 0,            # modify ylim bottom argument
              ylimTopMod = -.3           # modify ylim top argument
              #,asp=1                    # y x aspect ratio
)

# dev.off() # for png()

## ---- results="asis", comment=NA, echo=FALSE, eval=FALSE----------------------
#  cat(paste0("![](bigdfOfChrSize.png)" ) )

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
par(mar=rep(0,4)) 

plotIdiograms(dfChrSize=bigdfChrSizeHolo, # chr. size data.frame
              dfMarkColor=dfMarkColor,    # df of mark style
              dfMarkPos=bigdfMarkPosHolo, # df of marks' position
              
              markDistType="cen",         # measure towards center of mark
              squareness=6,               # vertices squareness of chr. and marks 
              
              karHeiSpace = 4,            # karyotype height including spacing
              karSepar=TRUE,              # reduce vertical space among karyotypes 
              amoSepar = 1,               # separation among karyotypes
              distTextChr=.5,             # distance text to chr.
              
              legendWidth = 1             # width of legend labels
              
              ,chrId="simple",            # numbering of chr., not using "original" name
              
              indexIdTextSize=.9,         # font size of chr names and indices
              markLabelSize=.9,           # font size of legends
              
              rulerPos=0,                 # position of ruler
              rulerNumberSize=.9,         # font size of ruler
              ruler.tck= -.004,           # tick length and orient.
              ylabline = -3,              # position of ruler units (title)
              
              ylimBotMod=.4               # modify ylim bottom argument
              ,xModifier=0.01             # separation among chromatids
)
dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
cat(paste0("![](bigdfChrSizeHolo.png)" ) )

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
              ,ylabline = -1.5             # ruler units pos.
              
              ,legend=""                   # no legend
              
              ,notes=notesdf2              # data.frame with notes NEW
              #,OTUasNote=TRUE             # TRY THIS (OTU name to the right)
              ,notesTextSize = 1.3         # font size of notes
              ,notesPos = 1.5              # space from chr. (right) to note
              
              ,ylimBotMod = 1              # ylim bottom argument mod.
)
# dev.off()

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

## ---- echo=TRUE, results="hide", fig.width=8, fig.height=7, message=FALSE,dev='png'----

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
              ,xModifier=.005              # separ. among chromatids
)
# dev.off()

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
              squareness=4,                  # vertices squareness of chr. and marks 
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
              ylabline = -6,                 # modifies position of ruler title (Mb)
              
              markLabelSize=.9,              # font size of legend
              legendWidth = 1.2,             # width of legends
              
              xlimLeftMod = 1,               # modify left argument of xlim
              ylimBotMod=.4                  # modify bottom argument of ylim
              ,chromatids=FALSE              # do not show chromatids
              )                     
dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
cat(paste0("![](bigdfChrSizeHolo2.png)" ) )

## ----returntooldpar, echo=FALSE-----------------------------------------------
suppressWarnings(par(opar) )

## ---- echo=TRUE, results="hide", fig.width=7, fig.height=7, message=FALSE,dev='png', warning=FALSE, eval=TRUE----
# fig.width=7, fig.height=7
bigdfOfChrSize3_100Mb<-bigdfOfChrSize3Mb
bigdfOfChrSize3_100Mb$chrSize<-bigdfOfChrSize3Mb$chrSize*33

bigdfOfMarks3_100Mb<-bigdfOfMarks3Mb
bigdfOfMarks3_100Mb$markPos<-bigdfOfMarks3_100Mb$markPos*33
bigdfOfMarks3_100Mb$markSize<-bigdfOfMarks3_100Mb$markSize*33

par(mar=rep(0,4))
plotIdiograms(dfChrSize   = bigdfOfChrSize3_100Mb,  # chr. size data.frame
              dfMarkPos   = bigdfOfMarks3_100Mb,    # mark position df
              
              chrWidth=.6,                # width of chr.
              chrSpacing = .6,            # space among chr.
              karHeight = 3,              # kar. height without interspace
              karHeiSpace = 5,            # vertical size of karyotype including spacer
              amoSepar =2,                # separ. among kar.
              
              indexIdTextSize=.6,         # font size of chr. name and indices
              markLabelSize=.7,           # font size of mark legends
              distTextChr = .65,          # separation among chr. names and indices
              
              fixCenBorder = TRUE         # use chrColor as border color of cen. or cen. marks
              ,legendWidth = 1.5          # legend items width
              
              ,ylabline = -2              # position of Mb (title) in ruler               
              ,rulerPos= 0,               # ruler position
              ruler.tck=-0.005,           # ticks of ruler size and orientation
              rulerNumberPos =.7,         # position of numbers in ruler
              rulerNumberSize=.7,         # font size of ruler numbers
              rulerInterval = 1.5,        # ruler interval for micrometeres
              rulerIntervalMb = 50000000, # ruler interval for Mb

              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
              ,chromatids=FALSE           # do not show chromatids
              
                         ####  NEW    #####
              ,threshold = 90             # this will allow to not to shrink data greater than 350 Mb
)

## ---- echo=TRUE, fig.width=10, fig.height=10, message=FALSE,dev='png', warning=FALSE,collapse=TRUE, comment="#"----
#fig.width=10, fig.height=10
# modify data in millions to hundreds of millions of Mb
bigdfOfChrSize3_100Mb<-bigdfOfChrSize3Mb[1:8,]
bigdfOfChrSize3_100Mb$chrSize<-bigdfOfChrSize3_100Mb$chrSize*100

bigdfOfMarks3_100Mb<-bigdfOfMarks3Mb
bigdfOfMarks3_100Mb$markPos <-bigdfOfMarks3_100Mb$markPos *100
bigdfOfMarks3_100Mb$markSize<-bigdfOfMarks3_100Mb$markSize*100

# merge data.frames in micrometers and number of bases
mixedThreeSpChrSize <- plyr::rbind.fill(bigdfOfChrSize[1:8,], bigdfOfChrSize3_100Mb)
# sort by OTU name
mixedThreeSpChrSize <- mixedThreeSpChrSize[order(mixedThreeSpChrSize$OTU),]

# add cenStyle marks to simulate centromeres in karyo. in Mb (holocen.)
# compare rulers
bigdfSimCenMarks<- bigdfOfChrSize3_100Mb
bigdfSimCenMarks$markPos<-bigdfSimCenMarks$chrSize/2

bigdfSimCenMarks$markName<-"sim. cen."
bigdfSimCenMarks$chrSize<-NULL

# merge marks in micrometers and bases
mixedThreeSpMarks <- plyr::rbind.fill(bigdfOfMarks , bigdfOfMarks3_100Mb,bigdfSimCenMarks)

# remove cenStyle mark info.
mixedThreeSpMarks<-mixedThreeSpMarks[which(!( mixedThreeSpMarks$OTU %in% "Species 2 genome" & 
                          mixedThreeSpMarks$chrName %in% c(1,4) &
                          mixedThreeSpMarks$markName %in% "sim. cen.") ),]

# constric. marks
mixedThreeSpMarks[which(mixedThreeSpMarks$OTU %in% "Species 2 genome" & 
                          mixedThreeSpMarks$chrName %in% c(1,4) ),]$markName<-c("cDAPI","cCMA")
                        
# add arrow mark
mixedThreeSpMarks <- dplyr::bind_rows(mixedThreeSpMarks , mixedThreeSpMarks[nrow(mixedThreeSpMarks),] )
mixedThreeSpMarks[nrow(mixedThreeSpMarks),]$markName<-"S58A"
mixedThreeSpMarks[nrow(mixedThreeSpMarks),]$markPos<-.7e+08
mixedThreeSpMarks[nrow(mixedThreeSpMarks),]$markSize<-.7e+08
  
dfMarkColorAndStyle<-idiogramFISH:::makedfMarkColorMycolors(unique(mixedThreeSpMarks$markName),
                                                            c("red","green","blue","yellow","blue","yellow","black")
)

# d.f. of marks'styles

dfMarkColorAndStyle$style[5:7]<-"cenStyle"
dfMarkColorAndStyle$markColor[7]<-NA
dfMarkColorAndStyle$style[8]<-"upArrow"

dfMarkColorAndStyle

par(mar=rep(0,4))
plotIdiograms(dfChrSize   = mixedThreeSpChrSize,  # chr. size data.frame
              dfMarkPos   = mixedThreeSpMarks,    # mark position df
              dfMarkColor = dfMarkColorAndStyle,
              
              chrWidth=.6,                # width of chr.
              chrSpacing = .6,            # space among chr.
              karHeight = 3,              # kar. height without interspace
              karHeiSpace = 5,            # vertical size of karyotype including spacer
              amoSepar =2,                # separ. among kar.
              
              indexIdTextSize=.6,         # font size of chr. name and indices
              markLabelSize=.7,           # font size of mark legends
              distTextChr = .65,          # separation among chr. names and indices
              lwd.mimicCen = 1.5,         # constric. line width
              
              legendWidth = 1.5,          # legend items width
              fixCenBorder = TRUE,        # use chrColor as border color of cen. or cen. marks
              
              ylabline = -10,             # position of Mb (title) in ruler
              rulerPos= 0,                # ruler position
              ruler.tck=-0.005,           # ticks of ruler size and orientation
              rulerNumberPos =.7,         # position of numbers in ruler
              rulerNumberSize=.7,         # font size of ruler numbers
              rulerInterval = 1.5,        # ruler interval for micrometeres
              rulerIntervalMb = 150000000,# ruler interval for Mb
              ceilingFactor = 1,          # affects rounding for ruler max. value
              
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
              ,holocenNotAsChromatids = TRUE # do not use chromatids in holocen.
              ,pattern="^c"               # regex pattern to remove from mark names
              ,remSimiMarkLeg = TRUE      # remove pseudoduplicate names arising from pattern removal
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
              ,holocenNotAsChromatids=TRUE# do not use chromatids in holocen. kar.
              
              ,ylabline = -10             # position of Mb or cM (title) in ruler               
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
              orderChr = "name",       # order chr. by name
              
              ruler=FALSE                # do not plot ruler
              
              ,ylimBotMod = 1            # modify ylim bottom argument
              ,legendWidth = 1           # width of legend
)
# dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
# cat(paste0("![](mydChrSizeHolo.png)" ) )
# cat(paste0("![](dfwithHetero.svg)" ) )

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
              orderChr = "name",          # order chr. by name
              karIndex = FALSE,             # do not add karyotype indices
              addOTUName = TRUE,            # add OTU name
              karHeiSpace = 4,              # height of kar. with spacing
              
              ruler=FALSE,                  # no ruler
              
              xlimLeftMod=-1,               # modify left argument of xlim
              xlimRightMod=0,               # modify right argument of xlim
              ylimBotMod=1.3                # modify bottom argument of ylim
              ,xModifier=0.005              # separ. among chromatids
)

