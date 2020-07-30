## ---- echo=F, warning=FALSE, error=FALSE, comment=NA--------------------------

if(Sys.info()['sysname']=="Darwin") {

      system('echo "---" > index.Rmd')
      system('echo "title: \'Credits\'" >> index.Rmd')
      system('echo "author: \'Fernando Roa\'" >> index.Rmd')
      system('echo "date: \'23 08 2019\'" >> index.Rmd')
      system('echo "output:" >> index.Rmd')
      system('echo "  html_document" >> index.Rmd')
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
}

if( length(rmarkdown::pandoc_version()<2) > 0 ) { # solaris workaround
  
if(rmarkdown::pandoc_version() < 2 ) {
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
          ) # me
  if(Sys.info()['sysname']=="Windows" ) {
    message("pandoc > 2 not available, see online vignettes")
    
    # remove vignettes with lua filter
    shell("del index.Rmd")
  
    #
    #    create new index.Rmd
    #
    
      shell("@echo off")
      shell('@echo --- > index.Rmd')
      shell('@echo title: "idiogramFISH: Idiograms with Marks and Karyotype Indices" >> index.Rmd')
      shell('@echo author: "Fernando Roa" >> index.Rmd')
      shell('@echo date: "23 08 2019" >> index.Rmd')
      shell('@echo output: >> index.Rmd')
      shell('@echo   html_document >> index.Rmd')
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

    } # if windows
  } # pandoc < 2
} # len

## ----cssjs, results="asis", echo=FALSE, message=FALSE, eval=TRUE--------------
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

## ----setup, include=FALSE, eval=T---------------------------------------------
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
require(idiogramFISH)
knitr::opts_chunk$set(eval = TRUE)
# knitr::opts_chunk$set(eval = FALSE)

# badge_devel_gitlab<-function(pkg, color){
#     v <- rvcheck:::check_github_gitlab(pkg, "gitlab")$latest_version
#     url <- paste0("https://gitlab.com/", pkg)
#     badger::badge_custom("devel version", v, color, url)
# }


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

## ---- echo=F,  results="asis", eval=T-----------------------------------------
img1_path <- "../man/figures/logo.png"
if(file.exists(img1_path)) {
cat(paste0("<img src=",img1_path,">") )
}

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----

if (requireNamespace("RCurl", quietly = TRUE)  ) {
  
cranversion <- "https://www.r-pkg.org/badges/version/idiogramFISH"
cranversion_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(cranversion) ), error=function(e) NA )

  if (!is.na(cranversion_cont)){
  cranversion_contFile <- "../man/figures/cranversion.svg"
  writeLines(cranversion_cont, con = cranversion_contFile)
  cranversion_contFile <- normalizePath(cranversion_contFile)
  # knitr::include_graphics(cranversion_contFile)
  # cat(paste0("![https://CRAN.R-project.org/package=idiogramFISH](",knitr::include_graphics(cranversion_contFile),")" ) )
  cat(paste0("[![CRAN repo](",knitr::include_graphics(cranversion_contFile),")](https://CRAN.R-project.org/package=idiogramFISH)" ) )
  } # cran version

# crandownloads<-"https://cranlogs.r-pkg.org/badges/grand-total/idiogramFISH?color=orange"
# crandownloads_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(crandownloads) ), error=function(e) NA )

doibadge <- "https://zenodo.org/badge/DOI/10.5281/zenodo.3579417.svg"
doibadge_cont      <- tryCatch(suppressWarnings(RCurl::getURLContent(doibadge)      ), error=function(e) NA )

  if (!is.na(doibadge_cont)){
  doibadge_contFile <- "../man/figures/doibadge.svg"
  writeLines(doibadge_cont, con = doibadge_contFile)
  doibadge_contFile <- normalizePath(doibadge_contFile)
  
  cat(paste0("&nbsp;[![10.5281/zenodo.3579417](",knitr::include_graphics(doibadge_contFile)         ,")](https://doi.org/10.5281/zenodo.3579417)" ) )
  } # doi

} # rcurl

## ---- echo=F, message=FALSE, warning=FALSE, include=TRUE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
  # v<-tryCatch(suppressWarnings(rvcheck:::check_github_gitlab("ferroao/idiogramFISH", "gitlab")$latest_version), error=function(e) NA )
  v<-sub("Version: ","",readLines("../DESCRIPTION")[3])
  link<-tryCatch(suppressWarnings(badger::badge_custom("devel version", v, "cornflowerblue","?logo=gitlab") ), error=function(e) NA )
  if(!is.na(link)){
  svglink<-gsub("\\[|\\]|!|\\(|\\)","", link)
  gitbadge_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svglink) ), error=function(e) NA )
    if (!is.na(gitbadge_cont)){
    # gitbadge_contFile <- tempfile(fileext = ".svg")
    gitbadge_contFile <- "../man/figures/gitbadge.svg"
    writeLines(gitbadge_cont, con = gitbadge_contFile)
    gitbadge_contFile <- normalizePath(gitbadge_contFile)
    cat(paste0("[![gitlab repo](",knitr::include_graphics(gitbadge_contFile),")](https://gitlab.com/ferroao/idiogramFISH){target='_blank'}") )
    }
  }
}

## ---- eval=FALSE--------------------------------------------------------------
#  # This installs package devtools, necessary for installing the dev version
#  install.packages("devtools")
#  
#  url <- "https://gitlab.com/ferroao/idiogramFISH"
#  
#  # Necessary packages for vignettes:
#  list.of.packages <- c(
#      "plyr",
#      "knitr",
#      "kableExtra",
#      "rmdformats",
#      "rmarkdown",
#      "RCurl",
#      "rvcheck",
#      "badger",
#      "rentrez"
#      )
#  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#  if(length(new.packages)) install.packages(new.packages)
#  
#  # Linux with vignettes and Windows
#  devtools::install_git(url = url,build_vignettes = TRUE, force=TRUE)
#  
#  # Mac with vignettes
#  devtools::install_git(url = url, build_opts=c("--no-resave-data","--no-manual") )

## ---- eval=FALSE--------------------------------------------------------------
#  # clone repository:
#  git clone "https://gitlab.com/ferroao/idiogramFISH"
#  
#  R CMD build idiogramFISH
#  # install
#  R CMD INSTALL idiogramFISH_*.tar.gz

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
#cran version
v<-"NEWS"#tryCatch(suppressWarnings(rvcheck::check_cran("idiogramFISH")$latest_version), error=function(e) NA )

link<-tryCatch(suppressWarnings(badger::badge_custom("gitlab", paste(v), "orange","?logo=gitlab") ), error=function(e) NA )
  if(!is.na(link)){
  svglink<-gsub("\\[|\\]|!|\\(|\\)","", link)
  news_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svglink) ), error=function(e) NA )
    if (!is.na(news_cont)){
    news_cont_contFile <- "../man/figures/NEWS.svg"
    writeLines(news_cont, con = news_cont_contFile)
    cat(paste0("[![NEWS](",knitr::include_graphics(news_cont_contFile),")](https://gitlab.com/ferroao/idiogramFISH/blob/master/NEWS.md){target='_blank'}" ) )
    }
  }
} # rcurl

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
archivelink<-tryCatch(suppressWarnings(badger::badge_custom("CRAN",  "archive","gray") ), error=function(e) NA )
if(!is.na(archivelink)){
svgnewdownlink<-gsub("\\[|\\]|!|\\(|\\)","", archivelink)
archive_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svgnewdownlink) ), error=function(e) NA )
}
if (!is.na(archive_cont)){
      archive_contFile <- "../man/figures/archive.svg"
      writeLines(archive_cont, con = archive_contFile)
      cat(paste0("[![archive](",knitr::include_graphics(archive_contFile),")](https://cran.r-project.org/src/contrib/Archive/idiogramFISH){target='_blank'}" ) )
}
} # rcurl

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
newdownlink<-tryCatch(suppressWarnings(badger::badge_custom("CRAN",  "downloads","green") ), error=function(e) NA )
if(!is.na(newdownlink)){
svgnewdownlink<-gsub("\\[|\\]|!|\\(|\\)","", newdownlink)
realdo_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svgnewdownlink) ), error=function(e) NA )
}
if (!is.na(realdo_cont)){
      realdo_contFile <- "../man/figures/realdownloads.svg"
      writeLines(realdo_cont, con = realdo_contFile)
                            # message                                              # link
      cat(paste0("[![downloads](",knitr::include_graphics(realdo_contFile),")](https://ferroao.gitlab.io/idiogramfishhelppages/downloads.png){target='_blank'}" ) )
}
} # rcurl

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
# version of manual
# v<-tryCatch(suppressWarnings(rvcheck:::check_github_gitlab("ferroao/idiogramFISH", "gitlab")$latest_version), error=function(e) NA )
#cran version
v<-tryCatch(suppressWarnings(rvcheck::check_cran("idiogramFISH")$latest_version), error=function(e) NA )

pkg<-"idiogramFISH"
link<-tryCatch(suppressWarnings(badger::badge_custom("bookdown", paste(pkg,v), "orange") ), error=function(e) NA )
  if(!is.na(link)){
  svglink<-gsub("\\[|\\]|!|\\(|\\)","", link)
  manual_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svglink) ), error=function(e) NA )
    if (!is.na(manual_cont)){
    bookdown_contFile <- "../man/figures/cranmanualbookdown.svg"
    writeLines(manual_cont, con = bookdown_contFile)
#    manual_contFile <- normalizePath(manual_contFile)
    # knitr::include_graphics(manual_contFile)
    cat(paste0("[![https://ferroao.gitlab.io/manualidiogramfish](",knitr::include_graphics(bookdown_contFile),")](https://ferroao.gitlab.io/manualidiogramfish/)" ) )
    }
  }
} # rcurl

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
# version of manual
# v<-sub("Version: ","",readLines("DESCRIPTION")[3])
v<-tryCatch(suppressWarnings(rvcheck:::check_github_gitlab("ferroao/idiogramFISH", "gitlab")$latest_version), error=function(e) NA )
#cran version
# v<-tryCatch(suppressWarnings(rvcheck::check_cran("idiogramFISH")$latest_version), error=function(e) NA )

pkg<-"idiogramFISH"
pkglink<-tryCatch(suppressWarnings(badger::badge_custom("pkgdown", paste(pkg,v), "cornflowerblue") ), error=function(e) NA )
  if(!is.na(pkglink)){
  pkgsvglink<-gsub("\\[|\\]|!|\\(|\\)","", pkglink)
  develpkgmanual_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(pkgsvglink) ), error=function(e) NA )
    if (!is.na(develpkgmanual_cont)){
    pkgdownmanual_contFile <- "../man/figures/pkgdownver.svg"
    writeLines(develpkgmanual_cont, con = pkgdownmanual_contFile)
    cat(paste0("[![https://ferroao.gitlab.io/idiogramFISH](",knitr::include_graphics(pkgdownmanual_contFile)," )](https://ferroao.gitlab.io/idiogramFISH)" ) )
    }
  }
} # rcurl

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
# version of manual
# v<-sub("Version: ","",readLines("DESCRIPTION")[3])
v<-tryCatch(suppressWarnings(rvcheck:::check_github_gitlab("ferroao/idiogramFISH", "gitlab")$latest_version), error=function(e) NA )
#cran version
# v<-tryCatch(suppressWarnings(rvcheck::check_cran("idiogramFISH")$latest_version), error=function(e) NA )

pkg<-"idiogramFISH"
link<-tryCatch(suppressWarnings(badger::badge_custom("vignettes", paste(pkg,v), "cornflowerblue") ), error=function(e) NA )
  if(!is.na(link)){
  vignettelink<-gsub("\\[|\\]|!|\\(|\\)","", link)
  vignettemanual_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(vignettelink) ), error=function(e) NA )
    if (!is.na(vignettemanual_cont)){
    vignettemanual_contFile <- "../man/figures/develmanualvignette.svg"
    writeLines(vignettemanual_cont, con = vignettemanual_contFile)
#    manual_contFile <- normalizePath(manual_contFile)
    # knitr::include_graphics(manual_contFile)
    cat(paste0("[![https://ferroao.gitlab.io/idiogramfishhelppages](",knitr::include_graphics(vignettemanual_contFile),")](https://ferroao.gitlab.io/idiogramfishhelppages)" ) )
    }
  }
} # rcurl

## ---- echo=TRUE, results="hide", message=FALSE--------------------------------
library(idiogramFISH)

data(dfOfChrSize) # chromsome data
data(dfMarkColor) # mark general data
data(dfOfMarks2)  # mark position data (inc. cen.)

# column Mbp not for plotting purposes
dfOfChrSize$Mbp<-(dfOfChrSize$shortArmSize+dfOfChrSize$longArmSize)*100

svg("dfOfChrSize.svg",width=10,height=6 )
# png("dfOfChrSize.png", width=500, height=400)
plotIdiograms(dfChrSize  =dfOfChrSize,  # data.frame of chr. size
              dfMarkColor=dfMarkColor,  # d.f of mark style <- Optional
              dfMarkPos=dfOfMarks2,     # df of mark positions (includes cen. marks)
              
              karHeight=5,              # kar. height
              chrWidth = 1.2,           # chr. width
              chrSpacing = 1,           # space among chr.
              
              morpho="Guerra",          # chr. morpho. classif. (Guerra, Levan, both, "" ) ver. >= 1.12 only
              chrIndex="CI",            # cen. pos. (CI, AR, both, "" ) ver. >= 1.12 only
              chrSize = TRUE,           # add chr. sizes under chr.
              chrSizeMbp = TRUE,        # add Mbp sizes under chr. (see above)
              
              rulerPos= 0,              # position of ruler
              ruler.tck=-0.01,          # size and orientation of ruler ticks
              rulerNumberSize=.8        # font size of rulers
              ,xPosRulerTitle = 3             # pos of ruler title
              
              ,legendWidth=1            # width of legend items
              ,fixCenBorder = TRUE      # use chrColor as border color of cen. or cen. marks
              ,distTextChr = 1.2        # chr. text separation
              
              ,xlimLeftMod = 2          # xlim left param.
              ,ylimBotMod = 0           # modify ylim bottom argument
              ,ylimTopMod = 0           # modify ylim top argument
)
dev.off() # close svg()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
# cat(paste0("![](dfOfChrSize.png)" ) )
cat(paste0("![](dfOfChrSize.svg)" ) )

## ---- results="hide"----------------------------------------------------------
dfOfChrSize

## ---- monocentrics, echo=FALSE, comment=NA------------------------------------
# chromsome data, if only 1 species, column OTU is optional
kableExtra::kable_styling(knitr::kable(dfOfChrSize) , full_width = F)
# mark general data

## ---- results="hide"----------------------------------------------------------
dfMarkColor

## ---- echo=FALSE, comment=NA--------------------------------------------------
kableExtra::kable_styling(knitr::kable(dfMarkColor) , full_width = F)

## ---- results="hide"----------------------------------------------------------
# mark position data (inc. cen.) 
dfOfMarks2

## ---- echo=FALSE, comment=NA--------------------------------------------------
kableExtra::kable_styling(knitr::kable(dfOfMarks2) , full_width = F)

## ----example2, echo=TRUE, results="hide", fig.width=10, fig.height=6, message=FALSE----
library(idiogramFISH)

# load some package data.frames - optional
data(dfChrSizeHolo, dfMarkColor, dfMarkPosHolo)

# column Mbp not for plotting purposes
dfChrSizeHolo$Mbp<-dfChrSizeHolo$chrSize*100

# svg("testing.svg",width=14,height=8 )
par(mar = c(0, 0, 0, 0), omi=rep(0,4) )

plotIdiograms(dfChrSize  =dfChrSizeHolo, # data.frame of chr. size
              dfMarkColor=dfMarkColor,   # df of mark style
              dfMarkPos  =dfMarkPosHolo, # df of mark positions
              
              addOTUName=FALSE,        # do not add OTU names
              distTextChr = 1,         # chr. name distance to chr.
              chrSize = TRUE,          # show chr. size under chr.
              chrSizeMbp = TRUE,       # show chr. size in Mbp under chr. requires Mbp column
              
              rulerPos=-0.1,           # position of ruler
              rulerNumberPos=.9        # position of numbers of rulers
              ,xPosRulerTitle = 3            # pos. of ruler title (units)
              
              ,xlimLeftMod=2           # modify xlim left argument of plot
              ,ylimBotMod=.2           # modify ylim bottom argument of plot
              ,legendHeight=.5         # height of legend labels
              ,legendWidth = 1.2       # width of legend labels
              ,xModifier = .025        # separ. among chromatids
              )                  
# dev.off() # close svg()

## ---- results="hide"----------------------------------------------------------
dfChrSizeHolo

## ----holocentrics, echo=FALSE, comment=NA-------------------------------------

kableExtra::kable_styling(knitr::kable(dfChrSizeHolo) , full_width = F)

## ---- results="hide"----------------------------------------------------------
dfMarkColor

## ---- echo=FALSE, comment=NA--------------------------------------------------
kableExtra::kable_styling(knitr::kable(dfMarkColor) , full_width = F)

## ---- results="hide"----------------------------------------------------------
dfMarkPosHolo

## ---- echo=FALSE, comment=NA--------------------------------------------------
kableExtra::kable_styling(knitr::kable(dfMarkPosHolo) , full_width = F)

## ---- echo=T,  comment=NA, results="hide", message=FALSE----------------------
# chromosome data, if only 1 species, column OTU is optional
require(plyr)
dfOfChrSize$OTU   <- "Species mono"
dfChrSizeHolo$OTU <- "Species holo"
 
monoholoCS <- plyr::rbind.fill(dfOfChrSize,dfChrSizeHolo)

dfOfMarks2$OTU     <-"Species mono"
dfOfMarks2[which(dfOfMarks2$markName=="5S"),]$markSize<-.7
dfMarkPosHolo$OTU <-"Species holo"

monoholoMarks <- plyr::rbind.fill(dfOfMarks2,dfMarkPosHolo)

## ---- echo=T, results="hide", fig.width=10, fig.height=6, message=FALSE-------
library(idiogramFISH)

#svg("testing.svg",width=14,height=10 )
png("monoholoCS.png", width=700, height=600)
par(mar=rep(0,4))
plotIdiograms(dfChrSize  = monoholoCS,   # data.frame of chr. size
              dfMarkColor= dfMarkColor,  # df of mark style
              dfMarkPos  = monoholoMarks,# df of mark positions, includes cen. marks
              
              chrSize = TRUE,            # show chr. size under chr.
              
              squareness = 4,            # vertices squareness
              roundedCen = FALSE,        # triangular cen.
              addOTUName = TRUE,         # add OTU names
              OTUTextSize = .7,          # font size of OTU
              distTextChr = .5,          # separ. among chr. and text and among chr. name and indices
              
              karHeiSpace = 4,           # karyotype height inc. spacing
              karIndexPos = .2,          # move karyotype index
              
              legendHeight= 1,           # height of legend labels
              legendWidth = 1,           # width of legend labels
              fixCenBorder = TRUE,       # use chrColor as border color of cen. or cen. marks
              
              rulerPos= 0,               # position of ruler
              ruler.tck=-0.02,           # size and orientation of ruler ticks
              rulerNumberPos=.9,         # position of numbers of rulers
              xPosRulerTitle = 3.5,      # ruler title (units) position
              
              xlimLeftMod=1,             # modify xlim left argument of plot
              xlimRightMod=3,            # modify xlim right argument of plot
              ylimBotMod= .2             # modify ylim bottom argument of plot
              
              ,chromatids=FALSE          # do not show separ. chromatids
              
              # for Circular Plot, add:
              
              # ,circularPlot = TRUE       # circularPlot
              # ,shrinkFactor = .9         # percentage 1 = 100% of circle with chr.
              # ,circleCenter = 3          # X coordinate of circleCenter (affects legend pos.)
              # ,chrLabelSpacing = .9      # chr. names spacing
              
              # ,OTUsrt = 0                # angle for OTU name (or number)
              # ,OTUplacing = "number"     # Use number and legend instead of name
              # ,OTULabelSpacerx = -1.5    # modify position of OTU label, when OTUplacing="number" or "simple"
              # ,OTUlegendHeight = 1.5     # space among OTU names when in legend - OTUplacing
)
dev.off() # close png

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
# cat(paste0("![](dfOfChrSize.png)" ) )
cat(paste0("![](monoholoCS.png)" ) )

## ---- echo=FALSE, results="hide", fig.width=10, fig.height=7, message=FALSE----
library(idiogramFISH)

# svg("testing.svg",width=14,height=10 )
par(mar=rep(0,4))
plotIdiograms(dfChrSize  = monoholoCS,   # data.frame of chr. size
              dfMarkColor= dfMarkColor,  # df of mark style
              dfMarkPos  = monoholoMarks,# df of mark positions, includes cen. marks
              
              squareness = 4,            # vertices squareness
              roundedCen = FALSE,        # triangular cen.
              addOTUName = TRUE,         # add OTU names
              distTextChr = .5,          # separ. among chr. and text and among chr. name and indices
              
              karHeiSpace = 3,           # karyotype height inc. spacing
              karIndexPos = .2,          # move karyotype index
              
              legendHeight= 1,           # height of legend labels
              legendWidth = 1,           # width of legend labels
              fixCenBorder = TRUE,       # use chrColor as border color of cen. or cen. marks
              
              rulerPos= 0,               # position of ruler
              ruler.tck=-0.02,           # size and orientation of ruler ticks
              rulerNumberPos=.9,         # position of numbers of rulers
              
              xlimLeftMod=1,             # modify xlim left argument of plot
              xlimRightMod=3,            # modify xlim right argument of plot
              ylimBotMod= .2             # modify ylim bottom argument of plot
              
              ,circularPlot = T          # circularPlot
              ,shrinkFactor = .9         # percentage 1 = 100% of circle with chr.
              ,circleCenter = 3          # X coordinate of circleCenter (affects legend pos.)
              ,OTUsrt = 0                # angle for OTU name (or number)
              ,OTUplacing = "number"     # Use number and legend instead of name
              ,chrLabelSpacing = .9      # chr. names spacing
              ,OTULabelSpacerx = -1.5    # modify position of OTU label, when OTUplacing="number" or "simple"
              ,OTUlegendHeight = 1.5     # space among OTU names when in legend - OTUplacing
              ,OTUTextSize = .7          # font size of OTU
)
#dev.off() # close svg()

## ----citation, echo=FALSE, comment=NA,results='asis'--------------------------
print(citation("idiogramFISH"),bibtex=FALSE )

## ---- echo=TRUE, results=FALSE, eval=FALSE------------------------------------
#  sink("idiogramFISH.bib")
#  toBibtex(citation("idiogramFISH"))
#  sink()

## ---- echo=F,  results="asis", eval=T-----------------------------------------
img1_path <- "../man/figures/kofi1.png"
img1_path <- normalizePath(img1_path)

if(file.exists(img1_path)) {
  cat(paste0("[Fernando Roa](https://ferroao.gitlab.io/curriculumpu/){target='_blank'}&nbsp;&nbsp;<a href='https://ko-fi.com/X7X71PZZG' target='_blank'><img src=",img1_path," width=\"10%\">") )
} else {
  cat(paste0("[Fernando Roa](https://ferroao.gitlab.io/curriculumpu/){target='_blank'}"))  
}

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis", eval=FALSE----
#  cat("<a href='https://ko-fi.com/X7X71PZZG' target='_blank'><img height='12' style='border:0px;height:12px;' src='../man/figures/kofi1.png' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>")

## ----include=FALSE,eval=FALSE-------------------------------------------------
#  # automatically create a bib database for R packages, this is currently not used by vignette refs/packages2.bib
#  knitr::write_bib(c(
#    .packages(), 'bookdown', 'knitr', 'rmarkdown',"devtools","pkgdown","crayon","ggtree","ggplot2","ggpubr","phytools","plyr","dplyr","tidyr","rentrez"
#  ), 'refs/packages2.bib')

## ---- echo=F------------------------------------------------------------------
chapterFile_plotting<- "../chapters/01-plotting.Rmd"
if(file.exists(chapterFile_plotting)){
  childExists_plotting<-TRUE
  child_docs_plotting <- chapterFile_plotting
} else {
  childExists_plotting<-FALSE
  child_docs_plotting <- ""
}

## ---- results="hide", message=FALSE, warning=FALSE----------------------------

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

## ---- echo=TRUE, results="hide", fig.width=12, fig.height=7, message=FALSE, dev='png'----
# svg("mydfChrSize.svg",width=12,height=6 )

par(mar = c(0, 0, 0, 0))

plotIdiograms(dfChrSize= mydfChrSize,     # chr. size data.frame
              dfMarkPos= mydfOfMarks,     # mark position data.frame (inc. cen.)
              dfMarkColor=mydfMarkColor,  # mark style d.f.
              
              chrSpacing=.6,              # separ. among chr.
              distTextChr = .7,           # separation among text and chr. names and ind.        
              orderChr = "name",          # order chr. by name
              karHeiSpace=2               # vertical size of karyotype including spacer
              
              ,arrowhead = .5             # proportion of head of arrow
              
              ,fixCenBorder = TRUE        # use chrColor as border color of cen. or cen. marks
              ,legendWidth = .8           # legend item width
              ,legendHeight = .5          # legend item height
              ,markLabelSpacer = 2        # legend spacer
              ,lwd.mimicCen = 2.5         # constric. mark line width
              
              ,rulerPos=0                 # ruler position
              ,ruler.tck=-0.01            # ticks of ruler size and orientation
              ,xPosRulerTitle = 2.5             # ruler units pos.
              
              ,markPer = "45S"           # show mark % of chr.  under kar.
              ,showMarkPos = TRUE         # show position of marks under kar. See bToRemove
              ,bToRemove = c("B mark","cB mark") # bands to remove from pos. calc. See showMarkPos
              
              ,notes=notesdf              # data.frame with notes 
              ,notesTextSize = 1.3        # font size of notes
              ,notesPos = .2              # space from chr. (right) to note
              
              ,ylimBotMod = 2             # modify ylim bottom argument
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

## ---- echo=TRUE, results="hide", fig.width=4.5, fig.height=4.5, message=FALSE,dev='png', eval=FALSE----
#  
#  png("mydfChrSize2.png", width=550, height=550)
#  par(mar = c(0, 0, 0, 0))
#  plotIdiograms(dfChrSize   = bigdfOfChrSize[1:8,],  # chr. size data.frame
#                dfMarkColor = mydfMarkColor,# mark style df
#                dfMarkPos   = bigdfOfMarks, # mark position df
#  
#                centromereSize = 0,         # <- HERE
#  
#                squareness=3,               # vertices squareness
#                chrSpacing = .7,            # space among chr.
#                karHeight = 2,              # karyotype rel. height
#                karHeiSpace=4,              # vertical size of karyotype including spacer
#                amoSepar= 2.5,              # separation among karyotype
#  
#                indexIdTextSize=.8,         # font size of chr. name and indices
#                karIndexPos = .1,           # position of kar. index
#                markLabelSize=.7,           # font size of mark legends
#                fixCenBorder = FALSE,       # do not use chrColor as border color of cen. or cen. marks
#                distTextChr = .8,           # separation among chr. and ind.
#  
#                rulerPos= 0,                # ruler position
#                ruler.tck=-0.01,            # ticks of ruler size and orientation
#                xPosRulerTitle = 2.6,              # ruler units pos.
#  
#                xlimLeftMod = 2,            # modify xlim left argument
#                ylimBotMod = 0.4,           # modify ylim bottom argument
#                ylimTopMod = 0              # modify ylim top argument
#                ,lwd.cM = 2                 # thickness of cM marks
#                )
#  dev.off()

## ---- echo=FALSE, results="hide", fig.width=4.5, fig.height=4.5, message=FALSE,dev='png', eval=TRUE, include=FALSE----

png("../vignettes/mydfChrSize2.png", width=550, height=550)
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
              xPosRulerTitle = 2.6,              # ruler units pos.
              
              xlimLeftMod = 2,            # modify xlim left argument 
              ylimBotMod = 0.4,           # modify ylim bottom argument
              ylimTopMod = 0              # modify ylim top argument
              ,lwd.cM = 2                 # thickness of cM marks 
              )
dev.off()

## ---- results="asis", comment=NA, echo=FALSE----------------------------------
mydfChrSize2.png<-normalizePath("../vignettes/mydfChrSize2.png")
cat(paste0("![](",mydfChrSize2.png,")" ) )
# cat(paste0("![](mydfChrSize2.jpg)" ) )

## ---- eval=FALSE--------------------------------------------------------------
#  unique(mydfOfMarks$markName)

## ---- echo=TRUE, results="hide", fig.width=10, fig.height=4.5, message=FALSE,dev="png", eval=FALSE----
#  
#  charVectorCol <- c("tomato3","darkolivegreen4","dfsd","blue","green")
#  
#  # Modify size of kar. to use rulerInterval and ceilingFactor (>= 1.13)
#  quo<-9
#  dfOfChrSize2<-dfOfChrSize
#  dfOfChrSize2$shortArmSize<-dfOfChrSize$shortArmSize/quo
#  dfOfChrSize2$longArmSize<-dfOfChrSize$longArmSize/quo
#  dfOfMarks2b<-dfOfMarks2
#  dfOfMarks2b$markSize<-dfOfMarks2$markSize/quo
#  dfOfMarks2b$markDistCen<-dfOfMarks2$markDistCen/quo
#  
#  png("dfOfChrSizeVector.png", width=1000, height=450)
#  par(mar=rep(0,4))
#  plotIdiograms(dfChrSize = dfOfChrSize2,    # d.f. of chr. sizes
#                dfMarkPos = dfOfMarks2b,     # d.f. of marks' positions
#                defaultStyleMark = "cM",     # forces "cM" style in d.f dfMarkColor (exc. 5S)
#  
#                mycolors = charVectorCol,    # colors to use
#  
#                distTextChr = .5,            # separ. text and chr.
#  
#                markLabelSize=.7,            # font size for labels (legend)
#                lwd.cM=2,                    # width of cM marks
#                legendWidth=0.9,             # legend item width
#                legendHeight=.5,
#  
#                rulerPos= 0,                 # ruler position
#                ruler.tck=-0.01,             # ruler tick orientation and length
#                rulerNumberSize=.5           # ruler font size
#                ,xPosRulerTitle = 2.8              # ruler units pos.
#  
#                ,xlimRightMod = 1            # modify xlim right arg.
#  )
#  dev.off() # close png

## ---- echo=FALSE, results="hide", fig.width=10, fig.height=4.5, message=FALSE,dev="png", eval=TRUE, include=FALSE----

charVectorCol <- c("tomato3","darkolivegreen4","dfsd","blue","green")

# Modify size of kar. to use rulerInterval and ceilingFactor (>= 1.13)  
quo<-9
dfOfChrSize2<-dfOfChrSize
dfOfChrSize2$shortArmSize<-dfOfChrSize$shortArmSize/quo
dfOfChrSize2$longArmSize<-dfOfChrSize$longArmSize/quo
dfOfMarks2b<-dfOfMarks2
dfOfMarks2b$markSize<-dfOfMarks2$markSize/quo
dfOfMarks2b$markDistCen<-dfOfMarks2$markDistCen/quo

png("../vignettes/dfOfChrSizeVector.png", width=1000, height=450)
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
              ,xPosRulerTitle = 2.8               # ruler units pos.
              
              ,xlimRightMod = 1            # modify xlim right arg.
)
dev.off() # close png

## ----hidethiscran, results="asis", comment=NA, echo=FALSE---------------------
dfOfChrSizeVector.png<-normalizePath("../vignettes/dfOfChrSizeVector.png")
cat(paste0("![](",dfOfChrSizeVector.png,")" ) )

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

## ----example, echo=TRUE, results="hide", fig.width=14, fig.height=6, message=FALSE, dev="png"----
# library(idiogramFISH)
# svg("mydfChrSizeHolo.svg",width=13.5,height=6 )
# png("mydChrSizeHolo.png", width=600, height=300)

par(mar=c(0,0,0,1)) # bottom left top right

plotIdiograms(dfChrSize  = mydfChrSizeHolo,# data.frame of chr. sizes
              dfMarkColor= mydfMarkColor,  # df of mark style
              dfMarkPos  = mydfMarkPosHolo,# df of mark positions
              addOTUName=FALSE,            # add OTU names
              
              xlimLeftMod= 2,              # modify xlim left argument
              ylimTopMod= -1,              # modify ylim top argument
              ylimBotMod= -2               # modify ylim bottom argument
              ,rulerPos = 0                # ruler position
              ,ruler.tck = -0.01           # ruler ticks size and orient.
              ,xPosRulerTitle=2.6
              
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

## ---- echo=TRUE, results="hide", fig.width=13.5, fig.height=6, message=FALSE, dev='png'----
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
mydfMarkPosHolo2<-plyr::rbind.fill(mydfMarkPosHolo,
                                   data.frame(OTU="Species one",
                                              chrName=1:4,
                                              markName=c("c45S","c5S","constr","cB"), # <- use new mark
                                              markPos=2.5,
                                              markSize=NA 
                                              )
                  )

# png("mydChrSizeHolo.png", width=600, height=300)
par(mar=c(0,0,0,1)) # bottom left top right

plotIdiograms(dfChrSize  = mydfChrSizeHolo, # data.frame of chr. sizes
              dfMarkColor= mydfMarkColor2,  # df of mark style
              dfMarkPos  = mydfMarkPosHolo2,# df of mark positions
              addOTUName=FALSE,             # add OTU names
              
              xlimLeftMod= 2,              # modify xlim left argument
              ylimTopMod= -1,              # modify ylim top argument
              ylimBotMod= -2               # modify ylim bottom argument
              ,rulerPos = 0
              ,ruler.tck = -0.01
              ,xPosRulerTitle = 2.6
              
              ,legendWidth=1               # width of legend
              ,legendHeight=.7             # height of legend item 
              ,lwd.mimicCen=2.5            # line width of const. mark
              ,pattern="^c"                # regex pattern to remove from mark names
              ,remSimiMarkLeg = TRUE       # remove pseudoduplicated mark names (got equal after pattern removal)
              # ,legend="inline"           # legends inline
              # ,bannedMarkName = c("c45S","cB") # do not use this names from labels
)
# dev.off() # closes png or svg

## ---- results="asis", eval=!childExists_plotting, echo=FALSE------------------
#  cat("# Plotting chromosomes")

## ---- results="asis", eval=!childExists_plotting, echo=FALSE------------------
#  cat("[https://ferroao.gitlab.io/idiogramfishhelppages](https://ferroao.gitlab.io/idiogramfishhelppages/#plotting-chromosomes)")

## ---- echo=F------------------------------------------------------------------
chapterFile_multiple<- "../chaptersBLOCK/02-multiple.Rmd"
if(file.exists(chapterFile_multiple)){
  childExists_multiple<-TRUE
  child_docs_multiple <- chapterFile_multiple
} else {
  childExists_multiple<-FALSE
  child_docs_multiple <- ""
}

## ---- results="asis", eval=!childExists_multiple, echo=FALSE------------------
cat("# Several OTUs")

## ---- results="asis", eval=!childExists_multiple, echo=FALSE------------------
cat("[https://ferroao.gitlab.io/idiogramfishhelppages](https://ferroao.gitlab.io/idiogramfishhelppages/#multiple-otus)")

## ---- echo=F------------------------------------------------------------------
chapterFile_units<- "../chaptersBLOCK/03-units.Rmd"
if(file.exists(chapterFile_units)){
  childExists_units<-TRUE
  child_docs_units <- chapterFile_units
} else {
  childExists_units<-FALSE
  child_docs_units <- ""
}

## ---- results="asis", eval=!childExists_units, echo=FALSE---------------------
cat("# Changing Units")

## ---- results="asis", eval=!childExists_units, echo=FALSE---------------------
cat("[https://ferroao.gitlab.io/idiogramfishhelppages](https://ferroao.gitlab.io/idiogramfishhelppages/#changing-units)")

## ---- echo=F------------------------------------------------------------------
chapterFile0<- "../chaptersBLOCK/04-gish.Rmd"
if(file.exists(chapterFile0)){
  childExists0<-TRUE
  child_docs0 <- chapterFile0
} else {
  childExists0<-FALSE
  child_docs0 <- ""
}

## ---- results="asis", eval=!childExists0, echo=FALSE--------------------------
cat("# GISH")

## ---- results="asis", eval=!childExists0, echo=FALSE--------------------------
cat("[https://ferroao.gitlab.io/idiogramfishhelppages](https://ferroao.gitlab.io/idiogramfishhelppages/#gish)")

## ---- echo=F------------------------------------------------------------------
chapterFile1<- "../chaptersBLOCK/05-groups.Rmd"
if(file.exists(chapterFile1)){
  childExists1<-TRUE
  child_docs1 <- chapterFile1
} else {
  childExists1<-FALSE
  child_docs1 <- ""
}

## ---- results="asis", eval=!childExists1, echo=FALSE--------------------------
cat("# Using groups")

## ---- results="asis", eval=!childExists1, echo=FALSE--------------------------
cat("[https://ferroao.gitlab.io/idiogramfishhelppages](https://ferroao.gitlab.io/idiogramfishhelppages/#groups)")

## ---- echo=F------------------------------------------------------------------
chapterFile2<- "../chaptersBLOCK/06-circular.Rmd"
if(file.exists(chapterFile2)){
  childExists2<-TRUE
  child_docs2 <- chapterFile2
} else {
  childExists2<-FALSE
  child_docs2<-""
}

## ---- results="asis", eval=!childExists2, echo=FALSE--------------------------
cat("# Circular plots")

## ---- results="asis", eval=!childExists2, echo=FALSE--------------------------
cat("[https://ferroao.gitlab.io/idiogramfishhelppages](https://ferroao.gitlab.io/idiogramfishhelppages/#circular-plots)")

## ---- echo=F------------------------------------------------------------------
chapterFile3<- "../chaptersBLOCK/07-phylogeny.Rmd"
if(file.exists(chapterFile3)){
  childExists3<-TRUE
  child_docs3 <- chapterFile3
} else {
  childExists3<-FALSE
  child_docs3<-""
}

## ---- results="asis", eval=!childExists3, echo=FALSE--------------------------
cat("# Plotting alongside phylogeny")

## ---- results="asis", eval=!childExists3, echo=FALSE--------------------------
cat("[https://ferroao.gitlab.io/idiogramfishhelppages](https://ferroao.gitlab.io/idiogramfishhelppages/#plotting-alongside-phylogeny)")

## ---- echo=F------------------------------------------------------------------
chapterFile<- "../chaptersBLOCK/08-citrushelp.Rmd"
if(file.exists(chapterFile)){
  childExists<-TRUE
  child_docs <- chapterFile
} else {
  childExists<-FALSE
  child_docs<-""
}

## ---- results="asis", eval=!childExists, echo=FALSE---------------------------
cat("# *Citrus* - helper functions")

## ---- results="asis", eval=!childExists, echo=FALSE---------------------------
cat("[https://ferroao.gitlab.io/idiogramfishhelppages](https://ferroao.gitlab.io/idiogramfishhelppages/#citrus---helper-functions)")

## ---- echo=F------------------------------------------------------------------
chapterFile4<- "../chaptersBLOCK/09-human.Rmd"
if(file.exists(chapterFile4)){
  childExists4<-TRUE
  child_docs4 <- chapterFile4
} else {
  childExists4<-FALSE
  child_docs4<-""
}

## ---- results="asis", eval=!childExists4, echo=FALSE--------------------------
cat("# Human karyotype")

## ---- results="asis", eval=!childExists4, echo=FALSE--------------------------
cat("[https://ferroao.gitlab.io/idiogramfishhelppages](https://ferroao.gitlab.io/idiogramfishhelppages/#human-karyotype)")

## ---- echo=F------------------------------------------------------------------
chapterFile_param<- "../chaptersBLOCK/10-functions.Rmd"
if(file.exists(chapterFile_param)){
  childExists_param<-TRUE
  child_docs_param <- chapterFile_param
} else {
  childExists_param<-FALSE
  child_docs_param<-""
}

## ---- results="asis", eval=!childExists_param, echo=FALSE---------------------
cat("# Functions")

## ---- results="asis", eval=!childExists_param, echo=FALSE---------------------
cat("[https://ferroao.gitlab.io/idiogramfishhelppages](https://ferroao.gitlab.io/idiogramfishhelppages/#functions)")

## ---- echo=F------------------------------------------------------------------
chapterFile_news<- "../chaptersBLOCK/11-news.Rmd"
if(file.exists(chapterFile_news)){
  childExists_news<-TRUE
  child_docs_news <- chapterFile_news
} else {
  childExists_news<-FALSE
  child_docs_news<-""
}

## ---- results="asis", eval=!childExists_news, echo=FALSE----------------------
cat("# News {-}")

## ---- results="asis", eval=!childExists_news, echo=FALSE----------------------
cat("[https://ferroao.gitlab.io/idiogramfishhelppages](https://ferroao.gitlab.io/idiogramfishhelppages/#news)")

