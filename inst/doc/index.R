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
library(knitr)

## ---- echo=F,  results="asis"--------------------------------------------
img1_path <- "../man/figures/logo.svg"
if(file.exists(img1_path)) {
cat(paste0("<img src=",img1_path," class=\"right\" width=\"20%\">") )
}

## ---- include = FALSE----------------------------------------------------
 badge_devel_gitlab<-function(pkg, color){
    v <- rvcheck:::check_github_gitlab(pkg, "gitlab")$latest_version
    url <- paste0("https://gitlab.com/", pkg)
    idiogramFISH:::badge_custom("devel version", v, color, url)
 }

## ---- echo=F, fig.show = "hold", fig.align = "default"-------------------
# library(badger)
if (requireNamespace("RCurl", quietly = TRUE)  ) {
cranversion <- "https://www.r-pkg.org/badges/version/idiogramFISH?color=orange"
cranversion_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(cranversion) ), error=function(e) NA )
if (!is.na(cranversion_cont)){
cranversion_contFile <- "cranversion.svg"
writeLines(cranversion_cont, con = cranversion_contFile)
cranversion_contFile <- normalizePath(cranversion_contFile)
knitr::include_graphics(cranversion_contFile)
}
}
# tryCatch(cat(paste(badger::badge_cran_release("idiogramFISH", "orange")  ,"&nbsp;" ) ), error=function(e) return("") )  
# tryCatch(cat(paste(badger::badge_cran_download("idiogramFISH", type="grand-total", color="orange") ) ), error=function(e) return("") )

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
# library(badger)
if (requireNamespace("RCurl", quietly = TRUE)  ) {
crandownloads<-"https://cranlogs.r-pkg.org/badges/grand-total/idiogramFISH?color=orange"
crandownloads_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(crandownloads) ), error=function(e) NA )
if (!is.na(crandownloads_cont)){
crandownloads_contFile <- "crandownload.svg"
writeLines(crandownloads_cont, con = crandownloads_contFile)
crandownloads_contFile <- normalizePath(crandownloads_contFile)
cat(paste0("&nbsp;![''](",knitr::include_graphics(crandownloads_contFile),")" ) )
#   this gives ERROR WHEN DEVTOOLS INSTALL IN WINDOWS.
# cat(paste0("&nbsp;![''](",crandownloads,")" ) )
}
}

## ---- echo=F, message=FALSE, warning=F, include=T------------------------
if (requireNamespace("RCurl", quietly = TRUE)  ) {
v<-tryCatch(suppressWarnings(rvcheck:::check_github_gitlab("ferroao/idiogramFISH", "gitlab")$latest_version), error=function(e) NA )
link<-tryCatch(suppressWarnings(idiogramFISH:::badge_custom("devel version", v, "green") ), error=function(e) NA )
if(!is.na(link)){
svglink<-gsub("\\[|\\]|!|\\(|\\)","", link)
gitbadge_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svglink) ), error=function(e) NA )
if (!is.na(gitbadge_cont)){
# gitbadge_contFile <- tempfile(fileext = ".svg")
gitbadge_contFile <- "gitbadge.svg"
writeLines(gitbadge_cont, con = gitbadge_contFile)
gitbadge_contFile <- normalizePath(gitbadge_contFile)
knitr::include_graphics(gitbadge_contFile)
}
}
}

## ---- eval=FALSE---------------------------------------------------------
#  # This installs package devtools, necessary for installing the dev version
#  install.packages("devtools")
#  
#  url <- "https://gitlab.com/ferroao/idiogramFISH"

## ---- eval=FALSE---------------------------------------------------------
#  # Necessary packages for vignettes:
#  list.of.packages <- c(
#      "knitr",
#      "kableExtra",
#      "prettydoc",
#      "rmarkdown",
#      "RCurl",
#      "rvcheck"
#      )
#  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#  if(length(new.packages)) install.packages(new.packages)

## ---- eval=FALSE---------------------------------------------------------
#  # Linux with vignettes and Windows
#  devtools::install_git(url = url,build_vignettes = TRUE, force=T)

## ---- eval=FALSE---------------------------------------------------------
#  # Mac with vignettes
#  devtools::install_git(url = url, build_opts=c("--no-resave-data","--no-manual") )

## ---- eval=FALSE---------------------------------------------------------
#  # clone repository:
#  git clone "https://gitlab.com/ferroao/idiogramFISH"
#  
#  R CMD build idiogramFISH
#  # install
#  R CMD INSTALL idiogramFISH_*.tar.gz

## ----example, echo=T, results="hide", fig.width=10, fig.height=6, message=FALSE, dev='svg'----
# fig.width=10, fig.height=6

library(idiogramFISH)

data(dfOfChrSize) # chromsome data
data(dfMarkColor) # mark general data
data(dfOfMarks2)  # mark position data (inc. cen.)

svg("dfOfChrSize.svg",width=12,height=8 )
# png("dfOfChrSize.png", width=500, height=400)
plotIdiograms(dfChrSize=dfOfChrSize,    # data.frame of chr. size
              dfMarkColor=dfMarkColor,  # d.f of mark style                 < == Optional for ver. > 1.0.0
              dfMarkPos=dfOfMarks2,     # df of mark positions (includes cen. marks)
              
              rulerPos=-.9,             # position of rulers
              ruler.tck=-0.01,          # size and orientation of ruler ticks
              rulerNumberSize=.8        # font size of rulers
              
              ,legendWidth=1            # width of legend items
              ,distTextChr = .5         # chr. text separation
              
              ,xlimLeftMod = 2          # xlim left param.
              ,ylimBotMod = 0           # modify ylim bottom argument
              ,ylimTopMod = 0           # modify ylim top argument
              ,asp=1                    # y/x aspect, see ?plot
)
dev.off()

## ---- results="asis", comment=NA, echo=FALSE-----------------------------
# cat(paste0("![](dfOfChrSize.png)" ) )
cat(paste0("![](dfOfChrSize.svg)" ) )

## ----monocentrics, comment=NA--------------------------------------------

# chromsome data, if only 1 species, column OTU is optional
dfOfChrSize
# mark general data
dfMarkColor 

## ---- comment=NA---------------------------------------------------------
# mark position data, if only 1 species, column OTU is optional
dfOfMarks2

## ----example2, echo=T, results="hide", fig.width=10, fig.height=6, message=FALSE----
library(idiogramFISH)

# load some package data.frames
data(dfChrSizeHolo, dfMarkColor, dfMarkPosHolo)

# plotIdiogramsHolo is deprecated
par(mar = c(0, 0, 0, 0), omi=rep(0,4), oma=rep(0,4) )
# svg("testing.svg",width=14,height=8 )
plotIdiograms(dfChrSize=dfChrSizeHolo, # data.frame of chr. size
              dfMarkColor=dfMarkColor, # df of mark style
              dfMarkPos=dfMarkPosHolo, # df of mark positions
              addOTUName=FALSE,        # do not add OTU names
              
              distTextChr = .5,        # chr. name distance to chr.
              
              rulerPos=-.9,            # position of ruler
              rulerNumberPos=.9,       # position of numbers of rulers
              
              xlimLeftMod=2,           # modify xlim left argument of plot
              ylimBotMod=.2            # modify ylim bottom argument of plot
              ,legendHeight=.5         # height of legend labels
              ,legendWidth = 1.2       # width of legend labels
              ,asp=1)                  # y/x aspect
# dev.off() # close svg()

## ----holocentrics, comment=NA--------------------------------------------
# chromsome data, if only 1 species, column OTU is optional
dfChrSizeHolo
# mark general data
dfMarkColor 
# mark position, if only 1 species, column OTU is optional
dfMarkPosHolo

## ---- echo=T,  comment=NA, results="hide", message=FALSE-----------------
# chromosome data, if only 1 species, column OTU is optional
require(plyr)
dfOfChrSize$OTU   <- "Species mono"
dfChrSizeHolo$OTU <- "Species holo"
 
monoholoCS <- plyr::rbind.fill(dfOfChrSize,dfChrSizeHolo)

dfOfMarks2$OTU     <-"Species mono"
dfOfMarks2[which(dfOfMarks2$markName=="5S"),]$markSize<-.7
dfMarkPosHolo$OTU <-"Species holo"

monoholoMarks <- plyr::rbind.fill(dfOfMarks2,dfMarkPosHolo)

## ---- echo=T, results="hide", fig.width=10, fig.height=6, message=FALSE, dev='svg'----
library(idiogramFISH)
# load some saved dataframes

# function plotIdiogramsHolo deprecated for ver. > 1.5.1

#svg("testing.svg",width=14,height=10 )
png("monoholoCS.png", width=700, height=500)
par(mar=rep(0,4))
plotIdiograms(dfChrSize  = monoholoCS,   # data.frame of chr. size
              dfMarkColor= dfMarkColor,  # df of mark style
              dfMarkPos  = monoholoMarks,# df of mark positions, includes cen. marks
              
              roundness = 4,             # vertices roundness
              addOTUName = TRUE,         # add OTU names
              
              karHeiSpace = 3,           # karyotype height inc. spacing
              karIndexPos = .2,          # move karyotype index
              
              legendHeight= 1,           # height of legend labels
              legendWidth = 1,           # width of legend labels
              
              rulerPos= -0.5,            # position of ruler
              ruler.tck=-0.02,           # size and orientation of ruler ticks
              rulerNumberPos=.9,         # position of numbers of rulers
              
              xlimLeftMod=1,             # modify xlim left argument of plot
              xlimRightMod=3,            # modify xlim right argument of plot
              ylimBotMod=-.2             # modify ylim bottom argument of plot
              ,asp=1                     # y x aspect ratio
)
dev.off()

## ---- results="asis", comment=NA, echo=FALSE-----------------------------
cat(paste0("![](monoholoCS.png)" ) )

## ---- echo=T, results="hide", fig.width=10, fig.height=9, message=FALSE----
library(idiogramFISH)
# load some saved dataframes

par(mar=rep(0,4))

# svg("allo.svg",width=10,height=9 )
plotIdiograms(dfChrSize = parentalAndHybChrSize,  # d.f. of chr. sizes
              dfMarkPos = dfAlloParentMarks,      # d.f. of marks' positions
              cenColor  = NULL                    # cen. color when GISH
              
              ,karHeiSpace=5,                     # karyotype height including spacing
              karSepar = FALSE,                   # equally sized karyotypes       
              
              rulerPos=-1,                        # ruler position
              ruler.tck= -0.002,                  # ruler tick orientation and length
              rulerNumberSize=.5                  # ruler font size
              ,legend=""                          # no legend
              ,asp=1                              # y x aspect ratio

              ,ylimBotMod = 1                     # modifiy ylim bottom argument
              ,xlimRightMod = 0                   # modify xlim right argument
              
)
#dev.off() # close svg()

## ---- comment=NA, echo=FALSE---------------------------------------------
cat("parentalAndHybChrSize")

## ---- comment=NA, echo=F-------------------------------------------------
print(parentalAndHybChrSize, row.names=F)

## ---- comment=NA, echo=FALSE---------------------------------------------
cat("dfAlloParentMarks")

## ----gish2, comment=NA, echo=F-------------------------------------------
print(dfAlloParentMarks, row.names=F)

## ----citation, results='asis', echo=FALSE--------------------------------
# chromsome data, if only 1 species, column OTU is optional
print(citation("idiogramFISH"),bibtex=FALSE)

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
# library(badger)
if (requireNamespace("RCurl", quietly = TRUE)  ) {
# donate_cont <- tryCatch(RCurl::getURLContent(donate), error=function(e) NA )
# if (!is.na(donate_cont)){
# donate_contFile <- "donate.svg"
# # donate_contFile <- tempfile(fileext = ".svg")
# writeLines(donate_cont, con = donate_contFile)
# cat(paste0("[![donate](",knitr::include_graphics(donate_contFile),")](https://liberapay.com/ferroao/donate)" ) )
# }
donate <- "https://liberapay.com/assets/widgets/donate.svg"
donate_contFile <- "donate.svg"
tryCatch(suppressWarnings(download.file(donate, donate_contFile) ), error=function(e) "")
if(file.exists(donate_contFile ) ) {
cat(paste0("[![donate](",knitr::include_graphics(donate_contFile),")](https://liberapay.com/ferroao/donate)" ) )
}
}

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", warning=FALSE, results="asis"----
# library(badger)
if (requireNamespace("RCurl", quietly = TRUE) ) {
donateweek <- "http://img.shields.io/liberapay/receives/ferroao.svg"
donateweek_contFile <- "donateweek.svg"
tryCatch(suppressWarnings(download.file(donateweek, donateweek_contFile) ), error=function(e) "")
if(file.exists(donateweek_contFile ) ) {
cat(paste0("[![donate](",knitr::include_graphics(donateweek_contFile),")](https://liberapay.com/ferroao/donate)" ) )
}
}

## ----include=FALSE-------------------------------------------------------
# automatically create a bib database for R packages, this is currently not used by vignette packages2.bib
knitr::write_bib(c(
  .packages(), 'bookdown', 'knitr', 'rmarkdown',"devtools","pkgdown","crayon","ggtree","ggplot2","ggpubr","phytools","plyr","dplyr"
), 'packages2.bib')

