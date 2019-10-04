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

## ---- echo=F,  results="asis"--------------------------------------------
img1_path <- "../man/figures/logo.svg"
if(file.exists(img1_path)) {
cat(paste0("<img src=",img1_path," class=\"right\" width=\"20%\">") )
}

## ---- include = FALSE----------------------------------------------------
 badge_devel_gitlab<-function(pkg, color){
    v <- rvcheck:::check_github_gitlab(pkg, "gitlab")$latest_version
    url <- paste0("https://gitlab.com/", pkg)
    badger::badge_custom("devel version", v, color, url)
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
# crandownloads_contFile <- tempfile(fileext = ".svg")
crandownloads_contFile <- "crandownload.svg"
writeLines(crandownloads_cont, con = crandownloads_contFile)
crandownloads_contFile <- normalizePath(crandownloads_contFile)
cat(paste0("&nbsp;![''](",knitr::include_graphics(crandownloads_contFile),")" ) )
}
}

## ---- echo=F, message=FALSE, warning=F, include=T------------------------
if (requireNamespace("RCurl", quietly = TRUE)  ) {
v<-tryCatch(suppressWarnings(rvcheck:::check_github_gitlab("ferroao/idiogramFISH", "gitlab")$latest_version), error=function(e) NA )
link<-tryCatch(suppressWarnings(badger::badge_custom("devel version", v, "green") ), error=function(e) NA )
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
#  # Linux with vignettes and Windows R-32bits
#  devtools::install_git(url = url,build_vignettes = TRUE, force=T)

## ---- eval=FALSE---------------------------------------------------------
#  # Windows R-64bits and Mac with vignettes
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
# load some package dataframes
data(dfOfChrSize) # chromsome data
data(dfMarkColor) # mark general data
data(dfOfMarks)   # mark position data (not cen.)
data(dfOfCenMarks)# centromeric mark data

svg("dfOfChrSize.svg",width=12,height=8 )
# png("dfOfChrSize.png", width=500, height=400)
plotIdiograms(dfChrSize=dfOfChrSize,    # data.frame of chr. size
              dfMarkColor=dfMarkColor,  # d.f of mark style
              dfMarkPos=dfOfMarks,      # df of mark positions (not centromeric)
              dfCenMarks=dfOfCenMarks,  # df of centromeric marks
              dotRoundCorr=2,           # correction of dots when non-circular
              
              chrWidth=2.5,             # width of chromosome
              chrSpacing = 2.5,         # horizontal space among chromosomes
              karHeiSpace=1.6,          # vertical size of karyotype including space
              
              indexIdTextSize=1,        # font size of chr names and indices
              markLabelSize=1,          # font size of legends
              
              rulerPos=-1.9,            # position of rulers
              ruler.tck=-0.02,          # size and orientation of ruler ticks
              rulerNumberPos=.5,        # position of numbers of rulers
              rulerNumberSize=1         # font size of rulers
              ,legend="aside"           # try this
              ,legendWidth=1            # width of legend
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
# mark position data (not cen.), if only 1 species, column OTU is optional
dfOfMarks
#centromeric mark data, if only 1 species, column OTU is optional
dfOfCenMarks

## ----example2, echo=T, results="hide", fig.width=10, fig.height=6, message=FALSE----
library(idiogramFISH)
# load some saved dataframes
data(dfChrSizeHolo, dfMarkColor, dfMarkPosHolo)

# plotIdiogramsHolo is deprecated

# svg("testing.svg",width=14,height=8 )
plotIdiograms(dfChrSize=dfChrSizeHolo, # data.frame of chr. size
                  dfMarkColor=dfMarkColor, # df of mark style
                  dfMarkPos=dfMarkPosHolo, # df of mark positions
                  addOTUName=FALSE,        # do not add OTU names
                  
                  dotRoundCorr=2.5,        # correction of roundness of dots (marks)  
                  chrWidth=2.5,            # chr. width
                  indexIdTextSize=1,       # font size of chr. name and indices
                  legend="aside" ,         # legend of marks to the right of plot
                  markLabelSize=1,         # font size of mark labels (legend)
                  
                  rulerNumberSize=1,       # font size of ruler
                  rulerPos=-.7,            # position of ruler
                  ruler.tck=-0.04,         # size and orientation of ruler ticks
                  rulerNumberPos=.9,       # position of numbers of rulers
                  
                  xlimLeftMod=1,           # modify xlim left argument of plot
                  xlimRightMod=10,         # modify xlim right argument of plot
                  ylimBotMod=.2            # modify ylim bottom argument of plot
                  ,legendHeight=.5         # height of legend labels
                  ,legendWidth = 1.2)      # width of legend labels
# dev.off()

## ----holocentrics, comment=NA--------------------------------------------
# chromsome data, if only 1 species, column OTU is optional
dfChrSizeHolo
# mark general data
dfMarkColor 
# mark position data (not cen.), if only 1 species, column OTU is optional
dfMarkPosHolo

## ---- echo=T,  comment=NA, message=FALSE---------------------------------
# chromsome data, if only 1 species, column OTU is optional
require(plyr)
dfOfChrSize$OTU  <-"Species mono"
dfChrSizeHolo$OTU<-"Species holo"
 
monoholoCS <- plyr::rbind.fill(dfOfChrSize,dfChrSizeHolo)

dfOfMarks$OTU     <-"Species mono"
dfMarkPosHolo$OTU <-"Species holo"

monoholoMarks <- plyr::rbind.fill(dfOfMarks,dfMarkPosHolo)

dfOfCenMarks$OTU <-"Species mono"

## ---- echo=T, results="hide", fig.width=10, fig.height=6, message=FALSE, dev='svg'----
library(idiogramFISH)
# load some saved dataframes

# function plotIdiogramsHolo deprecated for ver. > 1.5.1

#svg("testing.svg",width=14,height=10 )
png("monoholoCS.png", width=600, height=500)
par(mar=rep(0,4))
plotIdiograms(dfChrSize  = monoholoCS,   # data.frame of chr. size
              dfMarkColor= dfMarkColor,  # df of mark style
              dfMarkPos  = monoholoMarks,# df of mark positions
              dfCenMarks = dfOfCenMarks, # d.f. of cen. marks  
              roundness = 8,             # vertices roundness
              dotRoundCorr=1.5,          # correction of roundness of dots (marks)  
              
              addOTUName = TRUE,         # add OTU names
              OTUTextSize = 1,           # OTU name font size
              
              chrWidth=2.5,              # chr. width
              indexIdTextSize=1,         # font size of chr. name and indices
              
              legend="aside" ,           # legend of marks to the right of plot
              markLabelSize=1,           # font size of mark labels (legend)
              legendHeight=.5,           # height of legend labels
              legendWidth = 1,           # width of legend labels

              rulerNumberSize=1,         # font size of ruler
              rulerPos= -1.8,            # position of ruler
              ruler.tck=-0.02,           # size and orientation of ruler ticks
              rulerNumberPos=.9,         # position of numbers of rulers
              
              xlimLeftMod=4,             # modify xlim left argument of plot
              xlimRightMod=10,           # modify xlim right argument of plot
              ylimBotMod=-.2             # modify ylim bottom argument of plot
)
dev.off()

## ---- results="asis", comment=NA, echo=FALSE-----------------------------
cat(paste0("![](monoholoCS.png)" ) )

## ----citation, results='asis', echo=FALSE--------------------------------
# chromsome data, if only 1 species, column OTU is optional
print(citation("idiogramFISH"),bibtex=FALSE)

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
# library(badger)
if (requireNamespace("RCurl", quietly = TRUE)  ) {
donate <- "https://liberapay.com/assets/widgets/donate.svg"
donate_cont <- tryCatch(RCurl::getURLContent(donate), error=function(e) NA )
if (!is.na(donate_cont)){
donate_contFile <- "donate.svg"
# donate_contFile <- tempfile(fileext = ".svg")
writeLines(donate_cont, con = donate_contFile)
cat(paste0("[![donate](",knitr::include_graphics(donate_contFile),")](https://liberapay.com/ferroao/donate)" ) )
}
}

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", warning=FALSE, results="asis"----
# library(badger)
if (requireNamespace("RCurl", quietly = TRUE) ) {
donateweek <- "http://img.shields.io/liberapay/receives/ferroao.svg"
# donateweek_contFile <- tempfile(fileext = ".svg")
donateweek_contFile <- "donateweek.svg"
tryCatch(suppressWarnings(download.file(donateweek, donateweek_contFile) ), error=function(e) "")
if(file.exists(donateweek_contFile ) ) {
cat(paste0("[![donate](",knitr::include_graphics(donateweek_contFile),")](https://liberapay.com/ferroao/donate)" ) )
}
}

## ----include=FALSE-------------------------------------------------------
# automatically create a bib database for R packages, this is currently not used by vignette packages2.bib
knitr::write_bib(c(
  .packages(), 'bookdown', 'knitr', 'rmarkdown',"devtools","badger","pkgdown","crayon","ggtree","ggplot2","ggpubr","phytools","plyr"
), 'packages2.bib')

