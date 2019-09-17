## ---- echo=F,  results="asis"--------------------------------------------
img1_path <- "../man/figures/logo.png"
if(file.exists(img1_path)) {
cat(paste0("<img src=",img1_path," class=\"right\" width=\"20%\">") )
}

## ---- include = FALSE----------------------------------------------------
 badge_devel_gitlab<-function(pkg, color){
    v <- rvcheck:::check_github_gitlab(pkg, "gitlab")$latest_version
    url <- paste0("https://gitlab.com/", pkg)
    badger::badge_custom("devel version", v, color, url)
}

## ---- echo=F, results="asis"---------------------------------------------
library(badger)
cat(paste(badger::badge_cran_release("idiogramFISH", "orange"),"&nbsp;" ))
cat(paste(badger::badge_cran_download("idiogramFISH", type="grand-total", color="orange") ))

## ---- echo=F, results="asis", message="hide", warning=F, include=F-------
gitbadge<- badge_devel_gitlab("ferroao/idiogramFISH", color="green") 

## ---- echo=F, results="asis", message="hide", warning=F, include=T-------
cat(gitbadge)

## ----example, echo=T, results="hide", fig.width=10, fig.height=6, message=FALSE----
# fig.width=10, fig.height=6

library(idiogramFISH)
# load some package dataframes
data(dfOfChrSize) # chromsome data
data(dfMarkColor) # mark general data
data(dfOfMarks)   # mark position data (not cen.)
data(dfOfCenMarks)# centromeric mark data

plotIdiograms(dfChrSize=dfOfChrSize,    # data.frame of chr. size
              dfMarkColor=dfMarkColor,  # d.f of mark style
              dfMarkPos=dfOfMarks,      # df of mark positions (not centromeric)
              dfCenMarks=dfOfCenMarks,  # df of centromeric marks
              dotRoundCorr=2,           # correction of dots when non-circular
              
              chrWidth=2.5,             # width of chromosome
              chrSpacing = 2.5,         # horizontal space among chromosomes
              karSpacing=1.6,           # vertical size of karyotype including space
              
              indexIdTextSize=1,        # font size of chr names and indices
              markLabelSize=1,          # font size of legends
              
              rulerPos=-1.9,            # position of rulers
              ruler.tck=-0.02,          # size and orientation of ruler ticks
              rulerNumberPos=.5,        # position of numbers of rulers
              rulerNumberSize=1         # font size of rulers
)

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

plotIdiogramsHolo(dfChrSize=dfChrSizeHolo, # data.frame of chr. size
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
                  ylimBotMod=.2)           # modify ylim bottom argument of plot

## ----holocentrics, comment=NA--------------------------------------------
# chromsome data, if only 1 species, column OTU is optional
dfChrSizeHolo
# mark general data
dfMarkColor 
# mark position data (not cen.), if only 1 species, column OTU is optional
dfMarkPosHolo

## ----citation, results='asis', echo=FALSE--------------------------------
# chromsome data, if only 1 species, column OTU is optional
print(citation("idiogramFISH"),bibtex=FALSE)

## ----include=FALSE-------------------------------------------------------
# automatically create a bib database for R packages
knitr::write_bib(c(
  .packages(), 'bookdown', 'knitr', 'rmarkdown',"devtools","badger","pkgdown"
), 'packages.bib')

