## ----setup, include=FALSE-----------------------------------------------------
#Create myheader.html
fileConn <- file("myheader.html")
writeLines('<script src="https://kit.fontawesome.com/af0a13599b.js" crossorigin="anonymous"></script>', fileConn)
close(fileConn)

## ---- echo=F,  results="asis"-------------------------------------------------
img1_path <- "../man/figures/logo.png"
if(file.exists(img1_path)) {
cat(paste0("<img src=",img1_path," class=\"right\" width=\"20%\">") )
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
  cat(paste0("![https://cran.r-project.org/web/packages/idiogramFISH](",knitr::include_graphics(cranversion_contFile),")" ) )
  } # cran version

# crandownloads<-"https://cranlogs.r-pkg.org/badges/grand-total/idiogramFISH?color=orange"
# crandownloads_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(crandownloads) ), error=function(e) NA )

newdownlink<-tryCatch(suppressWarnings(badger::badge_custom("CRAN",  "downloads","green") ), error=function(e) NA )
if(!is.na(newdownlink)){
svgnewdownlink<-gsub("\\[|\\]|!|\\(|\\)","", newdownlink)
realdo_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svgnewdownlink) ), error=function(e) NA )
}

  # if (!is.na(crandownloads_cont)){
  # crandownloads_contFile <- "../man/figures/crandownload.svg"
  # writeLines(crandownloads_cont, con = crandownloads_contFile)
  # crandownloads_contFile <- normalizePath(crandownloads_contFile)
  # 
  # cat(paste0("&nbsp;![''](",knitr::include_graphics(crandownloads_contFile),")" ) )
  # } # cran down
    if (!is.na(realdo_cont)){
      realdo_contFile <- "../man/figures/realdownloads.svg"
      writeLines(realdo_cont, con = realdo_contFile)
                            # message                                              # link
      cat(paste0("&nbsp;[![downloads](",knitr::include_graphics(realdo_contFile),")](https://ferroao.gitlab.io/idiogramfishhelppages/downloads.png)" ) )
    }

doibadge <- "https://zenodo.org/badge/DOI/10.5281/zenodo.3579417.svg"
doibadge_cont      <- tryCatch(suppressWarnings(RCurl::getURLContent(doibadge)      ), error=function(e) NA )

  if (!is.na(doibadge_cont)){
  doibadge_contFile <- "../man/figures/doibadge.svg"
  writeLines(doibadge_cont, con = doibadge_contFile)
  doibadge_contFile <- normalizePath(doibadge_contFile)
  
  cat(paste0("&nbsp;[![10.5281/zenodo.3579417](",knitr::include_graphics(doibadge_contFile)         ,")](https://doi.org/10.5281/zenodo.3579417)" ) )
  } # doi

} # rcurl

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
    }
  }
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
    cat(paste0("![devel version](",knitr::include_graphics(gitbadge_contFile),")" ) )
    cat(paste0("&nbsp;[![NEWS](",knitr::include_graphics(news_cont_contFile),")](https://gitlab.com/ferroao/idiogramFISH/blob/master/NEWS.md)" ) )
    }
  }
}

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
#cran version
v <- "README" #tryCatch(suppressWarnings(rvcheck::check_cran("idiogramFISH")$latest_version), error=function(e) NA )

pkg<-"idiogramFISH"
link<-tryCatch(suppressWarnings(badger::badge_custom("gitlab", paste(v), "orange","?logo=gitlab") ), error=function(e) NA )
  if(!is.na(link)){
  svglink<-gsub("\\[|\\]|!|\\(|\\)","", link)
  manual_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svglink) ), error=function(e) NA )
    if (!is.na(manual_cont)){
    README_contFile <- "../man/figures/readme.svg"
    writeLines(manual_cont, con = README_contFile)
    # cat(paste0("&nbsp;[![bookdown](",knitr::include_graphics(README_contFile),")](https://gitlab.com/ferroao/idiogramFISH)" ) )
    }
  }
} # rcurl

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
#cran version
v<-tryCatch(suppressWarnings(rvcheck::check_cran("idiogramFISH")$latest_version), error=function(e) NA )

pkg<-"idiogramFISH"
link<-tryCatch(suppressWarnings(badger::badge_custom("bookdown", paste(pkg,v), "orange") ), error=function(e) NA )
  if(!is.na(link)){
  svglink<-gsub("\\[|\\]|!|\\(|\\)","", link)
  manual_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svglink) ), error=function(e) NA )
    if (!is.na(manual_cont)){
    bookdownmanual_contFile <- "../man/figures/cranmanualbookdown.svg"
    writeLines(manual_cont, con = bookdownmanual_contFile)
    # cat(paste0("&nbsp;[![bookdown](",knitr::include_graphics(bookdownmanual_contFile),")](https://ferroao.gitlab.io/manualidiogramfish/)" ) )
    }
  }
} # rcurl

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
# version of manual
v<-sub("Version: ","",readLines("../DESCRIPTION")[3])
# v<-tryCatch(suppressWarnings(rvcheck:::check_github_gitlab("ferroao/idiogramFISH", "gitlab")$latest_version), error=function(e) NA )
#cran version
# v<-tryCatch(suppressWarnings(rvcheck::check_cran("idiogramFISH")$latest_version), error=function(e) NA )
pkg<-"idiogramFISH"
link<-tryCatch(suppressWarnings(badger::badge_custom("pkgdown", paste(pkg,v), "cornflowerblue") ), error=function(e) NA )
  if(!is.na(link)){
  svglink<-gsub("\\[|\\]|!|\\(|\\)","", link)
  develmanual_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svglink) ), error=function(e) NA )
    if (!is.na(develmanual_cont)){
    pkgdownmanual_contFile <- "../man/figures/develmanualpkgdown.svg"
    writeLines(develmanual_cont, con = pkgdownmanual_contFile)
    cat(paste0("[![https://gitlab.com/ferroao/idiogramFISH](",knitr::include_graphics(README_contFile),")](https://gitlab.com/ferroao/idiogramFISH)" ) )
    cat(paste0("&nbsp;[![https://ferroao.gitlab.io/manualidiogramfish](",knitr::include_graphics(bookdownmanual_contFile),")](https://ferroao.gitlab.io/manualidiogramfish/)" ) )
    cat(paste0("&nbsp;[![https://ferroao.gitlab.io/idiogramFISH](",knitr::include_graphics(pkgdownmanual_contFile),")](https://ferroao.gitlab.io/idiogramFISH)" ) )
    }
  }
} # rcurl

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
if (requireNamespace("RCurl", quietly = TRUE)  ) {
# version of manual
# v<-tryCatch(suppressWarnings(rvcheck:::check_github_gitlab("ferroao/idiogramFISH", "gitlab")$latest_version), error=function(e) NA )
v<-sub("Version: ","",readLines("../DESCRIPTION")[3])
#cran version
# v<-tryCatch(suppressWarnings(rvcheck::check_cran("idiogramFISH")$latest_version), error=function(e) NA )

pkg<-"idiogramFISH"
link<-tryCatch(suppressWarnings(badger::badge_custom("vignettes", paste(pkg,v), "cornflowerblue") ), error=function(e) NA )
  if(!is.na(link)){
  svglink<-gsub("\\[|\\]|!|\\(|\\)","", link)
  develmanual_cont <- tryCatch(suppressWarnings(RCurl::getURLContent(svglink) ), error=function(e) NA )
    if (!is.na(develmanual_cont)){
    vignettemanual_contFile <- "../man/figures/develmanualvignette.svg"
    writeLines(develmanual_cont, con = vignettemanual_contFile)
        cat(paste0("[![bookdown](",knitr::include_graphics(bookdownmanual_contFile),")](https://ferroao.gitlab.io/manualidiogramfish/)" ) )
        cat(paste0("&nbsp;[![pkgdown](",knitr::include_graphics(pkgdownmanual_contFile),")](https://ferroao.gitlab.io/idiogramFISH)" ) )
        cat(paste0("&nbsp;[![vignettes](",knitr::include_graphics(vignettemanual_contFile),")](https://ferroao.gitlab.io/idiogramfishhelppages)" ) )
        
    }
  }
} # rcurl

## ----citation, echo=FALSE, comment=NA,results='asis'--------------------------
print(citation("idiogramFISH"),bibtex=FALSE )

## ---- echo=TRUE, results=FALSE, eval=FALSE------------------------------------
#  sink("idiogramFISH.bib")
#  toBibtex(citation("idiogramFISH"))
#  sink()

## ---- echo=F, message=FALSE, fig.show = "hold", fig.align = "default", results="asis"----
cat("<a href='https://ko-fi.com/X7X71PZZG' target='_blank'><img height='36' style='border:0px;height:36px;' src='../man/figures/kofi1.png' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>")
# library(badger)
# if (requireNamespace("RCurl", quietly = TRUE)  ) {
# donate <- "https://liberapay.com/assets/widgets/donate.svg"
# donate_contFile <- "../man/figures/donate2.svg"
# tryCatch(suppressWarnings(download.file(donate, donate_contFile) ), error=function(e) "")
# if(file.exists(donate_contFile ) ) {
# cat(paste0("[![donate](",knitr::include_graphics(donate_contFile),")](https://liberapay.com/ferroao/donate)" ) )
# }
# }
# library(badger)
# if (requireNamespace("RCurl", quietly = TRUE) ) {
# donateweek <- "http://img.shields.io/liberapay/receives/ferroao.svg"
# donateweek_contFile <- "../man/figures/donateweek.svg"
# tryCatch(suppressWarnings(download.file(donateweek, donateweek_contFile) ), error=function(e) "")
# if(file.exists(donateweek_contFile ) ) {
# cat(paste0("[![donate](",knitr::include_graphics(donateweek_contFile),")](https://liberapay.com/ferroao/donate)" ) )
# }
# }

## ----include=FALSE,eval=FALSE-------------------------------------------------
#  # automatically create a bib database for R packages, this is currently not used by vignette refs/packages2.bib
#  knitr::write_bib(c(
#    .packages(), 'bookdown', 'knitr', 'rmarkdown',"devtools","pkgdown","crayon","ggtree","ggplot2","ggpubr","phytools","plyr","dplyr","tidyr","rentrez"
#  ), 'refs/packages2.bib')

