idiogramFISH
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src=man/figures/logo.png align="right" width="12%" hspace="50">

# Shiny App. Idiograms with Marks and Karyotype Indices<br></br><br></br><br></br><br></br>

![<https://cran.r-project.org/web/packages/idiogramFISH/>](man/figures/cranversion.svg) [![downloads](man/figures/realdownloads.svg)](https://ferroao.gitlab.io/idiogramfishhelppages/downloads.png) [![10.5281/zenodo.3579417](man/figures/doibadge.svg)](https://doi.org/10.5281/zenodo.3579417)
 ![gitlab.com/ferroao/idiogramFISH](man/figures/gitbadge.svg)
<br></br><br></br><a href='https://ko-fi.com/X7X71PZZG' target='_blank'><img height='30' style='border:0px;height:30px;' src='man/figures/kofi1.png' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>
<!-- badges: end -->

The left side menu of the shiny app consists of:

-   data.frame inputs
    -   Load example
    -   Chr. data data.frame
    -   Mark Pos. data d.f.
    -   Marks’ Style
-   Param. & idiogram
    -   Parameters
    -   Log
    -   code

The goal of idiogramFISH is to plot idiograms of karyotypes, plasmids
and circ. chr. having a set of data.frames for chromosome data and
optionally marks’ data (`plotIdiograms` function) ([Roa and PC Telles,
2021](#ref-Roa2021)). Idiograms can also be plotted in concentric
circles. Separated chromatids can be visible when not in a circular
plot.<br> <br>Six styles of marks are available: square (squareLeft),
dots, cM (cMLeft), cenStyle, upArrow (downArrow), exProtein (inProtein);
its legend (label) can be drawn inline or to the right of karyotypes.
Three styles of centromere are available: rounded, triangular and
internal (inProtein). It is possible to calculate also chromosome and
karyotype indexes ([Romero-Zarco, 1986](#ref-Zarco1986new); [Watanabe
*et al.*, 1999](#ref-Watanabe1999)) and classify chromosome morphology
in the categories of Levan ([1964](#ref-Levan1964)), and
[Guerra](https://ferroao.gitlab.io/guerra1986/Guerra1986.pdf)
([1986](#ref-Guerra1986d)).

IdiogramFISH was written in R ([R Core Team, 2019](#ref-R-base)) and
also uses crayon ([Csárdi, 2017](#ref-R-crayon)), tidyr ([Wickham and
Henry, 2020](#ref-R-tidyr)) and dplyr packages ([Wickham *et al.*,
2019](#ref-R-dplyr)). Documentation was written with R-packages roxygen2
([Wickham *et al.*, 2018](#ref-R-roxygen2)), usethis ([Wickham and
Bryan, 2019](#ref-R-usethis)), bookdown ([Xie,
2016](#ref-bookdown2016)), knitr ([Xie, 2015](#ref-Xie2015)), pkgdown
([Wickham and Hesselberth, 2019](#ref-R-pkgdown)), Rmarkdown ([Xie *et
al.*, 2018](#ref-rmarkdown2018)), rvcheck ([Yu, 2019a](#ref-R-rvcheck)),
badger ([Yu, 2019b](#ref-R-badger)), kableExtra ([Zhu,
2019](#ref-R-kableExtra)), rmdformats ([Barnier,
2020](#ref-R-rmdformats)) and RCurl ([Temple Lang and CRAN team,
2019](#ref-R-RCurl)). For some vignette figures, packages rentrez
([Winter, 2017](#ref-rentrez2017)), plyr ([Wickham,
2011](#ref-plyr2011)), phytools ([Revell, 2012](#ref-phytools2012)),
ggtree ([Yu *et al.*, 2018](#ref-ggtree2018)), ggplot2 ([Wickham,
2016](#ref-ggplot22016)) and ggpubr ([Kassambara, 2019](#ref-R-ggpubr))
were used.

In addition, the shiny app `runBoard()` uses shiny ([Chang *et al.*,
2021](#ref-R-shiny)), shinydashboard ([Chang and Borges Ribeiro,
2018](#ref-R-shinydashboard)), rhandsontable ([Owen,
2018](#ref-R-rhandsontable)), gtools ([Warnes *et al.*,
2020](#ref-R-gtools)), rclipboard ([Bihorel, 2021](#ref-R-rclipboard))
and bib2df ([Ottolinger, 2019](#ref-R-bib2df)).

<!-- badger -->

## Releases

[News](https://gitlab.com/ferroao/idiogramFISH/blob/master/NEWS.md)

[CRAN
archive](https://cran.r-project.org/src/contrib/Archive/idiogramFISH/)

[Download
history](https://ferroao.gitlab.io/idiogramfishhelppages/downloads.png)

## Need help?

#### Manual in Bookdown style

 [![https://ferroao.gitlab.io/manualidiogramfish](man/figures/cranmanualbookdown.svg)](https://ferroao.gitlab.io/manualidiogramfish/)

#### Documentation in Pkgdown style

 [![https://ferroao.gitlab.io/idiogramFISH](man/figures/pkgdownver.svg)](https://ferroao.gitlab.io/idiogramFISH)

#### Vignettes:

Online:

 [![https://ferroao.gitlab.io/idiogramfishhelppages](man/figures/develmanualvignette.svg)](https://ferroao.gitlab.io/idiogramfishhelppages)

Launch vignettes from R for the installed version:

``` r
library(idiogramFISH)
packageVersion("idiogramFISH")
browseVignettes("idiogramFISH")
```

## Citation

To cite idiogramFISH in publications, please use:

Roa F, Telles MPC (2021) idiogramFISH: Shiny app. Idiograms with Marks
and Karyotype Indices, Universidade Federal de Goiás. Brazil. R-package.
version 2.0.2 <https://ferroao.gitlab.io/manualidiogramfish/>.
doi:<!-- breaklink -->10.5281/zenodo.3579417

To write citation to file:

``` r
sink("idiogramFISH.bib")
toBibtex(citation("idiogramFISH"))
sink()
```

## Authors

[Fernando Roa](https://ferroao.gitlab.io/curriculumpu/)  
[Mariana PC Telles](http://lattes.cnpq.br/4648436798023532)

## References

<div id="refs_normal">

<div id="ref-Guerra1986d" class="csl-entry">

Guerra M. 1986. <span class="nocase">Reviewing the chromosome
nomenclature of Levan et al.</span> *Brazilian Journal of Genetics*,
9(4): 741–743

</div>

<div id="ref-Levan1964" class="csl-entry">

Levan A, Fredga K, Sandberg AA. 1964. <span class="nocase">Nomenclature
for centromeric position on chromosomes</span> *Hereditas*, 52(2):
201–220. <https://doi.org/10.1111/j.1601-5223.1964.tb01953.x>.
<https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1601-5223.1964.tb01953.x>

</div>

<div id="ref-Zarco1986new" class="csl-entry">

Romero-Zarco C. 1986. <span class="nocase">A new method for estimating
karyotype asymmetry</span> *Taxon*, 35(3): 526–530.
<https://onlinelibrary.wiley.com/doi/abs/10.2307/1221906>

</div>

<div id="ref-Watanabe1999" class="csl-entry">

Watanabe K, Yahara T, Denda T, Kosuge K. 1999. <span
class="nocase">Chromosomal evolution in the genus Brachyscome
(Asteraceae, Astereae): statistical tests regarding correlation between
changes in karyotype and habit using phylogenetic information</span>
*Journal of Plant Research*, 112: 145–161.
<https://link.springer.com/article/10.1007/PL00013869>

</div>

</div>

## R-packages

<div id="refs_software">

<div id="ref-R-crayon" class="csl-entry">

Csárdi G. 2017. *Crayon: Colored terminal output*. R package version
1.3.4. <https://CRAN.R-project.org/package=crayon>

</div>

<div id="ref-R-ggpubr" class="csl-entry">

Kassambara A. 2019. *Ggpubr: ’ggplot2’ based publication ready plots*. R
package version 0.2.3. <https://CRAN.R-project.org/package=ggpubr>

</div>

<div id="ref-R-base" class="csl-entry">

R Core Team. 2019. *R: A language and environment for statistical
computing* R Foundation for Statistical Computing, Vienna, Austria.
<https://www.R-project.org/>

</div>

<div id="ref-phytools2012" class="csl-entry">

Revell LJ. 2012. Phytools: An r package for phylogenetic comparative
biology (and other things). *Methods in Ecology and Evolution*, 3:
217–223.
<https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2011.00169.x>

</div>

<div id="ref-Roa2021" class="csl-entry">

Roa F, PC Telles M. 2021. *<span class="nocase">idiogramFISH</span>:
Shiny app. Idiograms with marks and karyotype indices* Universidade
Federal de Goiás, UFG, Goiânia. R-package. version 2.0.0.
<https://doi.org/10.5281/zenodo.3579417>.
<https://ferroao.gitlab.io/manualidiogramfish/>

</div>

<div id="ref-plyr2011" class="csl-entry">

Wickham H. 2011. The split-apply-combine strategy for data analysis
*Journal of Statistical Software*, 40(1): 1–29.
<https://www.jstatsoft.org/article/view/v040i01>

</div>

<div id="ref-ggplot22016" class="csl-entry">

Wickham H. 2016. *ggplot2: Elegant graphics for data analysis*
Springer-Verlag New York. <https://ggplot2.tidyverse.org>

</div>

<div id="ref-R-dplyr" class="csl-entry">

Wickham H, François R, Henry L, Müller K. 2019. *Dplyr: A grammar of
data manipulation*. R package version 0.8.3.
<https://CRAN.R-project.org/package=dplyr>

</div>

<div id="ref-R-tidyr" class="csl-entry">

Wickham H, Henry L. 2020. *Tidyr: Tidy messy data*. R package version
1.0.2. <https://CRAN.R-project.org/package=tidyr>

</div>

<div id="ref-rentrez2017" class="csl-entry">

Winter DJ. 2017. <span class="nocase">rentrez</span>: An r package for
the NCBI eUtils API *The R Journal*, 9: 520–526

</div>

<div id="ref-ggtree2018" class="csl-entry">

Yu G, Lam TT-Y, Zhu H, Guan Y. 2018. Two methods for mapping and
visualizing associated data on phylogeny using ggtree. *Molecular
Biology and Evolution*, 35: 3041–3043.
<https://doi.org/10.1093/molbev/msy194>.
<https://academic.oup.com/mbe/article/35/12/3041/5142656>

</div>

</div>

## Shiny App

<div id="refs_shiny">

<div id="ref-R-rclipboard" class="csl-entry">

Bihorel S. 2021. *Rclipboard: Shiny/r wrapper for clipboard.js*. R
package version 0.1.3. <https://github.com/sbihorel/rclipboard/>

</div>

<div id="ref-R-shinydashboard" class="csl-entry">

Chang W, Borges Ribeiro B. 2018. *Shinydashboard: Create dashboards with
shiny*. R package version 0.7.1.
<http://rstudio.github.io/shinydashboard/>

</div>

<div id="ref-R-shiny" class="csl-entry">

Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J,
McPherson J, Dipert A, Borges B. 2021. *Shiny: Web application framework
for r*. R package version 1.6.0. <https://shiny.rstudio.com/>

</div>

<div id="ref-R-bib2df" class="csl-entry">

Ottolinger P. 2019. *bib2df: Parse a BibTeX file to a data frame*. R
package version 1.1.1. <https://github.com/ropensci/bib2df>

</div>

<div id="ref-R-rhandsontable" class="csl-entry">

Owen J. 2018. *Rhandsontable: Interface to the handsontable.js library*.
R package version 0.3.7. <http://jrowen.github.io/rhandsontable/>

</div>

<div id="ref-R-gtools" class="csl-entry">

Warnes GR, Bolker B, Lumley T. 2020. *Gtools: Various r programming
tools*. R package version 3.8.2. <https://github.com/r-gregmisc/gtools>

</div>

</div>

## Documentation

<div id="refs_docs">

<div id="ref-R-rmdformats" class="csl-entry">

Barnier J. 2020. *Rmdformats: HTML output formats and templates for
’rmarkdown’ documents*. R package version 0.3.7.
<https://CRAN.R-project.org/package=rmdformats>

</div>

<div id="ref-R-RCurl" class="csl-entry">

Temple Lang D, CRAN team the. 2019. *RCurl: General network
(HTTP/FTP/...) Client interface for r*. R package version 1.95-4.12.
<https://CRAN.R-project.org/package=RCurl>

</div>

<div id="ref-R-usethis" class="csl-entry">

Wickham H, Bryan J. 2019. *Usethis: Automate package and project setup*.
R package version 1.5.1. <https://CRAN.R-project.org/package=usethis>

</div>

<div id="ref-R-roxygen2" class="csl-entry">

Wickham H, Danenberg P, Eugster M. 2018. *roxygen2: In-line
documentation for r*. R package version 6.1.1.
<https://CRAN.R-project.org/package=roxygen2>

</div>

<div id="ref-R-pkgdown" class="csl-entry">

Wickham H, Hesselberth J. 2019. *Pkgdown: Make static HTML documentation
for a package*. R package version 1.4.1.
<https://CRAN.R-project.org/package=pkgdown>

</div>

<div id="ref-Xie2015" class="csl-entry">

Xie Y. 2015. *Dynamic documents with R and knitr* Chapman; Hall/CRC,
Boca Raton, Florida. ISBN 978-1498716963. <https://yihui.org/knitr/>

</div>

<div id="ref-bookdown2016" class="csl-entry">

Xie Y. 2016. *Bookdown: Authoring books and technical documents with R
markdown* Chapman; Hall/CRC, Boca Raton, Florida. ISBN 978-1138700109.
<https://github.com/rstudio/bookdown>

</div>

<div id="ref-rmarkdown2018" class="csl-entry">

Xie Y, Allaire JJ, Grolemund G. 2018. *R markdown: The definitive guide*
Chapman; Hall/CRC, Boca Raton, Florida. ISBN 9781138359338.
<https://bookdown.org/yihui/rmarkdown>

</div>

<div id="ref-R-badger" class="csl-entry">

Yu G. 2019b. *Badger: Badge for r package*. R package version 0.0.6.
<https://CRAN.R-project.org/package=badger>

</div>

<div id="ref-R-rvcheck" class="csl-entry">

Yu G. 2019a. *Rvcheck: R/package version check*. R package version
0.1.6. <https://CRAN.R-project.org/package=rvcheck>

</div>

<div id="ref-R-kableExtra" class="csl-entry">

Zhu H. 2019. *kableExtra: Construct complex table with ’kable’ and pipe
syntax*. R package version 1.1.0.
<https://CRAN.R-project.org/package=kableExtra>

</div>

</div>
