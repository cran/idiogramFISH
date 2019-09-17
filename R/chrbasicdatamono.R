#' @title Chr. basic data
#'
#' @docType data
#' @name chrbasicdatamono
NULL
#' @description dfOfChrSize: Example data for monocentrics
#' @format data.frame with cols:
#' \describe{
#'   \item{OTU}{OTU, species, optional if only one OTU (species)}
#'   \item{chrName}{name of chromosome}
#'   \item{shortArmSize}{size of short arm, micrometers }
#'   \item{longArmSize}{size of long arm, micrometers }
#'   \item{group}{chr group, optional}
#' }
#' @seealso \code{\link{plotIdiograms}}
#' @seealso \code{\link{armRatioCI}}
#' @seealso \code{\link{asymmetry}}
#' @seealso \code{\link{markpos}}
#' @seealso \code{\link{cenmarkdata}}
#' @rdname chrbasicdatamono
"dfOfChrSize"
#' @description bigdfOfChrSize: Example data for monocentrics for several species, OTU
#' @rdname chrbasicdatamono
"bigdfOfChrSize"
#' @description humChr: Example data for human karyotype (Adler 1994)
#' @source \url{http://www.pathology.washington.edu/research/cytopages/idiograms/human/}
#' @references Adler 1994. Idiogram Album. \url{http://www.pathology.washington.edu/research/cytopages/idiograms/human/}
#' @rdname chrbasicdatamono
"humChr"

