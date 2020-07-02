#' @title Mark characteristics
#' @description style column does not apply to cen. marks, only color.
#'
#' @docType data
#' @name dfMarkStyle
NULL
#' @description dfMarkColor: Example General data for marks NOT position
#'
#' @format dfMarkColor a data.frame with columns:
#' \describe{
#'   \item{markName}{name of mark}
#'   \item{markColor}{use R colors}
#'   \item{style}{character, use square or dots, optional}
#'   \item{protruding}{numeric, modifies aspect of cM/cMLeft marks, see 
#' parameter 
#' \code{protruding} in \code{\link{plotIdiograms}}, optional}
#' }
#' @seealso \code{\link{plotIdiograms}}
#' @seealso \code{\link{markposDFs}}
#' @seealso \code{\link{markdataholo}}
#'
#' @rdname dfMarkStyle
"dfMarkColor"
#' @description humMarkColor: human bands' characteristics, from Adler (1994)
#' @source 
#' \url{http://www.pathology.washington.edu/research/cytopages/idiograms/human/}
#' @references Adler 1994. Idiogram Album. URL: 
#' \url{http://www.pathology.washington.edu/research/cytopages/idiograms/human/}
#' @rdname dfMarkStyle
"humMarkColor"
#' @description mydfMaColor: mark characteristics used in vignette of phylogeny
#' @rdname dfMarkStyle
"mydfMaColor"
