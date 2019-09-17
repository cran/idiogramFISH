#' @title Mark Positional data - monocentrics
#'
#' @docType data
#' @name markpos
NULL
#' @description bigdfOfMarks: Example data for mark position with column OTU
#'
#' @format bigdfOfMarks a data.frame with cols:
#' \describe{
#'   \item{OTU}{OTU, species, mandatory if in dfChrSize}
#'   \item{chrName}{name of chromosome}
#'   \item{markName}{name of mark}
#'   \item{markArm}{use p for short and q for long}
#'   \item{markDistCen}{distance of mark to centromere}
#'   \item{markSize}{size of mark}
#' }
#' @seealso \code{\link{plotIdiograms}}
#' @seealso \code{\link{chrbasicdatamono}}
#' @seealso \code{\link{dfMarkColor}}
#'
#' @rdname markpos
"bigdfOfMarks"
#' @description dfOfMarks: Example data for mark position
#' @rdname markpos
"dfOfMarks"
#' @description humMarkPos: human karyotype mark position
#' @source \href{http://www.pathology.washington.edu/research/cytopages/idiograms/human/}{Washington U}
#' @references Adler 1994. Idiogram Album. URL: \href{http://www.pathology.washington.edu/research/cytopages/idiograms/human/}{Washington U.}
#' @rdname markpos
"humMarkPos"

