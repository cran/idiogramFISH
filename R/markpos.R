#' @title Mark Positional data - monocentrics
#' @description When several OTUs, some can be monocen. and others holocen. Marks distance for monocen. are measured from cen. and for
#' holocen. from top or bottom depending on \code{param} \code{origin}. See vignettes.
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
#'   \item{chrRegion}{use p for short arm, q for long arm, and cen for centromeric}
#'   \item{markDistCen}{distance of mark to centromere (not for cen)}
#'   \item{markSize}{size of mark (not for cen)}
#' }
#' @seealso \code{\link{markdataholo}}
#' @seealso \code{\link{plotIdiograms}}
#' @seealso \code{\link{chrbasicdatamono}}
#' @seealso \code{\link{dfMarkColor}}
#'
#' @rdname markpos
"bigdfOfMarks"
#' @description dfOfMarks: Example data for marks' position
#' @rdname markpos
"dfOfMarks"
#' @description dfOfMarks2: Marks' position including cen. marks
#' @rdname markpos
"dfOfMarks2"
#' @description humMarkPos: human karyotype marks' position
#' @source \href{http://www.pathology.washington.edu/research/cytopages/idiograms/human/}{Washington U}
#' @references Adler 1994. Idiogram Album. URL: \href{http://www.pathology.washington.edu/research/cytopages/idiograms/human/}{Washington U.}
#' @rdname markpos
"humMarkPos"
#' @description allMarksSample: Example data for marks' position
#' @rdname markpos
"allMarksSample"
#' @description dfAlloParentMarks: Example data for mark position of GISH of monocen.
#' @rdname markpos
"dfAlloParentMarks"
