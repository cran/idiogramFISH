#' @title Mark Positional data - Holocen.
#' @description When several OTUs, some can be monocen. and others holocen. 
#' Marks distance for 
#' monocen. are measured from cen. and for
#' holocen. from top or bottom depending on \code{param} \code{origin}. See 
#' vignettes.
#'
#' @docType data
#' @name markdataholo
NULL
#' @description bigdfMarkPosHolo: Example data for mark position of 
#' holocentrics with column OTU
#'
#' @format data.frame with columns:
#' \describe{
#'   \item{OTU}{OTU, species, optional}
#'   \item{chrName}{name of chromosome}
#'   \item{markName}{name of mark}
#'   \item{markPos}{position from bottom or top (see parameter \code{origin} 
#' in plotIdiograms)}
#'   \item{markSize}{size of mark in micrometers or Mb}
#' }
#' @seealso \code{\link{markposDFs}}
#' @seealso \code{\link{plotIdiograms}}
#' @seealso \code{\link{chrbasicdataHolo}}
#'
#' @rdname markdataholo
"bigdfMarkPosHolo"
#' @description dfMarkPosHolo: Example data for mark position of holocentrics
#'
#' @rdname markdataholo
"dfMarkPosHolo"
#' @description dfAlloParentMarksHolo: Example data for mark position of GISH
#' @rdname markdataholo
"dfAlloParentMarksHolo"
#' @description bigdfOfMarks3Mb: Example data for mark position in Mb
#' @rdname markdataholo
"bigdfOfMarks3Mb"




