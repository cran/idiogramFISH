#' @title Chr. basic data Holo.
#'
#' @docType data
#' @name chrbasicdataHolo
NULL
#' @description dfChrSizeHolo: Example data for holocentrics for 1 species
#'
#' @format data.frame with columns:
#' \describe{
#'   \item{OTU}{grouping OTU (species), optional if only one OTU}
#'   \item{chrName}{name of chromosome}
#'   \item{chrSize}{size of chromosome, micrometers or Mb}
#'   \item{group}{chromosome group, optional}
#'   \item{chrNameUp}{optional name over kar.}
#'   \item{Mbp}{optional name to show size in Mbp, use only when chrSize is not in Mbp}
#' }
#' @seealso \code{\link{asymmetryA2}}
#' @seealso \code{\link{plotIdiograms}}
#' @seealso \code{\link{markdataholo}}
#'
#' @rdname chrbasicdataHolo
"dfChrSizeHolo"
#' @description bigdfChrSizeHolo: Example data for holocentrics for several
#' species, OTU
#' @rdname chrbasicdataHolo
"bigdfChrSizeHolo"
#' @description parentalAndHybHoloChrSize: Example data for holocentrics for
#' several species, OTU
#' @rdname chrbasicdataHolo
"parentalAndHybHoloChrSize"
#' @description bigdfOfChrSize3Mb: Example data in Mb without chr. arms for
#' three species, OTU
#' @rdname chrbasicdataHolo
"bigdfOfChrSize3Mb"
