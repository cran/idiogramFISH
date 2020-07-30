#' @title Chr. basic data Monocen.
#'
#' @docType data
#' @name chrbasicdatamono
NULL
#' @description dfOfChrSize: Example data for monocentrics
#' @format data.frame with columns:
#' \describe{
#'   \item{OTU}{OTU, species, optional if only one OTU (species)}
#'   \item{chrName}{name of chromosome}
#'   \item{shortArmSize}{size of short arm, micrometers }
#'   \item{longArmSize}{size of long arm, micrometers }
#'   \item{group}{chr group, optional}
#'   \item{chrNameUp}{optional name over kar.}
#'   \item{Mbp}{optional name to show size in Mbp, use only when shortArmSize is not in Mbp}
#' }
#' @seealso \code{\link{plotIdiograms}}
#' @seealso \code{\link{armRatioCI}}
#' @seealso \code{\link{asymmetry}}
#' @seealso \code{\link{markposDFs}}
#'
#' @rdname chrbasicdatamono
"dfOfChrSize"
#' @description bigdfOfChrSize: Example data for monocentrics for several
#' species, OTU
#' @rdname chrbasicdatamono
"bigdfOfChrSize"
#' @description humChr: Data for human karyotype, measured from Adler (1994)
#' @source
#' \url{http://www.pathology.washington.edu/research/cytopages/idiograms/human/}
#' @references Adler 1994. Idiogram Album.
#' \url{http://www.pathology.washington.edu/research/cytopages/idiograms/human/}
#' @rdname chrbasicdatamono
"humChr"
#' @description allChrSizeSample: Example data for monocentrics for several
#' species, OTU
#' @rdname chrbasicdatamono
"allChrSizeSample"
#' @description parentalAndHybChrSize: Example data for monocentrics for GISH
#' @rdname chrbasicdatamono
"parentalAndHybChrSize"
#' @description traspadf: Example data for Tradescantia (Rhoeo) spathacea
#' (Golczyk et al. 2005)
#' @references Golczyk H, Hasterok R, Joachimiak AJ (2005) FISH-aimed
#' karyotyping and
#' characterization of Renner complexes in permanent heterozygote Rhoeo
#' spathacea. Genome
#' 48:145–153.
#' @rdname chrbasicdatamono
"traspadf"
