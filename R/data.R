#' Example data for holocentrics for several species
#'
#' A dataset containing chromosome basic data
#'
#' @format A data frame:
#' \describe{
#'   \item{OTU}{OTU, species}
#'   \item{chrName}{name of chromosome}
#'   \item{chrSize}{size of chromosome, micrometers or Mb}
#' }
"bigdfChrSizeHolo"
#' Example data for holocentrics for 1 species
#'
#' A dataset containing chromosome basic data
#'
#' @format A data frame:
#' \describe{
#'   \item{chrName}{name of chromosome}
#'   \item{chrSize}{size of chromosome, micrometers or Mb}
#' }
"dfChrSizeHolo"
#' Example data for mark position of holocentrics with column OTU
#'
#' A dataset containing mark position
#'
#' @format A data frame:
#' \describe{
#'   \item{OTU}{OTU, species}
#'   \item{chrName}{name of chromosome}
#'   \item{markName}{name of mark}
#'   \item{markPos}{position from bottom or top (see parameter origin)}
#'   \item{markSize}{size of mark in micrometers or Mb}
#' }
"bigdfMarkPosHolo"

#' Example data for mark position of holocentrics
#'
#' A dataset containing mark position
#'
#' @format A data frame:
#' \describe{
#'   \item{chrName}{name of chromosome}
#'   \item{markName}{name of mark}
#'   \item{markPos}{position from bottom or top (see parameter origin)}
#'   \item{markSize}{size of mark in micrometers or Mb}
#' }
"dfMarkPosHolo"

#' Example data for centromeric marks df with column OTU
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{OTU}{OTU, species}
#'   \item{chrName}{name of chromosome}
#'   \item{markName}{name of mark (site)}
#' }
"bigdfDataCen"
#' Example data for monocentrics for several species
#'
#' A dataset containing chromosome basic data
#'
#' @format A data frame:
#' \describe{
#'   \item{OTU}{OTU, species}
#'   \item{chrName}{name of chromosome}
#'   \item{shortArmSize}{size of short arm, micrometers }
#'   \item{longArmSize}{size of long arm, micrometers }
#' }
"bigdfOfChrSize"
#' Example data for monocentrics for several species
#'
#' A dataset containing chromosome basic data
#'
#' @format A data frame:
#' \describe{
#'   \item{chrName}{name of chromosome}
#'   \item{shortArmSize}{size of short arm, micrometers }
#'   \item{longArmSize}{size of long arm, micrometers }
#' }
"dfOfChrSize"
#' Example data for mark position of monocentrics with column OTU
#'
#' A dataset containing mark data
#'
#' @format A data frame:
#' \describe{
#'   \item{OTU}{OTU, species}
#'   \item{chrName}{name of chromosome}
#'   \item{markName}{name of mark}
#'   \item{markArm}{use p for short and q for long}
#'   \item{markDistCen}{distance of mark to centromere}
#'   \item{markSize}{size of mark in micrometers or Mb}
#' }
"bigdfOfMarks"
#' Example data for mark position of monocentrics
#'
#' A dataset containing mark data
#'
#' @format A data frame:
#' \describe{
#'   \item{chrName}{name of chromosome}
#'   \item{markName}{name of mark}
#'   \item{markArm}{use p for short and q for long}
#'   \item{markDistCen}{distance of mark to centromere}
#'   \item{markSize}{size of mark in micrometers or Mb}
#' }
"dfOfMarks"

#' Example data for centromeric marks of monocentrics
#'
#' A dataset containing centromeric mark
#'
#' @format A data frame:
#' \describe{
#'   \item{chrName}{name of chromosome}
#'   \item{markName}{name of mark}
#' }
"dfOfCenMarks"

#' Example General data for marks
#'
#' A dataset containing mark general data, not position
#'
#' @format A data frame: \describe{ \item{markName}{name of mark}
#'   \item{markColor}{use R colors} \item{style}{character, use square or dots}
#'   }
"dfMarkColor"

