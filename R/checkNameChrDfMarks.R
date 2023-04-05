#' Function to check names of OTUs (species) among data.frames
#'
#' This function reads two data.frames the one with chromosome sizes, and one df
#' with data of marks (sites) both have to have the column \code{chrName} and if
#' several species, \code{OTU}. The functions returns a filtered list with 1.
#' data.frame of
#' chrSizes,
#' 2. data.frame of marks, 3. mark names 4. max mark size.
#'
#' @param listChrSize list of data.frames, with columns: OTU (optional),
#' chrName, for chr.
#'   with cen.: shortArmSize, longArmSize, for holoc.: chrSize
#' @param listOfdfMarks list of data.frames of marks (sites): OTU (opt /
#' mandat. if in
#'   dfChrSize), chrName markName markSize, for chr. with cen.:  chrRegion (p,q)
#'   markDistCen, for holoc: markPos
#'
#' @keywords internal
#'
#' @return list
#'
#'
checkNameChrDfMarks <- function(listChrSize, listOfdfMarks) {
  pars <- as.character(match.call()[-1])
  message(paste(("\nComparing chromosome names among data.frames:\n")))

  markNames <- character()
  markSize <- numeric()

  for (s in seq_along(listChrSize)) {
    divisor2 <- as.numeric(attr(listChrSize[[s]], "divisor"))

    name <- names(listChrSize)[[s]]

    if (length(listChrSize[[s]]$chrName) != length(unique(listChrSize[[s]]$chrName))) {
      message(crayon::yellow(
        paste("\nWARNING Chromosome Names in data.frame", name, "duplicated - Error when d.f. of marks present")
      ))
    }

    if (length(which(names(listOfdfMarks) == name)) != 0) {
      message(paste(c(
        "\nComparing OTU named: ", name, "of main data.frame with correspondent data.frame ",
        pars[2],
        " "
      ),
      sep = " ",
      collapse = " "
      ))


      if (length(
        setdiff(
          listOfdfMarks[[which(names(listOfdfMarks) == name)]]$chrName,
          listChrSize[[which(names(listChrSize) == name)]]$chrName
        )
      ) > 0) {
        message(crayon::red(paste(c(
          "\nERROR Divergences Chromosome Name:",
          setdiff(
            listOfdfMarks[[which(names(listOfdfMarks) == name)]]$chrName,
            listChrSize[[which(names(listChrSize) == name)]]$chrName
          )
        ), sep = " ", collapse = " ")))

        message(crayon::red(paste(c(
          "\nERROR: There are chromosome Name(s) -see above - of OTU ",
          name,
          "missing in the data.frame of chromosome sizes\n this OTU's marks will be removed"
        ),
        sep = " ", collapse = " "
        )))

        listOfdfMarks[which(names(listOfdfMarks) == name)] <- NULL
      } else {
        message(paste("\n No divergence"))

        if (inherits(listOfdfMarks[[which(names(listOfdfMarks) == name)]], "data.frame")) {
          markNames <- c(markNames, unique(listOfdfMarks[[which(names(listOfdfMarks) == name)]]$markName))

          if (length(listOfdfMarks[[which(names(listOfdfMarks) == name)]]$markSizeOrig) > 0) {
            markSize <- c(
              markSize,
              tryCatch(max(listOfdfMarks[[which(names(listOfdfMarks) == name)]]$markSizeOrig / divisor2, na.rm = TRUE),
                error = function(e) {
                  NA
                }, warning = function(w) {
                  NA
                }
              )
            )
          }
        }
      }
    }
  }

  resultList <- list(listChrSize, listOfdfMarks, markNames, markSize)
  return(resultList)
  message(paste("\nChecks done for ", pars[2]))
}
