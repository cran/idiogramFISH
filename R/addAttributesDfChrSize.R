#' addAttributesDfChrSize

#' @description returns all cols. as character
#' @keywords internal
#' @return list of data.frames
#' @import crayon

addAttributesDfChrSize <- function(listOfdfChromSize, threshold, specialOTUNames, # nolint: cyclocomp_linter
                                   centromereSize, MbThreshold, cenFactor,
                                   chrWidth, specialChrWidth, squareness,
                                   modifyChr = TRUE,
                                   mymessage = TRUE) {
  for (i in seq_along(listOfdfChromSize)) {

    #
    # remove columns without info. per karyotype
    #

    listOfdfChromSize[[i]][listOfdfChromSize[[i]] == ""] <- NA
    listOfdfChromSize[[i]] <- listOfdfChromSize[[i]][, !apply(is.na(listOfdfChromSize[[i]]), 2, all)]

    # Does the data.frame have short and long info?
    if (mymessage) {
      message("\nChecking columns from dfChrSize\n")
    }
    #################################################################################################
    #
    #   let see if it is monocen
    #

    if (length(setdiff(
      c("chrName", "shortArmSize", "longArmSize"),
      colnames(listOfdfChromSize[[i]])
    )) == 0) {
      if (mymessage) {
        message("\nChecking mandatory columns from dfChrSize for chr. with cen.: \n
          chrName, shortArmSize,longArmSize,\n (column OTU  is necessary if more than one species)\n")
        message(crayon::green(paste("\nOTU ", names(listOfdfChromSize)[[i]],
          "has all columns with info to have monocen. If not, you have to clean your data")))
      }
      attr(listOfdfChromSize[[i]], "cenType") <- "monocen"
      attr(listOfdfChromSize[[i]], "indexStatus") <- "missing"

      #
      #   if larger than 100 000 reduce
      #

      myn <- max(pmax(listOfdfChromSize[[i]]$longArmSize + listOfdfChromSize[[i]]$shortArmSize))
      exp <- floor(log(myn, base = 10))
      divisor2 <- 10^exp
      threshold2 <- threshold / 10

      divisor2 <- ifelse((myn / divisor2) < threshold2, divisor2 <- 10^(exp - 1), divisor2)

      attr(listOfdfChromSize[[i]], "divisor") <- divisor2

      listOfdfChromSize[[i]]$longArmSize <- listOfdfChromSize[[i]]$longArmSize / divisor2
      listOfdfChromSize[[i]]$shortArmSize <- listOfdfChromSize[[i]]$shortArmSize / divisor2

      if (divisor2 >= MbThreshold) {
        attr(listOfdfChromSize[[i]], "units") <- "Mb"
        attr(listOfdfChromSize[[i]], "ytitle") <- "Mb"
      } else {
        attr(listOfdfChromSize[[i]], "units") <- "notMb"
        attr(listOfdfChromSize[[i]], "ytitle") <- "notMb"
      }
      if (attr(listOfdfChromSize[i], "name") %in% specialOTUNames) {
        attr(listOfdfChromSize[[i]], "ytitle") <- "cM"
      }

      if (!is.na(centromereSize)) {
        centromereSize2 <- centromereSize
      } else {
        centromereSize2 <- divisor2
      }


      if (attr(listOfdfChromSize[[i]], "ytitle") == "cM") {
        chrWidth2 <- specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }

      r2 <- chrWidth2 / (squareness * 2)

      attr(listOfdfChromSize[[i]], "r2") <- r2

      attr(listOfdfChromSize[[i]], "centromere") <- (centromereSize2 / divisor2) * cenFactor
      listOfdfChromSize[[i]]$centromereSize <- (centromereSize2 / divisor2) * cenFactor

      #
      # correcting for ruler collapseCen
      #

      if (modifyChr) {
        if (centromereSize2 > 0) {
          listOfdfChromSize[[i]]$shortArmSize <- listOfdfChromSize[[i]]$shortArmSize - ((centromereSize2 / divisor2) * cenFactor) / 2
          listOfdfChromSize[[i]]$longArmSize <- listOfdfChromSize[[i]]$longArmSize - ((centromereSize2 / divisor2) * cenFactor) / 2
        }
        listOfdfChromSize[[i]]$shortArmSize <- ifelse(listOfdfChromSize[[i]]$shortArmSize < 0, 0, listOfdfChromSize[[i]]$shortArmSize)
        listOfdfChromSize[[i]]$longArmSize <- ifelse(listOfdfChromSize[[i]]$longArmSize < 0, 0, listOfdfChromSize[[i]]$longArmSize)
      }
    } else if (length(setdiff(
      c("chrName", "chrSize"),
      colnames(listOfdfChromSize[[i]])
    )) == 0) {
      # end monocen success

      ############################################################################################## 3
      #   let see if it is holocen
      #
      if (mymessage) {
        message("\nChecking mandatory columns from dfChrSize for chr. without cen.: \n
          chrName, chrSize,\n (column OTU  is necessary if more than one species)\n")
        message(crayon::green(paste(c("\nOTU ", names(listOfdfChromSize)[[i]],
          " has all columns with info to have holocen. If not, you have to clean your data"))))
      }
      attr(listOfdfChromSize[[i]], "cenType") <- "holocen"
      attr(listOfdfChromSize[[i]], "indexStatus") <- "missing"

      #
      #   if larger than 100 000 reduce
      #

      myn <- max(listOfdfChromSize[[i]]$chrSize)
      exp <- floor(log(myn, base = 10))

      divisor2 <- 10^exp
      threshold2 <- threshold / 10

      ifelse((myn / divisor2) < threshold2, divisor2 <- 10^(exp - 1), divisor2)

      attr(listOfdfChromSize[[i]], "divisor") <- divisor2
      listOfdfChromSize[[i]]$chrSize <- listOfdfChromSize[[i]]$chrSize / divisor2

      if (divisor2 >= MbThreshold) {
        attr(listOfdfChromSize[[i]], "units") <- "Mb"
        attr(listOfdfChromSize[[i]], "ytitle") <- "Mb"
      } else {
        attr(listOfdfChromSize[[i]], "units") <- "notMb"
        attr(listOfdfChromSize[[i]], "ytitle") <- "notMb"
      }
      if (attr(listOfdfChromSize[i], "name") %in% specialOTUNames) {
        attr(listOfdfChromSize[[i]], "ytitle") <- "cM"
      }

      if (!is.na(centromereSize)) {
        centromereSize2 <- centromereSize
      } else {
        centromereSize2 <- divisor2
      }


      if (attr(listOfdfChromSize[[i]], "ytitle") == "cM") {
        chrWidth2 <- specialChrWidth
      } else {
        chrWidth2 <- chrWidth
      }
      r2 <- chrWidth2 / (squareness * 2)

      attr(listOfdfChromSize[[i]], "r2") <- r2

      attr(listOfdfChromSize[[i]], "centromere") <- (centromereSize2 / divisor2) * cenFactor
      listOfdfChromSize[[i]]$centromereSize <- (centromereSize2 / divisor2) * cenFactor
    } # if holocen success

    #
    # let see if it is not monocen
    #

    else if (length(setdiff(
      c("chrName", "shortArmSize", "longArmSize"),
      colnames(listOfdfChromSize[[i]])
    )) > 0) {
      if (mymessage) {
        message("\nChecking mandatory columns from dfChrSize for chr. with cen.: \n
          chrName, shortArmSize,longArmSize,\n (column OTU  is necessary if more than one species)\n")
        message(
          crayon::green(
            paste(c(
              "\nOTU ", names(listOfdfChromSize)[[i]], " does not have all columns with info to make a monocen.: ",
              paste0(c(
                setdiff(
                  c("chrName", "shortArmSize", "longArmSize"),
                  colnames(listOfdfChromSize[[i]])
                ), " will be removed"
              ), collapse = " ")
            ))
          )
        )
      }
      listOfdfChromSize[[i]] <- NA
    }
    #
    #   let see if it is not holocen
    #

    else if (length(setdiff(
      c("chrName", "chrSize"),
      colnames(listOfdfChromSize[[i]])
    )) > 0) {
      if (mymessage) {
        message("\nChecking mandatory columns from dfChrSize for chr. without cen.: \n
          chrName, chrSize,\n (column OTU  is necessary if more than one species)\n")
        message(crayon::green(paste(c(
          "\nOTU", names(listOfdfChromSize)[[i]], "does not have all columns with info to make a holocen:",
          paste0(c(
            setdiff(
              c("chrName", "chrSize"),
              colnames(listOfdfChromSize[[i]])
            ), " will be removed"
          ), collapse = " ")
        ))))
      }
      listOfdfChromSize[[i]] <- NA
    }
  }


  listOfdfChromSize <- listOfdfChromSize[!is.na(listOfdfChromSize)]

  #
  # rebuild with centromereSize
  #

  listOfdfChromSize <- lapply(listOfdfChromSize, function(x) makeCharCols(x))
  return(listOfdfChromSize)
}
##############
addChrSizeColumn <- function(listOfdfChromSize) {
  for (i in seq_along(listOfdfChromSize)) {
    if (inherits(listOfdfChromSize[[i]], "data.frame")) {
      if (attr(listOfdfChromSize[[i]], "cenType") == "monocen") {
        listOfdfChromSize[[i]] <- makeNumCols(listOfdfChromSize[[i]])
        listOfdfChromSize[[i]]$chrSize <- listOfdfChromSize[[i]]$shortArmSize + listOfdfChromSize[[i]]$longArmSize
      } # if
    } # if
  } # for
  return(listOfdfChromSize)
}

################### ordering function
addNeworderColumn <- function(listOfdfChromSize, orderlist) {
  for (s in seq_along(listOfdfChromSize)) {
    if (inherits(listOfdfChromSize[[s]], "data.frame")) {
      listOfdfChromSize[[s]] <- listOfdfChromSize[[s]][orderlist[[s]], ] # important THIS orders
      listOfdfChromSize[[s]]$neworder <- seq_len(nrow(listOfdfChromSize[[s]]))
    }
  } # end for
  return(listOfdfChromSize)
}
###################
addAttributesDfChrSizeSimple <- function(listOfdfChromSize, centromereSize = NA, threshold = 35, mymessage = TRUE) { # nolint: cyclocomp_linter
  for (i in seq_along(listOfdfChromSize)) {

    #
    # remove columns without info. per karyotype
    #

    listOfdfChromSize[[i]][listOfdfChromSize[[i]] == ""] <- NA
    listOfdfChromSize[[i]] <- listOfdfChromSize[[i]][, !apply(is.na(listOfdfChromSize[[i]]), 2, all)]

    # Does the data.frame have short and long info?
    if (mymessage) {
      message("\nChecking columns from dfChrSize\n") # mess
    }
    #################################################################################################
    #
    #   let see if it is monocen
    #

    if (length(setdiff(
      c("chrName", "shortArmSize", "longArmSize"),
      colnames(listOfdfChromSize[[i]])
    )) == 0) {
      if (mymessage) {
        message("\nChecking mandatory columns from dfChrSize for chr. with cen.: \n
          chrName, shortArmSize,longArmSize,\n (column OTU  is necessary if more than one species)\n")
        message(crayon::green(paste("\nOTU ", names(listOfdfChromSize)[[i]],
          "has all columns with info to have monocen. If not, you have to clean your data")))
      }
      attr(listOfdfChromSize[[i]], "cenType") <- "monocen"

      #
      #   if larger than 100 000 reduce
      #

      myn <- max(pmax(listOfdfChromSize[[i]]$longArmSize + listOfdfChromSize[[i]]$shortArmSize))
      exp <- floor(log(myn, base = 10))
      divisor2 <- 10^exp
      threshold2 <- threshold / 10

      divisor2 <- ifelse((myn / divisor2) < threshold2, divisor2 <- 10^(exp - 1), divisor2)

      attr(listOfdfChromSize[[i]], "divisor") <- divisor2

      if (!is.na(centromereSize)) {
        centromereSize2 <- centromereSize
      } else {
        centromereSize2 <- divisor2
      }

      attr(listOfdfChromSize[[i]], "centromere") <- centromereSize2 # /divisor2
      listOfdfChromSize[[i]]$centromereSize <- centromereSize2 # /divisor2
    }

    ############################################################################################## 3
    #   let see if it is holocen
    #

    else if (length(setdiff(
      c("chrName", "chrSize"),
      colnames(listOfdfChromSize[[i]])
    )) == 0) {
      if (mymessage) {
        message("\nChecking mandatory columns from dfChrSize for chr. without cen.: \n
          chrName, chrSize,\n (column OTU  is necessary if more than one species)\n")
        message(crayon::green(paste(c("\nOTU ", names(listOfdfChromSize)[[i]],
          " has all columns with info to have holocen. If not, you have to clean your data"))))
      }
      attr(listOfdfChromSize[[i]], "cenType") <- "holocen"

      #
      #   if larger than 100 000 reduce
      #

      myn <- max(listOfdfChromSize[[i]]$chrSize)
      exp <- floor(log(myn, base = 10))

      divisor2 <- 10^exp
      threshold2 <- threshold / 10

      ifelse((myn / divisor2) < threshold2, divisor2 <- 10^(exp - 1), divisor2)

      attr(listOfdfChromSize[[i]], "divisor") <- divisor2

      # listOfdfChromSize[[i]]$chrSize <- listOfdfChromSize[[i]]$chrSize/divisor2

      if (!is.na(centromereSize)) {
        centromereSize2 <- centromereSize
      } else {
        centromereSize2 <- divisor2
      }

      attr(listOfdfChromSize[[i]], "centromere") <- centromereSize2 # /divisor2
      listOfdfChromSize[[i]]$centromereSize <- centromereSize2 # /divisor2
    } # if holocen success

    #
    # let see if it is not monocen
    #

    else if (length(setdiff(
      c("chrName", "shortArmSize", "longArmSize"),
      colnames(listOfdfChromSize[[i]])
    )) > 0) {
      if (mymessage) {
        message("\nChecking mandatory columns from dfChrSize for chr. with cen.: \n
          chrName, shortArmSize,longArmSize,\n (column OTU  is necessary if more than one species)\n")
        message(
          crayon::green(
            paste(c(
              "\nOTU ", names(listOfdfChromSize)[[i]], " does not have all columns with info to make a monocen.: ",
              paste0(c(
                setdiff(
                  c("chrName", "shortArmSize", "longArmSize"),
                  colnames(listOfdfChromSize[[i]])
                ), " will be removed"
              ), collapse = " ")
            ))
          )
        )
      }
      listOfdfChromSize[[i]] <- NA
    }
    #
    #   let see if it is not holocen
    #
    else if (length(setdiff(
      c("chrName", "chrSize"),
      colnames(listOfdfChromSize[[i]])
    )) > 0) {
      if (mymessage) {
        message("\nChecking mandatory columns from dfChrSize for chr. without cen.:\n
          chrName, chrSize,\n (column OTU  is necessary if more than one species)\n")
        message(crayon::green(paste(c(
          "\nOTU", names(listOfdfChromSize)[[i]], "does not have all columns with info to make a holocen:",
          paste0(c(
            setdiff(
              c("chrName", "chrSize"),
              colnames(listOfdfChromSize[[i]])
            ), " will be removed"
          ), collapse = " ")
        ))))
      }
      listOfdfChromSize[[i]] <- NA
    }
  }


  listOfdfChromSize <- listOfdfChromSize[!is.na(listOfdfChromSize)]

  #
  # rebuild with centromereSize
  #

  listOfdfChromSize <- lapply(listOfdfChromSize, function(x) makeCharCols(x))
  return(listOfdfChromSize)
}
