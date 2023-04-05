#' @name runBoard
#' @title FUNCTION runBoard
#' @description runBoard: run shinyApp
#' @param installAll boolean, when \code{TRUE} dependences are installed without asking. Defaults to \code{FALSE}
#' @keywords shiny
#' @export
#' @rdname runBoard
#' @importFrom utils install.packages
#' @return shiny
runBoard <- function(installAll = FALSE) { # nolint
  appDir <- system.file("shinyApps", "iBoard", package = "idiogramFISH")

  if (appDir == "") {
    stop("Could not find inst folder with shiny app.", call. = FALSE)
  }

  missRentrez <- missRecPkg <- missPkg <- character(0)

  neededPkg <- c(
    "shiny",
    "shinydashboard",
    "rhandsontable",
    "gtools",
    "knitr",
    "rclipboard",
    "clipr",
    "shinyjs"
  )

  recomPkg <- c(
    "RCurl",
    "rvcheck",
    "badger",
    "svglite",
    "rmarkdown"
  )

  for (pkg in neededPkg) {
    if (system.file(package = pkg) == "") {
      missPkg <- c(missPkg, pkg)
    }
  }
  for (pkg in recomPkg) {
    if (system.file(package = pkg) == "") {
      missRecPkg <- c(missRecPkg, pkg)
    }
  }

  if (length(missPkg) && installAll == FALSE) {
    message(paste("you need to install", paste(missPkg, collapse = ", ")))
    answer <- readline("Do you want to proceed installing now (Yes or No) ? ")

    if (exists("answer")) {
      if (tolower(answer) %in% c("y", "ye", "yes", "yap", "yess")) {
        lapply(neededPkg, function(pkg) {
          if (system.file(package = pkg) == "") {
            tryCatch(utils::install.packages(pkg), error = function(w) {
              message(paste("failure installing", pkg))
            })
          }
        })
      } else {
        return(print("bye"))
      }
    }
  } else if (length(missPkg) && installAll) {
    lapply(neededPkg, function(pkg) {
      if (system.file(package = pkg) == "") {
        tryCatch(utils::install.packages(pkg), error = function(w) {
          message(paste("failure installing", pkg))
        })
      }
    })
  }

  if (length(missRecPkg) && installAll == FALSE) {
    message(paste("This packages are optional:", paste(missRecPkg, collapse = ", ")))
    answerRec <- readline("Do you want to proceed installing them (Yes or No) ? ")

    if (exists("answerRec")) {
      if (tolower(answerRec) %in% c("y", "ye", "yes", "yap", "yess")) {
        lapply(recomPkg, function(pkg) {
          if (system.file(package = pkg) == "") {
            tryCatch(utils::install.packages(pkg), error = function(w) {
              message(paste("failure installing", pkg))
            })
          }
        })
      } else {
        print("ok")
      }
    }
  } else if (length(missRecPkg) && installAll) {
    lapply(recomPkg, function(pkg) {
      if (system.file(package = pkg) == "") {
        tryCatch(utils::install.packages(pkg), error = function(w) {
          message(paste("failure installing", pkg))
        })
      }
    })
  }

  if (system.file(package = "rentrez") == "") {
    missRentrez <- "rentrez"
  }

  if (length(missRentrez) && installAll == FALSE) {
    message(paste(
      "you need to install rentrez",
      "for the Nucleotides page to download data"
    ))
    answer2 <- readline("Do you want to install it (Yes or No/empty) ? ")

    if (exists("answer2")) {
      if (tolower(answer2) %in% c("y", "ye", "yes", "yap", "yess")) {
        if (system.file(package = "rentrez") == "") {
          message("installing rentrez")
          tryCatch(utils::install.packages("rentrez"), error = function(w) {
            message("failure installing rentrez")
          })
        }
      }
    }
  } else if (length(missRentrez) && installAll) {
    if (system.file(package = "rentrez") == "") {
      message("installing rentrez")
      tryCatch(utils::install.packages("rentrez"), error = function(w) {
        message("failure installing rentrez")
      })
    }
  }


  if (requireNamespace("shiny", quietly = TRUE) &&
    requireNamespace("shinydashboard", quietly = TRUE) &&
    requireNamespace("rhandsontable", quietly = TRUE) &&
    requireNamespace("gtools", quietly = TRUE) &&
    requireNamespace("rclipboard", quietly = TRUE) &&
    requireNamespace("clipr", quietly = TRUE) &&
    requireNamespace("shinyjs", quietly = TRUE)
  ) {
    ev <- tryCatch(shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE), error = function(e) {
      "error"
    })
    if (is.character(ev)) {
      if (ev == "error") {
        shiny::runApp(appDir, display.mode = "normal")
      }
    }
  } else {
    message(crayon::red(paste("Please install:", paste(missPkg, collapse = ", "))))
  }
}
