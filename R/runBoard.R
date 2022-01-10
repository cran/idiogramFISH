#' @name runBoard
#' @title FUNCTION runBoard
#' @description runBoard: run shinyApp
#' @param installAll boolean, when \code{TRUE} dependences are installed without asking. Defaults to \code{FALSE}
#' @keywords shiny
#' @export
#' @rdname runBoard
#' @importFrom utils install.packages
#' @return shiny
runBoard <- function(installAll=FALSE) {

  appDir <- system.file("shinyApps", "iBoard", package = "idiogramFISH")

  if (appDir == "") {
    stop("Could not find inst folder with shiny app.", call. = FALSE)
  }

  missRentrez <-  missShiny <- missRecPkg <- missPkg <- character(0)

  if(F){
  oldshiny <- "https://cran.r-project.org/src/contrib/Archive/shiny/shiny_1.6.0.tar.gz"


  if (system.file(package = "shiny") == '') {
    if(packageVersion("shiny")>1.6){
      missShiny<-"shiny"
    }
  }

  if (length(missShiny) & installAll==FALSE) {
    message(paste("you need to install shiny 1.6 from", paste(oldshiny, collapse=", ") ))
    answer1 <- readline("Do you want to proceed installing now (Yes or No) ? ")

    if (exists("answer1") ) {
      if (tolower(answer1) %in% c("y","ye","yes","yap","yess") ) {

        utils::install.packages(oldshiny, repos = NULL, type ='source')

      } else {
        return(print("bye"))
      }
    }

  } else if(length(missShiny) & installAll) {
    utils::install.packages(oldshiny, repos = NULL, type ='source')
  }
  }

  neededPkg<-c(
               'shiny'
               ,'shinydashboard'
               ,'rhandsontable'
               ,'gtools'
               ,'knitr'
               ,'rclipboard'
               ,'clipr'
               )

  recomPkg <- c("RCurl",
                "rvcheck",
                "badger",
                "svglite",
                'rmarkdown'
  )

  for (pkg in neededPkg) {
    if (system.file(package = pkg) == '') {
      missPkg <- c(missPkg,pkg)
    }
  }
  for (pkg in recomPkg) {
    if (system.file(package = pkg) == '') {
      missRecPkg <- c(missRecPkg,pkg)
    }
  }

  if (length(missPkg) & installAll==FALSE) {
    message(paste("you need to install", paste(missPkg,collapse=", ") ))
    answer <- readline("Do you want to proceed installing now (Yes or No) ? ")

    if (exists("answer") ) {
      if (tolower(answer) %in% c("y","ye","yes","yap","yess") ) {
        lapply(neededPkg, function(pkg) {
          if (system.file(package = pkg) == '') {
            tryCatch(utils::install.packages(pkg),error=function(w){
              message(paste("failure installing",pkg) )
            })
            # install.packages(pkg)
          }
        }
        )
      } else {
        return(print("bye"))
      }
    }

  } else if (length(missPkg) & installAll) {
    lapply(neededPkg, function(pkg) {
      if (system.file(package = pkg) == '') {
        tryCatch(utils::install.packages(pkg),error=function(w){
          message(paste("failure installing",pkg) )
        })
      }
    }
    )
  }

  if (length(missRecPkg) & installAll==FALSE) {
    message(paste("This packages are optional:", paste(missRecPkg,collapse=", ") ))
    answerRec <- readline("Do you want to proceed installing them (Yes or No) ? ")

    if (exists("answerRec") ) {
      if (tolower(answerRec) %in% c("y","ye","yes","yap","yess") ) {
        lapply(recomPkg, function(pkg) {
          if (system.file(package = pkg) == '') {
            tryCatch(utils::install.packages(pkg),error=function(w){
              message(paste("failure installing",pkg) )
            })
          }
        }
        )
      } else {
        print("ok")
      }
    }

  } else if (length(missRecPkg) & installAll) {
    lapply(recomPkg, function(pkg) {
      if (system.file(package = pkg) == '') {
        tryCatch(utils::install.packages(pkg),error=function(w){
          message(paste("failure installing",pkg) )
        })
      }
    }
    )
  }

  if (system.file(package = "rentrez") == '') {
    missRentrez<-"rentrez"
  }

  if (length(missRentrez) & installAll==FALSE)  {
    message(paste("you need to install rentrez"
                  ,"for the Nucleotides page to download data" ))
    answer2 <- readline("Do you want to install it (Yes or No/empty) ? ")

    if (exists("answer2") ) {
      if (tolower(answer2) %in% c("y","ye","yes","yap","yess")) {
        if (system.file(package = "rentrez") == '') {
          message("installing rentrez")
          tryCatch(utils::install.packages("rentrez"),error=function(w){
            message("failure installing rentrez" )
          })
        }
      }
    }
  } else if (length(missRentrez) & installAll) {
    if (system.file(package = "rentrez") == '') {
      message("installing rentrez")
      # install.packages("rentrez")
      tryCatch(utils::install.packages("rentrez"),error=function(w){
        message("failure installing rentrez" )
      })
    }
  }


    if (requireNamespace("shiny", quietly = TRUE) &
        requireNamespace("shinydashboard", quietly = TRUE ) &
        requireNamespace("rhandsontable", quietly = TRUE ) &
        requireNamespace("gtools", quietly = TRUE ) &
        requireNamespace("rclipboard", quietly = TRUE ) &
        requireNamespace("clipr", quietly = TRUE )
    ) {
      ev <-tryCatch(shiny::runApp(appDir, display.mode = "normal",launch.browser = TRUE), error=function(e){"error"})
      if(is.character(ev)) {
        if(ev=="error"){
          shiny::runApp(appDir, display.mode = "normal")
        }
      }
    } else {
      message(crayon::red(paste("Please install:",paste(missPkg, collapse = ", ") ) ))
      # message(crayon::red("\nUse \ninstall.packages(\"shiny\")
                          # \netc"
                          # )
      # ) # m
      message(crayon::red("\ninstall shiny from",oldshiny)
      )
    }
}
