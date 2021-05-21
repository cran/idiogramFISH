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

  neededPkg<-c(#'bib2df'
               'rmarkdown'
               ,"svglite"
               ,'shiny','shinydashboard'
               ,'rhandsontable','gtools'
               ,'knitr','rclipboard'
               )

  missPkg <- character(0)

  for (pkg in neededPkg) {
    if (system.file(package = pkg) == '') {
      missPkg <- c(missPkg,pkg)
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
        # install.packages(pkg)
      }
    }
    )
  }

  missRentrez<-character(0)

  if (system.file(package = "rentrez") == '') {
    missRentrez<-"rentrez"
  }

  if (length(missRentrez) & installAll==FALSE)  {
    message(paste("you need to install rentrez"
                  # , paste(missRentrez, collapse=", ")
                  ,"for the Nucleotides page to download data" ))
    answer2 <- readline("Do you need to install it (Yes or No/nothing) ? ")

    if (exists("answer2") ) {
      if (tolower(answer2) %in% c("y","ye","yes","yap","yess")) {
        if (system.file(package = "rentrez") == '') {
          message("installing rentrez")
          # install.packages("rentrez")
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
        requireNamespace("rmarkdown", quietly = TRUE ) &
        requireNamespace("rhandsontable", quietly = TRUE ) &
        requireNamespace("gtools", quietly = TRUE ) &
        requireNamespace("rclipboard", quietly = TRUE )
        ) {
      ev <-tryCatch(shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE), error=function(e){"error"})
      if(is.character(ev)){
        if(ev=="error"){
          shiny::runApp(appDir, display.mode = "normal")
        }
      }
    } else {
      message(crayon::red(paste("Please install:",paste(neededPkg, collapse = ", ") ) ))
      message(crayon::red("\nUse \ninstall.packages(\"shiny\") and/or \ninstall.packages(\"shinyshinydashboard\")
                          \netc, to meet dependencies"
                          )
      ) # m
    }
}
