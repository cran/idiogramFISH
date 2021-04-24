#' @name runBoard
#' @title FUNCTION runBoard
#' @description runBoard: run shinyApp
#'
#' @keywords shiny
#' @return shiny
#' @rdname runBoard
#' @export
#' @importFrom utils install.packages
#'
runBoard <- function() {

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

  missPkg<-character(0)

  for (pkg in neededPkg) {
    if (system.file(package = pkg) == '') {
      missPkg <- c(missPkg,pkg)
    }
  }
  if (length(missPkg)){
    message(paste("you need to install", paste(missPkg,collapse=", ") ))
    answer <- readline("Do you want to proceed installing now (Yes or No) ? ")
  }
  if (exists("answer") ) {
    if (answer %in% c("y","Y","Ye","ye","Yes","yes")) {
    lapply(neededPkg, function(pkg) {
      if (system.file(package = pkg) == '') {
        install.packages(pkg)
      }
      }
      )
    } else {
      return(print("bye"))
    }
  }

  missRentrez<-character(0)
  if (system.file(package = "rentrez") == '') {
    missRentrez<-"rentrez"
  }

  if (length(missRentrez)) {
    message(paste("you need to install", paste(missRentrez, collapse=", "),"for the Nucleotides page to work" ))
    answer2 <- readline("Do you want to proceed installing now (Yes or No) ? ")
  }
  if (exists("answer2") ) {
    if (answer2 %in% c("y","Y","Ye","ye","Yes","yes")) {
        if (system.file(package = "rentrez") == '') {
          install.packages("rentrez")
        }
    }
  }

    if (requireNamespace("shiny", quietly = TRUE) &
        requireNamespace("shinydashboard", quietly = TRUE ) ) {
      ev<-tryCatch(shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE), error=function(e){"error"})
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
