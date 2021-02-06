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

  neededPkg<-c('bib2df', 'rmarkdown'
               , "svglite"
             ,'shiny','shinydashboard'
             ,'rhandsontable','gtools'
             ,'knitr','rclipboard')

  missPkg<-character(0)

  for (pkg in neededPkg) {
    if (system.file(package = pkg) == '') {
      missPkg <- c(missPkg,pkg) # readline("What is the value of x?")
    }
  }
  if (length(missPkg)){
    message(paste("you need to install", paste(missPkg,collapse=", ") ))
    answer <- readline("Do you want to proceed installing now (Yes or No) ?")
  }
  if (exists("answer") ) {
    if (answer %in% c("y","Y","Yes","yes")) {
    lapply(neededPkg, function(pkg) {
      if (system.file(package = pkg) == '') {
        install.packages(pkg)
      }
      })
    } else {
      return(print("bye"))
    }
  }
    if (requireNamespace("shiny", quietly = TRUE) &
        requireNamespace("shinydashboard", quietly = TRUE ) ) {
      shiny::runApp(appDir, display.mode = "normal")
    } else {
      message(crayon::red(paste("Please install:",paste(neededPkg, collapse = ", ") ) ))
      message(crayon::red("\nUse \ninstall.packages(\"shiny\") and/or \ninstall.packages(\"shinyshinydashboard\")
                          \netc, to meet dependencies"
                          )
      ) # m
    }
  }


