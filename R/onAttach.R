# nocov start
#' @importFrom utils packageVersion
#'
.onAttach <- function(libname, pkgname) {
packageStartupMessage(paste("version",packageVersion("idiogramFISH"),"See help: ?idiogramFISH - browseVignettes(\'idiogramFISH\') - https://ferroao.gitlab.io/manualidiogramfish") )
}
