#' @name make.uniqueIF
#' @title FUNCTION make.uniqueIF
#' @description make.uniqueIF: produces unique strings from duplicated
#'
#' @param string name of char. vector
#' @param sep separator
#' @param letter boolean, use numbers when \code{FALSE}
#'
#' @keywords string duplicates unique
#' @examples
#' make.uniqueIF(c(1, 1, 2, 2))
#'
#' @return character vector
#' @rdname make.uniqueIF
#' @importFrom stats setNames
#' @export
#'
make.uniqueIF <- function(string, sep = "_", letter = FALSE) {
  mstring <- make.unique(as.character(string), sep = "_")
  tmp <- !duplicated(string)
  for (i in seq_along(mstring[tmp])) {
    mstring[tmp][i] <- ifelse(string[tmp][i] %in% string[duplicated(string)],
      gsub("(.*)", paste0("\\1_0"), mstring[tmp][i]),
      mstring[tmp][i]
    )
  }
  end <- sub(paste0(".*_([0-9]+)"), "\\1", grep(paste0("_([0-9]*)$"), mstring, value = TRUE))
  beg <- sub(paste0("(.*)_[0-9]+"), "\\1", grep(paste0("_([0-9]*)$"), mstring, value = TRUE))

  if (letter) {
    map <- setNames(letters, 0:25)
    end[] <- map[unlist(end)]
  } else {
    end <- as.numeric(end) + 1
  }
  mstring[grep(paste0("_([0-9]*)$"), mstring)] <- paste0(beg, sep, end)
  return(mstring)
}
