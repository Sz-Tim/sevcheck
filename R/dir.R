

#' dir() but with full.names=T
#'
#' @param ... 
#'
#' @return A character vector containing the names of the files in the specified directories (empty if there were no files). If a path does not exist or is not a directory or is unreadable it is skipped.
#' @export
dirf <- function(...) {
  dir(..., full.names=T)
}




#' dir() but with full.names=T and recursive=T
#'
#' @param ... 
#'
#' @return A character vector containing the names of the files in the specified directories (empty if there were no files). If a path does not exist or is not a directory or is unreadable it is skipped.
#' @export
dirrf <- function(...) {
  dir(..., recursive=T, full.names=T)
}

