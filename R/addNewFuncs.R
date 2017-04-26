#' function for writing documentation for new functions
#'
#' This function loads the necessary libraries and runs document() to create the documentation for any newly added functions.

nf <- function() {
  require(devtools); require(roxygen2)
  document()
}
