#' function for writing documentation for new functions
#'
#' This function loads the necessary libraries and runs document() to create the 
#' documentation for any newly added functions.
#' @export

nf <- function() {
  devtools::document()
}
