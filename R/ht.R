#' Return the first and last parts of an object
#' 
#' Uses \code{head()} and \code{tail()} because sometimes I want to see both
#'
#' @param x an object
#' @param n an integer vector of length up to \code{dim(x)}
#' @param ... arguments to be passed to or from other methods
#'
#' @return list with head and tail
#' @export
ht <- function(x, n=6, ...) {
  list(head=head(x, n, ...),
       tail=tail(x, n, ...))
}
