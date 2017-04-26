#' logit function
#'
#' This function calculates the logit for the object 'p' (range 0 to 1),
#' returning the corresponding values (range -Inf to Inf).
#' @param p A numeric vector, matrix, or array of proportions
#' @return Object of the same dimensions as p
#' @keywords logit, proportion
#' @export
#' @examples
#' p <- runif(20, 0, 1)
#' logit(p)

logit <- function(p) {
  log(p/(1-p))
}




#' antilogit function
#'
#' This function calculates the antilogit for the object 'x' (range -Inf to Inf),
#' returning the corresponding proportions (range 0 to 1).
#' @param x A numeric vector, matrix, or array
#' @return Object of the same dimensions as x
#' @keywords antilogit, proportion
#' @export
#' @examples
#' x <- runif(20, -100, 100)
#' antlogit(x)

antilogit <- function(x) {
  exp(x)/(1+exp(x))
}
