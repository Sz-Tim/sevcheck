#' standard error function
#'
#' This function calculates the standard error for a vector because R for 
#' some reason doesn't. 
#' @param x A numeric vector
#' @return standard error value
#' @keywords standard error
#' @note NAs are removed from the calculation
#' @export
#' @examples
#' se(1:10)
#' se(c(1:10, NA))
#' se(c(NA, NA, 10))
#' se(c(NA, NA, 10, 20))

se <- function(x) {
  if(sum(!is.na(x)) > 1) {
    return(sd(x, na.rm=TRUE)/sqrt(sum(!is.na(x))))
  } else {
    return(0)
  }
}
