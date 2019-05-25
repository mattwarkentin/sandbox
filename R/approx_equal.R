#' @title Approximate Equality
#'
#' @description Test for approximate equality of two vectors.
#'
#' @param a Numeric vector
#' @param b Numeric vector
#' @param digits Integer number of digits for equality test
#'
#' @export

'%==%' <- function(a, b, digits = 10) {
  return(signif(a, digits) == signif(b, digits))
}
