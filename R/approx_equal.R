#' @title Approximate Equality
#'
#' @description Test for approximate equality of two vectors.
#'
#' @param a Numeric vector.
#' @param b Numeric vector.
#'
#' @export

'%==%' <- function(a, b) {
  return(signif(a, 10) == signif(b, 10))
}
