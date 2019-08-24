#' @title P-value and Z-Score Converter
#'
#' @description Two simple functions to convert between P-values and Z-scores.
#'
#' @param z Z-score.
#' @param p P-value.
#' @param sign Sign (-1/+1) of beta.
#'
#' @return Either a P-value or Z-score.
#'
#' @name pz

#' @rdname pz
#' @export
z_to_p <- function(z){ 2 * pnorm(-abs(z)) }

#' @rdname pz
#' @export
p_to_z <- function(p, sign){ sign(sign) * abs(qnorm(p / 2)) }
