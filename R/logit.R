#' @title Logit and Inverse-Logit
#'
#' @description Logit and Inverse-logit transformations.
#'
#' @param x Vector of log-odds.
#' @param p Vector of probabilities.
#'
#' @name logit
#'
#' @export

inv.logit <- function(x) 1 / (1 + exp(-x))

#' @export
#' @rdname logit
logit <- function(p) log(p) - log(1 - p)
