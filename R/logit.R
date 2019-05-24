#' @title Logit and Inverse-Logit
#'
#' @param x Vector of log-odds
#' @param p Vector of probabilities
#'
#' @export

inv.logit <- function(x) 1 / (1 + exp(-x))

logit <- function(p) log(p) - log(1 - p)
