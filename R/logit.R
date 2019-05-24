#' @title Logit and Inverse-Logit
#'
#' @export

inv.logit <- function(x) 1 / (1 + exp(-x))

logit <- function(p) log(p) - log(1 - p)
