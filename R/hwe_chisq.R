#' @title Hardy-Weinberg Equilibrium Test
#'
#' @description Tests for HWE for a table of counts of (aa, Aa, AA)
#'
#' @export

hwe_chisq <- function(counts) {
  freq <- c(0,1,2)
  n <- sum(counts)
  p <- sum(counts * freq) / (n*2)

  q <- 1 - p
  pvec <- c(q^2, 2*p*q, p^2)
  E <- n * pvec

  chisq <- sum((counts-E)^2 / E)
  p <- pchisq(chisq, 1, lower.tail=F)
  return(p)
}
