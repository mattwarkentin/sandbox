#' @title Hosmer-Lemeshow Test
#'
#' @description Tests for differences between observed and expected counts using the Hosmer-Lemeshow chi-squared test.
#'
#' @param ng Number of groups (default is 10)
#' @param obs Observed counts
#' @param exp Expected counts
#' @param lower.tail Logical value for whether p-value should be from lower tail of the distribution.
#'
#' @export

hl_chisq <- function(obs, exp, ng = 10, lower.tail = FALSE) {

  chi2 <- rep(NA, ng)

  for (i in 1:ng) {
    chi2[i] <- (obs[i] - exp[i])^2 / exp[i]
  }

  test.st <- sum(chi2)
  p.val <- pchisq(test.st, df = ng - 1, lower.tail = lower.tail)

  return(c('chisq.stat' = test.st,
           'p.value' = p.val))

}
