#' @title Tidy Robust Poisson Regression
#'
#' @description Fits a Poisson regression model with robust variance estimation (modified Poisson regressin) and returns a tidy version of the results.
#'
#' @param data Data frame.
#' @param formula Formula describing the model.
#' @param ci Confidence interval width.
#' @param expo Logical. Should the estimates and confidence intervals be exponentiated?
#'
#' @return A \code{tibble}.
#'
#' @export

tidy_robpois <- function(data, formula, ci = 0.95, expo = FALSE){

  assertthat::assert_that(any(class(data) %in% c('tbl', 'data.frame')))
  assertthat::assert_that(class(formula) == 'formula')
  assertthat::assert_that(dplyr::between(ci, 0, 1))
  assertthat::assert_that(is.logical(expo))

  y.var <- all.vars(formula)[1]
  mm <- glm(formula, data = data, family = 'poisson')
  n.coef <- length(names(coef(mm)))

  lmtest::coeftest(mm, vcov = sandwich::sandwich)

  lci = (1 - ci) / 2
  uci = ci + ((1 - ci) / 2)

  cov <- sandwich::vcovHC(mm, type = "HC0")
  std.err <- sqrt(diag(cov))
  ll <- coef(mm) + qnorm(lci) * std.err
  ul <- coef(mm) + qnorm(uci) * std.err
  pval <- 2 * pnorm(abs(coef(mm)/std.err), lower.tail = F)
  est <- coef(mm)

  if (expo) {
    est <- exp(est)
    ll <- exp(ll)
    ul <- exp(ul)
  }

  dd <- dplyr::bind_cols('term' = names(coef(mm)),
                  'estimate' = est,
                  'conf.low' = ll,
                  'conf.high' = ul,
                  'p.value' = pval,
                  'std.error' = std.err)

  return(dd)
}
