#' @title Effect Size Formatter
#'
#' @description Formats text as effect sizes plus confidence intervals for tabular presentation.
#'
#' @param effect Effect estimate.
#' @param se Standard error.
#' @param level Confidence level. Default is 0.95.
#' @param lci Confidence bound, lower.
#' @param uci Confidence bound, higher.
#' @param sep Character separator.
#' @param round Number of digits to round to. Default is 2.
#'
#' @details One of \code{se} or \code{lci}/\code{uci} must be specificed, but not both, and must be the same length vectors as \code{effect}, or length 1 (recycled to be the same length as \code{effect}). If both are provided, the confidence intervals will be computed using the standard error and \code{level}.
#'
#' @return Character vector of formatted data. Same length as \code{effect}.
#'
#' @examples
#'
#' set.seed(12345)
#' eff_fmt(rnorm(10), se = 0.1)
#' eff_fmt(rnorm(10), se = 0.1, round = 4L, level = 0.99)
#'
#' @export

eff_fmt <- function(effect, se = NULL, level = 0.95,
                    lci = NULL, uci = NULL, sep = '-',
                    round = 2L) {

  assertthat::assert_that(dplyr::between(level, 0, 1))
  assertthat::assert_that(is.character(sep))
  assertthat::assert_that(is.integer(round))

  level <- level + ((1 - level)/2)

  if (is.numeric(se)) {
    assertthat::assert_that(length(se)==1)

    lci <- format(round(effect - (qnorm(level) * se), round), nsmall = round)
    uci <- format(round(effect + (qnorm(level) * se), round), nsmall = round)
    effect <- format(round(effect, round), nsmall = round)

  } else if (is.numeric(lci) + is.numeric(uci) ==  2){
    assertthat::assert_that(length(lci)==length(effect) & length(uci)==length(effect) | length(lci)==1 & length(uci)==1)

    lci <- format(round(lci, round), nsmall = round)
    uci <- format(round(uci, round), nsmall = round)
    effect <- format(round(effect, round), nsmall = round)

  } else stop()

  out <- glue::glue('{effect} ({lci}{sep}{uci})')

  return(out)
}
