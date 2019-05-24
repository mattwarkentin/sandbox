#' @title False Discovery Threshold
#'
#' @description Determines the critical p-value for statistical significance using the Benjamini-Hochberg false-discovery rate procedure.
#'
#' NOTE: Significance is defined as P <= threshold, not <.
#'
#' @param pvals Vector of p-values
#' @param FDR False-discovert rate
#'
#' @export

bh_fdr <- function(pvals, FDR=0.05) {
  lp <- length(pvals)
  p.threshold <- FDR * (1:lp) / lp
  p.sorted <- sort(pvals)
  pass.threshold <- p.sorted <= p.threshold
  pass.threshold.index <- max((1:lp)[pass.threshold])
  p.sorted[pass.threshold.index]

}
