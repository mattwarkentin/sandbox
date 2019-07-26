#' @title SNPTEST Functions
#'
#' @description Functions for computing summary statistics or Frequentist Association Tests.
#'
#' @param data BGEN or PLINK format genotypes.
#' @param sample Sample file.
#' @param out Path and name of output file.
#' @param freq Frequenstist metho
#' @param method method
#' @param pheno peh
#' @param ... Additional arguments.
#'

snptest_sum <- function(data, sample, out, ...) {
  snptest <- getOption('snptest.path',
                       default = '/Applications/snptest_v2.5.2_MacOSX_x86_64/snptest_v2.5.2 ')

  data <- glue::glue('-data {bfile} {sample} ')

  out = glue::glue('-o {out} ')

  log

  command <- glue::glue('{snptest}{data}{out}{}')

  system(command)
}

snptest_fat <- function(data, sample, out, freq, method, pheno, ...){

  snptest <- getOption('snptest.path',
                       default = '/Applications/snptest_v2.5.2_MacOSX_x86_64/snptest_v2.5.2 ')

}
