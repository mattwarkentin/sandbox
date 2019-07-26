#' @title PLINK Allelic Scoring
#'
#' @description Interface to \href{http://www.cog-genomics.org/plink/1.9/score}{PLINK} to perform allelic scoring. This function will save the \code{PLINK} outputs to the current working directory, and read plink.profile into \code{R} for immediate use as a \code{\link[tibble]{tibble}}.
#'
#' @param bfile Path to \code{PLINK} files (bed, bim, fam); prefix only.
#' @param scores Path to scoring file; see details.
#' @param sum Logical. Should sum of allele scores be returned? Average allele scores returned if set to \code{FALSE}.
#' @param header Logical. Should \code{PLINK} ignore the first non-empty line (assumed to be a header) in \code{scores} and \code{data_file} (if specified)?
#' @param cols A string specifying the column positions for the variant names, allele codes, and scores. By default, \code{PLINK} assumes the column positions are 1, 2, and 3 in \code{scores}, respectively.
#' @param range_file Path to file containing range labels in the first column (i.e. named ID for a given range), lower bounds in the second column, and upper bounds in the third column. See details.
#' @param data_file Path to file containing variant IDs (column 1) and the key quantity (column 2) on each non-empty line. See details.
#' @param ... Additional arguments passed to \code{\link[readr]{read_delim}}.
#'
#' @details The \code{scores} file is white-space delimited, and should contain a row for every variant included in the scoring algorithm, and should have a minimum of 3 columns:
#' \enumerate{
#'   \item Variant name (usually rsID)
#'   \item Allele code (the allele that the score is in reference too)
#'   \item Score (score associated with the named allele)
#' }
#'
#' In other words, for a given variant (column 1), the score (column 3) may represent the per-allele increase in the log-odds of the phenotype for every additional allele named in column 2 (i.e. additive genetic model).
#'
#' \code{PLINK} also offers the ability to apply allelic scoring to a subset(s) of the variants in \code{scores} based on the range of some key quantity (e.g. P-value). To do this, you must additionally provide:
#'
#' \itemize{
#'   \item \strong{\code{range_file}} A white-space delimited text file with three columns (header optional; \code{PLINK} ignores non-numeric values in columns 2 and 3, by default). The first column is a unique ID for the range specified in columns 2 (lower bound) and 3 (upper bound), inclusive. This ID is used in producing the named output from \code{PLINK} (e.g. plink.ID.profile)
#'   \item \strong{\code{data_file}} A white-space delimited text file with two columns (header will be ignored or not based on the \code{header = TRUE/FALSE} argument; so presence of a header must be consistent with the \code{scores} file). Column 1 is the variant ID and column 2 is the key quantity that the range will be applied to (e.g. P-value)
#' }
#'
#' For example, a range of [0, 0.00000005] would perform allelic scoring for all variants in \code{scores} and \code{data_file} that have a GWAS-significant P-value.
#'
#' @return A \code{\link[tibble]{tibble}} with six columns:
#' \itemize{
#'   \item \strong{\code{FID}} Family ID (from \code{PLINK} .fam file)
#'   \item \strong{\code{IID}} Individual ID (from \code{PLINK} .fam file)
#'   \item \strong{\code{PHENO}} Phenotype (from \code{PLINK} .fam file)
#'   \item \strong{\code{CNT}} Number of non-missing alleles used for scoring
#'   \item \strong{\code{CNT2}} Sum of named allele counts
#'   \item \strong{\code{SCORE/SCORESUM}} Either the average of all allele scores, or the sum of the allele scores, depending on the \code{sum} argument
#' }
#'
#' If allelic scoring is done for multiple subsets of the variants, defined by \code{range_file} and \code{data_file}, then each plink.*.profile file is read, row-bound, and nested by file-name, to form a single nested \code{\link[tibble]{tibble}}. The \code{nested-tibble} contains a column for the file name and a list-column of \code{tibbles} for the nested scoring data. Use \code{\link[tidyr]{unnest}} to unnest the data back into a single \code{tibble}.
#'
#' The returned \code{\link[tibble]{tibble}} has the added attribute \code{log} which contains a \code{\link[tibble]{tibble}} for the log-file from \code{PLINK}. This can be accessed using \code{attr(x, 'log')}, where \code{x} is the name of your object.
#'
#' The \code{log} attribute contains a one-column \code{\link[tibble]{tibble}} with a row for every line-break in the log-file. This allows relatively easy access to the log while staying in \code{R}, and one can utilize \code{\link{stringr}} functions to query the log messages.
#'
#' @export

plink_scoring <- function(bfile, scores, sum = TRUE,
                          header = TRUE, cols = "1 2 3",
                          range_file = "", data_file = "",
                          ...) {
  assertthat::assert_that(fs::dir_exists(fs::path_dir(bfile)))
  assertthat::assert_that(fs::file_exists(scores))
  assertthat::assert_that(is.logical(sum))
  assertthat::assert_that(is.logical(header))
  assertthat::assert_that(any(class(cols) %in% c('character')))

  plink <- getOption('plink.path', default = "/Applications/plink_mac_20181202/plink ")

  bfile <- glue::glue('--bfile {bfile} ')

  score <- glue::glue('--score {scores} ')

  opts <- glue::glue('{ifelse(sum, "sum", "")} {ifelse(header, "header", "")}')

  cols <- glue::glue('{cols}')

  if (fs::is_file(range_file) & fs::is_file(data_file)) {
    q_score_range <- glue::glue('--q-score-range {range_file} {data_file}')
  } else {
    q_score_range <- ""
  }

  command <- glue::glue('{plink}{bfile}{score}{cols} {opts} {q_score_range}')

  cat(command)

  system(command)

  num_profile_files <- fs::dir_ls('.', type = 'file', glob = '*.profile')

  if (length(num_profile_files)==1){
    profile <- readr::read_delim('./plink.profile',
                                 delim = ' ',
                                 col_names = TRUE,
                                 trim_ws = TRUE,
                                 progress = TRUE, ...)
  } else {
    profile <- purrr::map_dfr(num_profile_files,
                              ~readr::read_delim(.,
                                                 delim = ' ',
                                                 col_names = TRUE,
                                                 trim_ws = TRUE,
                                                 progress = TRUE, ...),
                              .id = 'file') %>%
      group_nest(file)
  }

  log <- readr::read_delim('./plink.log',
                         delim = '\n',
                         col_names = c('messages'))

  data <- profile
  attr(data, 'log') <- log
  data
}
