#' @title Permutation Resampling
#'
#' @description A function for generating permuted datasets; where one can permute as many columns as desired. Stratified (i.e. group-based) shuffling can be achieved by specifying a column name for the \code{strata} argument. See details for a more complete description and guidance on usage.
#'
#' @param data A data frame.
#' @param ... Column names in \code{data} to permute/shuffle; or one of the \code{\link[tidyselect]{select_helpers}}.
#' @param strata A discrete varible for stratified permutations.
#' @param times Number of permutations.
#' @param apparent A \code{logical}. Should a copy of the input \code{data} be returned?
#' @param seed A numeric value used to set the RNG seed for reproducible permutations.
#'
#' @details This function was motivated by the \code{\link{rsample}} package which allows straightforward implementation of several common resampling methods (e.g. boostrap, K-fold crossvalidation). While the internal mechanisms of this function are quite different, the goal is to provide a function that works like rsample for permuted data. This function works well with the pipe. See \code{\link{magrittr}} for more details.
#'
#' After using \code{perms}, one can compute permutation-based P-values or other statistics using any function, including custom functions, in a concise manner. The syntax and usage of this function is motivated by the \code{tidy eval} principles. Thus, you specify both the names of the columns to permute and the stratitfying variable as bare column names, not quoted names. The default number of permutations is aligned with the default number of bootstraps for \code{rsample::bootstraps}.
#'
#' This function allows for easy integration with \code{\link[purrr]{map}} functions for functional programming. See the examples for a use-case. Also, consider the using \code{\link[furrr]{future_map}} equivalents for parallel computations.
#'
#' @return A data frame (\code{\link{tibble}}) where each row is a permuted version of the input data. The returned data frame has the added class \code{perms} which can be used by the \code{summary} generic for S3 methods dispatch.
#'
#' @examples
#' iris %>%
#'   perms(Sepal.Length)
#'
#' iris %>%
#'   perms(Sepal.Width, Sepal.Length) %>%
#'   dplyr::mutate(cor = purrr::map_dbl(data, ~with(., cor(Sepal.Width, Sepal.Length))))
#'
#' @export

perms <- function(data = NULL, ..., strata = NULL,
                  times = 25, apparent = FALSE, seed = NULL) {

  vars <- tidyselect::vars_select(names(data), ...)
  strata_quo <- dplyr::enquo(strata)
  strata_expr <- dplyr::enexpr(strata)

  if (!is.null(seed) & is.numeric(seed)){
    seed
  } else {
    seed <- NULL
    seed
  }
  set.seed(seed)

  assertthat::assert_that(any(class(data) %in% c('tbl', 'data.frame')))
  assertthat::assert_that(!is.na(as.integer(times)))
  assertthat::assert_that(is.logical(apparent))

  rec <- recipes::recipe(x = data) %>%
    recipes::step_shuffle(vars) %>%
    recipes::prep(verbose = FALSE, retain = FALSE)

  if (is.null(strata_expr)) {
    permd <- data %>%
      tidyr::crossing(., id = 1:times) %>%
      dplyr::group_nest(id) %>%
      dplyr::mutate(id = glue::glue('Permutation{stringr::str_pad(id,
                                  max(nchar(times)), side = "right", pad = "0")}')) %>%
      dplyr::mutate(data = purrr::map(data, ~recipes::bake(rec, new_data = .)))

  } else if (match(as.character(strata_expr), names(data))) {
    permd <- data %>%
      tidyr::crossing(., id = 1:times) %>%
      dplyr::group_nest(id) %>%
      dplyr::mutate(id = glue::glue('Permutation{stringr::str_pad(id,
                                  max(nchar(times)), side = "right", pad = "0")}')) %>%
      mutate(data = purrr::map(data, ~strat_bake(., !!strata_quo, rec)))

  } else {
    stop(glue::glue('Variable ({as.character(strata_expr)}) must be a column in supplied data set!'))
  }

  if (is.null(strata_expr)) {
    strata <- NA_character_
  } else {
    strata <- rlang::as_string(strata_expr)
  }

  suppressWarnings(if (!apparent) {
    permd
  } else {
    permd <- permd %>%
      dplyr::bind_rows(.,
                       tidyr::nest(data) %>%
                         dplyr::mutate(id = 'Apparent')
                       )
    permd
  })

  attr(permd, 'nperms') <- as.integer(times)
  attr(permd, 'strat_var') <- strata
  attr(permd, 'perm_cols') <-  vars
  attr(permd, 'apparent') <- apparent
  attr(permd, 'seed') <- seed

  class(permd) <- c('perms', class(permd))

  return(permd)
}

# Helper function to perform stratified shuffling
strat_bake <- function(data, strata, recipe){
  strata <- enquo(strata)

  data %>%
    group_nest({{ strata }}, keep = TRUE) %>%
    mutate({{ strata }} := purrr::map(data, ~bake(recipe, new_data = .))) %>%
    unnest({{ strata }})
}

#' @export
summary.perms <- function(obj){
  nperms <- attr(obj, 'nperms')
  strata_var <- attr(obj, 'strat_var')
  perm_cols <- attr(obj, 'perm_cols')
  apparent <- attr(obj, 'apparent')
  seed <- attr(obj, 'seed')

  cat(glue::glue('{cli::symbol$bullet} Number of Permutations: [{nperms}]',
                 '{cli::symbol$bullet} Stratification Variable: [{strata_var}]',
                 '{cli::symbol$bullet} Permuted Columns: [{stringr::str_flatten(perm_cols, collapse = ", ")}]',
                 '{cli::symbol$bullet} Apparent Included: {as.character(apparent)}',
                 '{cli::symbol$bullet} Seed Used: {ifelse(is.null(seed), "NULL", seed)}',
                 .sep = '\n'))
  cli::cat_line()
  cli::cat_rule()
  head(obj)
}
