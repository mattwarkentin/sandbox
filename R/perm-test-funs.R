#' @title Permutation P-Values
#'
#' @description A function for generating permuted datasets, where one can permute as many columns as desired. Stratified (i.e. group-based) shuffling can be achieved by specifying the `strata` argument. See details for a complete description.
#'
#' @param data A data frame.
#' @param ... Variables in data frame to permute/shuffle.
#' @param strata A discrete varible to perform stratified permutations.
#' @param times Number of permutations.
#' @param apparent A logical. Should a copy of the input `data` be returned?
#' @param seed A numeric value used to set the RNG seed for reproducible permutations.
#'
#' @details This function was motivated by the `rsample` package which allows straightforward implementation of several common resampling methods (e.g. boostrap, K-fold crossvalidation). While the internal mechanisms of this function are quite different, the goal is to provide a function that works like rsample for permuted data. This function works well with the pipe (`%>%`).
#'
#' After using `perms`, one can compute permutation-based P-values for using any function, including custom functions, in a concise manner. The syntax and usage of this function is motivated by the `tidy eval` principles. Thus, you specify both the names of the columns to permute and the stratitfying variable as bare column names, not quoted names. The default number of permutations is aligned with the default number of bootstraps for `rsample::bootstraps()`.
#'
#' This function allows for easy integration with `purrr::map` functions for functional programming. See the examples for a use-case. Also, consider the using `furrr` equivalents for parallel computations.
#'
#' @return A data frame (`tibble`) where each row is a permuted version of the input data. The returned data frame has the added class `perms` which can be used for S3 generic dispatch.
#'
#' @examples
#'
#' iris %>%
#'   perms(Sepal.Length)
#'
#' @export

perms <- function(data = NULL, ..., strata = NULL,
                  times = 25, apparent = FALSE, seed = NULL) {

  vars <- dplyr::enquos(...)
  strata_quo <- dplyr::enquo(strata)
  strata_expr <- dplyr::enexpr(strata)

  if (!is.null(seed) & is.numeric(seed)){
    seed
  } else {
    seed <- NULL
    seed
  }

  set.seed(seed)

  assertthat::assert_that(length(vars)>0,
                          all(match(purrr::map(vars, rlang::get_expr),
                                    names(data),
                                    nomatch = FALSE)),
                          msg = "One of the provided column names was not found in the data mask!")


  assertthat::assert_that(any(class(data) %in% c('tbl', 'data.frame')))
  assertthat::assert_that(!is.na(as.integer(times)))
  assertthat::assert_that(is.logical(apparent))

  rec <- recipes::recipe(x = data) %>%
    recipes::step_shuffle(!!!vars) %>%
    recipes::prep(verbose = FALSE, retain = FALSE)

  if (is.null(strata_expr)) {
    permd <- data %>%
      tidyr::crossing(., id = 1:times) %>%
      dplyr::group_nest(id) %>%
      dplyr::mutate(id = glue::glue('Permutation{stringr::str_pad(id,
                                  max(nchar(times)), pad = "0")}')) %>%
      dplyr::mutate(data = purrr::map(data, ~recipes::bake(rec, new_data = .)))

  } else if (match(as.character(strata_expr), names(data))) {
    permd <- data %>%
      tidyr::crossing(., id = 1:times) %>%
      dplyr::group_nest(id) %>%
      dplyr::mutate(id = glue::glue('Permutation{stringr::str_pad(id,
                                  max(nchar(times)), pad = "0")}')) %>%
      mutate(data = purrr::map(data, ~strat_bake(., !!strata_quo, rec)))

  } else {
    stop(glue::glue('Variable ({as.character(strata_expr)}) must be a column in supplied data set!'))
  }

  class(permd) <- c(class(permd), 'perms')

  suppressWarnings(if (!apparent) {
    return(permd)
  } else {
    permd <- permd %>%
      dplyr::bind_rows(.,
                       tidyr::nest(data) %>%
                         dplyr::mutate(id = 'Apparent')
                       )
    return(permd)
  })
}

strat_bake <- function(data, strata, recipe){
  strata <- enquo(strata)

  data %>%
    group_nest({{ strata }}, keep = TRUE) %>%
    mutate({{ strata }} := purrr::map(data, ~bake(recipe, new_data = .))) %>%
    unnest({{ strata }})
}
