#' @title Permutation P-Values
#'
#' @description Set of functions for computing permutation-based P-values for any function. The syntax and usage of these functions are motivated by the `tidy eval` principles. This function allows for easy integration with `purrr::map` functions.
#'
#' @param data A data frame.
#' @param ... Variables in data frame to permute/shuffle.
#' @param times Number of permutations.
#' @param strata A discrete varible to perform stratified sampling and permutations.
#' @param frac A fraction for up or downsampling your data. For `frac` greater than 1 you must also set `replace` to `TRUE` (i.e. upsample with replacement).
#' @param replace A logical. Should sampling be done with replacement?
#' @param apparent A logical. Should a copy of the input `data` be returned?
#'
#' @return A data frame (`tibble`) where each row is a permuted version of the input data. The returned data frame has the added class `ptbl` which can be used for S3 generics.
#'
#' @export


perm2 <- function(data = NULL, ..., times = 25, strata = NULL,
                  frac = 1.0, replace = FALSE, apparent = FALSE) {

  vars <- enquos(...)

  assertthat::assert_that(class(data) %in% c('tbl', 'data.frame'))
  assertthat::assert_that(!is.na(as.integer(times)))
  assertthat::assert_that(is.logical(replace))
  assertthat::assert_that(is.logical(apparent))

  if (is.null(strata)) {
    permd <- data %>%
      dplyr::sample_frac(size = frac, replace = replace) %>%
      tidyr::crossing(., id = 1:times) %>%
      dplyr::group_nest(id) %>%
      dplyr::mutate(id = glue::glue('Permutation{stringr::str_pad(id,
                                  max(nchar(times)), pad = "0")}')) %>%
      dplyr::select(id, dplyr::everything())

  } else if (match(as.character(strata), names(data))) {
    data <- data %>%
      group_by(!! strata) %>%
      dplyr::sample_frac(size = frac, replace = replace) %>%
      ungroup()


  } else {
    stop(glue::glue('Variable ({as.character(strata)}) must be a column in supplied data set!'))
  }

  if (!apparent) {
    return(permd)
  } else {
    permd %>%
      dplyr::bind_cols(.,
                       tidyr::nest(data) %>%
                         dplyr::mutate(id = 'Apparent')
                       )
    return(permd)
  }
}
