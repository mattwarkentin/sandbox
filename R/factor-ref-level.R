#' @title Declare and Recover Factor Reference Level
#'
#' @description Text goes here...
#'
#' @param data Data frame.
#' @param ... Factor variables to declare.
#' @param .null Null effect size.
#'
#' @details Text here...
#'
#' @return The input data is returned with attributes attached.

fct_declare <- function(data, ..., .sep = "") {
  assertthat::assert_that(any(class(data) %in% c('tbl', 'data.frame')))

  vars <- tidyselect::vars_select(names(data), ...)

  data <- data %>%
    select({{ vars }}) %>%
    mutate_all(forcats::as_factor)

  create_fcts <- function(level, sep = .sep){
    name <- names(level)
    level <- purrr::chuck(level, 1)

    lev_name <- tibble(
      term = glue::glue('{name}{level}')
    )
    lev_name
  }

  levels <- purrr::map(data, ~levels(.))
  tib <- purrr::map(levels, ~create_fcts(., sep = .sep))
  tib
}


fct_recover <- function(data, .null = NULL){
  assertthat::assert_that(!is.null(.null),
                          msg = "You must specify the null effect!")
}

