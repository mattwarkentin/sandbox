#' @title Table One Tibbles
#'
#' @description Functions for producing descriptive statistics for numeric and categorical data. The objective of these functions is to make it easier to produce "Table 1" for a manuscript.
#'
#' Tables are seperately created for numeric and categorical variable, and need to be combined manually afterward (probably using \code{\link[dplyr]{bind_rows}}).
#'
#' @param data Data frame.
#' @param ... Columns to summarise.
#' @param group Grouping variable for group summaries.
#' @param round Number of digits for rounding statistics (where applicable).
#' @param digits Number of significant digits to print (pad with zeroes if digits < round)
#' @param total Logical. Should a total column be added? (Default is FALSE) This argument only matters when a grouping variable is provided.
#' @param na.rm Logical. Should missing data be remove before computing summary statisitcs? (Default is FALSE)
#'
#' @details For numeric variables passed to \code{table1_numeric}, the returned summary statistics are means (\code{\link[base]{mean}}) and standard deviations (\code{\link[stats]{sd}}). For categorical variables (i.e. factors) passed to \code{table1_categorical}, the returned summary statistics are counts (\code{\link[base]{sum}}) and percentages (\code{\link[base]{mean}}) of indicator variables for each level in a factor variable.
#'
#' @return A \code{\link[tibble]{tibble}} with as many rows as the number of columns provided to \code{...}, and as many columns as levels in \code{group}. Cell values are \code{\link[glue]{glue}} objects.
#'
#' @name table1
#'
#' @export

table1_numeric <- function(data, ..., group = NULL,
                           round = 2, digits = round,
                           total = FALSE, na.rm = FALSE) {

  assertthat::assert_that(any(class(data) %in% c('tbl', 'data.frame')))
  assertthat::assert_that(!is.na(as.integer(round)))
  assertthat::assert_that(is.logical(na.rm))

  group_expr <- dplyr::enexpr(group)

  sprint <- glue::glue('%.{digits}f')

  if (total && !is.null(group_expr)) {
    total <- data %>%
      select(...) %>%
      gather(key = 'term', value = 'data') %>%
      group_by(term) %>%
      summarise(.total = glue::glue('{sprintf(sprint, round(mean(data, na.rm = na.rm), round))} [{sprintf(sprint, round(sd(data, na.rm = na.rm), round))}]')) %>%
      select(.total)

    data <- data %>%
      dplyr::select({{ group }}, ...) %>%
      tidyr::gather(key = 'term', value = 'data', - {{ group }}) %>%
      dplyr::group_by({{ group }}, term) %>%
      dplyr::summarise(val = glue::glue('{sprintf(sprint, round(mean(data, na.rm = na.rm), round))} [{sprintf(sprint, round(sd(data, na.rm = na.rm), round))}]')) %>%
      tidyr::spread({{ group }}, val)

    data <- dplyr::bind_cols(data, total)
    data
  } else if (!total && !is.null(group_expr)) {
    data <- data %>%
      dplyr::select({{ group }}, ...) %>%
      tidyr::gather(key = 'term', value = 'data', - {{ group }}) %>%
      dplyr::group_by({{ group }}, term) %>%
      dplyr::summarise(val = glue::glue('{sprintf(sprint, round(mean(data, na.rm = na.rm), round))} [{sprintf(sprint, round(sd(data, na.rm = na.rm), round))}]')) %>%
      tidyr::spread({{ group }}, val)
    data
  } else {
    data <- data %>%
      dplyr::select(...) %>%
      tidyr::gather(key = 'term', value = 'data') %>%
      dplyr::group_by(term) %>%
      dplyr::summarise(.total = glue::glue('{sprintf(sprint, round(mean(data, na.rm = na.rm), round))} [{sprintf(sprint, round(sd(data, na.rm = na.rm), round))}]'))
    data
  }
  data
}

#' @rdname table1
#' @export
table1_categorical <- function(data, ..., group = NULL,
                               round = 2, digits = round,
                               total = FALSE, na.rm = FALSE,
                               na_level = "(Missing)"){

  # todo: how to handle missing data for ... or group ----
  # todo: add total function like numerical ----

  vars <- tidyselect::vars_select(names(data), ...)

  assertthat::assert_that(all(unlist(data %>%
                            dplyr::select(...) %>%
                            purrr::map(., is.factor))),
                          msg = 'Variables to ... must be factors!')

  assertthat::assert_that(any(class(data) %in% c('tbl', 'data.frame')))
  assertthat::assert_that(!is.na(as.integer(round)))
  assertthat::assert_that(is.logical(na.rm))

  sprint <- glue::glue('%.{digits}f')

  data <- dplyr::select(data, {{ group }}, ...) %>%
    dplyr::mutate_at(vars({{ group }}), forcats::fct_explicit_na, na_level = na_level)

  data <- data %>% recipes::recipe(x = data) %>%
    recipes::step_dummy(vars, one_hot = TRUE) %>%
    recipes::prep() %>%
    recipes::juice()

  data <- data %>%
    tidyr::gather('var', 'val', -{{ group }}) %>%
    dplyr::group_by({{ group }}, var) %>%
    dplyr::summarise(val = glue::glue('{scales::comma(sum(val, na.rm = na.rm))} ({sprintf(sprint, round(mean(val*100, na.rm = na.rm), round))}%)')) %>%
    tidyr::spread({{ group }}, val)
  data
}
