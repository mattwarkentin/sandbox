#' @title Reorder terms for kable table
#'
#' @description Function to order the terms (usually regression variables) in a specific order to present in your table.
#'
#' @param data Data frame.
#' @param ... Column names to be ordered.
#' @param order Character vector of term names, in the desired order.
#' @param rev Logical. Should the order be reversed?
#'
#' @return A \code{\link[tibble]{tibble}} with arranged columns.
#'
#' @export

reorder_terms <- function(data, ..., order = letters, rev = FALSE) {

  assertthat::assert_that(is.character(levels))

  if (rev) { levels <- rev(levels) }

  data <- data %>%
    arrange_at(vars(...), ~factor(., levels = levels))
  data
}
