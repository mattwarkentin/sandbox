#' Deselect variables by name
#'
#' @description Choose variables from a tbl. deselect() drops only the variables you mention. Supports \code{tidyselect::select_helpers}
#'
#' @param .data Data frame.
#' @param ... One or more unquoted expressions separated by commas. You can treat variable names like they are positions, so you can use expressions like x:y to select ranges of variables.
#'
#' Positive values drop variables; negative values keep variables.
#'
#' @return An object of the same class as .data.
#' @export
#'
#' @examples
#'
#' iris %>%
#'   deselect(Species)
#'
deselect <- function(.data, ...){
  vars <- dplyr::enquos(...)

  .data %>%
   dplyr::select(-(!!!vars))
}
