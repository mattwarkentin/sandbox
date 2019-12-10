#' Group, slice, and ungroup again.
#'
#' @description Short-hand function to group data, slice within groupings, and ungroup.
#'
#' @param .data Data frame.
#' @param ... Unquoted grouping columns.
#' @param .slices Vector of slice numbers.
#'
#' @return An object with the same class as .data.
#' @export
#'
#' @examples
#'
#' iris %>%
#'   gsu(Species, .slices = c(1, 5, 10))
#'
gsu <- function(.data, ..., .slices = 1) {
  gp_vars <- rlang::enquos(...)

  .data %>%
    dplyr::group_by(!!!...) %>%
    dplyr::slice(.slices) %>%
    dplyr::ungroup()

}
