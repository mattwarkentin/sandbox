#' @title Mixed Sort
#'
#' @param x Matrix or data.frame
#'
#' @export

mixedsort <- function (x) {
  #### Nicked from gtools by Gregory R. Warnes, Ben Bolker, and Thomas Lumley ####
  x[mixedorder(x)]
}
