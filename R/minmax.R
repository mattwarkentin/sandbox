#' @title Minimum-Maximum of Object
#'
#' @description Function to get min and max for a bunch of objects (all coerced to vectors) to set xlim and ylim in plot
#'
#' @export

minmax <- function(..., na.rm=TRUE) {

  a <- numeric(0)
  things <- list(...)
  for(i in 1:length(things)) {
    a <- c(a, as.vector(things[[i]]))
  }
  return(c(min(a, na.rm=na.rm), max(a, na.rm = na.rm)))
}
