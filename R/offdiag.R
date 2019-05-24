#' @title Offdiagnoal for Symmetric Matrix
#'
#' @description Function to extract off-diagonal elements of a square matrix.
#'
#' @export

offdiags <- function(matrix, upper=T, both=F) {
  if(both) return(c(matrix[upper.tri(matrix)], matrix[lower.tri(matrix)]))
  else if(upper) return(matrix[upper.tri(matrix)])
  else return(matrix[lower.tri(matrix)])
}
