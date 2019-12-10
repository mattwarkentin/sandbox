#' Principal Component Analysis
#'
#' @description Tidy principal component analysis.
#'
#' @param data Data frame.
#' @param center Logical. Should the data be centered?
#' @param scale Logical. Should the data be scaled by the standard deviation?
#'
#' @details Perform principal component anlaysis on a numeric data frame.
#'
#' @return Tibble projected on the new PCA space.
#'
#' @export

pca <- function(data, center = TRUE, scale = TRUE) {
  new <- scale(data, center = center, scale = scale)
  eig <- eigen(cor(new))$vectors

  ret <- t(t(eig) %*% t(new))

  colnames(ret) <- c(glue::glue('pca{1:ncol(ret)}'))

  return(dplyr::as_tibble(ret))
}
