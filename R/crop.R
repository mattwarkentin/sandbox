#' @title Crop a DICOM series to an array
#'
#' @description \code{crop_dicom} will take a directory containing a DICOM series as an input volumes, and will crop the volume to dimensions determined by \code{height}, \code{width}, \code{num_slices_sup}, and \code{num_slices_inf}. The coordinates \code{x,y,z} orient the cropping task to a centroid. The function returns a 3D array object.
#'
#' @param path Path to DICOM series.
#' @param array A 3D array
#' @param x x-coordinate for the centroid.
#' @param y y-coordinate for the centroid.
#' @param z z-coordinate for the centroid.
#' @param num_slices_sup Number of slices to add superiorly.
#' @param num_slices_inf Number of slices to add inferiorly.
#' @param height Integer that describes the spatial row dimension.
#' @param width Integer that describes the spatial column dimension.
#' @param verbose A logical value (default is FALSE) for printing a message
#'
#' @note This function utilizes \code{\link{oro.dicom}} for its importing functionality. In general, \code{crop_array} is used internally to crop the DICOM series after conversion to a 3D array. However, I have exposed this function so that one can crop an array directly.
#'
#' @return A 3D array object (rows x columns x slices)
#'
#' @author Matthew T. Warkentin (warkentin@lunenfeld.ca)
#'
#' @export

crop_dicom <- function(path, x, y, z,
                       num_slices_sup, num_slices_inf,
                       height, width, verbose = FALSE) {

  assertthat::assert_that(fs::dir_exists(path))

  dcm <- readDICOM(path)

  array <- create3D(dcm)

  crop_array(array, x, y, z, num_slices_sup, num_slices_inf, height, width)
}


# Workhorse cropping function ----

#' @export
#' @rdname crop_dicom

crop_array <- function(array, x, y, z,
                       num_slices_sup, num_slices_inf,
                       height, width, verbose = FALSE) {

  x <- as.integer(x)
  y <- as.integer(y)
  z <- as.integer(z)
  num_slices_sup <- as.integer(num_slices_sup)
  num_slices_inf <- as.integer(num_slices_inf)

  height <- as.integer(height)
  width <- as.integer(width)

  slices = num_slices_sup + num_slices_inf + 1
  mat_height = height + height + 1
  mat_width  = width + width + 1

  if (verbose) {
  cat(glue('Cropping array to dimensions {mat_height} x {mat_width} x {slices} (rows x cols x slices)', .sep = ''))
  }

  a = x - height
  b = x + height

  c = y - width
  d = y + width

  e = z - num_slices_inf
  f = z + num_slices_sup

  array[a:b, c:d, e:f]

}
