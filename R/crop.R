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

crop_dicom <- function(path, x, y, z, add_slices, height, width){

  assertthat::assert_that(fs::dir_exists(path))

  dcm <- oro.dicom::readDICOM(path, flipud = FALSE)

  reorder_index <- extractHeader(dcm$hdr, 'InstanceNumber') %>%
    tibble::enframe() %>%
    rename(list_order = name,
           slice_order = value) %>%
    arrange(desc(slice_order)) %>%
    pull(list_order)

  dcm$img <- dcm$img[reorder_index]
  dcm$hdr <- dcm$hdr[reorder_index]

  array <- oro.dicom::create3D(dcm)

  x <- as.integer(x)
  y <- as.integer(y)

  z <- as.integer(z)
  add_slices <- as.integer(add_slices)
  height <- (as.integer(height) - 1) / 2
  width  <- (as.integer(width) - 1) / 2

  a = x - height
  b = x + height

  c = y - width
  d = y + width

  e = z - add_slices
  f = z + add_slices

  array <- array[a:b, c:d, e:f]
  array <- scales::rescale(array, to = c(0, 1), from = c(0,4096))

  return(array)

}


#' @export
#' @rdname crop_dicom

crop_array<- function(array, x, y, z, add_slices, height, width){

  x <- as.integer(x)
  y <- as.integer(y)

  z <- as.integer(z)
  add_slices <- as.integer(add_slices)
  height <- (as.integer(height) - 1) / 2
  width  <- (as.integer(width) - 1) / 2

  a = x - height
  b = x + height

  c = y - width
  d = y + width

  e = z - add_slices
  f = z + add_slices

  array <- array[a:b, c:d, e:f]
  array <- scales::rescale(array, to = c(0, 1), from = c(0,4096))

  return(array)

}
