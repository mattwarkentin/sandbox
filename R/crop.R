#' @title Crop a DICOM series to an array
#'
#' @description \code{crop_dicom} will take a directory containing a DICOM series as an input volumes, and will crop the volume to dimensions determined by \code{height}, \code{width}, \code{num_slices_sup}, and \code{num_slices_inf}. The coordinates \code{x,y,z} orient the cropping task to a centroid. The function returns a 3D array object.
#'
#' @param path Path to DICOM series.
#' @param array A 3D array
#' @param x x-coordinate for the centroid.
#' @param y y-coordinate for the centroid.
#' @param z z-coordinate for the centroid.
#' @param add_slices Number of slices to add above/below index slice (z).
#' @param height Integer that describes the spatial row dimension.
#' @param width Integer that describes the spatial column dimension.
#' @param verbose A logical flag (default is FALSE) for printing messages
#'
#' @note This function utilizes \code{\link{oro.dicom}} for its importing functionality. In general, \code{crop_dicom} will be used to crop the DICOM series after conversion to a 3D array. However, I have exposed the \code{crop_dicom} function so that one can crop an array directly if it has already be converted to a 3D-array.
#'
#' @return A 3D array object (rows x columns x slices)
#'
#' @author Matthew T. Warkentin (warkentin@lunenfeld.ca)
#'
#' @export

crop_dicom <- function(path, x=NULL, y=NULL, z=NULL,
                       add_slices=1, height=NULL, width=NULL,
                       verbose = FALSE){

  assertthat::assert_that(fs::dir_exists(path))

  is_odd <- function(x) {
    assert_that(is.numeric(x), length(x) == 1)
    x %% 2 == 1
  }

  assertthat::on_failure(is_odd) <- function(call, env) {
    paste0(deparse(call$x), " is even; provide odd dimensions")
  }

  if (is.null(height) & is.numeric(width) ) {
    height <- width
    message('Height not supplied; using width to crop square matrices')
  } else if  (is.null(width) & is.numeric(height) ) {
    width <- height
    message('Width not supplied; using height to crop square matrices')
  } else {
    height <- height
    width  <- width
  }

  assertthat::assert_that(is_odd(height), is_odd(width))

  assertthat::assert_that(!is.null(x), !is.null(y), !is.null(z))
  assertthat::assert_that(noNA(x), noNA(y), noNA(z))

  dcm <- oro.dicom::readDICOM(path, flipud = FALSE)

  reorder_index <- oro.dicom::extractHeader(dcm$hdr, 'InstanceNumber') %>%
    tibble::enframe() %>%
    dplyr::rename(list_order = name,
           slice_order = value) %>%
    dplyr::arrange(desc(slice_order)) %>%
    dplyr::pull(list_order)

  dcm$img <- dcm$img[reorder_index]
  dcm$hdr <- dcm$hdr[reorder_index]

  array <- oro.dicom::create3D(dcm)

  x <- as.integer(x)
  y <- as.integer(y)

  z <- as.integer(z)
  add_slices <- as.integer(add_slices)
  added_height <- (as.integer(height) - 1) / 2
  added_width  <- (as.integer(width) - 1) / 2
  tot_slices <- 1 + (add_slices * 2)

  a = x - added_height
  b = x + added_height

  c = y - added_width
  d = y + added_width

  e = z - add_slices
  f = z + add_slices

  array <- array[a:b, c:d, e:f]
  array <- scales::rescale(array, to = c(0, 1), from = c(0,4096))

  if (verbose) {
    cat(glue::glue('Cropped {path} to dimension {height} x {width} x {tot_slices} (rows * columns * slices)'))
    return(array)
  } else {
    return(array)
  }

}


#' @export
#' @rdname crop_dicom

crop_array<- function(array, x=NULL, y=NULL, z=NULL,
                      add_slices=1, height=NULL, width=NULL,
                      verbose = FALSE){

  is_odd <- function(x) {
    assertthat::assert_that(is.numeric(x), length(x) == 1)
    x %% 2 == 1
  }

  assertthat::on_failure(is_odd) <- function(call, env) {
    paste0(deparse(call$x), " is even; provide odd dimensions")
  }

  if (is.null(height) & is.numeric(width) ) {
    height <- width
    message('Height not supplied; using width to crop square matrices')
  } else if  (is.null(width) & is.numeric(height) ) {
    width <- height
    message('Width not supplied; using height to crop square matrices')
  } else {
    height <- height
    width  <- width
  }

  assertthat::assert_that(is_odd(height), is_odd(width))

  assertthat::assert_that(!is.null(x), !is.null(y), !is.null(z))
  assertthat::assert_that(noNA(x), noNA(y), noNA(z))

  x <- as.integer(x)
  y <- as.integer(y)

  z <- as.integer(z)
  add_slices <- as.integer(add_slices)
  added_height <- (as.integer(height) - 1) / 2
  added_width  <- (as.integer(width) - 1) / 2
  tot_slices <- 1 + (add_slices * 2)

  a = x - added_height
  b = x + added_height

  c = y - added_width
  d = y + added_width

  e = z - add_slices
  f = z + add_slices

  array <- array[a:b, c:d, e:f]
  array <- scales::rescale(array, to = c(0, 1), from = c(0,4096))

  if (verbose) {
    cat(glue::glue('Cropped array to dimension {height} x {width} x {tot_slices} (rows * columns * slices)'))
    return(array)
  } else {
    return(array)
  }

}
