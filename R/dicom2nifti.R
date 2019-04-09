#' @title Convert a DICOM series to Nifti
#'
#' @description This function will accept a directory containing a single DICOM
#'     series as the input, and will return a single nifti file (.nii)
#'
#' @author Matthew T. Warkentin (warkentin@lunenfeld.ca)
#'
#' @param dicom_dir Directory containing DICOM series
#' @param nifti_dir Directory to save nifti file
#' @param ... Arguments to pass on to \code{\link{oro.dicom}}
#'
#' @export

dicom2nifti <- function(dicom_dir, nifti_dir, ...) {
  NULL
}



# Check for dicome2nifti CLI ----

find_program <- function(program) {
  if (is_osx()) {
    res <- suppressWarnings({
      sanitized_path <- gsub("\\", "\\\\", Sys.getenv("PATH"),
                             fixed = TRUE)
      sanitized_path <- gsub("\"", "\\\"", sanitized_path,
                             fixed = TRUE)
      system(paste0("PATH=\"", sanitized_path, "\" /usr/bin/which ",
                    program), intern = TRUE)
    })
    if (length(res) == 0)
      "found"
    else 'not'
  }
  else {
    Sys.which(program)
  }
}

# Check dicom2nifi version ----

d2n_ver <- function(){
  NULL
}



is_osx <- function(){
  Sys.info()['sysname'] == 'Darwin'
}

is_windows <- function(){
  identical(.Platform$OS.type, 'windows')
}






