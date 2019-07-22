#' @title Partial AUC
#'
#' @description Function for estimating the partial area under the ROC curve. See details for a more comprehensive description.
#'
#' @param data Data frame.
#' @param sens Column name for sensitivity.
#' @param spec Column name for specificity.
#' @param range Range of sensitivity or specificity to integrate.
#' @param focus String describing whether the integration range is for sensitivity or specificity.
#' @param n Number of equally-spaced points for smooth curve calculations.
#' @param ... Not currently used.
#'
#' @return \code{Tibble} containing a column for the partial AUC, and a list-column containing the ROC data.
#'
#' @details Since the x-axis of the ROC curve is mapped as 1 - Specificity, if the \code{focus} is "sp", the range should be provided in the left-to-right orientation for specificity. In other words, if you want to integrate the entire ROC in terms of specificity, the range would be c(1,0); this translates to integrating the false-positive rate from c(0,1).
#'
#' If you only want to integrate the a section of the ROC curve (e.g. FPR from 0 to 0.2), you would specify the range in terms of specificity as c(1, 0.8).
#'
#' When the focus is "se" for sensitivity, the range should be specified from the bottom-to-top (standard y-axis) orientation. In other words, to integrate the whole ROC curve, if the focus is "se" the range should be specificed as c(0,1). To integrate only a section of the ROC, for example, when sensitivity is between 0 and 0.5, you would specify the focus as "se" and the range as c(0,0.5).
#'
#' @examples
#' dd <- tibble::tibble(sens = runif(100), spec = runif(100))
#'
#' partAUC(dd, sens, spec, range = c(1, 0.8))
#' partAUC(dd, sens, spec, range = c(1, 0.8), smooth = T, correct = T)
#'
#' @export

partAUC <- function(data = NULL, sens = NULL, spec = NULL,
                    range = c(1, 0), focus = 'sp', smooth = FALSE,
                    correct = FALSE, n = 1000, ...) {

  assertthat::assert_that(any(class(data) %in% c('tbl', 'data.frame')))
  assertthat::assert_that(focus %in% c('se', 'sp'))
  assertthat::assert_that(length(range)==2L)
  assertthat::assert_that(all(dplyr::between(range, 0, 1)))

  sens <- dplyr::enquo(sens)
  spec <- dplyr::enquo(spec)
  corrected <- ""
  smoothed <- ""


  data <- data %>%
    mutate(sens = {{ sens }},
           spec = {{ spec }})

  if (smooth) {
    data2 <- data %>% dplyr::mutate_all(qnorm)

    data2 <- data2[apply(data2, 1, function(x) all(is.finite(x))), ]

    lin.mod <- lm(sp ~ se, data2)

    se <- qnorm(seq(0, 1, 1 / (n - 1)))
    sp <- predict(lin.mod, newdata = data.frame(se))

    data <- tibble::tibble(sens = pnorm(se),
                           spec = pnorm(sp))
    coefs <- coefficients(lin.mod)
    smooth_auc <- unname(pnorm(coefs[1]/sqrt(1 + coefs[2]^2)))
    smoothed <- "Binormal Smoothed "
  }

  if (focus == "se") {
    spline_fun <- splinefun(x = 1 - data$sens, y = data$spec)
    focus <- 'Sensitivity'
  } else {
    spline_fun <- splinefun(x = 1 - data$spec, y = data$sens)
    focus <- 'Specificity'
  }

  p_auc <- integrate(spline_fun, 1 - range[1], 1 - range[2])$value

  if (correct) {
    min <- sum(1 - range) * abs(diff(range)) / 2
    max <- abs(diff(range))
    p_auc <- (1 + ((p_auc - min)/(max - min))) / 2
    corrected <- '(Corrected)'
  }

  cat(glue::glue('{smoothed}Partial AUC ({focus} {range[1]}-{range[2]}): {round(p_auc, 4)} {corrected}\n'))


  x <- bind_cols(auc = p_auc, nest(data, .key = 'roc'))

  attr(x, "smooth_roc") <- smooth
  attr(x, "corrected_auc") <- correct
  attr(x, "range") <- range
  attr(x, "focus") <- focus
  class(x) <- c("partial_auc", class(x))

  invisible(x)
}
