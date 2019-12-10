#' @title Partial Area Under the Curve
#'
#' @description Function for estimating the partial area under the ROC curve. ROC curve may be optionally smoothed using binormal smoothing before computing partial AUC. See \code{Details} for a more comprehensive description.
#'
#' @param data Data frame.
#' @param sens Column name in \code{data} for sensitivity.
#' @param spec Column name in \code{data} for specificity.
#' @param range Range of sensitivity or specificity to integrate.
#' @param focus String describing whether the integration range is for sensitivity or specificity.
#' @param smooth Logical. Should binormal smoothing be used?
#' @param correct Logical. Should a min-max correction be applied?
#' @param n Number of equally-spaced points for smooth curve calculations.
#' @param plot Logical. Should a partial AUC plot be returned?
#' @param opts List of options passed to plotting function if \code{plot = TRUE}. See details.
#' @param ... Not currently used.
#'
#' @return Invisibly returns a \code{\link[tibble]{tibble}} containing a column for the partial AUC, and a list-column containing the ROC data. The returned \code{\link[tibble]{tibble}} inherits the \code{\link[tibble]{tibble}} classes as well as the class \code{partial_auc}. The returned object also contains attributes indicating whether the ROC was smoothed (\code{smooth_roc}), corrected (\code{corrected_auc}), range of integration (\code{range}), and focus (\code{focus}).
#'
#' @details Since the x-axis of the ROC curve is mapped as 1 - Specificity, if the \code{focus} is "sp", the \code{range} should be provided in the left-to-right orientation for specificity. In other words, if you want to integrate the entire ROC in terms of specificity, the \code{range} would be c(1,0); this translates to integrating the false-positive rate from c(0,1).
#'
#' If you only want to integrate the a section of the ROC curve (e.g. FPR from 0 to 0.2), you would specify the \code{range} in terms of specificity as c(1, 0.8).
#'
#' When the \code{focus} is "se" for sensitivity, the \code{range} should be specified from the top-to-bottom orientation. In other words, to integrate the whole ROC curve, if the \code{focus} is "se" the \code{range} should be specificed as c(1,0). To integrate only a section of the ROC, for example, when sensitivity is between 0 and 0.5, you would specify the \code{focus} as "se" and the \code{range} as c(0.5,0).
#'
#' The list of options (\code{opts}) that can be passed to the plotting function are (passed as a list):
#' \itemize{
#'   \item \code{pcol} Colour of the partial area.
#'   \item \code{mcol} Colour for the maximal partial area.
#'   \item \code{fcol} Colour for the full AUC.
#'   \item \code{percent} Logical. Should axes be labeled as percentages?
#' }
#'
#' @examples
#' dd <- tibble::tibble(sens = runif(100), spec = runif(100))
#'
#' partAUC(dd, sens, spec, range = c(1, 0.8))
#' partAUC(dd, sens, spec, range = c(1, 0.8), smooth = TRUE, correct = TRUE)
#'
#' @export

partAUC <- function(data = NULL, sens = NULL, spec = NULL,
                    range = c(1, 0), focus = 'sp', smooth = FALSE,
                    correct = FALSE, n = 10000, plot = FALSE, opts = list(),
                    ...) {

  assertthat::assert_that(any(class(data) %in% c('tbl', 'data.frame')))
  assertthat::assert_that(focus %in% c('se', 'sp'))
  assertthat::assert_that(length(range)==2L)
  assertthat::assert_that(all(dplyr::between(range, 0, 1)))

  sens <- dplyr::enquo(sens)
  spec <- dplyr::enquo(spec)
  corrected <- ""
  smoothed <- ""


  data <- data %>%
    dplyr::transmute(sens = {{ sens }},
              spec = {{ spec }})

  if (smooth) {
    data2 <- data %>% dplyr::mutate_all(qnorm)

    data2 <- data2[apply(data2, 1, function(x) all(is.finite(x))), ]

    lin.mod <- lm(spec ~ sens, data2)

    sens <- qnorm(seq(0, 1, 1 / (n - 1)))
    spec <- predict(lin.mod, newdata = as.data.frame(sens))

    data <- tibble::tibble(sens = pnorm(sens),
                           spec = pnorm(spec))
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

  x <- dplyr::bind_cols(auc = p_auc, tidyr::nest(data, roc = c(sens, spec)))

  if (plot) {
    p <- plot_partial_auc(data = data, sens = sens, spec = spec,
                          range = range, spline_fun = spline_fun,
                          !!!list(opts = opts))
    x <- x %>%
      mutate(plot = list(p))
  }

  attr(x, "smooth_roc") <- smooth
  attr(x, "corrected_auc") <- correct
  attr(x, "range") <- range
  attr(x, "focus") <- focus
  class(x) <- c("partial_auc", class(x))

  invisible(x)
}

plot_partial_auc <- function(data, sens, spec, range,
                             pcol = 'red', mcol = 'black',
                             fcol = 'green', percent = FALSE,
                             spline_fun = NULL, opts) {

  red_part <- tibble::tibble(fpr = seq(1-range[1], 1-range[2], length = 1000),
                     sens = spline_fun(fpr))

  p <- ggplot2::ggplot() +

    ggplot2::annotate("rect", xmin=1-range[1], xmax=1-range[2],
             ymin=0, ymax=1,
             alpha=0.1, fill="black") +

    ggplot2::geom_ribbon(data = data, ggplot2::aes(x = 1 - {{ spec }},
                                 ymin = 0, ymax = {{ sens }}),
                alpha = 0.1, fill = 'green') +

    ggplot2::geom_ribbon(data = red_part, ggplot2::aes(x = fpr,
                                     ymin = 0, ymax = {{ sens }}),
                alpha = 0.5, fill = 'red') +

    ggplot2::geom_abline(slope = 1, intercept = 0, lty = 2) +

    ggplot2::geom_path(data = data, ggplot2::aes(1 - {{ spec }}, {{ sens }}), size = 1) +

    ggplot2::labs(x = '1 - Specificity',
         y = 'Sensitivity') +

    ggplot2::theme_light()

  if (percent) {
    p <- p +
      ggplot2::scale_x_continuous(labels = scales::percent_format()) +
      ggplot2::scale_y_continuous(labels = scales::percent_format())
  }
  p
}
