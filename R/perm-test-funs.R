#' @title Permutation P-Values
#'
#' @description Set of functions for computing permutation-based P-values for any function. The syntax and usage of these functions are motivated by the `tidy eval` principles. This function allows for easy integration with `purrr::map` functions.
#'
#' @param data A data frame.
#' @param N Number of permutations to perform.
#' @param strata A varible to perform stratified permutations.
#' @param obs A logical. Should an extra sample be added which is the original (observed) data set?
#' @param ... Not currently used.
#'

strata_check <- function (strata, vars)
{
  if (!is.null(strata)) {
    if (!is.character(strata) | length(strata) != 1)
      stop("`strata` should be a single character value",
           call. = FALSE)
    if (!(strata %in% vars))
      stop(strata, " is not in `data`")
  }
  invisible(NULL)
}


perm_splits <- function (data, times = 25, strata = NULL)
{
  n <- nrow(data)
  if (is.null(strata)) {
    indices <- purrr::map(rep(n, times), sample, replace = TRUE)
  }
  else {
    stratas <- tibble::tibble(idx = 1:n, strata = make_strata(getElement(data,
                                                                         strata)))
    stratas <- split(stratas, stratas$strata)
    stratas <- purrr::map_df(stratas, strat_sample, prop = 1,
                             times = times, replace = TRUE)
    indices <- split(stratas$idx, stratas$rs_id)
  }
  indices <- lapply(indices, boot_complement, n = n)
  split_objs <- purrr::map(indices, make_splits, data = data,
                           class = "boot_split")
  list(splits = split_objs, id = names0(length(split_objs),
                                        "Bootstrap"))
}

perm <- function(data, n = 100, strata = NULL, obs = FALSE, ...){
  # code from rsample::boostraps()
  strata_check(strata, names(data))

  split_objs <- boot_splits(data = data, times = times, strata = strata)

  if (apparent)
    split_objs <- bind_rows(split_objs, apparent(data))

  boot_att <- list(times = times, apparent = apparent, strata = !is.null(strata))

  new_rset(splits = split_objs$splits, ids = split_objs$id,
           attrib = boot_att, subclass = c("bootstraps", "rset"))
}


# Function to convert data to necessary form
discrete_distr_data <- function(x) {
  tibble::tibble(
    value = names(x),
    probability = as.numeric(x)
  )
}
discrete_distr_data(mpg_drv_dist)


#' @importFrom ggplot2 autoplot
autoplot.discrete_distr <- function(object, ...) {
  plot_data <- discrete_distr_data(object)
  ggplot(plot_data, aes(.data$value, .data$probability)) +
    geom_col() +
    coord_flip() +
    labs(x = "Value", y = "Probability")
}

#' @importFrom graphics plot
plot.discrete_distr <- function(x, ...) {
  print(autoplot(x, ...))
}


autoplot.perm() <- function(){
  # generic call for plotting perm dist and observed statistic using ggplot
  NULL
}

broom.tidy() <- function(){
  # Broom mehtod for cleaning up results for presenting
  # Read broom vignette
  NULL
}
