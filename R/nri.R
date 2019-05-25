#' @title Net reclassification index (NRI)
#'
#' @param data Data
#' @param outcome Outcome variable
#' @param class1 Classification system 1
#' @param class2 Classification system 1
#'
#' @export

nri <- function(data, outcome, class1, class2) {

  class1 <- dplyr::enquo(class1)
  class2 <- dplyr::enquo(class2)
  outcome <- dplyr::enquo(outcome)

  # Moved up or down
  data <- data %>%
    dplyr::mutate(reclass = dplyr::if_else(!!class1 < !!class2, 1,
                                dplyr::if_else(!!class1 > !!class2, 2, 0)))

  data <- data %>%
    dplyr::group_by(!!outcome) %>%
    dplyr::summarise(events = dplyr::n(),
              moved_u = sum(reclass==1, na.rm = TRUE),
              moved_d = sum(reclass==2, na.rm = TRUE)) %>%
    dplyr::mutate(prob_u = moved_u / events,
           prob_d = moved_d / events)

  pr_up <- data %>%
    dplyr::group_by(!!outcome) %>%
    dplyr::select(!!outcome, prob_u)

  pr_down <- data %>%
    dplyr::group_by(!!outcome) %>%
    dplyr::select(!!outcome, prob_d)

  nri_cat <- (pr_up$prob_u[2] - pr_down$prob_d[2]) + (pr_down$prob_d[1] - pr_up$prob_u[1])
  nri_cat
}
