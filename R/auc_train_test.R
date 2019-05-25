#' @title Area Under the Curve
#'
#' @description Get area under the ROC curve for both training and test data.
#'
#' @param model Model object.
#' @param train Training data.
#' @param test Testing data.
#' @param truth Ground-truth labels.
#' @param type Type of predictions to be made; passed to predict.
#'
#' @export

auc_train_test <- function(model, train, test, truth, type = 'response') {
  truth <- dplyr::enquo(truth)

  # Training AUC
  auc_train <- with(train, pROC::roc(!!lungcancer ~
                     predict(model, newdata = train, type = type),
                   ci = TRUE))
  # Testing AUC
  auc_test  <- with(test, pROC::roc(!!lungcancer ~
                     predict(model, newdata = test, type = type),
                   ci = TRUE))

  # Post-Processing
  tib_train_senspec <- dplyr::tibble('thr' = auc_train$thresholds,
                              'sens' = auc_train$sensitivities,
                              'spec' = auc_train$specificities,
                              'data' = rep('train',
                                           length(auc_train$thresholds)))

  tib_test_senspec <- dplyr::tibble('thr' = auc_test$thresholds,
                             'sens' = auc_test$sensitivities,
                             'spec' = auc_test$specificities,
                             'data' = rep('test',
                                          length(auc_test$thresholds)))

  tib_nest <- dplyr::as_tibble(dplyr::bind_rows(tib_train_senspec,
                                                tib_test_senspec)) %>%
    tidyr::nest(-data, .key = 'sens.spec')

  tib_train <- dplyr::tibble('data' = 'train',
                      'auc' = auc_train$auc,
                      'lci' = auc_train$ci[1],
                      'uci' = auc_train$ci[3])

  tib_test <- dplyr::tibble('data' = 'test',
                     'auc' = auc_test$auc,
                     'lci' = auc_test$ci[1],
                     'uci' = auc_test$ci[3])

  tib_tt <- dplyr::bind_rows(tib_train, tib_test)

  tib_nest <- tib_nest %>%
    dplyr::left_join(tib_tt, by = 'data') %>%
    dplyr::select(-sens.spec, sens.spec)

  return(tib_nest)
}
