#' @title Training and Testing Predictions
#'
#' @description Make predictions using in \code{train} and \code{test} data using a \code{model}. The \code{model} object must have a S3 predict method. In other words, there should be a \code{predict.class} for whatever class the model is. To check the class of a model run \code{class model_obj}. Also, check \code{methods('predict')} to check if your model class is supported as a S3 predict method.
#'
#' @param model Model object
#' @param train Training data
#' @param test Testing data
#' @param type Type of prediction; passed to \code{predict}
#'
#' @details Both training and testing must contian all the predictors used in the model. Predictor names must be a full match.
#'
#' @export

pred_train_test <- function(model, train, test, type = 'response') {

  train <- train %>%
    dplyr::mutate(pred = predict(model, newdata = train, type = ),
                  set = 'train')

  test <- test %>%
    dplyr::mutate(pred = predict(model, newdata = test,  type = type),
                  set = 'test')

  return(bind_rows(train, test))
}

