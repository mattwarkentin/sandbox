#' Index of Union
#'
#' @param roc.data An object returned from the roc function in the package
#'     pROC
#'
#' @return A list which contains the name of the method and the return IOU
#'
#' @export

# Optimization function for Index of Union (Unal, 2017)
indexOfUnion <- function(rocobj) {
  message("Finding optimal threshold that minimizes the Index of Union function...")
  message("")
  message("For details see manuscript --> Unal I. Defining an Optimal Cut-Point Value in ROC Analysis: An Alternative Approach. Computational and Mathemaical Methods in Medicine. Volume 2017, Article ID 3762651")

  sens <- rocobj$sensitivities
  spec <- rocobj$specificities
  thr  <- rocobj$thresholds
  auc  <- as.numeric(rocobj$auc)

  stopifnot(sens<=1 & sens>=0 &
            spec<=1 & spec>=0 &
             auc<=1 & auc>=0)

  term1 <- rep(NA, length(thr))
  term2 <- rep(NA, length(thr))

  for (i in 1:length(thr)) {
    term1[i] <- abs(sens[i] - auc)
    term2[i] <- abs(spec[i] - auc)
  }

  sum.of.terms <- term1 + term2

  min.row <- which.min(sum.of.terms)

  iu.threshold <- thr[min.row]
  to.return <- list("Index of Union (Unal, 2017)", "index.threshold" = iu.threshold)
  message("")
  message("Search complete")
  return(to.return)
}
