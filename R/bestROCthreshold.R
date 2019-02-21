#' @title Index of Union
#'
#' @description This function will compute the Index of Union statistic
#'     which represents the ideal ROC threshold according to maximizing
#'     the statistic (sensitivity - auc) + (specificity - auc)
#'
#' @param sens Sensitivities
#' @param spec Specificities
#' @param thr Threshold values for ROC
#' @param auc Area under the ROC curve
#'
#' @return A list containing the Index of Union value
#'
#' @examples
#' # Simulate some data
#' sens <- runif(100,0,1)
#' spec <- runif(100,0,1)
#' thr <- runif(100,0,1)
#' auc <- runif(1,0,1)
#'
#' indexOfUnion(sens, spec, thr, auc)
#'
#' @export
# Index of Union function -------------------------------------------------

indexOfUnion <- function(sens, spec, thr, auc) {
  message("Finding optimal threshold that minimizes the Index of Union
          function...")
  message("")

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
  to.return <- list("Index of Union (Unal, 2017)",
                    "index.threshold" = iu.threshold)
  message("")
  message("Search complete")
  return(to.return)
}


# Youden’s J Index --------------------------------------------------------
#' @title Youden's J Index
#'
#' @description This function will compute Youden's J index. This is computed
#'     by finding the threshold which maximizes the value
#'     \code{(sensitivity + specificity - 1)}
#'
#' @param sens Sensitivities
#' @param spec Specificities
#' @param thr Threshold values for ROC
#'
#' @return A list containing Youden's Index
#'
#' @examples
#' # Simulate some data
#' sens <- runif(100,0,1)
#' spec <- runif(100,0,1)
#' thr <- runif(100,0,1)
#'
#' youden(sens, spec, thr)
#'
#' @export

youden <- function(sens, spec, thr) {
  message("Finding optimal threshold that maximizes Youden's J index...")
  message("")
  stopifnot(sens<=1 & sens>=0 &
            spec<=1 & spec>=0)

  J.index <- (sens + spec - 1)

  max.row <- which.max(J.index)

  J.threshold <- thr[max.row]

  to.return <- list("Youden's J Index", "Youden.threshold" = J.threshold)

  message("")
  message("Search complete")

  return(to.return)
}


# Closest to Top Left ROC Index -------------------------------------------
#' @title Closest to Top Left
#'
#' @description Provides the receiver operating characteristic (ROC) threshold
#'     (x-axis) that minimizes the distance from the ROC curve to the  to the
#'     top left corner of the plot (1,1)
#'
#' @param sens Sensitivity
#' @param spec Specificity
#' @param thr Threshold
#'
#' @return A list with two elements. The first element is a character string
#'     letting you know which type of result is returned. The second element is the
#'     threshold which corresponds to the minimized distance
#'
#' @examples
#'
#' sens <- runif(100, 0, 1)
#' spec <- runif(100, 0, 1)
#' thr <- runif(100, 0, 1)
#'
#' topleft(sens, spec, thr)
#'
#' @export

topleft <- function(sens, spec, thr) {

  message("Finding optimal threshold that minimizes the Euclidean distance...")
  message("")

  topleft.index <- sqrt(((1 - sens)^2) + ((1 - spec)^2))

  min.row <- which.min(topleft.index)

  topleft.threshold <- thr[min.row]

  to.return <- list("Closest to top left", "topleft.threshold" = topleft.threshold)

  message("")
  message("Search complete")

  return(to.return)
}


# Minimum P Value Approach ------------------------------------------------
#' @title Minimum P-value
#'
#' @description Provides the receiver operating characteristic (ROC) threshold
#'     that minimizes the P-value according to Unal (2017), Article ID 3762651
#'
#' @param time Follow-up time
#' @param status Censoring status
#' @param pred Predictions
#'
#' @return A list with two elements. The first element is a character string
#'     letting you know which result is returned. The second element is the
#'     threshold which minimizes the p-value
#'
#' @examples
#'
#' time <- rexp(100, 10)
#' status <- rbinom(100, 1, 0.5)
#' pred <- runif(100, 0, 1)
#'
#' minPVal(time, status, pred)
#'
#' @export

minPVal <- function(time, status, pred) {

 message("Finding optimal threshold that minimizes the P-value...")
 message("")

 status <- as.factor(status)
 N <- length(status)
 thr <- unique(pred)

 pval_chisq1 <- rep(NA, length(thr))
 sens <- spec <- s <- r <- u <- v <- rep(NA, length(status))
 pred.status <- factor(x = c(0,1), levels = c("non-event", "event"))

 for (i in 1:length(thr)) {

   pred.status <- ifelse(pred <= thr[44], 0, 1) # X > c
   pred.status <- as.factor(pred.status)

   cmatrix <- table(status, pred.status)

   sens <- as.numeric(cmatrix$byClass[1])
   spec <- as.numeric(cmatrix$byClass[2])

   s <- cmatrix$table[4]
   r <- cmatrix$table[3]
   u <- cmatrix$table[2]
   v <- cmatrix$table[1]

   test.stat <- (N * ((s * v) - (u * r))^2) / ((s + r) * (u + v) * (s + u) *
                                                 (r + v))

   pval_chisq1[i] <- (pchisq(test.stat, df = 1, lower.tail = F))
 }

 min.row <- which.min(pval_chisq1)

 minP.threshold <- thr[min.row]

 to.return <- list("Minimum P-Value Method",
                   "minPval.threshold" = minP.threshold)

 message("")
 message("Search complete")

 return(to.return)
}

# Concordance Probability Method (CZ) -------------------------------------

#' @title Concordance Probability
#'
#' @description Provides the receiver operating characteristic (ROC) threshold
#'     that maximizes the concordance probability. The concordance probability
#'     is the product of the sensitivity and specificity.
#'
#' @param sens Sensitivity
#' @param spec Specificity
#' @param thr Threshold
#'
#' @return A list with two elements. The first element is a character string
#'     letting you know which result is returned. The second element is the
#'     concordance threshold
#'
#' @examples
#'
#' sens <- runif(100, 0, 1)
#' spec <- runif(100, 0, 1)
#' thr <- runif(100, 0, 1)
#'
#' conc_prob(sens, spec, thr)
#'
#' @export

conc_prob <- function(sens, spec, thr) {

  message("Finding optimal threshold that maximizes the Concordance
          Probability (product)...")
  message("")

  cz.index <- sens * spec

  max.row <- which.max(cz.index)

  cz.threshold <- thr[max.row]

  to.return <- list("Concordance Probability Method (CZ)",
                    "cz.threshold" = cz.threshold)

  message("")
  message("Search complete")

  return(to.return)
}
