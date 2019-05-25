#' @title Optimal ROC Threshold
#'
#' @description Functions to identify the optimal ROC threshold according to various optimization criteria.
#'
#' @param sens Sensitivities
#' @param spec Specificities
#' @param thr Threshold values for ROC
#' @param auc Area under the ROC curve
#' @param time Time
#' @param status Censoring status
#' @param pred Predictions
#'
#' @details \itemize{
#'
#' \item \strong{Youden's J Index}: This function will compute Youden's J index. This is computed by finding the threshold which maximizes the value \code{(sensitivity + specificity - 1)}
#'
#' \item \strong{Index of Union}: This function will compute the Index of Union statistic which represents the ideal ROC threshold according to maximizing the statistic (sensitivity - auc) + (specificity - auc)
#'
#' \item \strong{Top Left}: Provides the receiver operating characteristic (ROC) threshold
#'     (x-axis) that minimizes the distance from the ROC curve to the  to the
#'     top left corner of the plot (1,1)
#'
#' \item \strong{Minimum P-value}: Provides the receiver operating characteristic (ROC) threshold
#'     that minimizes the P-value according to Unal (2017), Article ID 3762651
#'
#' \item \strong{Concordance Probability}: Provides the receiver operating characteristic (ROC) threshold
#'     that maximizes the concordance probability. The concordance probability
#'     is the product of the sensitivity and specificity.
#'
#' }
#'
#' @return A scalar containing the chosen threshold.
#'
#' @examples
#' # Simulate some data
#' sens <- runif(100,0,1)
#' spec <- runif(100,0,1)
#' thr <- runif(100,0,1)
#' auc <- runif(1,0,1)
#'
#' index_of_union(sens, spec, thr, auc)
#' youden(sens, spec, thr)
#' top_left(sens, spec, thr)
#' conc_prob(sens, spec, thr)
#'
#' @name optimal_roc

NULL

# Index of Union function -------------------------------------------------

#' @rdname optimal_roc
#'
#' @export

index_of_union <- function(sens = NULL, spec = NULL, thr = NULL, auc = NULL) {
  message("Finding optimal threshold that minimizes the Index of Union
          function...\n")

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
  message("Search complete\n")
  return(to.return)
}


# Youden’s J Index --------------------------------------------------------

#' @rdname optimal_roc
#'
#' @export

youden <- function(sens = NULL, spec = NULL, thr = NULL) {
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

#' @rdname optimal_roc
#'
#' @export

top_left <- function(sens = NULL, spec = NULL, thr = NULL) {

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

#' @rdname optimal_roc
#'
#' @export

min_pval <- function(time = NULL, status = NULL, pred = NULL) {

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

#' @rdname optimal_roc
#'
#' @export

conc_prob <- function(sens = NULL, spec = NULL, thr = NULL) {

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
