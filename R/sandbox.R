manipulate <- function() {
   message("Loading select data manipulation packages")
   to_load <- c("tidyverse", "")
   lapply(to_load, require, character.only = TRUE, warn.conflicts = TRUE, quietly = TRUE)
   to_show <- paste("Packages loaded for data manipulation:", to_load)
   list(to_show)
   return(to_show)
}

ggsuite <- function() {
  message("Loading select data visualization packages")
  to_load <- c("ggalt", "ggcorrplot", "ggExtra", "ggplot2", "ggpubr", "ggrepel",
               "ggsci", "ggsignif", "ggtern", "ggthemes", "cowplot", "waterfalls")
  lapply(to_load, require, character.only = TRUE, warn.conflicts = TRUE, quietly = TRUE)
  to_show <- paste("Packages loaded for data visualization:", to_load)
  list(to_show)
  return(to_show)
}

learning <- function() {
  c("xgboost", "SuperLearner", "rpart", "party", "randomForest",
    "OneR", "nnet", "caret", "e1071", "naivebayes", "mgcv",
    "lars", "klaR", "GraBLD", "gbm", "glmnet", "gam", "FNN")
}

dobayes <- function() {
  to_load <- c("runjags", "rjags", "R2jags", "naivebayes",
               "mcmcplots", "mcmc", "coda", "boa")
}
