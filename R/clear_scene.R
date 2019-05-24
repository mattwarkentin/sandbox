#' @title Clear the Global Environment
#'
#' @export

clear <- function() rm(list=ls(envir=.GlobalEnv, all.names = TRUE),
                       envir=.GlobalEnv)
