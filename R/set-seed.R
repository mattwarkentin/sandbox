#' @title Set Seed for RNG
#'
#' @description User-friendly function for interactively setting the seed for random-number generation (RNG).
#'
#' @return Returns a message with the set-seed value.
#'
#' @examples
#' \dontrun{
#' set_seed()
#' 1234
#' }
#'
#' @export

set_seed <- function(){
  new_seed <- readline(prompt = 'Provide a seed number: ')

  new_seed <- as.integer(new_seed)

  assertthat::assert_that(is.integer(new_seed))

  set.seed(new_seed)

  cat(cli::col_green(glue::glue('{cli::symbol$tick} Set seed to {new_seed}')))
  cli::cat_line()
  cat(cli::col_red('NOTE: New seed will be used until R session is terminated!'))
}
