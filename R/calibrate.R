#' @title Competing Risk Calibration
#' @author Matthew T. Warkentin
#' @description This function will allow you to compute calibration metrics:
#'    - predicted and observed probabilities
#'    - predicted and observed event counts
#'
#' @param data Dataframe to use for absolute-risk predictions
#' @param horizon Time horizon for predictions
#' @param ng Number of deciles based on model-predicted risks (default is 10)
#' @param predvar Unquoted name of predicted risks in data frame
#' @param time Time
#' @param cens Censor status
#' @param cause Integer value for the event-of-interest to calibrate prediction
#'
#' @details These functions uses non-standard evaluation (tidy_eval). Thus, you can pass \code{predvar}, \code{time}, and \code{cens} as unquoted.
#'
#' \code{calibrate} uses \code{\link[dplyr]{ntile}} to place observations into groups based on predicted risks. \code{calibrate2} uses \code{\link[stats]{quantile}} and \code{\link[base]{cut}}, instead. This produces calibration metrics equivalent to \href{http://individual.utoronto.ca/osaarela/validstats-manual.pdf}{validstats::cical}. Lastly, \code{calibrate3} uses the same discretization as \code{calibrate} but uses \code{\link[cmprsk]{cuminc}} instead of \href{http://individual.utoronto.ca/osaarela/validstats-manual.pdf}{validstats::cif} to compute the cumulative incidence function.
#'
#' @return A \code{tibble} containining the decile index
#'    along with the predicted and observed cumulative incidences, by decile, and the predicted and observed events across the \code{n} risk quantiles. Risk and event count differences by decile are also included.
#'
#' @examples
#' library(riskRegression)
#'
#' # Simulate Data
#' sim.data <- sampleData(100, outcome = 'competing.risks')
#'
#' # Fit Fine-Gray Model
#' fgr.mod <- FGR(formula = Hist(time, event) ~ X6 + X7 + X8 + X9,
#'                data = sim.data, cause = 1)
#'
#' # Make predictions
#' horizon <- median(sim.data$time)
#' sim.data$preds <- predictRisk(fgr.mod, sim.data, cause = 1, time = horizon)
#'
#' calibrate(sim.data, horizon, 10, preds, time, events, 1)
#' calibrate2(sim.data, horizon, 10, preds, time, events, 1)
#' calibrate3(sim.data, horizon, 10, preds, time, events, 1)
#'
#' @import dplyr
#'
#' @export
#'

calibrate <- function(data, horizon, ng, predvar, time, cens, cause) {
  time <- dplyr::enquo(time)
  cens <- dplyr::enquo(cens)
  predvar <- dplyr::enquo(predvar)

  data <- data %>%
    dplyr::mutate(time_t1c = dplyr::if_else(!!time > horizon, horizon, !!time),
           fail_t1c = dplyr::if_else(!!time > horizon, 0, !!cens),
           groups = dplyr::ntile(!!predvar, ng))

  exp <- data %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(tot = dplyr::n(),
              exp_risk = mean(!!predvar),
              exp_events = exp_risk * tot)

  obs_risk <- obs_events <- vector('double', ng)

  # Inner for loop
  for (i in 1:ng){
    dd <- data %>%
      dplyr::filter(groups==i)

    cif <- validstats::cif(timestop = dd$time_t1c,
                           censvar = dd$fail_t1c,
                           casetype = cause)

    dd2 <- dd %>%
      dplyr::filter(fail_t1c==cause, time_t1c<horizon) %>%
      dplyr::summarise(timeval = max(time_t1c)) %>%
      dplyr::pull(timeval)

    obs_events[i] <- nrow(dd) * cif$ci[which(cif$times==dd2)]

    obs_risk[i] <- cif$ci[which(cif$times==dd2)]
  }

  ret <- exp %>%
    dplyr::mutate(obs_risk = obs_risk,
           obs_events = obs_events,
           event_diff = obs_events - exp_events,
           risk_diff = obs_risk - exp_risk)

  return(ret)

}

# This is the same as Olli's function validstats::cical
calibrate2 <- function(data, horizon, ng, predvar, time, cens, cause) {
  time <- dplyr::enquo(time)
  cens <- dplyr::enquo(cens)
  predvar <- dplyr::enquo(predvar)

  data <- data %>%
    dplyr::mutate(time_t1c = dplyr::if_else(!!time > horizon, horizon, !!time),
           fail_t1c = dplyr::if_else(!!time > horizon, 0, !!cens),
           groups = cut(!!predvar, quantile(!!predvar,
                                            probs = seq(0, 1, length = ng + 1),
                                            na.rm = TRUE,
                                            type = 7), include.lowest = TRUE,
                        right = FALSE, labels = c(1:ng)))

  exp <- data %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(tot = dplyr::n(),
              exp_risk = mean(!!predvar),
              exp_events = exp_risk * tot)

  obs_risk <- obs_events <- vector('double', ng)

  # Inner for loop
  for (i in 1:ng){
    dd <- data %>%
      dplyr::filter(groups==i)

    cif <- with(dd, cif(timestop = time_t1c,
                        censvar = fail_t1c,
                        casetype = cause))

    dd2 <- dd %>%
      dplyr::filter(fail_t1c==cause, time_t1c<horizon) %>%
      dplyr::summarise(timeval = max(time_t1c)) %>%
      dplyr::pull(timeval)

    obs_events[i] <- nrow(dd) * cif$ci[which(cif$times==dd2)]

    obs_risk[i] <- cif$ci[which(cif$times==dd2)]
  }

  ret <- exp %>%
    dplyr::mutate(obs_risk = obs_risk,
           obs_events = obs_events,
           event_diff = obs_events - exp_events,
           risk_diff = obs_risk - exp_risk)

  return(ret)

}

# Same as calibrate but uses cmprsk::cuminc instead of validstats::cif
# Thus is isn't forced to use 32-bit R, like validstats
calibrate3 <- function(data, horizon, ng, predvar, time, cens, cause) {
  time <- dplyr::enquo(time)
  cens <- dplyr::enquo(cens)
  predvar <- dplyr::enquo(predvar)

  data <- data %>%
    dplyr::mutate(time_t1c = dplyr::if_else(!!time > horizon, horizon, !!time),
           fail_t1c = if_else(!!time > horizon, 0, !!cens),
           groups = dplyr::ntile(!!predvar, ng))

  exp <- data %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(tot = dplyr::n(),
              exp_risk = mean(!!predvar),
              exp_events = exp_risk * tot)

  obs_risk <- obs_events <- vector('double', ng)

  # Inner for loop
  for (i in 1:ng){
    dd <- data %>%
      dplyr::filter(groups==i)

    cif <- cmprsk::cuminc(ftime = dd$time_t1c,
                  fstatus = dd$fail_t1c,
                  cencode = 0)

    cif <- cif$`1 1` %>% dplyr::as_tibble()

    dd2 <- dd %>%
      dplyr::filter(fail_t1c==cause, time_t1c<horizon) %>%
      dplyr::summarise(timeval = max(time_t1c)) %>%
      dplyr::pull(timeval)

    obs_events[i] <- nrow(dd) * max(cif$est[which(cif$time==dd2)])

    obs_risk[i] <- max(cif$est[which(cif$time==dd2)])
  }

  ret <- exp %>%
    dplyr::mutate(obs_risk = obs_risk,
           obs_events = obs_events,
           event_diff = obs_events - exp_events,
           risk_diff = obs_risk - exp_risk)

  return(ret)

}
