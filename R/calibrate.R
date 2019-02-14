#' @title Competing Risk Calibration
#' @author Matthew T. Warkentin
#' @description This function will allow you to compute calibration metrics:
#'    - predicted and observed probabilities
#'    - predicted and observed events of interest
#'
#' @param data Dataframe to use for absolute risk predictions
#' @param horizon Time horizon for predictions
#' @param groups Number of deciles based on model-predicted risks (default is 10)
#' @param cause Integer value for the event-of-interest to calibrate predictions
#' @param model The returened model object from \code{\link[riskRegression]{riskRegression}}
#'
#' @return A list containing three elements. The first element returns a string
#'    detailing the time horizon for calibration. The second element is a chi-sq
#'    test statistic and p-value for the Hosmer-Lemeshow test (with DF=groups-1).
#'    The third element is a \code{tibble} containining the decile index
#'    along with the predicted and observed cumulative incidences, and the
#'    predicted and observed events across the \code{n} risk quantiles.
#'
#' @examples
#' library(riskRegression)
#'
#' # Simulate Data
#' sim.data <- sampleData(100, outcome = 'competing.risks')
#'
#' # Fit Fine-Gray Model
#' fgr.mod <- FGR(formula = Hist(time, event) ~ X7 + X6 + X8 + X9,
#'                data = sim.data, cause = 1)
#'
#' calibrate(sim.data, 2, 10, 1, fgr.mod)
#'
#' @import dplyr
#'
#' @export
#'

calibrate <- function(data, horizon, groups=10, cause=1, model) {

  calplot <- data %>%
    dplyr::mutate(pred = riskRegression::predictRisk(model, times = horizon,
                              cause = cause,
                              newdata = data),
           dec = ntile(pred, groups)) %>%
    dplyr::arrange(dec) %>%
    dplyr::select(time0, failure, pred, dec)

  exp_events <- obs_events <- exp_prob <- obs_prob <- rep(NA, groups)

  for (i in 1:groups) {
    to_use <- calplot %>%
      filter(dec == i)

    cif <- cmprsk::cuminc(ftime = to_use$time0,
                          fstatus = to_use$failure,
                          cencode = cause)[[cause]]

    exp_prob[i] <- mean(to_use$pred)
    exp_events[i] <- exp_prob[i] * nrow(to_use)

    to_use2 <- to_use %>%
      filter(failure==cause & time0<horizon) %>%
      summarise(timeval = max(time0))

    obs_prob[i] <- cif$est[which(cif$time==to_use2$timeval)]
    obs_events[i] <- obs_prob[i] * nrow(to_use)

  }

  chi2 <- rep(NA, groups)
  for (i in 1:groups) {
    chi2[i] <- (obs_events[i] - exp_events[i])^2 / exp_events[i]
  }
  test.st <- sum(chi2)
  p.val <- pchisq(test.st, df = groups-1, lower.tail = FALSE)

  return(list(paste('Calibration for ', horizon, '-year probability', sep = ''),
              cbind(test.st, p.val),
              as_tibble(cbind(decile=1:groups,
                              exp_prob,
                              exp_events,
                              obs_prob,
                              obs_events))))
}
