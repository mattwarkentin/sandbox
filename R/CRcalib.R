

# Calibration plot for Competing Risk -------------------------------------

library(riskRegression)
library(dplyr)

install.packages('C:/Users/Olli/Dropbox/work/rpackage_olli/validstats_1.4.zip', repos=NULL)
library(validstats)

# Simulate Data
sim.data <- sampleData(1000, outcome = 'competing.risks')

# Fit Fine-Gray Model
fgr.mod <- FGR(formula = Hist(time, event) ~ X7 + X6 + X8 + X9, 
               data = sim.data, 
               cause = 1)

# Predict cumulative incidence at median follow-up time
sim.data$pred <- predictRisk(fgr.mod, sim.data, times = median(sim.data$time))

tau <- median(sim.data$time)
sim.data$timec <- ifelse(sim.data$time > tau, tau, sim.data$time)
sim.data$eventc <- ifelse(sim.data$time > tau, 0, sim.data$event)
addmargins(table(sim.data$event, sim.data$eventc))
plot(sim.data$time, sim.data$timec)

# Using Validstats --------------------------------------------------------

calib <- validstats::cical(p = sim.data$pred, 
                           ftime = sim.data$timec, 
                           censvar = sim.data$eventc,
                           casetype = 1, 
                           ngroups = 10)

plot(calib$expected, calib$observed,
     xlim = c(0,max(calib$observed, calib$expected)), 
     ylim = c(0,max(calib$observed, calib$expected)))
abline(a = 0, b = 1, lty = 2)


# Doing it manually -------------------------------------------------------

# Form deciles of predicted risk
matt <- sim.data %>%
  mutate(decile = ntile(pred, 10)) %>%
  arrange(decile) %>%
  select(time, event, pred, decile, everything())

# Vectorize for use in for loop
exp_events <- obs_events <- rep(NA, 10)

# Loop over 10 deciles and compute expected and observed events
for (i in 1:10) {
  
  # Only include decile i
  to_use <- matt %>%
    filter(decile==i)
  
  # CIF for decile i
  cif <- validstats::cif(timestop = to_use$time,
                         censvar = to_use$event,
                         casetype = 1)
  
  # Expected events
  exp_events[i] <- mean(to_use$pred) * nrow(to_use)
  
  # Find event time immediately prior to time horizon
  to_use2 <- to_use %>%
    filter(event==1 & time<median(sim.data$time)) %>%
    summarise(timeval = max(time))

  # Observed events
  obs_events[i] <- cif$ci[which(cif$times==to_use2$timeval)] * nrow(to_use)
  
  rm(list = c('to_use', 'to_use2', 'i'))
}


plot(exp_events, obs_events)
abline(a = 0, b = 1, lty = 2)


# Comparison of Methods ---------------------------------------------------

cbind('Matt Obs' = obs_events, 
      'cical Obs' = calib$observed,
      'Matt Exp' = exp_events, 
      'cical Exp' = calib$expected)
