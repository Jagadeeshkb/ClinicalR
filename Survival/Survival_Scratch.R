# Sample data
time <- c(5, 10, 15, 20, 25, 30)  # Example survival times

status <- c(1, 0, 1, 1, 0, 1)     # 1 for event occurred, 0 for censored

# 1) Survival Function S(t)
# This function calculates the survival function S(t) at a given time t.
# It returns the probability of surviving beyond time t.
survival_function <- function(time, status, t) {
  n <- length(time)
  if (t < min(time)) return(1)  # If time is before any observed times, survival is 1
  event_times <- time[status == 1]  # Times at which events occurred
  survival_probs <- numeric(length(event_times))
  for (i in seq_along(event_times)) {
    # Calculate the proportion of individuals surviving past each event time
    survival_probs[i] <- sum(time >= event_times[i] & status == 1) / sum(time >= event_times[i])
  }
  return(1 - mean(survival_probs))  # Return the complement of the mean survival probability
}

# 2) Hazard Function λ(t)
# This function calculates the hazard function λ(t) at a given time t.
# It returns the instantaneous rate of occurrence of the event at time t.
hazard_function <- function(time, status, t) {
  if (t < min(time)) return(0)  # If time is before any observed times, hazard is 0
  event_times <- time[status == 1]  # Times at which events occurred
  event_counts <- sum(event_times == t)  # Number of events at time t
  at_risk <- sum(time >= t)  # Number of individuals at risk at time t
  return(event_counts / at_risk)  # Return the hazard rate
}

# 3) Cumulative Hazard Function Λ(t)
# This function calculates the cumulative hazard function Λ(t) up to a given time t.
# It returns the cumulative hazard, which is the integral of the hazard function up to time t.
cumulative_hazard_function <- function(time, status, t) {
  event_times <- time[status == 1]  # Times at which events occurred
  # Calculate the hazard values for all event times up to t
  hazard_values <- sapply(event_times[event_times <= t], function(x) hazard_function(time, status, x))
  return(sum(hazard_values))  # Return the sum of hazard values
}

# 4) Probability Density Function (PDF) f(t)
# This function calculates the probability density function (PDF) f(t) at a given time t.
# It returns the density of the event occurring at time t.
pdf_function <- function(time, status, t) {
  hazard_function(time, status, t) * exp(-cumulative_hazard_function(time, status, t))
}

# 5) Cumulative Distribution Function (CDF) F(t)
# This function calculates the cumulative distribution function (CDF) F(t) at a given time t.
# It returns the probability of the event occurring by time t.
cdf_function <- function(time, status, t) {
  1 - survival_function(time, status, t)
}

# 6) Median Survival Time
# This function calculates the median survival time.
# It returns the time at which the survival probability is 0.5.
median_survival_time <- function(time, status) {
  time_sorted <- sort(time)  # Sort the survival times
  n <- length(time_sorted)
  if (n %% 2 == 1) {
    return(time_sorted[(n + 1) / 2])  # Return the middle value for odd number of times
  } else {
    return(mean(time_sorted[n / 2 + c(-1, 1)]))  # Return the average of the two middle values for even number of times
  }
}

# 7) Mean Survival Time
# This function calculates the mean survival time.
# It returns the average survival time for individuals who experienced the event.
mean_survival_time <- function(time, status) {
  mean(time[status == 1])  # Return the mean of times where the event occurred
}

# 8) Restricted Mean Survival Time (RMST)
# This function calculates the restricted mean survival time (RMST) up to a given time t_max.
# It returns the average survival time restricted to the interval [0, t_max].
restricted_mean_survival_time <- function(time, status, t_max) {
  time_event <- time[status == 1 & time <= t_max]  # Times of events up to t_max
  mean(time_event)  # Return the mean of these times
}

# 9) Survival Rate
# This function calculates the survival rate at a given time t.
# It returns the survival function S(t) at time t.
survival_rate <- function(time, status, t) {
  survival_function(time, status, t)
}

# 10) Kaplan-Meier Estimate
# This function calculates the Kaplan-Meier estimate of the survival function.
# It returns the estimated survival probabilities at each event time.
kaplan_meier_estimate <- function(time, status) {
  event_times <- sort(unique(time[status == 1]))  # Unique event times
  n <- length(time)
  km_estimates <- numeric(length(event_times))
  for (i in seq_along(event_times)) {
    t <- event_times[i]
    at_risk <- sum(time >= t)  # Number of individuals at risk at time t
    event_count <- sum(time == t & status == 1)  # Number of events at time t
    km_estimates[i] <- prod(1 - event_count / at_risk)  # Product of survival probabilities
  }
  return(km_estimates)
}

# Usage
t_max <- 30
print(paste("Survival Function S(t) at t = 20:", survival_function(time, status, 20)))
print(paste("Hazard Function λ(t) at t = 20:", hazard_function(time, status, 20)))
print(paste("Cumulative Hazard Function Λ(t) at t = 20:", cumulative_hazard_function(time, status, 20)))
print(paste("PDF f(t) at t = 20:", pdf_function(time, status, 20)))
print(paste("CDF F(t) at t = 20:", cdf_function(time, status, 20)))
print(paste("Median Survival Time:", median_survival_time(time, status)))
print(paste("Mean Survival Time:", mean_survival_time(time, status)))
print(paste("Restricted Mean Survival Time (RMST) up to t_max:", restricted_mean_survival_time(time, status, t_max)))
print(paste("Survival Rate at t = 20:", survival_rate(time, status, 20)))
print("Kaplan-Meier Estimate:")
print(kaplan_meier_estimate(time, status))
