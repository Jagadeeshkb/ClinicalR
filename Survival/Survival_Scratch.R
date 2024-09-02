# Sample data
time <- c(5, 10, 15, 20, 25, 30)  # Example survival times

status <- c(1, 0, 1, 1, 0, 1)     # 1 for event occurred, 0 for censored

# 1) Survival Function S(t)
survival_function <- function(time, status, t) {
  n <- length(time)
  if (t < min(time)) return(1)  # If time is before any observed times, survival is 1
  event_times <- time[status == 1]
  survival_probs <- numeric(length(event_times))
  for (i in seq_along(event_times)) {
    survival_probs[i] <- sum(time >= event_times[i] & status == 1) / sum(time >= event_times[i])
  }
  return(1 - mean(survival_probs))
}

# 2) Hazard Function λ(t)
hazard_function <- function(time, status, t) {
  if (t < min(time)) return(0)  # If time is before any observed times, hazard is 0
  event_times <- time[status == 1]
  event_counts <- sum(event_times == t)
  at_risk <- sum(time >= t)
  return(event_counts / at_risk)
}

# 3) Cumulative Hazard Function Λ(t)
cumulative_hazard_function <- function(time, status, t) {
  event_times <- time[status == 1]
  hazard_values <- sapply(event_times[event_times <= t], function(x) hazard_function(time, status, x))
  return(sum(hazard_values))
}

# 4) Probability Density Function (PDF) f(t)
pdf_function <- function(time, status, t) {
  hazard_function(time, status, t) * exp(-cumulative_hazard_function(time, status, t))
}

# 5) Cumulative Distribution Function (CDF) F(t)
cdf_function <- function(time, status, t) {
  1 - survival_function(time, status, t)
}

# 6) Median Survival Time
median_survival_time <- function(time, status) {
  time_sorted <- sort(time)
  n <- length(time_sorted)
  if (n %% 2 == 1) {
    return(time_sorted[(n + 1) / 2])
  } else {
    return(mean(time_sorted[n / 2 + c(-1, 1)]))
  }
}

# 7) Mean Survival Time
mean_survival_time <- function(time, status) {
  mean(time[status == 1])
}

# 8) Restricted Mean Survival Time (RMST)
restricted_mean_survival_time <- function(time, status, t_max) {
  time_event <- time[status == 1 & time <= t_max]
  mean(time_event)
}

# 9) Survival Rate
survival_rate <- function(time, status, t) {
  survival_function(time, status, t)
}

# 10) Kaplan-Meier Estimate
kaplan_meier_estimate <- function(time, status) {
  event_times <- sort(unique(time[status == 1]))
  n <- length(time)
  km_estimates <- numeric(length(event_times))
  for (i in seq_along(event_times)) {
    t <- event_times[i]
    at_risk <- sum(time >= t)
    event_count <- sum(time == t & status == 1)
    km_estimates[i] <- prod(1 - event_count / at_risk)
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
