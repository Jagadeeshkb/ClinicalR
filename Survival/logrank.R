# Example data
time <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)  # Survival times
status <- c(1, 1, 0, 1, 0, 1, 0, 1, 0, 1)        # Event status (1 = event, 0 = censored)
group <- c(1, 1, 1, 2, 2, 2, 1, 2, 1, 2)          # Group (1 or 2)

# Unique times for analysis
unique_times <- sort(unique(time))

# Initialize vectors for observed events and expected events
obs_events_group1 <- numeric(length(unique_times))
obs_events_group2 <- numeric(length(unique_times))
exp_events_group1 <- numeric(length(unique_times))
exp_events_group2 <- numeric(length(unique_times))

# Calculate observed events and expected events at each time
for (t in unique_times) {
  # Number of events at time t
  events_t <- status[time == t]
  groups_t <- group[time == t]
  
  # Observed events in each group
  obs_events_group1[which(unique_times == t)] <- sum(events_t[groups_t == 1])
  obs_events_group2[which(unique_times == t)] <- sum(events_t[groups_t == 2])
  
  # Number at risk in each group
  at_risk_group1 <- sum(time >= t & group == 1)
  at_risk_group2 <- sum(time >= t & group == 2)
  
  # Number of total at risk
  total_at_risk <- at_risk_group1 + at_risk_group2
  
  # Expected events in each group
  exp_events_group1[which(unique_times == t)] <- (at_risk_group1 / total_at_risk) * (obs_events_group1[which(unique_times == t)] + obs_events_group2[which(unique_times == t)])
  exp_events_group2[which(unique_times == t)] <- (at_risk_group2 / total_at_risk) * (obs_events_group1[which(unique_times == t)] + obs_events_group2[which(unique_times == t)])
}

# Calculate the test statistic
obs_events_total <- sum(obs_events_group1) + sum(obs_events_group2)
exp_events_total <- sum(exp_events_group1) + sum(exp_events_group2)
variance <- sum(((obs_events_group1 - exp_events_group1)^2 / exp_events_group1) + ((obs_events_group2 - exp_events_group2)^2 / exp_events_group2))


# Log-rank statistic
log_rank_statistic <- (sum(obs_events_group1) - sum(exp_events_group1))^2 / variance

# p-value
p_value <- 1 - pchisq(log_rank_statistic, df = 1)

# Print results
cat("Log-Rank Test Statistic:", log_rank_statistic, "\n")
cat("p-value:", p_value, "\n")
