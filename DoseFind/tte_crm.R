# Load necessary libraries
# dplyr: Provides functions for data manipulation
# ggplot2: Provides functions for visualization
# rjags: Provides tools for Bayesian analysis using JAGS (Just Another Gibbs Sampler)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("rjags")) install.packages("rjags", dependencies = TRUE)
library(dplyr)
library(ggplot2)
library(rjags)

# Sample data: Replace with actual trial data
# dose_level: Dose levels administered in the trial
# patients: Number of patients treated at each dose level
# time_to_dlt: Time until dose-limiting toxicity (DLT) or censoring time if no DLT occurred
# dlt_event: Indicator for DLT occurrence (1 = DLT occurred, 0 = censored)
data <- data.frame(
  dose_level = c(1, 2, 3, 4, 5),   # Dose levels
  patients = c(10, 10, 10, 10, 10),  # Number of patients per dose level
  time_to_dlt = c(40, 60, 30, 50, 70),  # Time to DLT or censoring time
  dlt_event = c(1, 1, 1, 1, 1)  # DLT event indicator (1 = DLT, 0 = censored)
)

# Define parameters for the TITE-CRM model
# These values may need adjustment based on prior knowledge or specifics of the trial
prior_alpha <- 1  # Shape parameter for the prior distribution
prior_beta <- 1   # Rate parameter for the prior distribution

# Function to fit a Bayesian time-to-event logistic regression model
fit_bayesian_model <- function(dose_data) {
  # Create a Bayesian model specification in JAGS
  model_string <- "
  model {
    for (i in 1:N) {
      dlt_event[i] ~ dbern(p[i])  # Bernoulli likelihood for DLT occurrence
      logit(p[i]) <- beta0 + beta1 * dose_level[i]  # Logistic regression model
      time_to_dlt[i] ~ dexp(lambda[i])  # Exponential distribution for time-to-event
      lambda[i] <- exp(beta0 + beta1 * dose_level[i])  # Hazard rate for time-to-event
    }
    # Priors for regression coefficients
    beta0 ~ dnorm(0, 0.01)
    beta1 ~ dnorm(0, 0.01)
  }
  "
  
  # Convert data to a list format for JAGS
  jags_data <- list(
    dlt_event = dose_data$dlt_event,
    time_to_dlt = dose_data$time_to_dlt,
    dose_level = dose_data$dose_level,
    N = nrow(dose_data)
  )
  
  # Fit the model using JAGS
  model <- jags.model(textConnection(model_string), data = jags_data, n.chains = 3)
  update(model, 1000)  # Burn-in
  
  # Sample from the posterior distribution
  samples <- coda.samples(model, c("beta0", "beta1"), n.iter = 5000)
  
  return(samples)
}

# Fit the Bayesian model to the sample data
bayesian_samples <- fit_bayesian_model(data)

# Extract posterior means for the regression coefficients
posterior_means <- summary(bayesian_samples)$statistics
beta0_mean <- posterior_means["beta0", "Mean"]
beta1_mean <- posterior_means["beta1", "Mean"]

# Calculate predicted DLT probabilities and hazard rates for each dose level
data <- data %>%
  mutate(
    predicted_dlt_rate = 1 / (1 + exp(-(beta0_mean + beta1_mean * dose_level))),  # Logistic function
    predicted_hazard_rate = exp(beta0_mean + beta1_mean * dose_level)  # Hazard rate for time-to-event
  )

# Define the target DLT rate and acceptable interval
target_dlt_rate <- 0.30
acceptable_interval <- 0.15

# Determine the acceptable DLT rate interval
lower_bound <- target_dlt_rate - acceptable_interval
upper_bound <- target_dlt_rate + acceptable_interval

# Identify dose levels within the acceptable interval
optimal_doses <- data %>%
  filter(predicted_dlt_rate >= lower_bound & predicted_dlt_rate <= upper_bound) %>%
  arrange(predicted_dlt_rate)  # Sort doses by predicted DLT rate

# Select the dose level with the closest predicted DLT rate to the target
if (nrow(optimal_doses) > 0) {
  optimal_dose <- optimal_doses %>%
    slice(1)  # Select the dose level closest to the target DLT rate
} else {
  # If no dose levels fall within the acceptable interval, select the closest dose level
  optimal_dose <- data %>%
    arrange(abs(predicted_dlt_rate - target_dlt_rate)) %>%
    slice(1)
}

# Print the result
print(paste("The Optimal Dose Level is:", optimal_dose$dose_level))

# Plotting: Dose levels vs. predicted DLT rates and hazard rates
# This plot shows the predicted DLT rates and hazard rates for each dose level
ggplot(data, aes(x = dose_level)) +
  geom_point(aes(y = predicted_dlt_rate), size = 3, color = "blue") +  # Predicted DLT rates
  geom_line(aes(y = predicted_dlt_rate), color = "blue") +
  geom_point(aes(y = predicted_hazard_rate), size = 3, color = "red") +  # Predicted hazard rates
  geom_line(aes(y = predicted_hazard_rate), color = "red") +
  geom_hline(yintercept = target_dlt_rate, linetype = "dashed", color = "green") +  # Target DLT rate
  geom_hline(yintercept = lower_bound, linetype = "dashed", color = "orange") +  # Lower bound of acceptable interval
  geom_hline(yintercept = upper_bound, linetype = "dashed", color = "orange") +  # Upper bound of acceptable interval
  labs(
    title = "TITE-CRM Design: Dose Levels vs. Predicted DLT and Hazard Rates",  # Plot title
    x = "Dose Level",  # X-axis label
    y = "Predicted Rate"  # Y-axis label
  ) +
  theme_minimal()  # Use a minimal theme for a clean look
