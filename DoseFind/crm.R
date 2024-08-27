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
# dlt_cases: Number of patients experiencing dose-limiting toxicity (DLT) at each dose level
data <- data.frame(
  dose_level = c(1, 2, 3, 4, 5),  # Dose levels
  patients = c(10, 10, 10, 10, 10),  # Number of patients per dose level
  dlt_cases = c(1, 2, 3, 4, 5)    # Number of DLT cases at each dose level
)

# Define prior parameters for the Bayesian model
# These priors represent our beliefs about the dose-toxicity relationship before observing data
prior_alpha <- 1  # Shape parameter for the prior distribution of toxicity
prior_beta <- 1   # Rate parameter for the prior distribution of toxicity

# Function to fit a Bayesian logistic regression model for dose escalation
fit_bayesian_model <- function(dose_data) {
  # Create a model specification in JAGS
  model_string <- "
  model {
    for (i in 1:N) {
      dlt_cases[i] ~ dbin(p[i], patients[i])  # Binomial likelihood
      logit(p[i]) <- beta0 + beta1 * dose_level[i]  # Logistic regression
    }
    # Priors for regression coefficients
    beta0 ~ dnorm(0, 0.01)
    beta1 ~ dnorm(0, 0.01)
  }
  "
  
  # Convert data to list format for JAGS
  jags_data <- list(
    dlt_cases = dose_data$dlt_cases,
    patients = dose_data$patients,
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

# Calculate predicted DLT probabilities for each dose level
data <- data %>%
  mutate(
    predicted_dlt_rate = 1 / (1 + exp(-(beta0_mean + beta1_mean * dose_level)))  # Logistic function
  )

# Determine the dose with a predicted DLT rate closest to the target rate (e.g., 30%)
target_dlt_rate <- 0.30
data <- data %>%
  mutate(
    dlt_diff = abs(predicted_dlt_rate - target_dlt_rate)  # Difference from target rate
  )

# Find the dose level with the smallest difference from the target rate
mtd_dose <- data %>%
  arrange(dlt_diff) %>%
  slice(1)  # Select the dose level closest to the target DLT rate

# Print the result
print(paste("The Maximum Tolerated Dose (MTD) is Dose Level:", mtd_dose$dose_level))

# Plotting: Dose levels vs. predicted DLT rates
# This plot shows the predicted DLT rates for each dose level based on the Bayesian model
ggplot(data, aes(x = dose_level, y = predicted_dlt_rate)) +
  geom_point(size = 3, color = "blue") +  # Add points for predicted DLT rates
  geom_line(aes(group = 1), color = "blue") +  # Add a line connecting the points
  geom_hline(yintercept = target_dlt_rate, linetype = "dashed", color = "red") +  # Add a horizontal line for the target DLT rate
  labs(
    title = "Continual Reassessment Method: Dose Levels vs. Predicted DLT Rates",  # Plot title
    x = "Dose Level",  # X-axis label
    y = "Predicted DLT Rate"  # Y-axis label
  ) +
  theme_minimal()  # Use a minimal theme for a clean look
