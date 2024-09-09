# Load necessary libraries for Bayesian analysis and data manipulation
library(rstanarm)
library(dplyr)

# Set the number of cores to use for parallel processing
options(mc.cores = parallel::detectCores())

# Set parameters for the simulation
set.seed(123)  # Set seed for reproducibility
n_interim = 10   # Number of interim analyses
true_success_rate = 0.7   # True success rate for the simulation
sample_size_per_stage = 20   # Number of observations at each stage

# Create a function to simulate binary data (success/failure) based on a success probability
simulate_data <- function(n, p) {
  rbinom(n, size = 1, prob = p)
}

# Initialize a data frame to store results of interim analyses
interim_results <- data.frame(
  stage = integer(),        # Analysis stage
  successes = integer(),    # Number of successes observed
  total = integer(),        # Total number of observations
  posterior_mean = numeric(), # Posterior mean of the success rate
  posterior_sd = numeric()   # Posterior standard deviation of the success rate
)

# Loop through each interim analysis stage
for (stage in 1:n_interim) {
  # Simulate the data for the current stage
  successes <- sum(simulate_data(sample_size_per_stage, true_success_rate))
  total <- stage * sample_size_per_stage  # Total number of observations up to this stage
  
  # Update the results data frame with the current stage's data
  interim_results <- rbind(interim_results, data.frame(
    stage = stage,
    successes = successes,
    total = total,
    posterior_mean = NA,
    posterior_sd = NA
  ))
  
  # Set prior parameters for Bayesian analysis (Beta prior)
  alpha_prior <- 1
  beta_prior <- 1
  
  # Calculate posterior parameters based on observed successes and failures
  alpha_post <- alpha_prior + successes
  beta_post <- beta_prior + (total - successes)
  
  # Calculate the posterior mean and standard deviation
  posterior_mean <- alpha_post / (alpha_post + beta_post)
  posterior_sd <- sqrt((alpha_post * beta_post) / ((alpha_post + beta_post)^2 * (alpha_post + beta_post + 1)))
  
  # Update the results data frame with the calculated posterior estimates
  interim_results$posterior_mean[stage] <- posterior_mean
  interim_results$posterior_sd[stage] <- posterior_sd
  
  # Print the interim results after each stage
  print(interim_results)
}

# Plot the posterior mean of the success rate over the stages of analysis
plot(interim_results$stage, interim_results$posterior_mean,
     type = "b", xlab = "Interim Analysis Stage", ylab = "Posterior Mean of Success Rate",
     main = "Posterior Mean of Success Rate Over Stages")
