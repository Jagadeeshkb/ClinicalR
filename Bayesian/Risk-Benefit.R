# Load necessary libraries
library(rstan)
library(ggplot2)

# Simulate data
set.seed(123)

# Define sample sizes
n_treatment <- 100
n_control <- 100

# True parameters (for simulation)
true_response_rate_treatment <- 0.7
true_response_rate_control <- 0.5
true_ae_rate_treatment <- 0.3
true_ae_rate_control <- 0.25

# Simulate efficacy (response rates)
response_treatment <- rbinom(n_treatment, 1, true_response_rate_treatment)
response_control <- rbinom(n_control, 1, true_response_rate_control)

# Simulate safety (adverse event rates)
ae_treatment <- rbinom(n_treatment, 1, true_ae_rate_treatment)
ae_control <- rbinom(n_control, 1, true_ae_rate_control)

# Bayesian logistic regression for efficacy (response rates)
model_eff <- stan_model(model_code = '
  data {
    int<lower=0> n_treatment;
    int<lower=0> n_control;
    int<lower=0,upper=1> response_treatment[n_treatment];
    int<lower=0,upper=1> response_control[n_control];
  }
  parameters {
    real log_odds_treatment;
    real log_odds_control;
  }
  model {
    log_odds_treatment ~ normal(0, 1);  // Prior for log odds in treatment group
    log_odds_control ~ normal(0, 1);   // Prior for log odds in control group
    
    for (i in 1:n_treatment) {
      response_treatment[i] ~ bernoulli_logit(log_odds_treatment);
    }
    for (i in 1:n_control) {
      response_control[i] ~ bernoulli_logit(log_odds_control);
    }
  }
')

# Bayesian logistic regression for safety (adverse event rates)
model_safety <- stan_model(model_code = '
  data {
    int<lower=0> n_treatment;
    int<lower=0> n_control;
    int<lower=0,upper=1> ae_treatment[n_treatment];
    int<lower=0,upper=1> ae_control[n_control];
  }
  parameters {
    real log_odds_treatment;
    real log_odds_control;
  }
  model {
    log_odds_treatment ~ normal(0, 1);  // Prior for log odds in treatment group
    log_odds_control ~ normal(0, 1);   // Prior for log odds in control group
    
    for (i in 1:n_treatment) {
      ae_treatment[i] ~ bernoulli_logit(log_odds_treatment);
    }
    for (i in 1:n_control) {
      ae_control[i] ~ bernoulli_logit(log_odds_control);
    }
  }
')

# Prepare data for Stan
data_eff <- list(
  n_treatment = n_treatment,
  n_control = n_control,
  response_treatment = response_treatment,
  response_control = response_control
)

data_safety <- list(
  n_treatment = n_treatment,
  n_control = n_control,
  ae_treatment = ae_treatment,
  ae_control = ae_control
)

# Fit the Bayesian models
fit_eff <- sampling(model_eff, data = data_eff, chains = 4, iter = 1000)
fit_safety <- sampling(model_safety, data = data_safety, chains = 4, iter = 1000)

# Extract posterior samples
posterior_samples_eff <- extract(fit_eff)
posterior_samples_safety <- extract(fit_safety)

# Calculate probabilities that efficacy outweighs safety
prob_efficacy_outweighs_safety <- mean(posterior_samples_eff$log_odds_treatment > posterior_samples_safety$log_odds_treatment)

# Print the result
cat("Probability that efficacy outweighs safety:", prob_efficacy_outweighs_safety, "\n")

