
# Generate some example data
set.seed(123)
n <- 100
time <- rexp(n, rate = 0.1)
status <- rbinom(n, 1, 0.7)
x1 <- rnorm(n)
x2 <- rnorm(n)

data <- data.frame(time = time, status = status, x1 = x1, x2 = x2)

# Function to calculate the risk score
risk_score <- function(beta, data) {
  exp(as.matrix(data[, -c(1, 2)]) %*% beta)
}

# Function to calculate the partial likelihood
partial_likelihood <- function(beta, data) {
  X <- as.matrix(data[, -c(1, 2)])  # Covariates
  y <- data$time
  status <- data$status
  
  # Calculate the risk score for each observation
  risk_scores <- risk_score(beta, data)
  
  # Calculate the partial likelihood
  likelihood <- 0
  for (i in 1:nrow(data)) {
    if (status[i] == 1) {
      likelihood <- likelihood + log(risk_scores[i] / sum(risk_scores[data$time >= y[i]]))
    }
  }
  -likelihood  # Return negative log-likelihood for optimization
}

# Initial values for beta (coefficients)
initial_beta <- rep(0, ncol(data) - 2)

# Optimize the partial likelihood to find the best beta
optim_results <- optim(par = initial_beta, fn = partial_likelihood, data = data, method = "BFGS")

# Extract the estimated coefficients
estimated_beta <- optim_results$par

# Print the results
cat("Estimated coefficients:\n")
print(estimated_beta)

# Load necessary library
library(survival)
# Optionally: Fit Cox model using the `survival` package for comparison
cox_model <- coxph(Surv(time, status) ~ x1 + x2, data = data)
summary(cox_model)
