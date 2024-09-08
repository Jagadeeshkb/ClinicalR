library(ggplot2)
library(survival)
library(survminer)

# Example dataset creation
set.seed(123)  # For reproducibility

# Generate example data
n <- 95  # Number of CTCA patients
time_ctca <- abs(rnorm(n, mean = 5, sd = 2))  # Example survival times in years
status_ctca <- sample(c(1, 0), n, replace = TRUE)  # 1 = event (death), 0 = censored

# SEER data (assuming a larger sample size for illustration)
n_seer <- 500
time_seer <- abs(rnorm(n_seer, mean = 5, sd = 2))
status_seer <- sample(c(1, 0), n_seer, replace = TRUE)

# Combine data into a data frame
data <- data.frame(
  time = c(time_ctca, time_seer),
  status = c(status_ctca, status_seer),
  group = c(rep("CTCA", n), rep("SEER", n_seer))
)

# Fit Kaplan-Meier survival curves
km_fit <- survfit(Surv(time, status) ~ group, data = data)


# Create the survival plot
ggsurvplot(
  km_fit,
  data = data,
  palette = c("blue", "red"),  # Colors for CTCA and SEER
  risk.table = TRUE,  # Show risk table
  pval = TRUE,  # Show p-value of the log-rank test
  conf.int = TRUE,  # Show confidence intervals
  xlab = "Time (Years)",  # X-axis label
  ylab = "Survival Probability",  # Y-axis label
  legend.title = "Group",  # Legend title
  legend.labs = c("CTCA", "SEER"),  # Legend labels
  ggtheme = theme_minimal()  # Use a minimal theme for a cleaner look
)


