# Install necessary packages if not already installed
if (!require(survival)) install.packages("survival")
if (!require(survminer)) install.packages("survminer")

# Load libraries
library(survival)
library(survminer)

# Example dataset: You should replace this with your own dataset
# Simulate some example data
set.seed(123)
n <- 100
data <- data.frame(
  time = rexp(n, rate = 0.1),       # Survival times
  status = sample(c(0, 1), n, replace = TRUE), # Censoring indicator (0 = censored, 1 = event occurred)
  gene_expression = rnorm(n)         # Gene expression values
)

# Create a binary grouping based on median gene expression
data$group <- ifelse(data$gene_expression > median(data$gene_expression), "High", "Low")

# Fit the survival model
surv_fit <- survfit(Surv(time, status) ~ group, data = data)

# Plot the Kaplan-Meier survival curve
ggsurvplot(
  surv_fit, 
  data = data, 
  pval = TRUE,                             # Show p-value
  risk.table = TRUE,                       # Show risk table
  conf.int = TRUE,                        # Show confidence intervals
  palette = c("#E7B800", "#2E9FDF"),      # Custom colors for groups
  title = "Kaplan-Meier Survival Curve by Gene Expression Group",
  xlab = "Time",
  ylab = "Survival Probability"
)
