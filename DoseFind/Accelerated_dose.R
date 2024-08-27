# Load necessary libraries
# dplyr: Provides functions for data manipulation
# ggplot2: Provides functions for creating visualizations
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
library(dplyr)
library(ggplot2)

# Sample data: Replace this with actual trial data
# dose_level: Dose levels administered in the trial
# patients: Number of patients treated at each dose level
# dlt_cases: Number of patients experiencing dose-limiting toxicity (DLT) at each dose level
data <- data.frame(
  dose_level = c(1, 2, 3, 4, 5),  # Dose levels
  patients = c(10, 10, 10, 10, 10),  # Number of patients per dose level
  dlt_cases = c(1, 2, 3, 4, 5)    # Number of DLT cases at each dose level
)

# Define the maximum acceptable DLT rate for dose escalation
# For example, if DLT rate > 30%, stop escalation at the previous dose
max_dlt_rate <- 0.30

# Function to perform Accelerated Dose Escalation
accelerated_dose_escalation <- function(dose_data, max_dlt_rate) {
  # Calculate DLT rate for each dose level
  dose_data <- dose_data %>%
    mutate(
      dlt_rate = dlt_cases / patients  # Proportion of patients with DLT
    )
  
  # Identify dose levels that have DLT rate below the maximum acceptable rate
  tolerable_doses <- dose_data %>%
    filter(dlt_rate <= max_dlt_rate) %>%  # Filter doses with acceptable DLT rates
    arrange(dose_level)  # Order doses in ascending order
  
  # Determine the highest dose level with an acceptable DLT rate
  mtd_dose <- if (nrow(tolerable_doses) > 0) {
    tolerable_doses %>% 
      slice_tail(n = 1)  # Select the highest dose level that is tolerable
  } else {
    # If no dose level is tolerable, return the last dose level as the highest safe dose
    dose_data %>%
      slice_tail(n = 1) 
  }
  
  return(mtd_dose)
}

# Apply the accelerated dose escalation function to the sample data
mtd_result <- accelerated_dose_escalation(data, max_dlt_rate)

# Print the result to the console
print(paste("The Maximum Tolerated Dose (MTD) is Dose Level:", mtd_result$dose_level))

# Plotting: Dose levels vs. DLT rates
# This plot helps visualize the relationship between dose levels and DLT rates
ggplot(data, aes(x = dose_level, y = dlt_cases / patients)) +
  geom_point(size = 3, color = "blue") +  # Add points for each dose level
  geom_line(aes(group = 1), color = "blue") +  # Add a line connecting the points
  geom_hline(yintercept = max_dlt_rate, linetype = "dashed", color = "red") +  # Add a horizontal line for the max DLT rate
  labs(
    title = "Accelerated Dose Escalation: Dose Levels vs. DLT Rates",  # Plot title
    x = "Dose Level",  # X-axis label
    y = "DLT Rate"     # Y-axis label
  ) +
  theme_minimal()  # Use a minimal theme for a clean look
