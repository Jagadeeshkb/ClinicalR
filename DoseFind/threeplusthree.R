#3+3 study design

# Load necessary libraries
# dplyr: Provides tools for data manipulation
# ggplot2: Provides tools for data visualization
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
library(dplyr)
library(ggplot2)

# Sample data: dose levels and number of patients with dose-limiting toxicity (DLT)
# This data frame should be replaced with your actual trial data
data <- data.frame(
  dose_level = c(1, 2, 3, 4, 5),  # Dose levels administered in the trial
  patients = c(6, 6, 6, 6, 6),    # Number of patients treated at each dose level
  dlt_cases = c(0, 1, 2, 4, 5)    # Number of patients experiencing dose-limiting toxicity (DLT) at each dose level
)

# Function to determine Maximum Tolerated Dose (MTD) based on the 3+3 dose escalation design
determine_mtd <- function(dose_data) {
  # Calculate the dose-limiting toxicity (DLT) rate for each dose level
  dose_data <- dose_data %>%
    mutate(
      # Calculate the proportion of patients experiencing DLT at each dose level
      dlt_rate = dlt_cases / patients,
      # Determine if the dose level is a candidate for MTD
      # Here, we use a simple criterion: if DLT rate is below 33%, it's a candidate
      # You can adjust this threshold based on your specific criteria or guidelines
      mtd_candidate = ifelse(dlt_rate < 0.33, 1, 0)  
    )
  
  # Identify the highest dose level that is considered tolerable
  mtd_dose <- dose_data %>%
    filter(mtd_candidate == 1) %>%  # Filter for dose levels that are candidates
    arrange(desc(dose_level)) %>%   # Sort dose levels in descending order
    head(1)                         # Take the highest dose level that meets the criteria
  
  return(mtd_dose)  # Return the dose level that is identified as the MTD
}

# Call the function to determine the MTD using the provided data
mtd_result <- determine_mtd(data)

# Print the result to the console
print(paste("The Maximum Tolerated Dose (MTD) is Dose Level:", mtd_result$dose_level))

# Plotting: Dose levels vs. DLT rates
# This plot helps visualize the relationship between dose levels and the proportion of patients with DLT
ggplot(data, aes(x = dose_level, y = dlt_cases / patients)) +
  geom_point(size = 3) +                 # Add points for each dose level
  geom_line(aes(group = 1)) +            # Add a line connecting the points
  labs(
    title = "Dose Escalation and DLT Rates",  # Plot title
    x = "Dose Level",                        # X-axis label
    y = "DLT Rate"                           # Y-axis label
  ) +
  theme_minimal()                          # Use a minimal theme for a clean look
