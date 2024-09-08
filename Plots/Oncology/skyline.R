library(ggplot2)
library(dplyr)

# Example data frame
data <- data.frame(
  time = c(0, 1, 2, 3, 4, 5, 6),
  hr = c(1.2, 1.1, 1.3, 1.4, 1.2, 1.3, 1.1),
  lower_ci = c(1.1, 1.0, 1.2, 1.3, 1.1, 1.2, 1.0),
  upper_ci = c(1.3, 1.2, 1.4, 1.5, 1.3, 1.4, 1.2)
)


# Define a function to create the skyline plot
  ggplot(data, aes(x = time, y = hr)) +
    geom_line(color = "blue") +  # Line for HR
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.3) +  # CI ribbon
    geom_point(size = 3, color = "blue") +  # Points for HR
    labs(
      x = "Time to Progression",
      y = "Hazard Ratio",
      title = "Skyline Plot of Hazard Ratios and 95% CIs"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    scale_y_continuous(trans = 'log')  # Optional: log transformation for better visualization




