
library(survival)

library(ggplot2)
library(ggfortify)

# Example data
data <- data.frame(
  time = c(5, 10, 15, 10, 25, 30, 45, 55, 60, 70),
  status = c(1, 1, 0, 1, 0, 1, 0, 0, 1, 1)  # 1=event occurred, 0=censored
)

# Create Survival object
surv_object <- Surv(time = data$time, event = data$status)

# Fit Kaplan-Meier model
km_fit <- survfit(surv_object ~ 1)

plot(km_fit, lty = c("solid", "dashed"), col = c("black", "grey"), xlab = "Survival Time In Days", ylab = "Survival Probabilities")

legend("topright", c("Clinic 1", "Clinic 2"), lty = c("solid", "dashed"), col = c("black", "grey"))


autoplot(km_fit) + 
  labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n", 
       title = "Survival Times Of \n Methadone Patients \n") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
        legend.title = element_text(face="bold", size = 10))
