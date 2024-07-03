library(DiagrammeR)
library(ggplot2)

library(survival)
library(survminer)

mermaid("
graph TD;
    A[Enrollment] --> B[Allocation];
    B --> C1[Intervention Group];
    B --> C2[Control Group];
    C1 --> D1[Follow-up];
    C2 --> D2[Follow-up];
    D1 --> E1[Analysis];
    D2 --> E2[Analysis];
")



# Baseline Characteristics Plot
df <- data.frame(
  group = rep(c("Treatment", "Control"), each = 50),
  age = rnorm(100, mean = 50, sd = 10)
)

ggplot(df, aes(x = group, y = age)) +
  geom_boxplot() +
  labs(title = "Baseline Age Distribution", x = "Group", y = "Age")



# Kaplan-Meier Survival Curves
surv_data <- data.frame(
  time = c(4, 3, 1, 1, 2, 2, 3),
  status = c(1, 1, 1, 0, 1, 1, 0),
  group = c(1, 1, 2, 2, 1, 2, 2)
)

fit <- survfit(Surv(time, status) ~ group, data = surv_data)
ggsurvplot(fit, data = surv_data, pval = TRUE)

#forestplot
library(forestplot)

# Example data
forest_data <- data.frame(
  variable = c("Outcome 1", "Outcome 2"),
  HR = c(0.8, 1.2),
  lower = c(0.6, 1.0),
  upper = c(1.1, 1.5)
)

forestplot(labeltext = forest_data$variable, 
           mean = forest_data$HR, 
           lower = forest_data$lower, 
           upper = forest_data$upper,
           title = "Forest Plot")

# AE plot
adverse_events <- data.frame(
  event = c("Headache", "Nausea", "Fatigue"),
  frequency = c(20, 15, 30),
  group = c("Treatment", "Treatment", "Treatment")
)

ggplot(adverse_events, aes(x = event, y = frequency, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Adverse Events Frequency", x = "Event", y = "Frequency")

#waterfall plot
waterfall_data <- data.frame(
  patient = 1:20,
  change = runif(20, min = -100, max = 100)
)

ggplot(waterfall_data, aes(x = reorder(patient, change), y = change)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Waterfall Plot of Tumor Size Changes", x = "Patient", y = "Change (%)")


#Lab Box
lab_results <- data.frame(
  group = rep(c("Treatment", "Control"), each = 50),
  value = rnorm(100, mean = 100, sd = 15)
)

ggplot(lab_results, aes(x = group, y = value)) +
  geom_boxplot() +
  labs(title = "Laboratory Results", x = "Group", y = "Value")

#Line
line_data <- data.frame(
  time = rep(1:10, 2),
  value = c(rnorm(10, mean = 100, sd = 15), rnorm(10, mean = 110, sd = 15)),
  group = rep(c("Treatment", "Control"), each = 10)
)

ggplot(line_data, aes(x = time, y = value, color = group)) +
  geom_line() +
  labs(title = "Lab Results Over Time", x = "Time", y = "Value")


#PK
pk_data <- data.frame(
  time = rep(1:10, 2),
  concentration = c(rnorm(10, mean = 50, sd = 10), rnorm(10, mean = 60, sd = 10)),
  group = rep(c("Dose 1", "Dose 2"), each = 10)
)

ggplot(pk_data, aes(x = time, y = concentration, color = group)) +
  geom_line() +
  labs(title = "Concentration-Time Curve", x = "Time (hours)", y = "Concentration (ng/mL)")

#Vital Signs and Other Continuous Measures

vital_data <- data.frame(
  time = rep(1:10, 2),
  bp = c(rnorm(10, mean = 120, sd = 5), rnorm(10, mean = 130, sd = 5)),
  group = rep(c("Treatment", "Control"), each = 10)
)

ggplot(vital_data, aes(x = time, y = bp, color = group)) +
  geom_line() +
  labs(title = "Blood Pressure Over Time", x = "Time (days)", y = "Blood Pressure (mmHg)")

#Scatter
scatter_data <- data.frame(
  var1 = rnorm(100),
  var2 = rnorm(100)
)

ggplot(scatter_data, aes(x = var1, y = var2)) +
  geom_point() +
  labs(title = "Scatter Plot of Continuous Variables", x = "Variable 1", y = "Variable 2")

#8. Quality of Life and Patient-Reported Outcomes
library(fmsb)

# Example data
quality_of_life <- data.frame(
  dimension = c("Physical", "Emotional", "Social", "Functional"),
  score_treatment = c(80, 70, 90, 60),
  score_control = c(75, 65, 85, 55)
)

# Preparing data
data_radar <- as.data.frame(t(quality_of_life[-1]))
colnames(data_radar) <- quality_of_life$dimension

radarchart(data_radar, axistype = 1, 
           pcol = c("red", "blue"), 
           plwd = 2, 
           title = "Quality of Life Radar Chart")


#--------
qol_data <- data.frame(
  time = rep(1:10, 2),
  score = c(rnorm(10, mean = 80, sd = 5), rnorm(10, mean = 70, sd = 5)),
  group = rep(c("Treatment", "Control"), each = 10)
)

ggplot(qol_data, aes(x = time, y = score, color = group)) +
  geom_line() +
  labs(title = "Quality of Life Scores Over Time", x = "Time (months)", y = "Score")

#-----------
subgroup_data <- data.frame(
  subgroup = c("Age < 50", "Age >= 50", "Male", "Female"),
  HR = c(0.75, 0.85, 0.90, 0.80),
  lower = c(0.60, 0.70, 0.75, 0.65),
  upper = c(0.95, 1.05, 1.10, 0.95)
)

forestplot(labeltext = subgroup_data$subgroup, 
           mean = subgroup_data$HR, 
           lower = subgroup_data$lower, 
           upper = subgroup_data$upper,
           title = "Subgroup Analysis Forest Plot")


#-Compliance and Exposure Plots

compliance_data <- data.frame(
  exposure = c(rep("Low", 50), rep("Medium", 30), rep("High", 20))
)

ggplot(compliance_data, aes(x = exposure)) +
  geom_bar() +
  labs(title = "Compliance Histogram", x = "Exposure Level", y = "Frequency")



#----
compliance_over_time <- data.frame(
  time = rep(1:10, 2),
  compliance = c(rnorm(10, mean = 90, sd = 5), rnorm(10, mean = 85, sd = 5)),
  group = rep(c("Treatment", "Control"), each = 10)
)

ggplot(compliance_over_time, aes(x = time, y = compliance, color = group)) +
  geom_line() +
  labs(title = "Compliance Over Time", x = "Time (days)", y = "Compliance (%)")


