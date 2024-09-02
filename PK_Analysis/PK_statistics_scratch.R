# Load necessary libraries
library(dplyr)   # Provides data manipulation functions
library(ggplot2) # For plotting (optional, not used in this script but useful for visualization)
library(zoo)
# Example data
# pk_data <- data.frame(time = c(...), concentration = c(...))
# Replace the above line with your actual data.

# Function to calculate Cmax (Maximum Concentration)
calc_Cmax <- function(concentration) {
  # Return the maximum value from the concentration vector
  return(max(concentration, na.rm = TRUE))
}

# Function to calculate Tmax (Time to Maximum Concentration)
calc_Tmax <- function(time, concentration) {
  # Find the time at which the maximum concentration occurs
  return(time[which.max(concentration)])
}

# Function to calculate AUC (Area Under the Curve)
calc_AUC <- function(time, concentration) {
  # Use the trapezoidal rule to approximate the area under the curve
  # diff(time) calculates the differences between consecutive time points
  # rollapply is used to compute the mean concentration over intervals
  auc <- sum(diff(time) * rollapply(concentration, width = 2, FUN = mean, align = "left", fill = NA), na.rm = TRUE)
  return(auc)
}

# Function to calculate t1/2 (Half-life)
calc_half_life <- function(time, concentration) {
  # Fit an exponential decay model to estimate the elimination rate constant (Ke)
  model <- nls(concentration ~ A * exp(-Ke * time), start = list(A = max(concentration), Ke = 0.1))
  Ke <- coef(model)["Ke"]
  # Calculate half-life from the elimination rate constant
  return(log(2) / Ke)
}

# Function to calculate CL (Clearance)
calc_Clearance <- function(AUC, dose) {
  # Clearance is the dose divided by the AUC
  return(dose / AUC)
}

# Function to calculate Vd (Volume of Distribution)
calc_Vd <- function(Cmax, dose) {
  # Volume of Distribution is the dose divided by the maximum concentration
  return(dose / Cmax)
}

# Function to calculate MRT (Mean Residence Time)
calc_MRT <- function(time, concentration) {
  # Calculate AUC
  AUC <- calc_AUC(time, concentration)
  # MRT is the weighted average time of the drug in the system
  return(sum(concentration * time) / AUC)
}

# Function to calculate Bioavailability (F)
calc_Bioavailability <- function(AUC_oral, AUC_IV, dose_IV, dose_oral) {
  # Bioavailability is the fraction of the oral dose that reaches systemic circulation compared to IV dose
  return((AUC_oral / AUC_IV) * (dose_IV / dose_oral))
}

# Function to calculate Cmin (Minimum Concentration)
calc_Cmin <- function(concentration) {
  # Return the minimum value from the concentration vector
  return(min(concentration, na.rm = TRUE))
}

# Function to calculate Ke (Elimination Rate Constant)
calc_Ke <- function(time, concentration) {
  # Fit an exponential decay model to estimate the elimination rate constant (Ke)
  model <- nls(concentration ~ A * exp(-Ke * time), start = list(A = max(concentration), Ke = 0.1))
  return(coef(model)["Ke"])
}

# Function to calculate Lag Time (Tlag)
calc_LagTime <- function(time, concentration) {
  # Tlag is the time before the drug starts to significantly increase in concentration
  # Here, we're assuming that the lag time is when the concentration first becomes greater than zero
  return(min(time[concentration > 0], na.rm = TRUE))
}

# Function to calculate Fluctuation
calc_Fluctuation <- function(Cmax, Cmin) {
  # Fluctuation is the percentage difference between Cmax and Cmin
  return((Cmax - Cmin) / Cmin * 100)
}

# Example usage with mock data
pk_data <- data.frame(time = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                      concentration = c(0, 2, 5, 8, 10, 9, 7, 5, 3, 2, 1))

# Calculate PK metrics
Cmax <- calc_Cmax(pk_data$concentration)
Tmax <- calc_Tmax(pk_data$time, pk_data$concentration)
AUC <- calc_AUC(pk_data$time, pk_data$concentration)
t1_2 <- calc_half_life(pk_data$time, pk_data$concentration)

# Assuming a dose of 100 units for the following calculations
dose <- 100
CL <- calc_Clearance(AUC, dose)
Vd <- calc_Vd(Cmax, dose)
MRT <- calc_MRT(pk_data$time, pk_data$concentration)

# Assuming an IV AUC of 150 and oral AUC of 75 for Bioavailability calculation
F <- calc_Bioavailability(AUC_oral = 75, AUC_IV = 150, dose_IV = 100, dose_oral = 100)
Cmin <- calc_Cmin(pk_data$concentration)
Ke <- calc_Ke(pk_data$time, pk_data$concentration)
LagTime <- calc_LagTime(pk_data$time, pk_data$concentration)
Fluctuation <- calc_Fluctuation(Cmax, Cmin)

# Print results
cat("Cmax:", Cmax, "\n")
cat("Tmax:", Tmax, "\n")
cat("AUC:", AUC, "\n")
cat("t1/2:", t1_2, "\n")
cat("Clearance:", CL, "\n")
cat("Volume of Distribution:", Vd, "\n")
cat("Mean Residence Time:", MRT, "\n")
cat("Bioavailability (F):", F, "\n")
cat("Cmin:", Cmin, "\n")
cat("Elimination Rate Constant (Ke):", Ke, "\n")
cat("Lag Time (Tlag):", LagTime, "\n")
cat("Fluctuation:", Fluctuation, "\n")
