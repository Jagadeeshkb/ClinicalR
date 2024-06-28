# Required library
if(!require(PKNCA)){
  install.packages("PKNCA")
}

library(PKNCA)
library(dplyr)
## Load the PK concentration data
d_conc <-
  as.data.frame(datasets::Theoph) %>%
  mutate(Subject=as.numeric(as.character(Subject)))
## Generate the dosing data
d_dose <- d_conc[d_conc$Time == 0,]
d_dose$Time <- 0

## Create a concentration object specifying the concentration, time, and
## subject columns.  (Note that any number of grouping levels is
## supported; you are not restricted to just grouping by subject.)
conc_obj <-
  PKNCAconc(
    d_conc,
    conc~Time|Subject
  )
## Create a dosing object specifying the dose, time, and subject
## columns.  (Note that the grouping factors should be the same as or a
## subset of the grouping factors for concentration, and the grouping
## columns must have the same names between concentration and dose
## objects.)
dose_obj <-
  PKNCAdose(
    d_dose,
    Dose~Time|Subject
  )
## Combine the concentration and dosing information both to
## automatically define the intervals for NCA calculation and provide
## doses for calculations requiring dose.
data_obj <- PKNCAdata(conc_obj, dose_obj)

## Calculate the NCA parameters
results_obj <- pk.nca(data_obj)

## Summarize the results
summary(results_obj)


#CMAX
cmax <- max(concentration)
print(cmax)

# Extract Tmax
tmax <- time[which.max(concentration)]
print(tmax)

# Calculate AUC using the trapezoidal rule
auc <- pk.calc.auc(time, concentration, interval=c(0, 24))
print(auc)

# Calculate terminal elimination rate constant (Ke) and half-life (t1/2)
ke <- pk.calc.half.life(time, concentration)
t_half <- log(2) / ke$half.life
print(t_half)

# Assuming IV bolus administration and known dose (D)
dose <- 100  # Example dose
cl <- dose / auc #clearence
vd <- cl / ke$half.life #volume distribution
print(cl)
print(vd)

# Calculate MRT
mrt <- pk.calc.mrt(time, concentration)
print(mrt)


# Bioavailability F, requires comparison with IV dose AUC
# Assuming F is calculated as AUC(po)/AUC(iv) * Dose(iv)/Dose(po)
auc_iv <- 500  # Example AUC from IV administration
dose_iv <- 100  # Example IV dose
dose_po <- 100  # Example oral dose
f <- (auc / auc_iv) * (dose_iv / dose_po)
print(f)

# Extract Cmin
cmin <- min(concentration)
print(cmin)

# Extract Elimination Rate Constant (Ke)
print(ke$half.life)

# Calculate Tlag
tlag <- min(time[concentration > 0])
print(tlag)

# Calculate Fluctuation
fluctuation <- cmax - cmin
print(fluctuation)
