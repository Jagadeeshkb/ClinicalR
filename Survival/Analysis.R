library(survival)
library(timereg)
library(cmprsk)

# 1) Survival function
fit <- survfit(Surv(time, status) ~ 1, data = lung)
a = as.character(summary(fit)$time)
Suv = c(summary(fit)$surv)
names(Suv) <- a # Survival function values at different times
plot(Suv,type="b",xlab="Days")

# 2) Estimate the hazard function using a kernel density approach
time <- fit$time
n.event <- fit$n.event
hazard <- n.event / (fit$n.risk * diff(c(0, time))) # Basic hazard function estimation
names(hazard) <- a
hazard

# 3) Cumulative Hazard Function (Λ(t)):
cumulative_hazard <- summary(fit)$cumhaz
names(cumulative_hazard) <- a
cumulative_hazard

# 4) Probability Density Function (PDF) (f(t))
pdf_estimate <- diff(1 - summary(fit)$surv) / diff(c(0, fit$time)) # Approximation

# 5) Cumulative Distribution Function (CDF) (F(t))
cdf <- 1 - summary(fit)$surv
names(cdf) <- a
View(cdf)

# 6 Median Survival Time
median_time <- summary(fit)$table["median"]
median_time

# 7 Mean Survival Time
mean_time <- summary(fit)$table["mean"]

# 8 Restricted Mean Survival Time (RMST)
restricted_mean <- summary(fit, rmean = 500)$table["*rmean"]

# 9 Survival Rate
survival_rate <- summary(fit, times = c(365, 1825))$surv # 1-year and 5-year survival rates

# 10 Kaplan-Meier Estimate
plot(fit)

# 11 Log-Rank Test
fit <- survdiff(Surv(time, status) ~ sex, data = lung)
print(fit)

# 12 Cox Proportional Hazards Model
fit <- coxph(Surv(time, status) ~ age + sex, data = lung)
summary(fit)

# 13 Concordance Index (C-index)
fit <- coxph(Surv(time, status) ~ age + sex, data = lung)
concordance <- summary(fit)$concordance

# 14 Aalen’s Additive Model
fit <- aalen(Surv(time, status) ~ age + sex, data = lung)
summary(fit)

# 15 Competing Risks
fit <- cuminc(ftime = lung$time, fstatus = lung$status, group = lung$sex)
plot(fit)

# 16 Cumulative Incidence Function (CIF)
fit <- cuminc(ftime = lung$time, fstatus = lung$status, group = lung$sex)
plot(fit)

# 17 Time-Dependent Covariates
fit <- coxph(Surv(start, stop, status) ~ age + sex + time_dependent_covariate, data = lung_td)
summary(fit)
