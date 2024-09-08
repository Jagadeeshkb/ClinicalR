library(survival)
library(ggplot2)

model <- coxph(Surv(time, status) ~ age, data = lung)

newdata <- expand.grid(age = 30:80, time = c(182, 365, 5*365), status = 1)

preds <- predict(model, newdata = newdata, type = 'expected', se.fit = TRUE)

newdata$pred <- exp(-preds$fit)
newdata$upper <- exp(-(preds$fit + 1.96 * preds$se.fit))
newdata$lower <- exp(-(preds$fit - 1.96 * preds$se.fit))



ggplot(newdata, aes(age, pred, color = factor(time))) +
  geom_ribbon(aes(ymax = upper, ymin = lower, fill = factor(time)),
              alpha = 0.2, color = NA) +
  geom_line() +
  scale_fill_discrete('Time', labels = c('6 months', '1 year', '5 years')) +
  scale_color_discrete('Time', labels = c('6 months', '1 year', '5 years')) +
  scale_y_continuous('Survival probability', labels = scales::percent) +
  theme_minimal() +
  ggtitle(paste('Survival Probability according to age',
                'at 6 months, 1 year, 5 years'))