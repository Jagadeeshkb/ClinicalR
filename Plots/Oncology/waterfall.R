library(ggplot2)
library(dplyr)

# Sample cancer data
data <- data.frame(
  Phase = c("Initial", "Treatment Phase 1", "Treatment Phase 2", "Side Effects", "Final Outcome"),
  Change = c(1000, -200, -150, -50, NA)  # Example values in some unit
)

# Calculate cumulative values
data <- data %>%
  mutate(
    Cumulative = cumsum(replace(Change, is.na(Change), 0)),
    Start = lag(Cumulative, default = 0),
    End = Cumulative
  ) %>%
  mutate(
    Change = End - Start
  )

# Create the waterfall plot
ggplot(data, aes(x = Phase, y = Change, fill = Change > 0)) +
  geom_bar(stat = "identity", aes(y = Change, fill = Change > 0), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Change)), vjust = -0.5) +
  geom_segment(aes(xend = Phase, yend = Start), color = "black") +
  geom_point(aes(x = Phase, y = End), color = "black") +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal() +
  labs(title = "Waterfall Plot for Cancer Data", x = "Treatment Phase", y = "Change in Outcome") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
