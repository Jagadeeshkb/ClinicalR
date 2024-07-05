library(ggplot2)

# Sample data
set.seed(123)
data <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  z = sample(letters[1:3], 100, replace = TRUE)
)

# Plot with ggplot
ggplot(data, aes(x = x, y = y, color = z, shape = z, size = abs(x - y))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("blue", "green", "red")) +
  scale_shape_manual(values = c(1, 2, 3)) +
  scale_size_continuous(range = c(1, 5)) +
  labs(
    title = "Example Plot with ggplot2",
    x = "X Axis Label",
    y = "Y Axis Label",
    color = "Category",
    shape = "Category",
    size = "Difference",
    caption = "Data source: Randomly generated"
  ) +
  theme_minimal()




# Bar Plot


