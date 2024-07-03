#Heat maps
library(ggplot2)

# Example data
heatmap_data <- matrix(rnorm(100), nrow = 10, ncol = 10)
heatmap_data <- as.data.frame(as.table(heatmap_data))

ggplot(heatmap_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap", x = "X-axis", y = "Y-axis")


#-Volcano Plots
# Example data
volcano_data <- data.frame(
  logFC = rnorm(100),
  pvalue = runif(100, 0, 1)
)

volcano_data$significant <- volcano_data$pvalue < 0.05

ggplot(volcano_data, aes(x = logFC, y = -log10(pvalue), color = significant)) +
  geom_point() +
  labs(title = "Volcano Plot", x = "Log Fold Change", y = "-Log10 P-value")

#ROC Curves
library(pROC)

# Example data
roc_data <- data.frame(
  outcome = sample(c(0, 1), 100, replace = TRUE),
  predictor = runif(100)
)

roc_curve <- roc(roc_data$outcome, roc_data$predictor)
plot(roc_curve, main = "ROC Curve")


#Bland Altman 
library(BlandAltmanLeh)

# Example data
measurement1 <- rnorm(100)
measurement2 <- measurement1 + rnorm(100, sd = 0.1)

bland.altman.plot(measurement1, measurement2, main = "Bland-Altman Plot")



#- Heat Trees or Network Plots
library(igraph)

# Example data
g <- erdos.renyi.game(10, 0.5, directed = FALSE)

plot(g, main = "Network Plot")


#Sankey Diagrams
library(networkD3)

# Example data
sankey_data <- list(
  nodes = data.frame(name = c("A", "B", "C", "D")),
  links = data.frame(source = c(0, 0, 1, 1, 2, 2), 
                     target = c(2, 3, 2, 3, 3, 3), 
                     value = c(8, 4, 2, 4, 6, 4))
)

sankeyNetwork(Links = sankey_data$links, Nodes = sankey_data$nodes,
              Source = "source", Target = "target", Value = "value", NodeID = "name")


#--COr
library(corrplot)

# Example data
cor_data <- cor(mtcars)

corrplot(cor_data, method = "circle")

#Swarm Plots


# Example data
swarm_data <- data.frame(
  group = rep(c("A", "B"), each = 100),
  value = c(rnorm(100), rnorm(100, mean = 1))
)

ggplot(swarm_data, aes(x = group, y = value)) +
  geom_quasirandom() +
  labs(title = "Swarm Plot", x = "Group", y = "Value")

#Raincloud Plots
library(ggplot2)
library(cowplot)

# Example data
raincloud_data <- data.frame(
  group = rep(c("A", "B"), each = 100),
  value = c(rnorm(100), rnorm(100, mean = 1))
)

ggplot(raincloud_data, aes(x = group, y = value, fill = group)) +
  geom_flat_violin(position = position_nudge(x = 0.2)) +
  geom_point(position = position_jitter(width = 0.15), size = 1.5) +
  geom_boxplot(width = 0.1, position = position_nudge(x = 0.2)) +
  labs(title = "Raincloud Plot", x = "Group", y = "Value")

#----
library(ggplot2)

# Example data
hist_density_data <- rnorm(100)

ggplot(data.frame(value = hist_density_data), aes(x = value)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Plot", x = "Value", y = "Density")
