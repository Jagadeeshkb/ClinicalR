#_----LDA
# Load necessary library
library(MASS)

# Sample dataset
data(iris)

# Fit LDA model
lda_model <- lda(Species ~ ., data = iris)

# Summary of the model
print(lda_model)

# Predict using the model
predictions <- predict(lda_model, iris)
head(predictions$class)



#-------
# Load necessary library
library(e1071)

# Sample dataset
data(iris)

# Fit SVM model
svm_model <- svm(Species ~ ., data = iris)

# Summary of the model
print(svm_model)

# Predict using the model
predictions <- predict(svm_model, iris)
(predictions)==iris$Species

# Load necessary library
library(rpart)
library(rpart.plot)

# Sample dataset
data(iris)

# Fit regression tree model
tree_model <- rpart(Species ~ ., data = iris)

# Summary of the model
print(tree_model)

# Plot the tree
rpart.plot(tree_model)

# Predict using the model
predictions <- predict(tree_model, iris)
head(predictions)


#---PCA
# Sample dataset
data(iris)

# Standardize the data
iris_standardized <- scale(iris[, -5])

# Perform PCA
pca_result <- prcomp(iris_standardized)

# Summary of PCA
summary(pca_result)

# Plot PCA results
biplot(pca_result)

#K-Means
# Sample dataset
data(iris)

# Standardize the data
iris_standardized <- scale(iris[, -5])

# Fit k-means clustering
set.seed(123) # For reproducibility
kmeans_result <- kmeans(iris_standardized, centers = 3)

# Summary of clustering result
print(kmeans_result)

# Plot clustering result
library(cluster)
clusplot(iris_standardized, kmeans_result$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)


#-----Hierarchical Clustering
# Sample dataset
data(iris)

# Standardize the data
iris_standardized <- scale(iris[, -5])

# Compute the distance matrix
dist_matrix <- dist(iris_standardized)

# Perform hierarchical clustering
hc_result <- hclust(dist_matrix)

# Plot the dendrogram
plot(hc_result)

# Cut the dendrogram into clusters
clusters <- cutree(hc_result, k = 3)
table(clusters, iris$Species)

