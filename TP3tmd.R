library(MASS)
library(cluster)
library(e1071)
# data(crabs)

# crabs.log <- log(crabs[, 4:8])
# crabs.scaled <- scale(crabs.log)
# crabs.scaled.pca <- prcomp(crabs.scaled, scale = FALSE)
# crabs.pca.scaled <- scale(prcomp(crabs.log, scale = FALSE)$x)

# kmeans.log <- kmeans(crabs.log, centers = 2, nstart = 10)
# kmeans.scaled <- kmeans(crabs.scaled, centers = 2, nstart = 10)
# kmeans.scaled.pca <- kmeans(crabs.scaled.pca$x, centers = 2, nstart = 10)
# kmeans.pca.scaled <- kmeans(crabs.pca.scaled, centers = 2, nstart = 10)

# pam.log <- pam(crabs.log, k = 2, )
# pam.scaled <- pam(crabs.scaled, k = 2)
# pam.scaled.pca <- pam(crabs.scaled.pca$x, k = 2)
# pam.pca.scaled <- pam(crabs.pca.scaled, k = 2)

# hc.log.average <- hclust(dist(crabs.log), method = "average")
# hc.scaled.average <- hclust(dist(crabs.scaled), method = "average")
# hc.scaled.pca.average <- hclust(dist(crabs.scaled.pca$x), method = "average")
# hc.pca.scaled.average <- hclust(dist(crabs.pca.scaled), method = "average")

# hc.log.single <- hclust(dist(crabs.log), method = "single")
# hc.scaled.single <- hclust(dist(crabs.scaled), method = "single")
# hc.scaled.pca.single <- hclust(dist(crabs.scaled.pca$x), method = "single")
# hc.pca.scaled.single <- hclust(dist(crabs.pca.scaled), method = "single")

# hc.log.complete <- hclust(dist(crabs.log), method = "complete")
# hc.scaled.complete <- hclust(dist(crabs.scaled), method = "complete")
# hc.scaled.pca.complete <- hclust(dist(crabs.scaled.pca$x), method = "complete")
# hc.pca.scaled.complete <- hclust(dist(crabs.pca.scaled), method = "complete")

# # Step 1: Extract the true labels for Species and Gender
# true_species <- crabs[, 1]  # Species (B or O)
# true_gender <- crabs[, 2]   # Gender (M or F)

# Step 2: Define a helper function to create and display confusion matrices with optimized matching
compare_clusters <- function(true_labels, predicted_clusters, title) {
    # Convert true labels to factors if they are not already
    true_labels <- as.factor(true_labels)
    
    # Create a confusion matrix
    confusion_matrix <- table(True = true_labels, Predicted = predicted_clusters)
    
    # Use matchClasses to find the optimal permutation of predicted clusters
    class.match <- matchClasses(as.matrix(confusion_matrix), method = "exact")
    
    # Reorder the confusion matrix using the optimal match
    optimized_confusion_matrix <- confusion_matrix[, class.match]
    
    # Calculate accuracy using the optimized confusion matrix
    accuracy <- sum(diag(optimized_confusion_matrix)) / sum(optimized_confusion_matrix)
    
    print(title)
    print(optimized_confusion_matrix)
    print("----------------------------------------------------")
}

# # Step 3: Compare k-means clustering results with actual classes (Species and Gender)
# compare_clusters(true_species, kmeans.log$cluster, "K-means (Log-transformed Data) vs Species")
# compare_clusters(true_gender, kmeans.log$cluster, "K-means (Log-transformed Data) vs Gender")

# compare_clusters(true_species, kmeans.scaled$cluster, "K-means (Scaled Data) vs Species")
# compare_clusters(true_gender, kmeans.scaled$cluster, "K-means (Scaled Data) vs Gender")

# compare_clusters(true_species, kmeans.scaled.pca$cluster, "K-means (PCA Scaled Data) vs Species")
# compare_clusters(true_gender, kmeans.scaled.pca$cluster, "K-means (PCA Scaled Data) vs Gender")

# compare_clusters(true_species, kmeans.pca.scaled$cluster, "K-means (PCA + Scaled Data) vs Species")
# compare_clusters(true_gender, kmeans.pca.scaled$cluster, "K-means (PCA + Scaled Data) vs Gender")

# # Compare PAM clustering results with actual classes (Species and Gender)
# compare_clusters(true_species, pam.log$clustering, "PAM (Log-transformed Data) vs Species")
# compare_clusters(true_gender, pam.log$clustering, "PAM (Log-transformed Data) vs Gender")

# compare_clusters(true_species, pam.scaled$clustering, "PAM (Scaled Data) vs Species")
# compare_clusters(true_gender, pam.scaled$clustering, "PAM (Scaled Data) vs Gender")

# compare_clusters(true_species, pam.scaled.pca$clustering, "PAM (PCA Scaled Data) vs Species")
# compare_clusters(true_gender, pam.scaled.pca$clustering, "PAM (PCA Scaled Data) vs Gender")

# compare_clusters(true_species, pam.pca.scaled$clustering, "PAM (PCA + Scaled Data) vs Species")
# compare_clusters(true_gender, pam.pca.scaled$clustering, "PAM (PCA + Scaled Data) vs Gender")

# # Step 4: Create hierarchical clusters using three different linkage methods
# linkage_methods <- c("average", "single", "complete")
# for (method in linkage_methods) {
#     hc.log <- hclust(dist(crabs.log), method = method)
#     hc.scaled <- hclust(dist(crabs.scaled), method = method)
#     hc.scaled.pca <- hclust(dist(crabs.scaled.pca$x), method = method)
#     hc.pca.scaled <- hclust(dist(crabs.pca.scaled), method = method)
    
#     # Cut the hierarchical clusters to obtain 2 clusters and compare with actual classes
#     hc_clusters_log <- cutree(hc.log, k = 2)
#     hc_clusters_scaled <- cutree(hc.scaled, k = 2)
#     hc_clusters_scaled_pca <- cutree(hc.scaled.pca, k = 2)
#     hc_clusters_pca_scaled <- cutree(hc.pca.scaled, k = 2)
    
#     # Compare with the actual species
#     compare_clusters(true_species, hc_clusters_log, paste("Hierarchical Clustering (Log-transformed Data, Method =", method, ") vs Species"))
#     compare_clusters(true_gender, hc_clusters_log, paste("Hierarchical Clustering (Log-transformed Data, Method =", method, ") vs Gender"))
    
#     compare_clusters(true_species, hc_clusters_scaled, paste("Hierarchical Clustering (Scaled Data, Method =", method, ") vs Species"))
#     compare_clusters(true_gender, hc_clusters_scaled, paste("Hierarchical Clustering (Scaled Data, Method =", method, ") vs Gender"))
    
#     compare_clusters(true_species, hc_clusters_scaled_pca, paste("Hierarchical Clustering (PCA Scaled Data, Method =", method, ") vs Species"))
#     compare_clusters(true_gender, hc_clusters_scaled_pca, paste("Hierarchical Clustering (PCA Scaled Data, Method =", method, ") vs Gender"))
    
#     compare_clusters(true_species, hc_clusters_pca_scaled, paste("Hierarchical Clustering (PCA + Scaled Data, Method =", method, ") vs Species"))
#     compare_clusters(true_gender, hc_clusters_pca_scaled, paste("Hierarchical Clustering (PCA + Scaled Data, Method =", method, ") vs Gender"))
# }



# # Cargar el dataset
load("lampone.Rdata")

# summary(lampone)

# Separar las características y las clases
lampone.feat <- lampone[, -c(1, 143, 144)]
lampone.classes <- lampone[, c(1, 143)]

lampone.feat <- lampone.feat[, apply(lampone.feat, 2, var) != 0]

lampone.pca <- prcomp(lampone.feat, scale = TRUE)
biplot(lampone.pca, main = "Lampone PCA")


lampone.log <- log(lampone.feat)
lampone.scaled <- scale(lampone.log)

lampone.scaled <- lampone.scaled[, apply(lampone.scaled, 2, var) != 0]

lampone.scaled.pca <- prcomp(lampone.scaled, scale = FALSE)
lampone.pca.scaled <- scale(prcomp(lampone.log, scale = FALSE)$x)

kmeans.log <- kmeans(lampone.log, centers = 2, nstart = 10)
kmeans.scaled <- kmeans(lampone.scaled, centers = 2, nstart = 10)
kmeans.scaled.pca <- kmeans(lampone.scaled.pca$x, centers = 2, nstart = 10)
kmeans.pca.scaled <- kmeans(lampone.pca.scaled, centers = 2, nstart = 10)

pam.log <- pam(lampone.log, k = 2, )
pam.scaled <- pam(lampone.scaled, k = 2)
pam.scaled.pca <- pam(lampone.scaled.pca$x, k = 2)
pam.pca.scaled <- pam(lampone.pca.scaled, k = 2)

hc.log.average <- hclust(dist(lampone.log), method = "average")
hc.scaled.average <- hclust(dist(lampone.scaled), method = "average")
hc.scaled.pca.average <- hclust(dist(lampone.scaled.pca$x), method = "average")
hc.pca.scaled.average <- hclust(dist(lampone.pca.scaled), method = "average")

hc.log.single <- hclust(dist(lampone.log), method = "single")
hc.scaled.single <- hclust(dist(lampone.scaled), method = "single")
hc.scaled.pca.single <- hclust(dist(lampone.scaled.pca$x), method = "single")
hc.pca.scaled.single <- hclust(dist(lampone.pca.scaled), method = "single")

hc.log.complete <- hclust(dist(lampone.log), method = "complete")
hc.scaled.complete <- hclust(dist(lampone.scaled), method = "complete")
hc.scaled.pca.complete <- hclust(dist(lampone.scaled.pca$x), method = "complete")
hc.pca.scaled.complete <- hclust(dist(lampone.pca.scaled), method = "complete")

# Step 1: Extract the true labels for
true_anno <- lampone.classes[, 1]
true_type <- lampone.classes[, 2]

# Step 2: Compare k-means clustering results with actual classes
compare_clusters(true_anno, kmeans.log$cluster, "K-means (Log-transformed Data) vs Anno")
compare_clusters(true_type, kmeans.log$cluster, "K-means (Log-transformed Data) vs Type")

compare_clusters(true_anno, kmeans.scaled$cluster, "K-means (Scaled Data) vs Anno")
compare_clusters(true_type, kmeans.scaled$cluster, "K-means (Scaled Data) vs Type")

compare_clusters(true_anno, kmeans.scaled.pca$cluster, "K-means (PCA Scaled Data) vs Anno")
compare_clusters(true_type, kmeans.scaled.pca$cluster, "K-means (PCA Scaled Data) vs Type")

compare_clusters(true_anno, kmeans.pca.scaled$cluster, "K-means (PCA + Scaled Data) vs Anno")
compare_clusters(true_type, kmeans.pca.scaled$cluster, "K-means (PCA + Scaled Data) vs Type")

# Compare PAM clustering results with actual classes
compare_clusters(true_anno, pam.log$clustering, "PAM (Log-transformed Data) vs Anno")
compare_clusters(true_type, pam.log$clustering, "PAM (Log-transformed Data) vs Type")

compare_clusters(true_anno, pam.scaled$clustering, "PAM (Scaled Data) vs Anno")
compare_clusters(true_type, pam.scaled$clustering, "PAM (Scaled Data) vs Type")

compare_clusters(true_anno, pam.scaled.pca$clustering, "PAM (PCA Scaled Data) vs Anno")
compare_clusters(true_type, pam.scaled.pca$clustering, "PAM (PCA Scaled Data) vs Type")

compare_clusters(true_anno, pam.pca.scaled$clustering, "PAM (PCA + Scaled Data) vs Anno")
compare_clusters(true_type, pam.pca.scaled$clustering, "PAM (PCA + Scaled Data) vs Type")

# Step 4: Create hierarchical clusters using three different linkage methods
linkage_methods <- c("average", "single", "complete")
for (method in linkage_methods) {
    hc.log <- hclust(dist(lampone.log), method = method)
    hc.scaled <- hclust(dist(lampone.scaled), method = method)
    hc.scaled.pca <- hclust(dist(lampone.scaled.pca$x), method = method)
    hc.pca.scaled <- hclust(dist(lampone.pca.scaled), method = method)
    
    # Cut the hierarchical clusters to obtain 2 clusters and compare with actual classes
    hc_clusters_log <- cutree(hc.log, k = 2)
    hc_clusters_scaled <- cutree(hc.scaled, k = 2)
    hc_clusters_scaled_pca <- cutree(hc.scaled.pca, k = 2)
    hc_clusters_pca_scaled <- cutree(hc.pca.scaled, k = 2)
    
    # Compare with the actual classes
    compare_clusters(true_anno, hc_clusters_log, paste("Hierarchical Clustering (Log-transformed Data, Method =", method, ") vs Anno"))
    compare_clusters(true_type, hc_clusters_log, paste("Hierarchical Clustering (Log-transformed Data, Method =", method, ") vs Type"))
    
    compare_clusters(true_anno, hc_clusters_scaled, paste("Hierarchical Clustering (Scaled Data, Method =", method, ") vs Anno"))
    compare_clusters(true_type, hc_clusters_scaled, paste("Hierarchical Clustering (Scaled Data, Method =", method, ") vs Type"))
    
    compare_clusters(true_anno, hc_clusters_scaled_pca, paste("Hierarchical Clustering (PCA Scaled Data, Method =", method, ") vs Anno"))
    compare_clusters(true_type, hc_clusters_scaled_pca, paste("Hierarchical Clustering (PCA Scaled Data, Method =", method, ") vs Type"))
    
    compare_clusters(true_anno, hc_clusters_pca_scaled, paste("Hierarchical Clustering (PCA + Scaled Data, Method =", method, ") vs Anno"))
    compare_clusters(true_type, hc_clusters_pca_scaled, paste("Hierarchical Clustering (PCA + Scaled Data, Method =", method, ") vs Type"))
}



stocks <- read.csv('/content/metrics_stocks.csv')
stocks.feats = stocks[,-1]

stocks.pca<-prcomp(stocks.feats, scale = T)
biplot(stocks.pca, main= "Stocks PCA")

stocks.feats = stocks[-c(36,14,46,30,34),-1]

stocks.pca<-prcomp(stocks.feats, scale = T)
biplot(stocks.pca, main= "Stocks PCA")

kmeans <- kmeans(stocks.pca, centers = 2, nstart = 10)
biplot(stocks.pca, col = kmeans$cluster, main = "K-means Clustering (Stocks PCA)")  