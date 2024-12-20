# Load necessary libraries
library(cluster)  # For PAM clustering
library(ggplot2)  # For better plotting

# Load the CSV file
data <- read.csv('annualized_metrics.csv')

# Select the columns for clustering, excluding 'daily_avg_volume_usd'
# data_for_clustering <- data[, c("annual_avg_return", "annual_avg_volatility")]S

# Create the plot
p <- ggplot(data, aes(x = annual_avg_volatility, y = annual_avg_return, label = ticker)) +
  geom_point(color = "blue", size = 3) +         # Plot points
  geom_text(vjust = -0.5, hjust = 0.5) +         # Add ticker names as labelsS
  ggtitle("Return vs Volatility with Ticker Names") +  # Add title
  xlab("Annual Avg Volatility") +                # X-axis label
  ylab("Annual Avg Return") +                    # Y-axis label
  theme_minimal()                                # Use a clean theme

# Save the plot to a file
ggsave("return_vs_volatility_countries.png", plot = p, width = 10, height = 6, dpi = 300)

# # Set rownames to the tickers for labeling in plots
# rownames(data_for_clustering) <- data$ticker

# # K-means clustering
# set.seed(123)  # Ensure reproducibility
# kmeans_res <- kmeans(data_for_clustering, centers = 3)

# # PAM clustering
# pam_res <- pam(data_for_clustering, k = 3)

# # Single linkage hierarchical clustering
# hc_res <- hclust(dist(data_for_clustering), method = "average")

# # Plotting K-means clustering
# plot(data_for_clustering, col = kmeans_res$cluster, pch = 19, 
#      main = "K-means Clustering")
# text(data_for_clustering, labels = rownames(data_for_clustering), pos = 3, cex = 0.8)

# # Plotting PAM clustering
# plot(data_for_clustering, col = pam_res$clustering, pch = 19, 
#      main = "PAM Clustering")
# text(data_for_clustering, labels = rownames(data_for_clustering), pos = 3, cex = 0.8)

# # Plotting hierarchical clustering dendrogram
# plot(hc_res, labels = rownames(data_for_clustering), main = "Hierarchical Clustering Dendrogram")

# # Cut tree to form clusters and plot colors on dendrogram
# rect.hclust(hc_res, k = 3, border = 2:4)

# # Advanced plots with ggplot for better visualization
# # Create a combined dataframe for ggplot
# data_with_clusters <- data.frame(data_for_clustering, 
#                                  kmeans_cluster = factor(kmeans_res$cluster),
#                                  pam_cluster = factor(pam_res$clustering))

# # K-means clustering plot with ggplot
# ggplot(data_with_clusters, aes(x = annual_avg_return, y = annual_avg_volatility, 
#                                color = kmeans_cluster, label = rownames(data_with_clusters))) +
#   geom_point(size = 3) +
#   geom_text(vjust = -0.5, hjust = 0.5) +
#   ggtitle("K-means Clustering") +
#   theme_minimal()

# # PAM clustering plot with ggplot
# ggplot(data_with_clusters, aes(x = annual_avg_return, y = annual_avg_volatility, 
#                                color = pam_cluster, label = rownames(data_with_clusters))) +
#   geom_point(size = 3) +
#   geom_text(vjust = -0.5, hjust = 0.5) +
#   ggtitle("PAM Clustering") +
#   theme_minimal()
