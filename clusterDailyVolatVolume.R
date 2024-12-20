# Load necessary libraries
library(ggplot2)      # For better plotting
library(cluster)      # For PAM clustering
library(dplyr)        # For data manipulation
library(factoextra)   # For clustering visualization

# Load the CSV file into an R DataFrame
df <- read.csv('average_daily_metrics_with_volume.csv')

# Plot the daily average volume in USD with ticker names
p <- ggplot(df, aes(x = daily_avg_volume_usd, y = daily_volatility, label = ticker)) +
  geom_point(color = "blue", size = 5) +                 # Plot points
  geom_text(vjust = -0.5, hjust = 0.5) +                 # Add ticker names as labels
  ggtitle("Daily Volume vs Daily Volatility with Ticker Names") +  # Add title
  xlab("Avg Daily Volume (USD)") +                        # X-axis label
  ylab("Daily Volatility") +                               # Y-axis label
  theme_minimal()                                         # Use a clean theme

# Save the plot to a file
ggsave("volume_vs_volatility_plot.png", plot = p, width = 10, height = 6, dpi = 300)

# Exclude the Avg Daily Return column for clustering
data <- df %>%
  select(daily_avg_volume_usd, daily_volatility)

# K-means clustering
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(data, centers = 5)  # Change centers as needed
df$kmeans_cluster <- as.factor(kmeans_result$cluster)

# PAM clustering
pam_result <- pam(data, k = 5)  # Change k as needed
df$pam_cluster <- as.factor(pam_result$clustering)

# Hierarchical clustering
dist_matrix <- dist(data)  # Compute distance matrix
hclust_result <- hclust(dist_matrix, method = "single")  # Single linkage

# Add cluster information to the original dataframe
df$hclust_cluster <- cutree(hclust_result, k = 5)  # Change k as needed

# Plotting the clustering results for K-means
kmeans_plot <- ggplot(df, aes(x = daily_avg_volume_usd, y = daily_volatility, color = kmeans_cluster, label = ticker)) +
  geom_point(size = 4) +
  geom_text(vjust = -1, hjust = 1) +
  labs(title = "K-means Clustering (Volume)", x = "Daily Avg Volume (USD)", y = "Daily Volatility") +
  theme_minimal()

# Save K-means plot
ggsave("kmeans_clustering_volume_plot.png", plot = kmeans_plot, width = 10, height = 6, dpi = 300)

# Plotting the clustering results for PAM
pam_plot <- ggplot(df, aes(x = daily_avg_volume_usd, y = daily_volatility, color = pam_cluster, label = ticker)) +
  geom_point(size = 4) +
  geom_text(vjust = -1, hjust = 1) +
  labs(title = "PAM Clustering (Volume)", x = "Daily Avg Volume (USD)", y = "Daily Volatility") +
  theme_minimal()

# Save PAM plot
ggsave("pam_clustering_volume_plot.png", plot = pam_plot, width = 10, height = 6, dpi = 300)

# Save dendrogram as a file
png("dendrogram_volume_plot.png", width = 800, height = 600)
plot(hclust_result, labels = df$ticker, main = "Dendrogram of Hierarchical Clustering (Volume)", sub = "", xlab = "Tickers")
dev.off()

print("Clustering analysis completed based on daily average volume and plots saved.")
