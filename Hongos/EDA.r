setwd("/Users/iandalton/development/Github/finalPredictiva/Hongos")
data <- read.csv("mushrooms_clean.csv")

library(ggplot2)
library(dplyr)
library(tidyverse)

# getting basic info about the data
str(data)
summary(data)

# checking for missing values
colSums(is.na(data))

# checking for duplicated rows
data[duplicated(data), ]

# checking for duplicated columns
data[, duplicated(t(data))]

# The color pallette should be R42 G89 B137 and 127 128 129 for the other witch is in hex #2A5989 and #7F8081

# graphing the distribution of the target variable as percerntage with the palette
ggplot(data, aes(x = class, fill = class)) +
  geom_bar() +
  scale_fill_manual(values = c("#2A5989", "#7F8081")) +
  labs(title = "Distribution de clases", x = "Class", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -1)


plot_count <- function(df, x) {
  group <- df %>%
    group_by(!!sym(x), class) %>%
    summarise(Count = n()) %>%
    ungroup()

  return(ggplot(group, aes_string(x = x, y = "Count", fill = "class")) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#2A5989", "#7F8081")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Cantidad de", x)) +
    ylab("Count"))
}

# List of column names
cols <- colnames(data)[-1]

# Initialize an empty list to store the plots
plot_list <- list()

# Loop over the columns and append each plot to the list
for (column in cols) {
  plot_list[[column]] <- plot_count(data, column)
}

data_enconded <- read.csv("mushrooms_clean_encoded.csv")

# Cluster the plots in a grid

constant_cols <- sapply(data_enconded, function(x) length(unique(x)) == 1)

df_c <- data_enconded[, !constant_cols] # Remove constant columns
variance_threshold <- 0.01 # Adjust as needed
df_c <- df_c[, sapply(df_c, function(x) var(x, na.rm = TRUE) > variance_threshold)] # Remove near-constant columns

library(factoextra)
hopkins <- factoextra::get_clust_tendency(df_c, n = 35, seed = 321)
cat("Hopkins =", hopkins$hopkins_stat)

# 0.814 -> Se puede hacer el cluster


# elbow
fviz_nbclust(df_c,
  FUNcluster = hcut, method = "wss", k.max = 20,
  diss = dist(df_c, method = "manhattan"), hc_method = "average"
)
dim(data)
# silhouette
fviz_nbclust(df_c,
  FUNcluster = hcut, method = "silhouette", k.max = 15,
  diss = dist(df_c, method = "manhattan"), hc_method = "average"
)



km <- kmeans(df_c, 13, nstart = 50)
fviz_cluster(km, data = df_c, ellipse.type = "convex", geom = "point")

df_c$cluster <- as.factor(km$cluster)


density_plots <- lapply(names(df_c)[1:(ncol(df_c) - 1)], function(col) {
  ggplot(df_c, aes_string(x = col, fill = "cluster")) +
    geom_density(alpha = 0.5) +
    labs(title = paste(col)) +
    theme_minimal()
})

grid.arrange(grobs = density_plots, ncol = 4)


# Calculate the standard deviation for each column by cluster
std_devs <- df_c %>%
  group_by(cluster) %>%
  summarise(across(everything(), sd))

# Convert all columns to the same type
std_devs[] <- lapply(std_devs, as.numeric)

# Reshape the data frame into a long format
std_devs_long <- tidyr::pivot_longer(std_devs, everything(), names_to = "cluster", values_to = "std_dev")


# Sort the data frame by the standard deviation
std_devs_sorted <- std_devs_long[order(-std_devs_long$std_dev), ]
# remove the clusters named cluster
std_devs_sorted <- std_devs_sorted[-c(1:13), ]

# Select the top 12 clusters
top_12_clusters <- std_devs_sorted$cluster[1:12]

# Generate density plots for these clusters
density_plots <- lapply(top_12_clusters, function(cluster) {
  ggplot(df_c, aes_string(x = cluster, fill = "cluster")) +
    geom_density(alpha = 0.5) +
    labs(title = paste(cluster)) +
    theme_minimal() +
    guides(fill = "none") # Do not display color legend
})

# Arrange the plots
grid.arrange(grobs = density_plots, ncol = 4)
ggplot(df_c, aes_string(x = "class_venenoso", fill = "cluster")) +
  geom_density(alpha = 0.5) +
  labs(title = paste("class_venenoso")) +
  theme_minimal()

# Arrange the plots
grid.arrange(grobs = density_plots, ncol = 4)

library(gridExtra)
grid.arrange(grobs = density_plots, ncol = 4)


df_cat <- data
df_cat <- data.frame(lapply(df_cat, function(col) {
  if (is.numeric(col)) {
    as.character(col)
  } else {
    col
  }
}))
library(FactoMineR)
mca <- MCA(df_cat, graph = T)

fviz_mca_var(mca,
  col.var = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE, # Avoid text overlapping
  select.var = list(cos2 = 0.1), # Choose variables with cos2 >= 0.6s
  ggtheme = theme_minimal()
)
