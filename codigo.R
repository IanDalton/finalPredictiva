# paquetes

install.packages("mice")
install.packages("heatmaply")
install.packages("ggmap")

library(ggmap)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(janitor)
library(skimr)
library(GGally)
library(ggpubr)
library(hexbin)
library(glue)
library(scales)
library(ggridges)
library(plotly)
library(factoextra)
library(amap)
library(sf)
library(igraph)
library(factoextra)
library(FactoMineR)
library(gridExtra)
library(mice)
library(heatmaply)
install.packages("pheatmap")
install.packages("dendextend")
library(pheatmap)
library(dendextend)
install.packages("viridis")
library(viridis)

data = read.csv("airbnb_clean.csv")
# on data baths replace Private with 1 and Half-bath with 0.5 and shared with 0
data$bath <- gsub("Private", 1, data$bath)
data$bath <- gsub("Half-bath", 0.5, data$bath)
data$bath <- gsub("Shared", 0, data$bath)
#turn into numeric
data$bath <- as.numeric(data$bath)


# Get the map



# Plot the map



# Create a map of Ciudad autonoma de buenos aires




# Missings

# Assuming missing_percentages is a data frame with one column
missing_percentages <- map_dbl(data, ~ mean(is.na(.)) * 100) %>% 
  set_names(names(data)) %>%  # Set names to match your column names
  sort(decreasing = TRUE) %>%
  as.data.frame()

# Filter columns with missing values
missing_cols <- missing_percentages %>%
  filter(missing_percentages[, 1] > 0)  # Adjust the column index accordingly

# Create a bar plot for columns with missing values
ggplot(missing_cols, aes(x = reorder(row.names(missing_cols), -!!sym(names(missing_cols)[1])), y = !!sym(names(missing_cols)[1]))) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  labs(title = "Percentage of Missing Values by Column",
       x = "Column",
       y = "Percentage Missing") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Center the plot title

#Matriz de Correlación

GGally::ggcorr(
  data, method=c("pairwise","spearman"),  
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)

# Clustering

cols <- c("minimum_nights", "number_of_reviews", "reviews_per_month", "calculated_host_listings_count", "availability_365", "number_of_reviews_ltm", "bedroom", "bed", "bath")

rob_scale <- function(x) {
  if (is.numeric(x)) {
    (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
  } else {
    warning("Non-numeric column detected. Skipping scaling for column.")
    x}}

df_c = data %>% 
select(cols) %>% 
mutate_all(rob_scale) %>%
as.data.frame()

df_c <- na.omit(df_c)

constant_cols <- sapply(df_c, function(x) length(unique(x)) == 1)

df_c <- df_c[, !constant_cols] # Remove constant columns
variance_threshold <- 0.01  # Adjust as needed
df_c <- df_c[, sapply(df_c, function(x) var(x, na.rm = TRUE) > variance_threshold)] # Remove near-constant columns

hopkins = factoextra::get_clust_tendency(df_c, n=100, seed=321)
cat("Hopkins =", hopkins$hopkins_stat)

###Hopkins = 0.9769666

# agrupaciones

# elbow
fviz_nbclust(df_c, FUNcluster=hcut, method="wss", k.max=20
             ,diss=dist(df_c, method="manhattan"), hc_method="average")

#silhouette
fviz_nbclust(df_c, FUNcluster=hcut, method="silhouette", k.max=20
             ,diss=dist(df_c, method="manhattan"), hc_method="average") 


km <- kmeans(df_c, 3, nstart = 50)
fviz_cluster(km, data = df_c, ellipse.type = "convex", geom = "point")

df_c$cluster <- as.factor(km$cluster)

density_plots <- lapply(names(df_c)[1:(ncol(df_c) - 1)], function(col) {
  ggplot(df_c, aes_string(x = col, fill = "cluster")) +
    geom_density(alpha = 0.5) +
    labs(title = paste(col, "por Clúster")) +
    theme_minimal()
})

grid.arrange(grobs = density_plots, ncol = 3) 

# Distancias


# MCA

df_cat = data %>% select(neighbourhood,room_type,type)

df_cat = data.frame(lapply(df_cat, function(col) {
  if (is.numeric(col)) {
    as.character(col)
  } else {
    col
  }
}))

mca = MCA(df_cat,graph=T)

fviz_mca_var(mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

#################################################################################

# outlier de precio

ggplot(data, aes(x = 1, y = price)) +
  geom_boxplot() +
  labs(x = NULL) + 
  scale_y_continuous(labels = scales::comma_format(scale = 1, big.mark = ",")) +
  theme_minimal()

data_filtrada <- filter(data, price <= 50000)

# histograma de precio --> hasta 50.000

ggplot(data_filtrada, aes(x = price)) +
  geom_histogram(fill = "black", alpha = 0.5) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal()

# boxplot de precio --> hasta 50.000

ggplot(data_filtrada, aes(x = 1, y = price)) +
  geom_boxplot() +
  labs(x = NULL) + 
  scale_y_continuous(labels = scales::comma_format(scale = 1, big.mark = ",")) +
  theme_minimal()

