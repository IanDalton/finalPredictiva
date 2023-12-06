# paquetes

library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
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
  labs(
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

cols <- c("host_listings_count", "bathrooms", "bedrooms", "beds", "minimum_nights", "maximum_nights", "availability_30", "availability_60", "availability_90", "availability_365", "number_of_reviews",  "reviews_per_month", "is_shared_bathroom")

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

hopkins = factoextra::get_clust_tendency(df_c, n=35, seed=321)
cat("Hopkins =", hopkins$hopkins_stat)

###Hopkins = 0.9338175

# agrupaciones

# elbow
fviz_nbclust(df_c, FUNcluster=hcut, method="wss", k.max=20
             ,diss=dist(df_c, method="manhattan"), hc_method="average")

#silhouette
fviz_nbclust(df_c, FUNcluster=hcut, method="silhouette", k.max=15
             ,diss=dist(df_c, method="manhattan"), hc_method="average") 


km <- kmeans(df_c, 3, nstart = 50)
fviz_cluster(km, data = df_c, ellipse.type = "convex", geom = "point")

df_c$cluster <- as.factor(km$cluster)

density_plots <- lapply(names(df_c)[1:(ncol(df_c) - 1)], function(col) {
  ggplot(df_c, aes_string(x = col, fill = "cluster")) +
    geom_density(alpha = 0.5) +
    labs(title = paste(col)) +
    theme_minimal()
})

grid.arrange(grobs = density_plots, ncol = 4) 

# MCA

df_cat = data %>% select(neighbourhood_cleansed,room_type,host_response_time)

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

data$price <- as.numeric(gsub("[$,]", "", data$price))

# Crear el boxplot
ggplot(data, aes(y = price)) +
  geom_boxplot() +
  labs(x = NULL, y = "Precio") +
  scale_y_continuous(labels = scales::comma_format(scale = 1, big.mark = ",")) +
  theme_minimal()

ggplot(data, aes(y = price)) +
  geom_boxplot() +
  labs(x = NULL, y = "Precio") +
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

# frecuencia barrios

frecuencia_barrios <- table(data$neighbourhood_cleansed)
barrios_ordenados <- names(sort(frecuencia_barrios, decreasing = TRUE))
top5_barrios <- barrios_ordenados[1:5]
datos_top5 <- data[data$neighbourhood_cleansed %in% top5_barrios, ]
datos_top5$neighbourhood_cleansed <- factor(datos_top5$neighbourhood_cleansed, levels = top5_barrios)
grafico <- ggplot(datos_top5, aes(x = neighbourhood_cleansed, fill = neighbourhood_cleansed)) +
  geom_bar(stat = "count") +
  labs(
       x = NULL,  
       y = NULL) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +  
  guides(fill = FALSE)
print(grafico)

# analisis de room_type

grafico <- ggplot(data, aes(x = reorder(room_type, -table(room_type)[room_type]), fill = room_type)) +
  geom_bar() +
  labs(
       x = "Tipo de habitación",
       y = "Cantidad de registros") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())  

print(grafico)

# precio vs room_type

data$price <- as.numeric(data$price)
data_filtrada2 <- filter(data, price <= 700000)
data$room_type <- as.factor(data$room_type)


grafico_puntos <- ggplot(data_filtrada2, aes(x = room_type, y = price, color = room_type)) +
  geom_point(position = position_jitter(width = 0.2), size = 3) +
  labs(
       x = "Tipo de Habitación",
       y = "Precio") +
  scale_y_continuous(labels = scales::number_format(scale = 1, accuracy = 1)) +  # Establecer el formato de etiqueta del eje y
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())  +
  guides(color = FALSE)  
print(grafico_puntos)

# frecuencias de forma de verificacion 

data %>%
  mutate(categorias = str_extract_all(host_verifications, "'(.*?)'")) %>%   # Extraer palabras entre comillas
  unnest(categorias) %>%                                              # Expandir la lista a filas
  count(categorias) %>%                                                # Contar la frecuencia
  arrange(desc(n)) %>%                                                # Ordenar de mayor a menor frecuencia
  ggplot(aes(x = reorder(categorias, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  labs( x = NULL ,  y = "Frecuencia") +
  theme_minimal()

# frecuencias de amenities --> top 10

data %>%
  mutate(categorias = str_extract_all(amenities, '"(.*?)"') %>%
           lapply(str_trim)) %>%  
  unnest(categorias) %>%
  filter(!grepl(",", categorias)) %>% 
  count(categorias) %>%
  arrange(desc(n)) %>%
  head(10) %>%  # Select the top 10 rows
  ggplot(aes(x = reorder(categorias, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# boxplot Price vs host_is_superhost

data <- na.omit(data)
ggplot(data, aes(x = host_is_superhost, y = price)) +
  geom_boxplot() +
  labs(x = 'Superhost', y = 'Precio') +
  theme_minimal()

modelo_anova <- aov(price ~ host_is_superhost, data = data)
summary(modelo_anova)

# boxplot Price vs host_response_time

data$host_response_time <- factor(data$host_response_time,
                                  levels = c('within an hour', 'within a few hours', 'within a day', 'a few days or more'))

ggplot(data, aes(x = host_response_time, y = price)) +
  geom_boxplot() +
  labs(
       x = "Cantidad de camas",
       y = "Precio",
       caption = "Se uso una escala logaritmica",
       subtitle = paste("ANOVA p-value: ", anova(lm(log(price) ~ factor(beds), data = data))$`Pr(>F)`[1])
       ) +scale_y_continuous(labels = scales::number_format(scale = 1, accuracy = 1)) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())  +
  guides(color = FALSE)

# botplot  de cantidad de baños y precio. un boxplot por cada baño

ggplot(data, aes(x = factor(bathrooms), y = log(price), fill = factor(bathrooms))) +
  geom_boxplot() +
  labs(
       x = "Cantidad de baños",
       y = "Precio",
       caption = "Se uso una escala logaritmica",
       subtitle = paste("ANOVA p-value: ", anova(lm(log(price) ~ factor(bathrooms), data = data))$`Pr(>F)`[1])
       ) +
  scale_y_continuous(labels = scales::number_format(scale = 1, accuracy = 1)) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())  +
  guides(color = FALSE)

# histograma mostrando la cantidad de cantidad de habitaciones

ggplot(data, aes(x = bedrooms)) +
  geom_histogram(fill = "black", alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 8, 1)) +  
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Cantidad de habitaciones",
       x = "Cantidad de habitaciones",
       y = "Cantidad de registros") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())  +
  guides(color = FALSE)

data_modified <- data %>%
  mutate(categorias = str_extract_all(host_verifications, "'(.*?)'")) %>%   # Extract words between quotes
  unnest(categorias) 

ggplot(data_modified, aes(x = categorias, y = log(price), fill = categorias)) +
  geom_boxplot() +
  labs(
       x = "Verificaciones",
       y = "Precio",
       caption = "Se uso una escala logaritmica",
       subtitle = paste("ANOVA p-value: ", anova(lm(log(price) ~ categorias, data = data_modified))$`Pr(>F)`[1])
       ) +
  scale_y_continuous(labels = scales::number_format(scale = 1, accuracy = 1)) +
  theme_minimal()

# piecharts bar de cantidad y tipo de propiedad, separado por tiempo de respuesta

data %>%
  group_by( host_response_time,room_type) %>%
  summarise(count = n()) %>%
  group_by(room_type) %>%
  mutate(percentage = count / sum(count)) %>%
  ggplot(aes(x = "", y = percentage, fill = room_type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  facet_wrap(~room_type) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(
       x = NULL,
       y = NULL,
       fill = "Tipo de habitación") 

# Now the other way around

data %>%
  group_by(room_type,host_response_time) %>%
  summarise(count = n()) %>%
  group_by(room_type) %>%
  mutate(percentage = count / sum(count)) %>%
  ggplot(aes(x = "", y = percentage, fill = host_response_time)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  facet_wrap(~room_type) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(
       x = NULL,
       y = NULL,
       fill = "Tipo de habitación")
