library(ggplot2)

# Construct Data
set.seed(23)
lat <- c(seq(from = 16.3478, to = 14.1329876, length.out = 500),
seq(from = 18.5478, to = 19.567, length.out = 500))
lat <- sample(x = lat, size = 100)

lon <- seq(from = 45.987, to = 46.98237, length.out = 1000)
lon <- sample(x = lon, size = 100)

# Place inside data.frame
df_latlon <- data.frame(lat, lon)

cluster_latlon <- kmeans(x = df_latlon, centers = 2, nstart = 20)
df_latlon <- cbind(df_latlon, cluster_latlon$cluster)

# Output ggplot with colored values
ggplot(df_latlon) +
geom_point(aes(lat, lon, color = as.factor(cluster_latlon$cluster)))

cluster_latlon$centers
cluster_latlon$cluster
