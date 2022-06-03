library(ggplot2)
library(class)
library(caTools)
library(factoextra)

setwd("C:\\Users\\sksmi\\PeytoAccess\\Personal\\Bellevue\\DSC520\\dsc520")

clustering_data <- read.csv("data\\clustering-data.csv")

wss <- data.frame()
for (k in 1:12) {
  km_cluster <- kmeans(clustering_data, k)
  print(fviz_cluster(km_cluster, data=clustering_data))
  wss <- rbind(wss, c(k, km_cluster$tot.withinss))
}
colnames(wss) <- c("k", "wss")

ggplot(data=wss, aes(x=k, y=wss)) + geom_point() + geom_line()



