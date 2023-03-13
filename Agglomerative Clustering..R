library(cluster)
library(colorspace) 
library(dendextend) 
data=iris[-5]
m <- c( "average", "single", "complete")
names(m) <- c( "average", "single", "complete")
#function to compute Agglomerative coefficient
ac <- function(x) {
  agnes(data, method = x)$ac}
#calculate Agglomerative coefficient for each clustering linkage method
sapply(m, ac)
#complete has the highest coefficient
hc_iris <- hclust(dist(data), method = "complete")
iris_species <- rev(levels(iris[,5]))
dend <- as.dendrogram(hc_iris)
# order it the closest
dend <- rotate(dend, 1:150)
# Color the branches based on the clusters:
dend <- color_branches(dend, k=3) 
#match the labels to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(iris[,5])[order.dendrogram(dend)])]
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
dend <- set(dend, "labels_cex", 0.5)
plot(dend, 
     main = "Clustered Iris data set")
rect.hclust(hc_iris , k=3 ,border = 2:5)
legend("topright", legend = iris_species, fill = rainbow_hcl(3))