library(factoextra)
#counting number of each type
iris.labels <- iris$Species
table(iris.labels)

#Data
iris_data = iris[1:4]

#scale
iris_data_std=scale(iris_data)

#Distance by default euclidean
iris.dist = dist(iris_data_std)

#Hierarchial clustering algorithm
dv <- diana(iris_data_std )
dv

#Dendrogram
plot(dv)
dend <-as.dendrogram(dv)
rect.hclust(dv , k=3 ,border = 2:5)

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
legend("topright", fill = rainbow_hcl(3))


# count the clusters
iris.clusters <- cutree(dv , k=3)

#visualize the cluster 
rownames(iris_data_std) <- paste(iris$Species , 1:dim(iris)[1],sep = "_")
fviz_cluster(list(data=iris_data_std , cluster = iris.clusters))
#miss classified
table(iris.clusters,iris$Species)