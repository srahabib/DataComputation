k_means <- function(data, k, pca = FALSE) {
  
  #option for Principal Component Analysis
  if(pca == TRUE){
    data <- princomp(data)
    data <- data$scores %>%
      as.data.frame() %>%
      select(Comp.1, Comp.2)
  }
  
  #randomly select the indices of k rows to use as starting
  #centers of the k clusters
  rand <- sample(1:nrow(data), k)
  
  #data frame with k observations that were randomly selected
  clusters <- data[rand,]
  
  #empty vectors that will contain the cluster assignments for each observation
  cluster_vec <- c()
  last_vec <- c(0)
  
  #iteration counter
  iter <- 1
  
  #algorithm will stop once stop is equal to 1
  stop <- 0
  
  while (stop == 0) {
    
    #loop through each observation
    for (i in 1:nrow(data)) {
      
      #find the euclidean distance of the ith observation to each of the clusters
      dist <- data[i,] %>%
        rbind(clusters) %>%
        dist()
      
      #find which cluster the ith observation has the smallest distance with
      i_cluster <- dist[1:k] %>%
        which.min()
      
      #add the cluster assignment for the ith observation to a vector
      #containing the cluster assignments of all observations
      cluster_vec[i] <- i_cluster
      
    }
    
    #check to see if the cluster assignments have changed at all since
    #the last iteration
    if (all(cluster_vec == last_vec)) {
      stop <-  1
    }
    
    #save the cluster assignments from this iteration to another object
    #so we can check to see if cluster assignments changed
    last_vec <- cluster_vec
    
    #group the observations into their assigned clusters and find the means
    #of all the columns to use as the new cluster centers
    clusters <- data %>%
      cbind(cluster_vec) %>%
      group_by(cluster_vec) %>%
      summarize_all(mean)
    
    #remove the first column that contains the cluster number
    clusters <- clusters[, -1]
    
    iter <- iter + 1
    
    if (stop == 1) {
      sizes <- data %>% 
        cbind(cluster_vec) %>% 
        count(cluster_vec) %>% 
        pull(n)
      
      clusters <- data %>%
        cbind(cluster_vec) %>%
        group_by(cluster_vec) %>%
        summarize_all(mean)
      
    }
    
  }
  
  result <- list("Sizes" = sizes, 
                 "Cluster Means" = clusters,
                 "Clustering Vector" = cluster_vec,
                 "Iterations" = iter)
  return(result)
}
library(tidyverse)
data(iris)
iris2 <- iris %>% 
  select(-Species)

head(iris2)

set.seed(23)
k_means(data = iris2, k = 3)

##visualize results 
iris3 <- iris %>% 
  select(Sepal.Length, Petal.Length)

head(iris3)

set.seed(78)
result <- k_means(iris3, 3)
result

#save the clustering assignments to an object
assignments <- result$`Clustering Vector`

iris3 %>% 
  cbind(assignments) %>% 
  ggplot(aes(x = Sepal.Length, y = Petal.Length, color = as.factor(assignments))) + 
  geom_point() + 
  theme_bw() + 
  labs(color = "Cluster")

## TSS = BSS + WSS
set.seed(42)    # Set seed since kmeans uses a random start.
fit <- kmeans(iris2, 3)
clusters <- fit$cluster

# Subtract each value from the grand mean and get the number of observations in each cluster.
iris2.cent <- scale(iris2, scale=FALSE)
nrows <- table(clusters)

(TSS <- sum(iris2.cent^2))
# [1] 681.3706
(WSS <- sapply(split(iris2, clusters), function(x) sum(scale(x, scale=FALSE)^2)))
#        1        2        3 
# 15.15100 39.82097 23.87947 
(BSS <- TSS - sum(WSS))
# [1] 602.5192
# Compute BSS directly
gmeans <- sapply(split(iris2, clusters), colMeans)
means <- colMeans(iris2)
(BSS <- sum(colSums((gmeans - means)^2) * nrows))
# [1] 602.5192

