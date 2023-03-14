k_means <- function(data, k, pca = FALSE) {
  # option for Principal Component Analysis
  if (pca == TRUE) {
    data <- princomp(data)
    data <- data$scores %>%
      as.data.frame() %>%
      select(Comp.1, Comp.2)
  }

  # randomly select the indices of k rows to use as starting
  # centers of the k clusters
  rand <- sample(1:nrow(data), k)

  # data frame with k observations that were randomly selected
  clusters <- data[rand, ]

  # empty vectors that will contain the cluster assignments for each observation
  cluster_vec <- c()
  last_vec <- c(0)

  # iteration counter
  iter <- 1

  # algorithm will stop once stop is equal to 1
  stop <- 0

  while (stop == 0) {
    # loop through each observation
    for (i in 1:nrow(data)) {
      # find the euclidean distance of the ith observation to each of the clusters
      dist <- data[i, ] %>%
        rbind(clusters) %>%
        dist()

      # find which cluster the ith observation has the smallest distance with
      i_cluster <- dist[1:k] %>%
        which.min()

      # add the cluster assignment for the ith observation to a vector
      # containing the cluster assignments of all observations
      cluster_vec[i] <- i_cluster
    }

    # check to see if the cluster assignments have changed at all since
    # the last iteration
    if (all(cluster_vec == last_vec)) {
      stop <- 1
    }

    # save the cluster assignments from this iteration to another object
    # so we can check to see if cluster assignments changed
    last_vec <- cluster_vec

    # group the observations into their assigned clusters and find the means
    # of all the columns to use as the new cluster centers
    clusters <- data %>%
      cbind(cluster_vec) %>%
      group_by(cluster_vec) %>%
      summarize_all(mean)

    # remove the first column that contains the cluster number
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

  ## TSS = BSS + WSS
  # set.seed(42)    # Set seed since kmeans uses a random start.
  # fit <- k_means(data=iris2, 3)
  # fit
  # clusters <- fit[3]
  # clusters

  # Subtract each value from the grand mean and get the number of observations in each cluster.
  data.cent <- scale(data, scale = FALSE)
  nrows <- table(cluster_vec)

  # TSS BSS WSS
  (TSS <- sum(data.cent^2))
  # [1] 681.3706
  (WSS <- sapply(split(data, cluster_vec), function(x) sum(scale(x, scale = FALSE)^2)))
  #        1        2        3
  # 15.15100 39.82097 23.87947

  total_w <- sum(WSS)
  # 78.85567
  (BSS <- TSS - sum(WSS))
  # [1] 602.5192
  # Compute BSS directly
  gmeans <- sapply(split(data, cluster_vec), colMeans)
  means <- colMeans(data)
  (BSS <- sum(colSums((gmeans - means)^2) * nrows))
  # [1] 602.5192

  result <- list(
    "Sizes" = sizes,
    "Cluster Means" = clusters,
    "Clustering Vector" = cluster_vec,
    "Iterations" = iter,
    "total within sum of squares" = total_w,
    "WSS" = WSS,
    "TSS" = TSS,
    "BSS" = BSS
  )
  # cc = cluster_vec
  return(result)
}



library(tidyverse)
data(iris)
iris2 <- iris %>%
  select(-Species)

head(iris2)

set.seed(23)
k_means(data = iris2, k = 3)

## visualize results
iris3 <- iris %>%
  select(Sepal.Length, Petal.Length)
head(iris3)
result <- k_means(iris3, 3)
result

# save the clustering assignments to an object
assignments <- result$`Clustering Vector`

iris3 %>%
  cbind(assignments) %>%
  ggplot(aes(x = Sepal.Length, y = Petal.Length, color = as.factor(assignments))) +
  geom_point() +
  theme_bw() +
  labs(color = "Cluster")



###### Elbow method
# install.packages("purrr")
library(purrr)
# set.seed(123)

# function to compute total within-cluster sum of square
wss <- function(k) {
  fit <- k_means(data = iris2, k)
  clusters <- fit[5]
  return(clusters[["total within sum of squares"]])
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
  type = "b", pch = 19, frame = FALSE,
  xlab = "Number of clusters K",
  ylab = "Total within-clusters sum of squares"
)

c1 <- list(c(0, 0), c(0, 1), c(2, 3))
c2 <- list(c(3, 3), c(3, 4))
n1 <- length(c1)
n2 <- length(c2)



c1[[2]][2]
c2
n1
n2

# A Score
a_score_1 <- abs(c1[[1]][1] - c1[[2]][1]) + abs(c1[[1]][2] - c1[[2]][2])
a_score_1
# 1

a_score_2 <- abs(c1[[1]][1] - c1[[3]][1]) + abs(c1[[1]][2] - c1[[3]][2])
a_score_2
# 5

a_score <- (a_score_1 + a_score_2) / 2
a_score
# 3

## B score
b_score_1 <- abs(c1[[1]][1] - c2[[1]][1]) + abs(c1[[1]][2] - c2[[1]][2])
b_score_1
# 6

b_score_2 <- abs(c1[[1]][1] - c2[[2]][1]) + abs(c1[[1]][2] - c2[[2]][2])
b_score_2
# 7

b_score <- (b_score_1 + b_score_2) / 2
b_score
# 13/2

## S score
s_score <- (b_score - a_score) / pmax(a_score, b_score)
s_score
# 0.53
