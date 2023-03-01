set.seed(42)    # Set seed since kmeans uses a random start.
fit <- kmeans(data, 3)
clusters <- fit$cluster

# Subtract each value from the grand mean and get the number of observations in each cluster.
data.cent <- scale(data, scale=FALSE)
nrows <- table(clusters)

(TSS <- sum(data.cent^2))
# [1] 681.3706
(WSS <- sapply(split(data, clusters), function(x) sum(scale(x, scale=FALSE)^2)))
#        1        2        3 
# 15.15100 39.82097 23.87947 
(BSS <- TSS - sum(WSS))
# [1] 602.5192
# Compute BSS directly
gmeans <- sapply(split(data, clusters), colMeans)
means <- colMeans(data)
(BSS <- sum(colSums((gmeans - means)^2) * nrows))

# [1] 602.5192
