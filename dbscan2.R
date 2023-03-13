install.packages("fpc",dependencies = TRUE)
install.packages("dbscan",dependencies = TRUE)
library(fpc)
library(dbscan)
data("iris")
str(iris)
# Remove label form dataset
df = iris[,-5]
df
# Fitting DBScan clustering Model 
# to training dataset
model=dbscan(df,eps=0.45,MinPts=5,seeds=TRUE)
model
# Checking cluster
model$cluster
#table
table(model$cluster, iris$Species)
plot(model, df, main = "DBScan clusters")

