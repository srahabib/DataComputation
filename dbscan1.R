install.packages("factoextra",dependencies = TRUE)
install.packages("fpc",dependencies = TRUE)
install.packages("dbscan",dependencies = TRUE)
library(factoextra)
library(fpc)
library(dbscan)
data("iris")
str(iris)

df = iris[,-5]

df
model=dbscan(df,eps=0.45,MinPts=5,seeds=TRUE)
model

model$cluster
table(model$cluster, iris$Species)

plot(model, df, main = "DBScan clusters")

