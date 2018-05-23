library(datasets)
head(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

set.seed(20)
print("hello")
irisCluster <- kmeans(iris[, 1:4], 3, nstart = 20)
irisCluster

table(irisCluster$cluster, iris$Species)


irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()
