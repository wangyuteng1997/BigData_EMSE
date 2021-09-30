Data <- iris
summary(Data)


### K-means
#cluster 1-4 colone ，5 colone is label 
km <- kmeans(iris[,1:4], 3)

#plot

plot(iris[,c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width")], col = km$cluster, pch = as.integer(iris$Species))
points(km$centers[,c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width")], col = 1:3, pch = 8, cex=2)
title(paste("Apres le k-means  k =", 3))
#matrix confused
table(km$cluster,iris[,5])

#reslut of k-means
#print(km)


### K-medoid (pam)
library(fpc)
kd <- pam(iris[,1:4], 3,metric="euclidean")
plot(kd)
plot(iris[,c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width")], col = km$cluster, pch = as.integer(iris$Species))
points(kd$centers[,c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width")], col = 1:3, pch = 8, cex=2)
title(paste("Apres le k-means pour tout 4 variabels k =", k))
#matrix confused
table(kd$cluster,iris[,5])



###clustering hierarchique
md <- dist(iris[,1:4])

hh <- hclust(md,'complete')
plot(hh)

hh <- hclust(md,'ward')
plot(hh)
#matrix confused
table(md$cluster,iris[,5])

###DBSCAN
db_x <- dbscan(iris[,1:4],eps=0.4,MinPts=3)
db_x
db_x$cluster
plot(iris[,1:4], col = db_x$cluster + 1)
title("DBSCAN pour eps = 0.2")
table(db_x$cluster,iris[,5])



