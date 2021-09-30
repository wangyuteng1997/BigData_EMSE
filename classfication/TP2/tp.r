library(cluster.datasets)
Data <- mammal.dentition

Data <- Data[,-1]
summary(Data)


### K-means
#cluster 
km <- kmeans(mammal.dentition[,2:9], 3)


plot(mammal.dentition[c("top.i", "bottom.i","top.c", "bottom.c","top.pm", "bottom.pm","top.m", "bottom.m")], col = km$cluster)
points(km$centers[c("top.i", "bottom.i","top.c", "bottom.c","top.pm", "bottom.pm","top.m", "bottom.m")], col = 1:3, pch = 8, cex=2)
title(paste("Apres le k-means  "))

#reslut of the distance of k-means:

#print(km$totss) 
#The total distance squared sum is the difference between all the data and the mean value

#print(km$withinss)
#Squared sum of distances within each cluster

#In general, the distance required within the group is as small as possible and the distance between groups is as large as possible

### K-medoid (pam)

library(cluster)
kd <- pam(mammal.dentition[,2:9], 3,metric="euclidean")
plot(kd)
plot(mammal.dentition[c("top.i", "bottom.i","top.c", "bottom.c","top.pm", "bottom.pm","top.m", "bottom.m")], col = km$cluster)
points(km$centers[c("top.i", "bottom.i","top.c", "bottom.c","top.pm", "bottom.pm","top.m", "bottom.m")], col = 1:3, pch = 8, cex=2)
title(paste("Apres le k-medoid "))


###clustering hierarchique
md <- dist(mammal.dentition[,2:9])
#md is the distance of every point
hh <- hclust(md,'complete')
plot(hh)

hh <- hclust(md,'ward')
plot(hh)


###DBSCAN
db_x <- dbscan(mammal.dentition[,2:9],eps=0.4,MinPts=3)
db_x$cluster
plot(mammal.dentition[,2:9], col = db_x$cluster + 1)
title("DBSCAN pour eps = 0.2")


###KNN
data1<-read.table('md_classes.txt',header=TRUE)

data1 <- data1[,-1]

data1 <- data1[-c(1,12,13,14,36,57),]

#make the index of row by order
library(dplyr)
data <- arrange(data1)

library (class)
m <- dim(data)[1]

val_1 <- sample(1:m, size = round(2*m/3), replace = FALSE, prob = rep(1/m, m))
train <- data[val_1,1:8]
train_label <- data[val_1,9]

val_2 <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m))
test <- data[val_2,1:8]
test_label <- data[val_2,9]

mknn1 <- knn(train, test,train_label, k=1)
table(mknn1, test_label)

mknn1 <- knn(train, test,train_label, k=3)
table(mknn1, test_label)

mknn1 <- knn(train, test,train_label, k=5)
table(mknn1, test_label)

#comparer differnt k value

