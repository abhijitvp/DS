#### HEIRARCHICAL CLUSTERING ####
data(mtcars)
cars=mtcars

#we need to standardize the data before we can begin clustering if we dont want inherrent
#weightages present in the data to prevail.
medians = apply(cars,2,median)
mads = apply(cars,2,mad)
cars.std=data.frame(scale(cars))

#create a distance matrix for all the observations in the data. 
#This is the resource consuming step which becomes insanley slow as the size of the data increases.
cars.dist = dist(cars.std)

#Using this distance matrix we can now built a hierarchical clustering model with function hclust.
cars.hclust = hclust(cars.dist)
plot(cars.hclust,labels=rownames(cars),main='Default from hclust',col="mediumturquoise")


#You can various information regarding each group of the cluster using function cutree
groups.3=cutree(cars.hclust,3)
groups.3
plot(groups.3)

table(groups.3)
rownames(cars)[groups.3==1]



#### K-MEANS CLUSTERING ######

#Preparing the data for K-Means Clustering

n = 100
g = 6
set.seed(g)
d <- data.frame(
  x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))),
  y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
plot(d,col="mediumturquoise",pch=16,
     xlab="Arrival Time Deviations",
     ylab="Departure time Deviations",
     main="Scatter Plot: Delays from Schedule in Minutes ")


mydata <- d
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",col="mediumseagreen",pch=12)


#There is another way of deciding the value of K by calinski criterion.
require(vegan)

## Loading required package: vegan
## Loading required package: permute
## Loading required package: lattice
## This is vegan 2.2-1

fit <- cascadeKM(scale(d, center = TRUE, scale = TRUE), 1, 10, iter = 1000)
# 1 -> The number of groups for the partition with the smallest number of groups of the cascade (min).
# 10 -> The number of groups for the partition with the largest number of groups of the cascade (max).
# iter -> The number of random starting configurations for each value of K

plot(fit, sortg = TRUE, grpmts.plot = TRUE)

calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")


fit <- kmeans(mydata,5 )
d$cluster=fit$cluster
library(ggplot2)
ggplot(d,aes(x,y,color=as.factor(cluster)))+geom_point()


