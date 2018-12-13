install.packages("cluster")
install.packages("factoextra")

library("cluster")
library("factoextra")

data("USArrests")
attach(USArrests)

df = as.data.frame(USArrests)
df
str(df)

df <- scale(USArrests)
head(df, n=5)

 #now compute optimal nuber of clusters

#Elbow method
fviz_nbclust(df,kmeans,nstart=25,method="wss")+labs(subtitle="Elow Method")
