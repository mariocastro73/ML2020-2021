library(GGally)
library(factoextra)
library(cluster)

data("USArrests")
head(USArrests)
summary(USArrests)
df <-  data.frame(scale(USArrests))
summary(df)
set.seed(123)
km <- kmeans(df,centers=5,nstart=25,trace = TRUE)
summary(km)
km$cluster
km$tot.withinss
sil <- silhouette(km$cluster,dist=dist(df))
sil[,3]
df[8,]
fviz_silhouette(sil)

km <- kmeans(df,centers=2,nstart=25,trace = TRUE)
summary(km)
km$cluster
km$tot.withinss
sil <- silhouette(km$cluster,dist=dist(df))
fviz_silhouette(sil)

elbow <- c()
mean.sil <- c()

for(k in 2:10) {
  km <- kmeans(df,centers=k,nstart=25)
  sil <- silhouette(km$cluster,dist=dist(df))
  elbow <- c(elbow,km$tot.withinss)
  mean.sil <- c(mean.sil,mean(sil[,3]))
}

plot(2:10,elbow,type='b',xlab='k') # k=4
plot(2:10,mean.sil,type='b',xlab='k') # k=2

library(NbClust)
nb.fit <- NbClust(df,index='all',method='complete',min.nc = 2,max.nc = 10)
fviz_nbclust(nb.fit)
set.seed(1234)
km <- kmeans(df,centers=2,nstart=25,trace = TRUE)

df$Cluster <- as.factor(km$cluster)
ggpairs(df,aes(col=Cluster),upper=list(continuous=wrap('cor',family='sans')))
fviz_cluster(km,data=df[,-5])
# Add some labels
#km$cluster <- as.factor(ifelse(km$cluster==1,"High crime","Low crime"))
#fviz_cluster(km,data=df[,-5],col=as.factor(ifelse(1,"High crime","Low crime")))

set.seed(1234)
km <- kmeans(df,centers=4,nstart=25,trace = TRUE)
df$Cluster <- as.factor(km$cluster)
ggpairs(df,aes(col=Cluster),upper=list(continuous=wrap('cor',family='sans')))
fviz_cluster(km,data=df[,-5])
