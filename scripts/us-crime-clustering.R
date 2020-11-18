library(GGally)
library(factoextra)
library(cluster)

data("USArrests")      # Loading the data set
ggpairs(USArrests)
df <- as.data.frame(scale(USArrests)) # Scaling the data

# View the firt 3 rows of the data
head(df, n = 3)


# Compute k-means with k = 4
set.seed(123)

km <- kmeans(df,2,nstart=25)
km$tot.withinss
sil <- silhouette(km$cluster,dist=dist(df))
fviz_silhouette(sil)

km <- kmeans(df,5,nstart=25)
km$tot.withinss
sil <- silhouette(km$cluster,dist=dist(df))
fviz_silhouette(sil)


elbow <- c()
mean.sil <- c()
for(k in 2:10) {
km <- kmeans(df, k, nstart = 25)
sil <- silhouette(km$cluster,dist=dist(df))

elbow <- c(elbow,km$tot.withinss)
mean.sil<- c(mean.sil,mean(sil[,3]))
}

plot(2:10,elbow/max(elbow),type='b')
points(2:10,mean.sil/max(mean.sil),type='b',col=2)
# k=2 from Silhouette and k=4 from elbow

km <- kmeans(df,4,nstart=25)
sil <- silhouette(km$cluster,dist=dist(df))
fviz_silhouette(sil)
km <- kmeans(df,2,nstart=25)
sil <- silhouette(km$cluster,dist=dist(df))
fviz_silhouette(sil)

library(NbClust)
nb.fit <- NbClust(df,index='all',method = 'complete',max.nc = 10)
fviz_nbclust(nb.fit)

df$col <- as.factor(km$cluster)
ggpairs(df,aes(col=col)) 
df
?kmeans

km$cluster <- as.factor(ifelse(km$cluster==1,"High crime","Low crime"))
fviz_cluster(km,data=df[,-5],col=as.factor(ifelse(1,"High crime","Low crime")))
