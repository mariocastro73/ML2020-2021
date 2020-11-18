data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data
df <- as.data.frame(df) # Scaling the data

# View the firt 3 rows of the data
head(df, n = 3)

library(factoextra)
library(cluster)

# Compute k-means with k = 4
set.seed(123)

pairs.panels(df)

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

pairs.panels(df,col=km$cluster)
df$col <- as.factor(km$cluster)
library(gridExtra)
str(df)
grid.arrange(ggplot(df,aes(x=Murder,y=Assault,col=col)) + geom_point(),
ggplot(df,aes(x=Murder,y=UrbanPop,col=col)) + geom_point(),
ggplot(df,aes(x=Murder,y=Rape,col=col)) + geom_point(),
ggplot(df,aes(x=Assault,y=Rape,col=col)) + geom_point())
library(GGally)

ggpairs(df,aes(col=col)) 
