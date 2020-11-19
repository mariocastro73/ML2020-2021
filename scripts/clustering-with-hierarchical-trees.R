library(ggplot2)
library(NbClust)
library(cluster) # Silhouette's among other functions
library(factoextra) # fviz

## Load dataset -------------------------------------------------------------------------------------------------------
data <- read.table("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/food-bank.csv",header = FALSE, sep = ",")

ggplot(data)+geom_point(aes(x=V1,y=V2))


## Hierarchical clustering -------------------------------------------------------------------------------------------------------
# Compute distances
dd <- dist(scale(data[,1:2]), method = "euclidean")
# hierarchical clustering
set.seed(123)
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1,labels = F)
barplot(diff(hc$height))
barplot(diff(hc$height[25:40]),names.arg =26:40,las=2 ) # k=3, then 5
plot(hc,hang = -1,labels = F)
abline(h=mean(hc$height[38:39]),col=2,lwd=2)
abline(h=mean(hc$height[36:37]),col=2,lwd=2)

#Cut the tree to a given number of clusters and obtain the associated cluster for each observation
cluster <-  cutree(hc, k = 3) #Use "h = " to cut by a given height 
table(cluster) #check number of observations assigned to each cluster
#ggplot(data)+geom_point(aes(x=V1,y=V2,color=as.factor(cluster)))
#plot the dendrogram changing the colors according to the number of clusters
library(dendextend) # Function color_branches
hc.col <- color_branches(as.dendrogram(hc),k=3)
plot(hc.col)
r <- rect.hclust(hc,
                 k = 3, # k is used to specify the number of clusters
                 border = "blue")

data$color <-as.factor(cluster)
ggplot(data,aes(x=V1,y=V2,col=color)) + geom_point()

#silhouette plot for each point

sil <- silhouette(cluster, dd)
library(factoextra) # For fviz_silhouette
fviz_silhouette(sil)
# average silhouette
mean(sil[,3])

# Brute-force
nb.fit<-NbClust(data[,1:2], distance = "euclidean", min.nc=2, max.nc=8, 
             method = "complete", index = "all")
par(mfrow=c(1,1))

fviz_nbclust(nb.fit)
data$color <- as.factor(nb.fit$Best.partition)
ggplot(data,aes(x=V1,y=V2,col=color)) + geom_point()

# Let's repeat using the "sub-optimal" one
cluster <-  cutree(hc, k = 5) 
data$color <- as.factor(cluster)
ggplot(data,aes(x=V1,y=V2,col=color)) + geom_point()

######################################################################################
# Now let's play with different metrics and linkages
dd <- dist(scale(data[,1:2]), method = "manhattan")
hc <- hclust(dd, method = "ward.D")
hc <- hclust(dd, method = "single")
hc <- hclust(dd, method = "complete")
hc <- hclust(dd, method = "ward.D2")
cluster <-  cutree(hc, k = 5) 
data$color <- as.factor(cluster)
ggplot(data,aes(x=V1,y=V2,col=color)) + geom_point()


