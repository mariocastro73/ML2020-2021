library(cluster)
library(factoextra)
library(GGally)
library(dendextend)


# another example
## Manufacturers
# American Home Food Products
# General Mills
# Kellogs
# Nabisco
# Post
# Quaker Oats
# Ralston Purina

cereal <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/cereal.csv')
summary(cereal)

data <- as.data.frame(scale(cereal[,-c(1:3,13,15)]))
summary(data)
ggpairs(data) 

library(corrplot)
corrplot(cor(data),diag = FALSE)
data <- as.data.frame(scale(cereal[,-c(1:3,12:16)]))
corrplot(cor(data),diag = FALSE)

fviz_pca_ind(prcomp(data)) # remove non-numerical attributes to plot
fviz_dist(dist(data),show_labels=FALSE) 
# repeat removing 1,3 and 4
fviz_pca_ind(prcomp(data[-c(1,3,4),])) # remove non-numerical attributes to plot

# Let's cluster
agnes.cl <- hclust(dist(data))
par(mfrow=c(1,1))
plot(agnes.cl,hang=-1,lagels=FALSE)
for(i in 1:nrow(data)) abline(h=agnes.cl$height,col=2,lty=3,lwd=.5) # 2 or 5
# another way
barplot(diff(agnes.cl$height))

library(factoextra)
fviz_dend(agnes.cl,k=2)
fviz_dend(agnes.cl,k=5)
clusters <- cutree(agnes.cl,k=5)
table(clusters)
cereal[clusters<3,]

cereal2 <- cereal[clusters>2,]
data2 <- data[clusters>2,]
fviz_pca_ind(prcomp(data2)) # remove non-numerical attributes to plot
fviz_dist(dist(data2),show_labels=FALSE) 

agnes.cl2 <- hclust(dist(data2))
plot(agnes.cl2,hang=-1,lagels=FALSE)
for(i in 1:nrow(data2)) abline(h=agnes.cl2$height,col=2,lty=3,lwd=.5) # 2 or 5

diana.cl2 <- diana(data2)
plot(diana.cl2,hang=-1,lagels=FALSE)
for(i in 1:nrow(data2)) abline(h=diana.cl2$height,col=2,lty=3,lwd=.5) # 2 or 5

## Cluster validation
library(clValid)
# Compute clValid
clmethods <- c("hierarchical","diana","kmeans","pam")
internal <- clValid(data2, nClust = 2:6,
                    clMethods = clmethods, validation = "internal")
# Summary
summary(internal)

# Stability measures
clmethods <- c("hierarchical","diana","kmeans","pam")
stab <- clValid(data2, nClust = 2:6, clMethods = clmethods,
                validation = "stability")
# Display only optimal Scores
summary(stab)
library(NbClust)
nb <- NbClust(data2,method='complete',index='all')
fviz_nbclust(nb)
cluster <- cutree(agnes.cl2,k=3)
cluster <- cutree(diana.cl2,k=2)
cluster <- cutree(diana.cl2,k=3)
pam.cl <- pam(data2,k=6)
cluster <- pam.cl$clustering
fviz_cluster(list(data=data2,cluster=cluster))

ggpairs(data2,col=cluster)
