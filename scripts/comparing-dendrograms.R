library(cluster)
library(factoextra)
library(GGally)

data <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/comparing-dends.csv')
# Visualize the data
ggplot(data,aes(x=x,y=y)) + geom_point() # 2, maybe 3 clusters
# another example
cereal <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/cereal.csv')
fviz_pca_ind(prcomp(cereal[,-(1:3)])) # remove non-numerical attributes to plot
fviz_dist(dist(scale(cereal[,-(1:3)])),show_labels=FALSE) 


# Visualize the correlations
fviz_dist(dist(scale(data)),show_labels=FALSE) # 3 blocks, I'd say
mock <- cbind(rnorm(nrow(data)),rnorm(nrow(data)))
fviz_dist(dist(scale(mock)),show_labels=FALSE) # 3 blocks, I'd say
mock <- data.frame(x=c(rnorm(nrow(data)/2),rnorm(nrow(data)/2,10))
                    ,y=c(rnorm(nrow(data)/2),rnorm(nrow(data)/2,10)))
ggplot(mock,aes(x=x,y=y)) + geom_point()
fviz_dist(dist(scale(mock)),show_labels=FALSE) # 3 blocks, I'd say


# Hierarchical methods
agnes.cl <- agnes(data,stand = TRUE)  # Ag..Aggregation
fviz_dend(agnes.cl) # k=2,
fviz_dend(agnes.cl,k=2) # k=2, repeat using colors

diana.cl <- diana(data,stand=TRUE) # Di...Divisive
fviz_dend(diana.cl) # k=2 or 3
fviz_dend(diana.cl,k=2) # k=2 followed by 3

clusters.ag <- cutree(agnes.cl,k=2)
clusters.di <- cutree(diana.cl,k=2) # Looks like both are splitting in the same way
table(clusters.ag,clusters.di) # Same clustering!

# Compare dendrograms visually
library(dendextend)
dend1 <- as.dendrogram(agnes(data,stand=TRUE))
dend2 <- as.dendrogram(diana(data,stand=TRUE))
tanglegram(dend1,dend2) # not very neat

tanglegram(dendlist(dend1,dend2),
            highlight_distinct_edges=TRUE,
           highlight_branches_lwd=FALSE,lwd=1,
           k_branches=2)

d1 <- as.dendrogram(hclust(dist(USArrests),method='complete'))
d2 <- as.dendrogram(hclust(dist(USArrests),method='single'))
tanglegram(dendlist(d1,d2),
           highlight_branches_lwd=FALSE,lwd=1)

## Cluster validation
library(clValid)
# Compute clValid
clmethods <- c("hierarchical","kmeans","pam")
internal <- clValid(data, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(internal)

# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(USArrests, nClust = 2:6, clMethods = clmethods,
                validation = "stability")
# Display only optimal Scores
summary(stab)
