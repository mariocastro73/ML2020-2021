library(factoextra)
library(MASS)
library(ggplot2)

data <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/comparing-dends.csv')
ggplot(data,aes(x=x,y=y)) + geom_point()
ggplot(data,aes(x=x,y=y)) + geom_point() +geom_density2d()

library(gridExtra)
grid.arrange(ggplot(data,aes(x=x,y=y)) + geom_point() +geom_density2d(adjust=5),  
             ggplot(data,aes(x=x,y=y)) + geom_point() +geom_density2d(adjust=1),
             ggplot(data,aes(x=x,y=y)) + geom_point() +geom_density2d(adjust=.5),
             ggplot(data,aes(x=x,y=y)) + geom_point() +geom_density2d(adjust=.1))

library(mclust)
dens <- densityMclust(data)
plot(dens,what = 'density',type='persp')
mc <- Mclust(data) # Model-based-clustering
# mc <- Mclust(data,modelNames = 'EII') # Model-based-clustering
summary(mc)
# look inside
mc$data
plot(mc)
fviz_mclust(mc,ellipse.type = 'euclid') # becareful with fviz_mclust
# BIC values used for choosing the number of clusters
fviz_mclust(mc, "classification")
# Classification uncertainty
fviz_mclust(mc, "uncertainty")

mc$modelName
mc$parameters
points(t(mc$parameters$mean),cex=2,pch=19)

#

mc <- Mclust(data,G=2) # Model-based-clustering, forcing the number of clusters
plot(mc)
mc$parameters
fviz_mclust(mc,"BIC")
fviz_mclust(mc,'uncertainty')
########################################################
# Model based hierarchical clustering
########################################################

ggpairs(data,upper=list(continuous=wrap('cor',family='sans')))
hcTree <- hc(modelName="VVV", data = data)
cl <- hclass(hcTree,2)
ggplot(data,aes(x=x,y=y,col=as.factor(cl))) + geom_point()
cl <- hclass(hcTree,3)
ggplot(data,aes(x=x,y=y,col=as.factor(cl))) + geom_point()
fviz_dend(as.dendrogram(hcTree),show_labels=FALSE,k=3)


###
cereal <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/cereal.csv')
data <- as.data.frame(scale(cereal[,-c(1:3,12:16)]))
# plot(densityMclust(data,what='density',type='perspective'))
mc <- Mclust(data)
summary(mc)
fviz_mclust_bic(mc)
fviz_mclust(mc)

mc <- Mclust(data,G=4)
summary(mc)
fviz_mclust(mc)


### Cool example
data(banknote)
ggpairs(banknote,aes(col=Status),upper=list(continuous=wrap('cor',family='sans')))
mc <- Mclust(banknote[,-1])
summary(mc)
fviz_mclust_bic(mc)
fviz_mclust(mc)
fviz_mclust(mc,'uncertainty')
table(banknote$Status,fit.mc$classification)
