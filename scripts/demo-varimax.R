breast <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/breast-cancer-data.csv')

str(breast)
breast <- breast[,-33]
data <- breast[,-c(1,2)]

res.pca <- PCA(data) # Traditional PCA

library(psych)
fit.varimax <- principal(data, nfactors=3, rotate="varimax") # PCA with varimax rotation
corrplot(res.pca$var$cor[,1:3],is.corr = FALSE)
corrplot(fit.varimax$loadings,is.corr = FALSE)
psych::biplot.psych(fit.varimax)
