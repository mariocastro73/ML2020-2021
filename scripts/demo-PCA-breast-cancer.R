breast <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/breast-cancer-data.csv')

str(breast)
breast <- breast[,-33]
data <- breast[,-c(1,2)]
summary(data)

#Redundancy!
library(corrplot)
corrplot(cor(data),diag=FALSE)

library(FactoMineR)
library(factoextra)
res.pca <- PCA(data)

# Inspect eigenvalues
res.pca$eig
fviz_eig(res.pca)

# Correlations
fviz_pca_var(res.pca,axes = 2:3,repel=TRUE)
corrplot(res.pca$var$coord)
fviz_contrib(res.pca,choice='var')
fviz_contrib(res.pca,choice='var',axes=2)
fviz_contrib(res.pca,choice='var',axes=3)
fviz_contrib(res.pca,choice='var',axes=1:3)

# individuals
fviz_pca_ind(res.pca,col.ind=breast$diagnosis)
fviz_pca_ind(res.pca,col.ind=breast$diagnosis,axes=c(2,3))

fviz_pca_biplot(res.pca,col.ind=breast$diagnosis)

library(Factoshiny)
res.shiny <- PCAshiny(breast)
