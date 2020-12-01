library(factoextra)
library(FactoMineR)

decathlon2
str(decathlon2)
summary(decathlon2)

data <- decathlon2[1:23, 1:10]
str(data)
summary(data)

res.pca <- PCA(data, graph = FALSE)
print(res.pca)
# Compute PCA
res.pca <- PCA(data)
# Inspect eigenvalues
res.pca$eig
# Visualize variance
fviz_eig(res.pca)
fviz_eig(res.pca, addlabels = TRUE)
# Inspect correlations
fviz_pca_var(res.pca)
head(res.pca$var$coord)
# Correlogram
library(corrplot)
corrplot(res.pca$var$coord)
# Contribution
fviz_contrib(res.pca,choice = 'var',top=10)
fviz_contrib(res.pca,choice = 'var',top=10,axes = 2)

# Project data on new variables
fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca,repel = TRUE) # Slow because "repeling" takes time

# Fancier
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# Biplot
fviz_pca_biplot(res.pca)


fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = res.pca$ind$cos2, # color by groups
             addEllipses = TRUE) # Concentration ellipses

fviz_pca_biplot(res.pca,
                col.ind = data$Subsaharan,
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE)


# Quality of the representation
res.pca$var$cos2
corrplot(var$cos2, is.corr=FALSE)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel =TRUE)
fviz_pca_biplot(res.pca, col.var = "cos2",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel =TRUE)
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)



# Plotting supplementary individuals
fviz_pca_ind(res.pca, col.ind.sup = "blue", repel = TRUE)

# Facto shiny
library(Factoshiny)
res.shiny <- PCAshiny(data)



##### Another example
library(factoextra)
library(FactoMineR)

Countries <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/Countries.dat",sep=' ')
str(Countries)
data <- Countries[,-1]
rownames(data) <- Countries$Country
head(data)

res.pca <- PCA(data, graph = FALSE)
print(res.pca)
# Inspect eigenvalues
res.pca$eig
# Visualize variance
fviz_eig(res.pca, addlabels = TRUE)
# Inspect correlations
fviz_pca_var(res.pca)
# Contribution
fviz_contrib(res.pca,choice = 'var',top=10)
fviz_contrib(res.pca,choice = 'var',top=10,axes = 2)
# Correlogram
library(corrplot)
corrplot(res.pca$var$coord)

# Project data on new variables
fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca,repel = TRUE)
fviz_pca_ind(res.pca,repel = TRUE,axes=c(2,4))

# Fancier
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# Biplot
fviz_pca_biplot(res.pca)


fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = res.pca$ind$cos2, # color by groups
             addEllipses = TRUE) # Concentration ellipses

fviz_pca_biplot(res.pca,
                col.ind = data$Subsaharan,
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE)


# Quality of the representation
res.pca$var$cos2
corrplot(var$cos2, is.corr=FALSE)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel =TRUE)
fviz_pca_biplot(res.pca, col.var = "cos2",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel =TRUE)
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)



# Plotting supplementary individuals
fviz_pca_ind(res.pca, col.ind.sup = "blue", repel = TRUE)

# Facto shiny
library(Factoshiny)
res.shiny <- PCAshiny(data)
