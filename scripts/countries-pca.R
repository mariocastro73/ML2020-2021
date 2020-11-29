library(readr)
library(factoextra)
library(FactoMineR)

countries <- read_csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/countries.csv")
summary(countries)
str(countries)
countries$Climate <- as.factor(countries$Climate)
countries$Country <- as.factor(countries$Country)
countries$Region <- as.factor(countries$Region)
data <- na.omit(countries)
# If you want to display country names you can uncomment the following line of code
# rownames(data) <- data$Country
str(data)
summary(data)
# Compute PCA
res.pca <- PCA(data, scale.unit = TRUE, quali.sup = c(1,2,15), ncp = 5, graph = T)
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
fviz_pca_ind(res.pca,col.ind=data$Region,label='point')
fviz_pca_ind(res.pca,col.ind=data$Climate,label='point')

# Fancier
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# Biplot
fviz_pca_biplot(res.pca)


# Ellipses (empirical densities)
data$Subsaharan <- as.factor(ifelse(data$Region=="SUB-SAHARAN AFRICA","SUB-SAHARAN","REST OF THE WORLD"))
fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = data$Subsaharan, # color by groups
             addEllipses = TRUE) # Concentration ellipses

fviz_pca_biplot(res.pca,
                col.ind = data$Subsaharan,
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE)
