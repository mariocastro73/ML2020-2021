library(factoextra)
library(FactoMineR)

data(housetasks)
library(gplots)
# 1. convert the data as a table
dt <- as.table(as.matrix(housetasks))
# 2. Graph
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# Computing Correspondence Analysis
res.ca <- CA(housetasks, graph = FALSE)
res.ca$eig
fviz_eig(res.ca)
fviz_eig(res.ca,addlabels = TRUE)

# Inspect eigenvalues
fviz_eig(res.ca)
# Inspect correlations
fviz_ca_row(res.ca,repel = TRUE)
# Correlogram
library(corrplot)
corrplot(res.ca$row$coord,is.corr = FALSE)

# Contribution of rows
fviz_contrib(res.ca,choice = 'row',top=10)
fviz_contrib(res.ca,choice = 'row',top=10,axes = 2)

# Project data on new variables
fviz_ca_col(res.ca)

# Contribution of columns
fviz_contrib(res.ca, choice = "col", axes = 1)

# Biplot
fviz_ca_biplot(res.ca, repel = TRUE)
fviz_ca_biplot(res.ca, 
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)


res.ca$row
