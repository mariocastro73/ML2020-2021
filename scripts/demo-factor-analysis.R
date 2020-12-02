library(FactoMineR)
library(factoextra)

data(wine)
str(wine)
table(wine$Label)
table(wine$Soil)

res.famd <- FAMD(wine,graph=FALSE,ncp=10)
res.famd <- FAMD(wine,graph=FALSE)
res.famd$eig
# Scree plot
fviz_screeplot(res.famd,addlabels=TRUE) # Elbow => 2-3 dimensions
# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)

# To extract the results for quantitative variables, type this:
quanti.var <- get_famd_var(res.famd, "quanti.var")
quanti.var$coord
library(corrplot)
corrplot(quanti.var$coord,is.corr = FALSE)
# Analyze quantitative variables
fviz_famd_var(res.famd, "quanti.var", repel = TRUE)
fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var='cos2',gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_famd_var(res.famd, "quanti.var", repel = TRUE,axes=2:3)
# Analyze qualitative variables
fviz_famd_var(res.famd, "quali.var", repel = TRUE)
fviz_famd_var(res.famd, "quali.var", repel = TRUE,
              col.var='cos2',gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_famd_var(res.famd, "quali.var", repel = TRUE,axes=2:3)

# Individuals
fviz_famd_ind(res.famd,repel = TRUE)
fviz_famd_ind(res.famd,repel = TRUE,
              col.ind='cos2',gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Using categorical variables as colors
fviz_ellipses(res.famd, c("Label", "Soil"), repel = TRUE)

library(Factoshiny)
FAMDshiny(wine)

### Optional: bonus
# Repeat with relevant variables
aux <- get_famd_var(res.famd) # Extract the data (instead of plotting)
aux
aux$contrib
# Find the elements that contribute the most to the first 3 dimensions
i <- which(rowSums(aux$contrib[,1:3])/3>1/31*100)
i
df <- wine[,i]
res.famd2 <- FAMD(df)
# You can copy and paste from above and analyze the data

########################################################
# Another analysis to show that sometimes life is hard

library(psych)
bfi
summary(bfi)
data <- na.omit(bfi)
data <- sapply(data,as.factor)
data <- as.data.frame(data)
summary(data)
data$age <- as.numeric(data$age)

res.famd <- FAMD(data,graph=FALSE,ncp=15)
fviz_eig(res.famd) # 4 dims OK!

library(corrplot)
corrplot(res.famd$var$coord)
fviz_famd_var(res.famd)