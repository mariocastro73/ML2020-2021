library(caret)
# featurePlot
data(iris)
str(iris)
featurePlot(x=iris[,1:4],y=iris$Species,plot='pairs')

# NA imputation
Titanic <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/Titanic.csv")
age.mean <- mean(Titanic$Age,na.rm = TRUE)
age.sd <- sd(Titanic$Age,na.rm = TRUE)
pre <- preProcess(Titanic, method = "knnImpute", k = 5)
Titanic.imputed <- predict(pre,Titanic)
Titanic$Age[10:15]
Titanic.imputed$Age[10:15]*age.sd+age.mean

# preprocessing
data(iris)
summary(iris)
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(iris[,1:4], method=c("range"))
# transform the dataset using the pre-processing parameters
transformed <- cbind(predict(preprocessParams, iris[,1:4]),Species=iris$Species)
# summarize the transformed dataset
summary(transformed)

# dummyVars
data <- data.frame(vars=as.factor(c("Brown","Blonde","Black","Red","Grey")))
dv <- dummyVars(~vars,data=data,fullRank = T)
cbind(data,predict(dv,data))

# near zero values
data(mdrr)
dim(mdrrDescr)
# summary(mdrrDescr[,c("MW","nTB")]) # Look that nTB is almost 0
nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
filteredDescr <- mdrrDescr[, !nzv$nzv]
dim(x=filteredDescr) # We have removed 45 features

#  data partitioning
# install.packages('ISLR')
data(Default, package = "ISLR")
dim(Default)
default_idx = createDataPartition(Default$default, p = 0.75, list = FALSE)
default_trn = Default[default_idx, ]
default_tst = Default[-default_idx, ]
str(default_trn) # Training
str(default_tst) # Testing

# Fitting like a pro
admision <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/college.csv")
fit.logistic <-  train(admit ~ .,data = admision.trn,method = "glm",family = "binomial")
fit.knn <-  train(admit ~ .,data = admision.trn,method = "knn")
pred.logistic <- predict(fit.logistic,admision.tst) # Different from predict for glm(...)
pred.knn  <- predict(fit.knn,admision.tst) # Different from predict after a knn(...) 
table(admision.tst$admit,pred.logistic) # More accurate than knn
table(admision.tst$admit,pred.knn)
# Comparing preditions
library(mlbench)
results <- resamples(list(LR=fit.logistic, KNN=fit.knn)) # resamples is a caret function
dotplot(results) # dotplot is an mlbech function

