## Comparing different methods using caret and ROC curves
library(MLeval)
library(caret)
## load data and run Caret
set.seed(75757575)
data(Sonar)
ctrl <- trainControl(method="cv", summaryFunction=twoClassSummary, 
                     classProbs=T,# Required for the ROC curves
                     savePredictions = T) # Required for the ROC curves
fit1 <- train(Class ~ .,data=Sonar,method="rpart",trControl=ctrl,tuneLength=20)
fit2 <- train(Class ~ .,data=Sonar,method="knn",trControl=ctrl,tuneLength=20)
fit3 <- train(Class ~ .,data=Sonar,method="rf",trControl=ctrl,tuneLength=5)
fit4 <- train(Class ~ .,data=Sonar,method="svmLinear",trControl=ctrl,tuneLength=5)
fit5 <- train(Class ~ .,data=Sonar,method="svmPoly",trControl=ctrl,tuneLength=5)
fit6 <- train(Class ~ .,data=Sonar,method="svmRadial",trControl=ctrl,tuneLength=5)
fit7 <- train(Class ~ .,data=Sonar,method="glm",trControl=ctrl,family=binomial())


## run MLeval
models <- list(rpart=fit1,knn=fit2,rf=fit3,linear=fit4,poly=fit5,radial=fit6,log.reg=fit7)
res <- evalm(models,gnames=c('rpart','knn','rf','linear','poly','radial','logistic'),plots='r')
summary(resamples(models))
dotplot(resamples(models))

