library(caret)
library(rpart.plot)
library(MLeval)

bw <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/birth-weight.csv")
str(bw)
train <- createDataPartition(bw[,"low"],p=0.8,list=FALSE)
bw.trn <- bw[train,]
bw.tst <- bw[-train,]

ctrl  <- trainControl(method  = "cv",number  = 10, 
                      summaryFunction =twoClassSummary,
                      classProbs=T,# Required for the ROC curves,
                      savePredictions = T) # Required for the ROC curves

fit.tree <- train(low ~ ., data = bw.trn, method = "rpart",
  trControl = ctrl, 
  # tuneGrid =data.frame(cp=0.01))
  tuneLength = 10)

pred <- predict(fit.tree,bw.tst)
confusionMatrix(table(bw.tst[,"low"],pred))
print(fit.tree)
plot(fit.tree)
rpart.plot(fit.tree$finalModel,fallen.leaves = F)
plot(varImp(fit.tree))
evalm(fit.tree)


#########


fit.rf <- train(low ~ ., data = bw.trn, method = "rf",
                  trControl = ctrl, 
                  # tuneGrid =data.frame(cp=0.01))
                  tuneLength = 10)

plot(varImp(fit.rf))
evalm(fit.rf)


fit.knn <- train(low ~ ., data = bw.trn, method = "knn",
                trControl = ctrl, 
                # tuneGrid =data.frame(cp=0.01))
                tuneLength = 10)

plot(varImp(fit.knn))
evalm(fit.knn)

fit.glm <- train(low ~ ., data = bw.trn, method = "glm",
                 family=binomial(),
                trControl = ctrl, 
                # tuneGrid =data.frame(cp=0.01))
                tuneLength = 10)

plot(varImp(fit.glm))
evalm(fit.glm)
exp(coef(fit.glm$finalModel))
dotplot(resamples(list(knn=fit.knn,tree=fit.tree,rf=fit.rf,logreg=fit.glm)))
