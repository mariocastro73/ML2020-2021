library(rpart.plot)

ad <- read.csv('datasets/advertising.csv')
ad <- ad[,-c(5,6,8,9)]
str(ad)
summary(ad)
ad$Clicked.on.Ad <- as.factor(ifelse(ad$Clicked.on.Ad==0,"No","Yes"))

train <- createDataPartition(ad[,"Clicked.on.Ad"],p=0.8,list=FALSE)
ad.trn <- ad[train,]
ad.tst <- ad[-train,]

ctrl <- trainControl(method="cv", number = 10,
                     summaryFunction=multiClassSummary, 
                     classProbs=T,# Required for the ROC curves
                     savePredictions = T) # Required for the ROC curves
                      

fit.tree <- train(Clicked.on.Ad ~ ., data = ad.trn, method = "rpart",
  trControl = ctrl, 
  # tuneGrid =data.frame(cp=.1))
  tuneLength = 10)

print(fit.tree)
plot(fit.tree)

rpart.plot(fit.tree$finalModel,fallen.leaves = F)
pred <- predict(fit.tree,ad.tst)
confusionMatrix(table(ad.tst[,"Clicked.on.Ad"],pred))
plot(varImp(fit.tree))


#### random forest

fit.rf <- train(Clicked.on.Ad ~ ., data = ad.trn, method = "rf",
                trControl = ctrl, 
                # tuneGrid =data.frame(cp=.1))
                tuneLength = 10)

print(fit.rf)
plot(fit.rf)

pred <- predict(fit.rf,ad.tst)
confusionMatrix(table(ad.tst[,"Clicked.on.Ad"],pred))
plot(varImp(fit.tree))
plot(varImp(fit.rf))
models<-list(tree=fit.tree,RF=fit.rf)
evalm(models,gnames = c("Tree","RF"))
dotplot(resamples(models))
