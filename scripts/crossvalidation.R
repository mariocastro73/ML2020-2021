library(MLTools)
library(caret)

# Let's play a little bit with knn
data <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/data-for-knn.csv')
set.seed(1234)

# Use the caret snippet 
train <- createDataPartition(data[,"Y"],p=0.8,list=FALSE)
data.trn <- data[train,]
data.tst <- data[-train,]

ctrl  <- trainControl(method  = "cv",number  = 10)#,classProbs = TRUE)

fit.cv <- train(Y ~ ., data = data.trn, method = "knn",
  trControl = ctrl, 
  preProcess = c("center","scale"), 
  # tuneGrid =data.frame(k=c(5,10,25,100)))
  tuneLength = 25)

pred <- predict(fit.cv,data.tst)
confusionMatrix(table(data.tst[,"Y"],pred))
print(fit.cv)
plot(fit.cv)

library(lattice)
data.tst$probs <- predict(fit.cv,data.tst,type = 'prob')$YES
histogram(~probs|Y,data.tst)




Plot2DClass(data.trn[,1:2], #Input variables of the model
            data.trn$Y,     #Output variable
            fit.knn,#Fitted model with caret
            var1 = "X1", var2 = "X2",
            selClass = "YES")


# Including additional metrics for CV  (beyond Accuracy)
ctrl  <- trainControl(method  = "cv",number  = 10, summaryFunction = multiClassSummary)
fit.knn.cv <- train(Y ~ ., data = data.trn, method = "knn",
                    trControl = ctrl, 
                    preProcess = c("center","scale"), 
                    tuneLength = 30)
print(fit.knn.cv)
plot(fit.knn.cv)
plot(fit.knn.cv,metric = "Specificity")
plot(fit.knn.cv,metric = "Sensitivity")
plot(fit.knn.cv,metric = "Kappa")

# Partition tree
ctrl  <- trainControl(method  = "cv",number  = 10)
fit.tree <- train(Y ~ ., data = data.trn, method = "rpart",
                trControl = ctrl, 
                preProcess = c("center","scale"),
                tuneLength=20)
print(fit.tree)
plot(fit.tree)
rpart.plot(fit.tree$finalModel)
Plot2DClass(data.trn[,1:2], #Input variables of the model
            data.trn$Y,     #Output variable
            fit.tree,#Fitted model with caret
            var1 = "X1", var2 = "X2",
            selClass = "YES")
