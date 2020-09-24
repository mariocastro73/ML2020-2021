library(MLTools)
library(caret)

# Let's play a little bit with knn and logistic-regression
data <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/data-for-knn.csv')
set.seed(1234)
train.idx <- createDataPartition(data$Y,p=0.8,list=FALSE)
data.trn <- data[train.idx,]
data.tst <- data[-train.idx,]
str(data.trn)
str(data.tst)

fit.knn <- train(Y ~ ., data = data.trn, 
                 method = "knn", 
                 preProcess = c("center","scale"),
                 tuneGrid = data.frame(k=5))
fit.knn
Plot2DClass(data.trn[,1:2], #Input variables of the model
            data.trn$Y,     #Output variable
            fit.knn,#Fitted model with caret
            var1 = "X1", var2 = "X2",
            selClass = "YES")
pred.knn <- predict(fit.knn,data.tst)
cm.knn <- table(data.tst$Y,pred.knn)
confusionMatrix(cm.knn)
# Choosing manually the values of the knn parameter "k"
ctrl  <- trainControl(method  = "cv",number  = 10)
fit.knn.cv <- train(Y ~ ., data = data.trn, method = "knn",
                    trControl = ctrl, 
                    preProcess = c("center","scale"), 
                    tuneGrid =data.frame(k=seq(1,100,by=4)))
                    # tuneLength = 50)
print(fit.knn.cv)
plot(fit.knn.cv)

# Using tuneLength
ctrl  <- trainControl(method  = "cv",number  = 10)
fit.knn.cv <- train(Y ~ ., data = data.trn, method = "knn",
                    trControl = ctrl, 
                    preProcess = c("center","scale"), 
                    tuneLength = 30) # Try 30 (odd) values
print(fit.knn.cv)
plot(fit.knn.cv)

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

# Logistic regression
fit.lr <- train(Y ~ ., data = data.trn, method = "glm",
                    family=binomial(),
                    preProcess = c("center","scale"))
print(fit.lr)

ctrl  <- trainControl(method  = "cv",number  = 10)
fit.lr.cv <- train(Y ~ ., data = data.trn, method = "glm",
                    family=binomial(),
                    trControl = ctrl, 
                    preProcess = c("center","scale"), 
                    tuneLength = 30) # Try 30 (odd) values
print(fit.lr.cv)

Plot2DClass(data.trn[,1:2], #Input variables of the model
            data.trn$Y,     #Output variable
            fit.lr.cv,#Fitted model with caret
            var1 = "X1", var2 = "X2",
            selClass = "YES")

# Partition tree
ctrl  <- trainControl(method  = "cv",number  = 10)
fit.tree <- train(Y ~ ., data = data.trn, method = "rpart",
                trControl = ctrl, 
                preProcess = c("center","scale"),
                tuneLength=20)
print(fit.tree)
plot(fit.tree)
rpart.plot(fit.tree$finalModel)
