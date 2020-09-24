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

# fit.knn <- train(Y~.,data.trn,method='knn') # Default k=5
fit.knn <- train(Y~.,data.trn,method='knn',tuneGrid=data.frame(k=28)) # sqrt(nrow(data.trn))=28.3

Plot2DClass(data.trn[,1:2], #Input variables of the model
            data.trn$Y,     #Output variable
            fit.knn,#Fitted model with caret
            var1 = "X1", var2 = "X2",
            selClass = "YES")
pred.knn <- predict(fit.knn,data.tst)
cm.knn <- table(data.tst$Y,pred.knn)
confusionMatrix(cm.knn)
summary(fit.knn)

# Time for logistic regression
fit.lr <- train(Y~.,data.trn,method='glm',family=binomial())
summary(fit.lr)
exp(coef(fit.lr$finalModel))
1/exp(coef(fit.lr$finalModel))
pred.lr <- predict(fit.lr,data.tst)
cm.lr <- table(data.tst$Y,pred.lr)
confusionMatrix(cm.lr)
Plot2DClass(data.trn[,1:2], #Input variables of the model
            data.trn$Y,     #Output variable
            fit.lr,#Fitted model with caret
            var1 = "X1", var2 = "X2",
            selClass = "YES")

# Let's improve the fit using a parabola
data.trn$X1X1 <- data.trn$X1^2
data.tst$X1X1 <- data.tst$X1^2
fit.lr2 <- train(Y~.,data.trn,method='glm',family=binomial())
summary(fit.lr2)
pred.lr2 <- predict(fit.lr2,data.tst)
cm.lr2 <- table(data.tst$Y,pred.lr2)
confusionMatrix(cm.lr2)
