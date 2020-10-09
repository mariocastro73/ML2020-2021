library(ggplot2)
library(caret)
library(NeuralNetTools)
library(nnet)
library(MLeval)

set.seed(1234)
data <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/simple-class.csv")
str(data)
summary(data)
data$Y <- as.factor(ifelse(data$Y==0,"No","Yes"))

with(data,plot(data,Y,col=Y))
# ggplot(data,aes(X1,Y,col=Y)) + geom_point()

train.index <- createDataPartition(data[,"Y"],p=0.8,list=FALSE)
data.trn <- data[train.index,]
data.tst <- data[-train.index,]

ctrl  <- trainControl(method  = "repeatedcv",
            repeats=5,
            number  = 10, # 10 folds
            classProbs=T,# Required for the ROC curves
            savePredictions = T) # Required for the ROC curves
set.seed(1234)
fit.1 <- train(Y ~ ., data = data.trn, method = "nnet",
  trControl = ctrl, 
 tuneGrid = data.frame(decay=0,size=1))

fit.mlp <- train(Y ~ ., data = data.trn, method = "nnet",
  trControl = ctrl, 
 tuneLength = 5)
plot(fit.mlp)
print(fit.mlp)
pred <- predict(fit.1,data.tst)
table(data.tst[,"Y"],pred)
pred <- predict(fit.mlp,data.tst)
table(data.tst[,"Y"],pred)


with(data,plot(X1,as.numeric(Y)-1,col=Y))
x <- seq(0,1,length=100)
lines(x,predict(fit.1,data.frame(X1=x),type = 'prob')$Yes,pch=19,cex=.4,col=3)
lines(x,predict(fit.mlp,data.frame(X1=x),type = 'prob')$Yes,pch=19,cex=.4,col=4)
legend('bottomright',legend=c("1 neuron, decay=0","5 neurons, decay=0.001"),
       col=c(3,4),lwd=2,cex=.8)

models <- list(net1=fit.1,netcv=fit.mlp)
dotplot(resamples(models))
evalm(models,gnames=c("1","cv"))

# Dummy variable


data$X2 <- runif(nrow(data))
ggplot(data,aes(X1,X2,col=Y)) + geom_point()

set.seed(1234)
train.index <- createDataPartition(data[,"Y"],p=0.8,list=FALSE)
data.trn <- data[train.index,]
data.tst <- data[-train.index,]
fit.1 <- train(Y ~ ., data = data.trn, method = "nnet",
               trControl = ctrl, 
               tuneGrid = data.frame(decay=0,size=1))

fit.mlp <- train(Y ~ ., data = data.trn, method = "nnet",
                 trControl = ctrl, 
                 tuneLength = 5)
plot(fit.mlp)
print(fit.mlp)
pred <- predict(fit.1,data.tst)
table(data.tst[,"Y"],pred)
pred <- predict(fit.mlp,data.tst)
table(data.tst[,"Y"],pred)

varImp(fit.1$finalModel)
varImp(fit.mlp$finalModel)

with(data,plot(X1~Y,col=c(2,3)))
with(data,plot(X2~Y,col=c(2,3)))
histogram(~X1+X2|Y,data)

models <- list(net1=fit.1,netcv=fit.mlp)
dotplot(resamples(models))
evalm(models,gnames=c("1","cv"))

library(NeuralSens)
SensAnalysisMLP(fit.mlp)


