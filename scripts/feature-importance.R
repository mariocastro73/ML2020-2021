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

# The problem of overfitting: Let's compare a two mlp's with 1 and variable neurons each
set.seed(1234)
fit.1 <- train(Y ~ ., data = data.trn, method = "nnet",
  trControl = ctrl, 
 tuneGrid = data.frame(decay=0,size=1))

fit.mlp <- train(Y ~ ., data = data.trn, method = "nnet",
  trControl = ctrl, 
 tuneLength = 5)
plot(fit.mlp)
print(fit.mlp)
# [Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were size = 5 and decay = 0.001.]
# It seems that 5 neurons is better than one...but....
pred <- predict(fit.1,data.tst)
confusionMatrix(table(data.tst[,"Y"],pred))
pred <- predict(fit.mlp,data.tst)
confusionMatrix(table(data.tst[,"Y"],pred))
# Confusion matrices are similar and so they are other metrics
# fit.1: Accuracy : 0.9596 
# fit.mlp: Accuracy : 0.9293
models <- list(net1=fit.1,netcv=fit.mlp)
dotplot(resamples(models))
evalm(models,gnames=c("1","cv"))

# But let our eyes decide...
with(data,plot(X1,as.numeric(Y)-1,col=Y))
x <- seq(0,1,length=100)
lines(x,predict(fit.1,data.frame(X1=x),type = 'prob')$Yes,pch=19,cex=.4,col=3)
lines(x,predict(fit.mlp,data.frame(X1=x),type = 'prob')$Yes,pch=19,cex=.4,col=4)
legend('bottomright',legend=c("1 neuron, decay=0","5 neurons, decay=0.001"),
       col=c(3,4),lwd=2,cex=.8)
# fit.1 wins!



# Variable importance
# Let's create a dummy variable
data$X2 <- runif(nrow(data))
ggplot(data,aes(X1,X2,col=Y)) + geom_point() # X2 is pure noise

train.index <- createDataPartition(data[,"Y"],p=0.8,list=FALSE)
data.trn <- data[train.index,]
data.tst <- data[-train.index,]

ctrl  <- trainControl(method  = "cv",number  = 10,
                      summaryFunction = multiClassSummary,
                      classProbs=T,# Required for the ROC curves
                      savePredictions = T) # Required for the ROC curves

# Same nnets as above
fit.1 <- train(Y ~ ., data = data.trn, method = "nnet",
  trControl = ctrl, 
  preProcess = c("center","scale"), 
  tuneGrid = data.frame(decay=0,size=1))

fit.mlp <- train(Y ~ ., data = data.trn, method = "nnet",
  trControl = ctrl, 
  preProcess = c("center","scale"), 
  tuneLength=5)

pred <- predict(fit.1,data.tst)
confusionMatrix(table(data.tst[,"Y"],pred))
pred <- predict(fit.mlp,data.tst)
confusionMatrix(table(data.tst[,"Y"],pred))
# fit.1: Accuracy : 0.967 
# fit.mlp: Accuracy : 0.9495
plot(fit.mlp)
models <- list(net1=fit.1,netcv=fit.mlp)
dotplot(resamples(models))
evalm(models,gnames=c("1","cv"))
# So how can we check what variable is important?
# Tip #1: Use your eyes
par(mfrow=c(1,2))
with(data,plot(X1~Y,col=c(2,3)))
with(data,plot(X2~Y,col=c(2,3)))
par(mfrow=c(1,1))
# Clearly X2 is random noise

# What else? 
# For some models varImp gives ranks the variables
varImp(fit.1$finalModel)
varImp(fit.mlp$finalModel) # Both agree but note overfitting (for X2)

# Simple correlations to asses variable importance
filterVarImp(data[,-2],data[,2]) # Logistic regression "inside"
# Clearly X2 sucks...

#  Sensitivity analysis
library(NeuralSens)
SensAnalysisMLP(fit.mlp)

########################################################
library(CORElearn)

reliefValues <- attrEval(Y ~ ., data = data,
                           ## There are many Relief methods
                           ## available. See ?attrEval
                           estimator = "ReliefFequalK",
                           ## The number of instances tested:
                           ReliefIterations = 50)
head(reliefValues)
histogram(~X1+X2|Y,data)
