library(caret)
library(rpart.plot)
set.seed(999)
n <- 120
t <- seq(0,3.5*pi/2,length=n)
data <- data.frame(Predictor=t,Output=sin(t)-.5*sin(2*t)+rnorm(n,0,.1))

train.index <- createDataPartition(data[,"Output"],p=0.8,list=FALSE)
data.trn <- data[train.index,]
data.tst <- data[-train.index,]

ctrl  <- trainControl(method  = "cv",number  = 10) 
#, summaryFunction = multiClassSummary,
# classProbs=T,# Required for the ROC curves
# savePredictions = T) # Required for the ROC curves

fit.cv <- train(Output ~ ., data = data.trn, method = "rpart",
  trControl = ctrl, 
  preProcess = c("center","scale"), 
  tuneGrid=data.frame(cp=10^seq(-5,-2,length=10)))
  # tuneLength = 20)
plot(fit.cv)
fit.cv
pred <- predict(fit.cv,data)
plot(data)
lines(data$Predictor,pred,col='darkorange',lwd=2)
plot(data$Output-pred~data$Predictor)
hist(data$Output-pred)
boxplot(data$Output-pred)
qqnorm(data$Output-pred)
qqline(data$Output-pred,col=2,lwd=2)

rpart.plot(fit.cv$finalModel)


# Forcing cp
set.seed(1234)
fit.cv <- train(Output ~ ., data = data.trn, method = "rpart",
                trControl = ctrl, 
                preProcess = c("center","scale"), 
                tuneGrid=data.frame(cp=.05))
# tuneLength = 20)
pred <- predict(fit.cv,data)
plot(data)
lines(data$Predictor,pred,col='darkorange',lwd=2)
rpart.plot(fit.cv$finalModel)
