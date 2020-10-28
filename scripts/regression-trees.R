library(caret)
library(rpart.plot)
library(ggplot2)

set.seed(123123)
n <- 120
t <- seq(0,3.5*pi/2,length=n)
data <- data.frame(Predictor=t,Output=sin(t)-.5*sin(3*t)+rnorm(n,0,.3))
ggplot(data,aes(x=Predictor,y=Output)) + geom_point() 

train.index <- createDataPartition(data[,"Output"],p=0.8,list=FALSE)
data.trn <- data[train.index,]
data.tst <- data[-train.index,]

ctrl  <- trainControl(method  = "cv",number  = 10) 
######################################################################################
#  Multilayer pereceptron
######################################################################################
fit.mlp <- train(Output ~ ., data = data.trn, 
                 trControl = ctrl, 
                 method = "nnet",
                 linout = TRUE,
                 maxit = 200,
                 tuneGrid = expand.grid(size = seq(5,50,length.out = 5),
		              decay =  10^(c(-2:2))))
# plot(fit.mlp)
ggplot(fit.mlp)
pred <- predict(fit.mlp,data)
data$pred.mlp <- pred
ggplot(data,aes(x=Predictor,y=Output)) + geom_point() +geom_line(aes(x=Predictor,y=pred.mlp),col='darkorange',size=2)
# Beautiful
ggplot(data,aes(x=Predictor,y=pred.mlp-Output)) + geom_point()
ggplot(data,aes(x=pred.mlp-Output)) + geom_histogram(bins = 10)
ggplot(data,aes(x=pred.mlp-Output)) + geom_boxplot()
RMSE(data$pred.mlp,obs = data$Output)
qqnorm(data$Output-pred)
qqline(data$Output-pred,col=2,lwd=2)

######################################################################################
# SVM
######################################################################################
fit.svm <- train(Output ~ ., data = data.trn, 
                 trControl = ctrl, 
                 method = "svmRadial",
                 linout = TRUE,
                 preProcess = c("center","scale"),
                 tuneLength=10)
# plot(fit.mlp)
ggplot(fit.svm)
pred <- predict(fit.svm,data)
data$pred.svm  <- pred
ggplot(data,aes(x=Predictor,y=Output)) + geom_point() +geom_line(aes(x=Predictor,y=pred.svm),col='darkorange',size=2)
# Beautiful
ggplot(data,aes(x=Predictor,y=pred.svm-Output)) + geom_point()
ggplot(data,aes(x=pred.svm-Output)) + geom_histogram(bins = 10)
ggplot(data,aes(x=pred.svm-Output)) + geom_boxplot()
RMSE(data$pred.svm,obs = data$Output) # Minimum error but....
qqnorm(data$Output-pred)
qqline(data$Output-pred,col=2,lwd=2) # Ups,clearly SVM doesn't care for outliers

######################################################################################
# Regression trees
######################################################################################
fit.rpart <- train(Output ~ ., data = data.trn, method = "rpart",
  trControl = ctrl, 
  preProcess = c("center","scale"), 
  tuneGrid=data.frame(cp=10^seq(-5,-2,length=10)))
  # tuneLength = 20)
ggplot(fit.rpart)
# plot(fit.rpart)
fit.rpart
pred <- predict(fit.rpart,data)
data$pred.tree <- pred
ggplot(data,aes(x=Predictor,y=Output)) + geom_point() +geom_line(aes(x=Predictor,y=pred.tree),col='darkorange',size=2)
# plot(data)
# lines(data$Predictor,pred,col='darkorange',lwd=2)
ggplot(data,aes(x=Predictor,y=pred.tree-Output)) + geom_point()
ggplot(data,aes(x=pred.tree-Output)) + geom_histogram(bins = 10)
ggplot(data,aes(x=pred.tree-Output)) + geom_boxplot()
# plot(data$Output-pred~data$Predictor)
# hist(data$Output-pred)
# boxplot(data$Output-pred)
qqnorm(data$Output-pred)
qqline(data$Output-pred,col=2,lwd=2)

rpart.plot(fit.rpart$finalModel)
RMSE(data$pred.tree,obs = data$Output)

# Forcing cp
set.seed(1234)
fit.rpart <- train(Output ~ ., data = data.trn, method = "rpart",
                trControl = ctrl, 
                preProcess = c("center","scale"), 
                tuneGrid=data.frame(cp=.05))
# tuneLength = 20)
pred <- predict(fit.rpart,data)
rpart.plot(fit.rpart$finalModel)
RMSE(pred,obs = data$Output)

######################################################################################
# Let's play with random forests
######################################################################################
set.seed(1234)
Diabetes <- read.csv('datasets/Diabetes.csv',sep=';')
summary(Diabetes)

fit.rf <- train(GLUCOSE ~ BLOODPRESS+SKINTHICKNESS+INSULIN+BODYMASSINDEX+AGE, data = Diabetes, method = "rf",
                trControl = ctrl, 
                preProcess = c("center","scale"),
                tuneLength = 4)
fit.rf
pred <- predict(fit.rpart,data)
plot(data)
lines(data$Predictor,pred,col='darkorange',lwd=2)
rpart.plot(fit.rpart$finalModel)
