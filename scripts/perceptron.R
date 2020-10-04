library(ggplot2)
library(caret)
library(NeuralNetTools)
library(nnet)

set.seed(9191)
dose <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/dose0.csv")
with(dose,plot(Dose,Survived,col=Survived))

fit.glm <- train(Survived ~ Dose, data = dose, method='glm',
                   family=binomial())

fit.nnet <- train(Survived ~ Dose, data = dose, method='nnet',
                   tuneGrid = data.frame(size=0,decay=0),skip=TRUE)
summary(fit.glm)
summary(fit.nnet)

plotnet(fit.nnet$finalModel)

activation.function <- function(x,bias,weight) {
  return(1/(1+exp(-(bias+weight*x))))
}


pred.glm <- predict(fit.glm,dose)
table(dose$Survived,pred.glm)
# confusionMatrix(table(dose$Survived,pred.glm))
pred.nnet<- predict(fit.nnet,dose)
table(dose$Survived,pred.nnet)


# x11() # If you 
with(dose,{
  y <- as.numeric(Survived)-1
  plot(Dose,y,col=Survived,pch=19,cex=2,cex.lab=2,cex.axis=1.5,ylab="")
  })
x <- seq(0.,0.7,by=0.1)
points(x,activation.function(x,-12.7,45.56),pch=19,col=4,cex=4)
x <- seq(0,.7,length=100)
lines(x,activation.function(x,-12.7,45.56),col='orange',lwd=4)

##################### New dataset ###############################################

dose <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/dose.csv")
with(dose,{
  y <- as.numeric(Survived)-1
  plot(Dose,y,col=Survived,pch=19,cex=2,cex.lab=2,cex.axis=1.5,ylab="")
})

fit.glm <- train(Survived ~ Dose, data = dose, method='glm',
                 family=binomial())

fit.nnet <- train(Survived ~ Dose, data = dose, method='nnet',
                  tuneGrid = data.frame(size=0,decay=0),skip=TRUE)

fit.nnet2 <- train(Survived ~ Dose, data = dose, method='nnet',
                   tuneGrid = data.frame(size=2,decay=0))
plotnet(fit.nnet2)
summary(fit.nnet2)

pred.glm <- predict(fit.glm,dose)
table(dose$Survived,pred.glm)
pred.nnet<- predict(fit.nnet,dose)
table(dose$Survived,pred.nnet)
pred.nnet2<- predict(fit.nnet2,dose)
table(dose$Survived,pred.nnet2)


x <- seq(0,1,length=100)
with(dose,plot(Dose,as.numeric(Survived)-1,col=Survived,cex=2,pch=19))
lines(x,predict(fit.glm,data.frame(Dose=x),type = 'prob')$Yes)
lines(x,predict(fit.nnet,data.frame(Dose=x),type = 'prob')$Yes,col=3)
lines(x,predict(fit.nnet2,data.frame(Dose=x),type = 'prob')$Yes,col='orange',lwd=3)

summary(fit.nnet2)
with(dose,plot(Dose,as.numeric(Survived)-1,col=Survived,cex=1.5,pch=19,cex.lab=2,cex.axis=1.5,ylab=""))
lines(x,y <- activation.function(x,6.22,-6.41),col='blue',lwd=4)
lines(x,z <- activation.function(x,-6.09  ,29.63 ),col='darkgreen',lwd=4)
lines(x,1/(1+exp(-(-38.13 +y* 19.48 +z* 22.43))),col=2,lwd=3)

######################################################################################

library(MLeval)

fit.nnet2 <- train(Survived ~ Dose, data = dose, method='nnet',
                   tuneGrid = data.frame(size=2,decay=0))
evalm(list(fit.nnet2))


train <- createDataPartition(dose[,"Survived"],p=0.8,list=FALSE)
dose.trn <- dose[train,]
dose.tst <- dose[-train,]

ctrl  <- trainControl(method  = "cv",number  = 10, summaryFunction=multiClassSummary, 
                      classProbs=T,# Required for the ROC curves
                      savePredictions = T) # Required for the ROC curves

fit.nnet2 <- train(Survived ~ Dose, data = dose.trn, method = "nnet",
  trControl = ctrl, 
  # tuneGrid =data.frame(k=10))
  tuneLength = 5)
plot(fit.nnet2)
evalm(fit.nnet2,plots="r")
pred.nnet2<- predict(fit.nnet2,dose)
table(dose$Survived,pred.nnet2)

fit.nnet2 <- train(Survived ~ Dose, data = dose.trn, method = "nnet",
  trControl = ctrl, 
  tuneGrid =data.frame(size=3,decay=c(0,0.0001,0.001,0.01,0.1)))
  # tuneLength = 5)
plot(fit.nnet2)
evalm(fit.nnet2,plots="r")
pred.nnet2<- predict(fit.nnet2,dose)
table(dose$Survived,pred.nnet2)
