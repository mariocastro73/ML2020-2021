library(caret)
bmi <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/bmi.csv")
str(bmi)
with(bmi,plot(Body.Fat~BMI,pch=19,col=4))
summary(fit <- lm(Body.Fat~BMI,bmi))
abline(fit,lwd=2,col=2)

fit.cv <- train(Body.Fat ~ BMI, data = bmi, method = "lm",
  trControl = trainControl(method  = "cv",number  = 10), 
  preProcess = c("center","scale"))  
summary(fit.cv$finalModel)
summary(fit.cv$resample)

### Motivation for Ridge and the LASSO
set.seed(123)
n <- 20
x1 <- runif(n)
x2 <- runif(n)
y <- 2+3*x1+1*x2+rnorm(n,0,1)
data <- data.frame(x1,x2,y)
print(summary(fit <- lm(y~x1+x2,data))) # Linear regression us trying to minimize error, whatever it takes
library(psych)
pairs.panels(data)
cc0 <- coef(fit)
# Ridge regression with CV
ridge.fit = train(y ~ .,data = data,method = "glmnet",
                  tuneGrid = expand.grid(
                    lambda = 10^ seq(-3,1, length =20), # Lambda cannot be greater than 10^6
                    alpha = 0), # alpha=0 for ridge regression
                  trControl = trainControl(method  = "cv",number  = 10))

ridge.fit #information about the resampling
par(mar=c(5.1,7.1,4.1,2.1)) # Change margins
plot(ridge.fit$finalModel,ylim=c(0,4),cex.lab=2,cex.axis=1.5,lwd=3)
abline(h=3)
abline(h=1,col=2)
abline(h=cc0[2],lty=3)
abline(h=cc0[3],col=2,lty=3)
# ggplot(ridge.fit)+scale_x_log10()
cc <- coef(ridge.fit$finalModel, ridge.fit$bestTune$lambda)
abline(h=cc[2],lty=2)
abline(h=cc[3],lty=2,col=2)

######################################################################################
# Partial least squares
library(caret)
library(AppliedPredictiveModeling)
data(solubility)
data <- solTrainXtrans
data$solubility = solTrainY

set.seed(100)
fit.pls <- train(solubility ~ ., data = data, method = "pls",
  trControl = trainControl(method  = "cv",number  = 10), 
  preProcess = c("center","scale"), 
  tuneLength = 100)

print(fit.pls)
plot(fit.pls)

par(mar=c(5.1,7.1,4.1,2.1)) # Change margins
plot(data$solubility,predict(fit.pls, data, ncomp=18),xlab='Data',ylab='Prediction',cex.lab=1.7,cex.axis=1.5)
abline(0,1,lwd=3,col=2)

hist(data$solubility-predict(fit.pls, data, ncomp=18),xlab='residuals',cex.lab=1.7,cex.axis=1.5,col='skyblue',main="")
