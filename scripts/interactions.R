
set.seed(999)
######################################################################################

kidiq <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/kidiq.csv')
# str(kidiq)
par(mfrow=c(1,1))

with(kidiq,plot(kid_score~mom_iq,xlab="Mom's IQ",ylab="Kid's score",pch=19,cex.axis=1.5,cex.lab=2))

summary(fit.hs <- lm(kid_score ~ mom_hs, kidiq))
summary(fit.iq <- lm(kid_score ~ mom_iq, kidiq))
abline(fit.iq,col=2,lwd=3)
par(mfrow=c(2,2))
plot(fit.iq)

summary(fit.additive <- lm(kid_score ~ mom_hs+mom_iq, kidiq))
par(mfrow=c(2,2))
plot(fit.additive)

library(psych)
pairs.panels(kidiq[,1:3])

summary(fit.interaction <- lm(kid_score ~ mom_hs*mom_iq, kidiq))
par(mfrow=c(2,2))
plot(fit.interaction)
# Look at the residuals
boxplot(fit.interaction$residuals~kidiq$mom_hs)
plot(fit.interaction$residuals~kidiq$mom_iq)

summary(fit <- train(kid_score ~ mom_hs*mom_iq, kidiq,method='lm'))


######################################################################################
# MOCK Data
######################################################################################
n <- 300
noise <- 0.2
x1 <- runif(n)
x2 <- runif(n)
y <- 10 + 2*x1 + 3*x2 + 5*x1*x2 + rnorm(n,0,noise)
data <- data.frame(x1=x1,x2=x2,y=y)
pairs.panels(data)

print(summary(fit1 <- lm(y ~ x1+x2,data)))
# Looks good, but...
par(mfrow=c(2,2))
plot(fit1)
# Always do this
pairs.panels(data.frame(data$x1,data$x2,fit1$residuals)) # Panels in the last line look terrible

######################################################################################
# Let's use interactions
print(summary(fit2 <- lm(y ~ x1*x2,data))) # R^2 is better that doesn't count until we check the residuals
plot(fit2) # Bingo!

print(sd(fit1$residuals))
print(sd(fit2$residuals))

# Always do this
pairs.panels(data.frame(data$x1,data$x2,fit2$residuals)) # Panels in the last line look really nice.

######################################################################################
# Use cross validation to see how well the model generalizes to new data

library(caret)
fit.cv <- train(y ~ x1+x2, data = data, method = "lm",
                trControl = trainControl(method  = "cv",number  = 10), 
                preProcess = c("center","scale"))  
print(fit.cv)
print(fit.cv$resample)
summary(fit.cv$resample)

fit.cv2 <- train(y ~ x1*x2, data = data, method = "lm",
                 trControl = trainControl(method  = "cv",number  = 10), 
                 preProcess = c("center","scale"))  
print(fit.cv2)
print(fit.cv2$resample)
summary(fit.cv2$resample)
summary(fit.cv$resample)



######################################################################################
# Interaction with factors
set.seed(999)
n <- 300
noise <- 0.2
x1 <- runif(n)
x2 <- rep(c(0,2,1),n/3)
y <- 10 + 2*x1 + 3*(x2>0) + 5*x1*(x2>0) + rnorm(n,0,noise)
data <- data.frame(x1=x1,x2n=x2,x2=as.factor(x2),y=y)
pairs.panels(data)
library(ggplot2)
ggplot(data,aes(x1,y,col=x2)) +geom_point()

par(mfrow=c(2,2))

print(summary(fit0 <- lm(y~ x1+x2n,data)))
plot(fit0)
print(summary(fit1 <- lm(y~ x1+x2,data)))
par(mfrow=c(2,2))
plot(fit1)
print(summary(fit2 <- lm(y~ x1*x2,data)))
plot(fit2)

