library(psych)
set.seed(999)
######################################################################################

kidiq <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/kidiq.csv')
str(kidiq)
with(kidiq,plot(kid_score~mom_iq))

summary(fit <- train(kid_score ~ mom_iq, kidiq,method='lm'))
abline(fit$finalModel,col=2,lwd=3)
par(mfrow=c(2,2))
plot(fit$finalModel)

summary(fit <- train(kid_score ~ mom_hs+mom_iq, kidiq,method='lm'))
par(mfrow=c(2,2))
plot(fit$finalModel)

pairs.panels(kidiq[,1:3])

summary(fit <- train(kid_score ~ mom_hs*mom_iq, kidiq),method='lm')
par(mfrow=c(2,2))
plot(fit$finalModel)
par(mfrow=c(1,1))



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

# Another way to plot it 
# par(mfrow=c(1,1))
# pred <- predict(fit1,data)
# plot(pred,fit1$residuals)

######################################################################################
# Let's use interactions
print(summary(fit2 <- lm(y ~ x1*x2,data))) # R^2 is better that doesn't count until we check the residuals
plot(fit2) # Bingo!

print(sd(fit1$residuals))
print(sd(fit2$residuals))

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

