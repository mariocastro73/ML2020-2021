# 
kidiq <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/kidiq.csv')
str(kidiq)
cols <- c('skyblue','orange')
print(with(kidiq,boxplot(kid_score ~ as.factor(mom_hs),xlab='Mom finished High School?',ylab="Kid's score",
                         col=cols,cex.axis=1.7,cex.lab=2))) 
set.seed(1234)

summary(fit <- lm(kid_score~mom_hs,kidiq))
par(mar=c(5.1,7.1,4.1,2.1)) # Change margins
with(kidiq,plot(kid_score~jitter(mom_hs,.1),cex=.5,xlab='Mom finished High School?',ylab="Kid's score",
                col=cols[mom_hs+1],pch=19,cex.axis=1.7,cex.lab=2))
abline(fit,col=2,lwd=4)
kidiq$mom_hs2 <- 2*kidiq$mom_hs-1
summary(fit <- lm(kid_score~mom_hs2,kidiq))
par(mar=c(5.1,7.1,4.1,2.1)) # Change margins
with(kidiq,plot(kid_score~jitter(mom_hs2,.1),cex=.5,xlab='Mom finished High School?',ylab="Kid's score",
                col=cols[mom_hs+1],pch=19,cex.axis=1.7,cex.lab=2))
abline(fit,col=2,lwd=4)

# Here we can see that the median and confidence intervals are disjoint, so there is a huge difference between groups
# We should transform mom_hs to factor, but linear regression works with 0's and 1's
# Let's use linear regression
summary(fit <- lm(kid_score~mom_hs,kidiq))
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

# Same with caret
library(caret)
fit <- train(kid_score ~ mom_hs, data = kidiq, method = "lm")
par(mfrow=c(2,2))
plot(fit$finalModel)


# Mock data for simpson's paradox
x1 <- rnorm(10)
x2 <- rnorm(10,3)
y <- c(x1,x2)
x <- rep(c(0,1),each=10)
summary(fit <- lm(y~x))
par(mfrow=c(2,2))
plot(fit)
x <- rep(c(-1,1),each=10)
plot(d$y~d$x)
summary(fit <- lm(y~x))
par(mfrow=c(2,2))
plot(fit)
