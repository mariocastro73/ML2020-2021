library(psych)
set.seed(1919)
n <- 75
x <- 2+rnorm(n)
y <- 1-x+1.5*x^2+rnorm(n)
data <- data.frame(x=x,y=y)
plot(data)
summary(fit1 <- lm(y~x,data))
abline(fit1,lwd=2,col=2)
par(mfrow=c(2,2))
plot(fit1)
par(mfrow=c(1,1))

# Test different methods
data$x2 <- data$x^2
summary(fit2 <- lm(y~.,data))
summary(fit2.bis <- lm(y~x+I(x^2),data))
par(mfrow=c(2,2))
plot(fit2.bis)
summary(fit3 <- lm(y~x+I(x^2)+I(x^3),data))
plot(fit3)

x11()
par(mar=c(5.1,7.1,4.1,2.1)) # Change margins
par(mfrow=c(1,3))
plot(data$y,predict(fit1,data),main='Linear regression',pch=19,
     cex=1.5,cex.lab=2,cex.axis=2,xlab='data',ylab='prediction',cex.main=2)
abline(0,1,col=2,lwd=4)
plot(data$y,predict(fit1,data),main='Quadratic regression',pch=19,
     cex=1.5,cex.lab=2,cex.axis=2,xlab='data',ylab='prediction',cex.main=2)
abline(0,1,col=2,lwd=4)
