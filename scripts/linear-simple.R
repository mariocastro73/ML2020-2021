data <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/anscombe.csv')
with(d,plot(x1,y1,pch=20,col=4,cex=2))
abline(2,.5,lwd=3,col=3)
abline(5,.25,lwd=3,col=4)
abline(3,.5,lwd=3,col=2)

for(a in seq(1,5,length=5)) {
    for(b in seq(0,1,length=5)) {
         yest <- b*d$x1+a
         res <-sum((d$y1-yest)^2)
         cat(c(a,b,res,"\n"))
       }
}

fit <- lm(y1~x1,d)
summary(fit)
par(mfrow=c(1,1))
with(d,plot(y1,pch=20,col=4,cex=2))
abline(h=mean(d$y1),lwd=3,col=2)
with(d,plot(x1,y1,pch=20,col=4,cex=2))
abline(fit,lwd=3,col=2)

fit2 <- train(y1 ~ x1, data = d, method = "lm")
summary(fit2)
pred <- predict(fit.cv,data.tst)


# How to interpret the coefficients and the correlation?
bmi <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/bmi.csv")
with(bmi,plot(Fat~BMI,pch=19,col=4))
summary(fit <- lm(Fat~BMI,bmi))
abline(fit,lwd=2,col=2)

# Correlation is the slope for the standardize variables
f <- preProcess(bmi,method = c("center","scale")) # Standardize with caret
bmi.new <- predict(f,bmi)
with(bmi.new,plot(Fat~BMI,pch=19,col=4))
points(0,0,pch=19,col=2,cex=3)
summary(fit <- lm(Fat~BMI,bmi.new))
abline(fit,col=2,lwd=2)
print(coef(fit)[2]^2)
