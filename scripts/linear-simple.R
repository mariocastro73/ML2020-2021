######################################################################################
# Import dataset
d <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/anscombe.csv')
with(d,plot(x1,y1,pch=20,col=4,cex=2)) # Basic plotting
abline(2,.5,lwd=3,col=3)
abline(5,.25,lwd=3,col=4)
abline(3,.5,lwd=3,col=2) # Best line

# for fun (Anscombe's quartet)
par(mfrow=c(2,2))
with(d,{
  plot(y1~x1,pch=19,col='darkorange')
  abline(fit <- lm(y1~x1))
  print(summary(fit)$r.squared)
  plot(y2~x2,pch=19,col='darkorange')
  abline(fit <- lm(y2~x2))
  print(summary(fit)$r.squared)
  plot(y3~x3,pch=19,col='darkorange')
  abline(fit <- lm(y3~x3))
  print(summary(fit)$r.squared)
  plot(y4~x4,pch=19,col='darkorange')
  abline(fit <- lm(y4~x4))
  print(summary(fit)$r.squared)
})

# Low-cost minimization
out <- c()
for(a in seq(1,5,length=5)) {
    for(b in seq(0,1,length=5)) {
         yest <- b*d$x1+a
         res <-sum((d$y1-yest)^2)
         out <- rbind(out,c(a,b,res))
         cat(c(a,b,res,"\n"))
       }
}
out[which.min(out[,3]),] # Slope: 0.5 Intercept: 3

fit <- lm(y1~x1,d)
summary(fit) # Same slope and intercept
# Keep this: R^2=0.6665
par(mfrow=c(1,1))
with(d,plot(y1,pch=20,col=4,cex=2)) # Plot dependent variable
abline(h=mean(d$y1),lwd=3,col=2) # Plot its mean
with(d,plot(x1,y1,pch=20,col=4,cex=2)) # Plot Full data
abline(fit,lwd=3,col=2) # Plot best fit

variance.to.mean <- with(d,sum((y1-mean(y1))^2))
ypred <- predict(fit,d)
variance.to.fit <- with(d,sum((y1-ypred)^2))
print(R2.by.hand <- 1-variance.to.fit/variance.to.mean)
print(summary(fit)$r.squared)

# Let's do the fit using caret
library(caret)
fit2 <- train(y1 ~ x1, data = d, method = "lm")
summary(fit2) # Same result as lm is computed using mathematical formulas.

# How to interpret the coefficients and the correlation?
bmi <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/bmi.csv")
str(bmi)
with(bmi,plot(Body.Fat~BMI,pch=19,col=4))
summary(fit <- lm(Body.Fat~BMI,bmi))
abline(fit,lwd=2,col=2)

# Correlation is the slope for the standardize variables
f <- preProcess(bmi,method = c("center","scale")) # Standardize with caret
bmi.new <- predict(f,bmi)
with(bmi.new,plot(Body.Fat~BMI,pch=19,col=4))
points(0,0,pch=19,col=2,cex=3)
summary(fit <- lm(Body.Fat~BMI,bmi.new))
abline(fit,col=2,lwd=2)
print(coef(fit)[2]^2)
print(7.282e-01^2) # Same within rounding errors 

# Regression toward the mean
galton <- read.csv("https://ytliu0.github.io/Stat390EF-R-Independent-Study-archive/RMarkdownExercises/Galton.txt",sep='\t') # \t for tabs
# str(galton)
male <- galton$Gender=="M"
with(galton[male,],{
  plot(Height~Father,col=rgb(0,0,.7,.3),pch=19,cex=1.3)
  print(summary(fit <- lm(Height~Father)))
  abline(fit,lwd=5,col=2)
  points(mean(Father),mean(Height),pch=19,cex=3,col=2)
  })
f <- preProcess(galton,method = c('scale','center'))
galton.new <- predict(f,galton)
with(galton.new[male,],{
  plot(Height~Father,col=rgb(0,0,.7,.3),pch=19,cex=1.3)
  print(summary(fit <- lm(Height~Father)))
  abline(fit,lwd=5,col=2)
  points(mean(Father),mean(Height),pch=19,cex=3,col=2)
  })

sapply(galton[male,c(2,5)],mean)
sapply(galton[male,c(2,5)],sd)
