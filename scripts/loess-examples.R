library(ggplot2)

set.seed(999)
t <- seq(0,3.5*pi/2,length=100)
data <- data.frame(Predictor=t,Output=sin(t)+rnorm(100,0,.35))
plot(data)

ggplot(data,aes(x=Predictor,y=Output))+geom_point()+geom_smooth(method='loess')
ggplot(data,aes(x=Predictor,y=Output))+geom_point()+geom_smooth(method='gam')

summary(fit <- loess(Output~Predictor,data))
plot(data)       
pred <- predict(fit,data)
lines(data$Predictor,pred)
plot(pred,data$Output)
abline(0,1)

out <- loess(Output~Predictor,data,model=T)
plot(out$model)

library(ISLR)
library(psych)

data(Wage)
str(Wage)
with(Wage,plot(wage~age))
fit <- loess(wage~age,Wage)
new <- data.frame(age=seq(10,90,length=100))
pred <- predict(fit,new)
lines(new$age,pred,col=2,lwd=3)
