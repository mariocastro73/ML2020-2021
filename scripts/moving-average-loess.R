require(smooth)
library(ggpubr)
library(ggplot2)

########################################################
set.seed(999)
t <- seq(0,3.5*pi/2,length=100)
data <- data.frame(Predictor=t,Output=sin(t)+rnorm(100,0,.35))
plot(data)

# Simple Rolling average

sma(data$Output,order=2,h=0,silent=FALSE) # 2 day average
sma(data$Output,order=7,h=0,silent=FALSE) # 7 day average
sma(data$Output,order=14,h=0,silent=FALSE) # 14 day average

# Exponential rolling average
alpha <- .5
plot(data,col='white',main=alpha)
S <- data$Output[1]
points(data$Predictor[1],S)
for(i in 2:100) {
S <- alpha*data$Output[i]+(1-alpha)*S
points(data$Predictor[i],data$Output[i])
points(data$Predictor[i],S,col=2)
}

########################################################
# Loess
########################################################
# Averages are also "models"
plot(data$Output)
delta <- 14
lines(delta:(100-1),sapply(1:(100-delta),function(x)coef(lm(Output~1,data[(x):(x+delta),]))),col=4)

summary(fit <- loess(Output~Predictor,data))
plot(data)       
pred <- predict(fit,data)
lines(data$Predictor,pred,col=2,lwd=2)
plot(pred,data$Output)
abline(0,1,col=2,lwd=2)

library(ISLR)
library(psych)

data(Wage)
str(Wage)
with(Wage,plot(wage~age))
fit <- loess(wage~age,Wage)
new <- data.frame(age=seq(10,90,length=100))
pred <- predict(fit,new)
lines(new$age,pred,col=2,lwd=3)

#################################################################
# ggplot do all this stuff for us with the function "geom_smooth"
#################################################################

# Linear regression
gg.lm <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+geom_smooth(method='lm')
print(gg.lm)
# Quadratic regression
gg.qua <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='lm',formula= y ~ poly(x,2))
print(gg.qua)
# Cubic regression
gg.cub <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='lm',formula= y ~ poly(x,3))
print(gg.cub)
# Loess
gg.lo <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='loess')
# Automatic GAM
gg.gam <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='gam')
print(gg.gam)
# Natural splines
gg.ns2 <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::ns(x, 2)) 
print(gg.ns2)
gg.ns3 <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::ns(x, 3)) 
print(gg.ns3)
gg.bs2 <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::bs(x, 2)) 
print(gg.bs2)
gg.bs30 <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::bs(x, 30)) 
print(gg.bs30)

x11(width = 16,height = 10)
ggarrange(gg.lm,gg.qua,gg.cub,gg.lo,gg.gam,gg.ns2,gg.ns3,gg.bs2,gg.bs30,
          labels = c("Linear", "Quadratic", "Cubic","LOESS","GAM","NatSpline2","NatSpline3","Spline2","Spline3"))

