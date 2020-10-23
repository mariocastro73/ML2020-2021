data <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/DAILY_DEMAND.csv", sep = ";")
str(data)
data$WD = as.factor(data$WD);
library(psych)
pairs.panels(data)
data <- data[data$WD==7,]

with(data,plot(TEMP,DEM,pch=19,cex=.5,xlim=c(-10,40),ylim=c(500,900)))
summary(fit.quad <- lm(DEM~TEMP+I(TEMP^2),data))
new.temp <- seq(-10,40,length=100)
pred <- predict(fit.quad,data.frame(TEMP=new.temp))
lines(new.temp,pred,col=2)
summary(fit.cub <- lm(DEM~TEMP+I(TEMP^2)+I(TEMP^3),data))
pred <- predict(fit.cub,data.frame(TEMP=new.temp))
lines(new.temp,pred,col=3)
summary(fit.quar <- lm(DEM~TEMP+I(TEMP^2)+I(TEMP^3)+I(TEMP^4),data))
pred <- predict(fit.quar,data.frame(TEMP=new.temp))
lines(new.temp,pred,col=4)
# By pieces
with(data,plot(TEMP,DEM,pch=19,cex=.5,xlim=c(-10,40),ylim=c(500,900)))
i <- data$TEMP<15
low <- data[i,]
hi <- data[-i,]
new.temp.low <- seq(-10,15,length=100)
new.temp.hi <- seq(15,40,length=100)
summary(fit.cub.low <- lm(DEM~TEMP+I(TEMP^2)+I(TEMP^3),low))
pred.low <- predict(fit.cub.low,data.frame(TEMP=new.temp.low))
lines(new.temp.low,pred.low,col=3,lwd=3)
summary(fit.cub.hi <- lm(DEM~TEMP+I(TEMP^2)+I(TEMP^3),hi))
pred.hi <- predict(fit.cub.hi,data.frame(TEMP=new.temp.hi))
lines(new.temp.hi,pred.hi,col=3,lwd=3)
