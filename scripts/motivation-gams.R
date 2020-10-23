library(ISLR)
library(psych)

data(Wage)
str(Wage)
summary(Wage)
pairs.panels(Wage.trn[,c("age","year","education","wage")]) # Looks that no interaction

train.index <- createDataPartition(Wage[,"wage"],p=0.8,list=FALSE)
Wage.trn <- Wage[train.index,]
Wage.tst <- Wage[-train.index,]

ctrl  <- trainControl(method  = "cv",
                      number  = 10,
                      savePredictions = TRUE,
                      returnResamp = "final")

fit.lm <- train(wage ~ year + age +education, 
                data = Wage.trn, 
                method = "lm",
                trControl = ctrl,
                preProcess = c("center","scale"), 
                tuneLength = 50)

summary(fit.lm)
pred <- predict(fit.lm,Wage.tst)
plot(Wage.tst$wage~pred)
abline(0,1,col=2)
print(RMSE.tst <- sqrt(mean((pred-Wage.tst$wage)^2)))

par(mfrow=c(2,2))
plot(fit.lm$finalModel)
par(mfrow=c(1,1))
Wage.trn$res <- fit.lm$finalModel$residuals

pairs.panels(Wage.trn[,c("age","res")])

# Let's try quadratic
fit.poly <- train(wage ~ year + age + I(age^2) +education, 
                data = Wage.trn, 
                method = "lm",
                trControl = ctrl,
                preProcess = c("center","scale"), 
                tuneLength = 50)

summary(fit.lm)
summary(fit.poly)
pred <- predict(fit.poly,Wage.tst)
plot(Wage.tst$wage~pred)
abline(0,1,col=2)
print(RMSE.tst <- sqrt(mean((pred-Wage.tst$wage)^2)))

par(mfrow=c(2,2))
plot(fit.poly$finalModel)
par(mfrow=c(1,1))
Wage.trn$res <- fit.poly$finalModel$residuals

pairs.panels(Wage.trn[,c("age","year","education","res")])
pairs.panels(Wage.trn[,c("age","res")])
# Splines directly
set.seed(999)
t <- seq(0,3.5*pi/2,length=100)
data <- data.frame(Predictor=t,Output=sin(t)+rnorm(100,0,.35))
plot(data)

fit.natural <-lm(Output ~ ns(Predictor,df=5),data)
fit.spline <-lm(Output ~ bs(Predictor,df=5),data)

ypred1<-predict(fit.natural,data,interval="c")
ypred2<-predict(fit.spline,data,interval="c")
plot(data)
lines(data$Predictor,ypred1[,1],lty=1,col="red",lwd=2)
lines(data$Predictor,ypred1[,2],lty=2,col="red",lwd=1)
lines(data$Predictor,ypred1[,3],lty=2,col="red",lwd=1)
plot(data)
lines(data$Predictor,ypred2[,1],lty=1,col="blue",lwd=2)
lines(data$Predictor,ypred2[,2],lty=2,col="blue",lwd=1)
lines(data$Predictor,ypred2[,3],lty=2,col="blue",lwd=1)

plot(data)
lines(data$Predictor,ypred1[,1],lty=1,col="red",lwd=2)
lines(data$Predictor,ypred2[,1],lty=1,col="blue",lwd=2)

print(fit.df <-lm(Output ~ bs(Predictor,df=3),data))

ypred<-predict(fit.natural,data,interval="c")
plot(data)
lines(data$Predictor,ypred[,1],lty=1,col="red",lwd=2)

# Splines with caret
gam.fit <-  train(wage ~ age+year, 
                data = Wage.trn, 
                method = "gamSpline",
                tuneGrid = data.frame(df =seq(2,25,2)),
                preProcess = c("center","scale"),
                trControl = ctrl)
gam.fit
plot(gam.fit)
pred <- predict(gam.fit,Wage.tst)
plot(Wage.tst$wage~pred)
abline(0,1,col=2)
print(RMSE.tst <- sqrt(mean((pred-Wage.tst$wage)^2)))
library(gam)
plot.Gam(gam.fit$finalModel, se=TRUE ,col ="blue ")

gam.fit.2 <-  train(wage ~ age+year, 
                  data = Wage.trn, 
                  method = "gamSpline",
                  tuneGrid = data.frame(df =2),
                  # tuneLength=10,
                  preProcess = c("center","scale"),
                  trControl = ctrl)
plot.Gam(gam.fit.2$finalModel, se=TRUE ,col ="blue ")

gam.fit.20 <-  train(wage ~ age+year, 
                    data = Wage.trn, 
                    method = "gamSpline",
                    tuneGrid = data.frame(df =20),
                    # tuneLength=10,
                    preProcess = c("center","scale"),
                    trControl = ctrl)
plot.Gam(gam.fit.20$finalModel, se=TRUE ,col ="blue ")
plot.Gam(gam.fit$finalModel, se=TRUE ,col ="blue ",residuals = TRUE)
summary(gam.fit.2$finalModel)


########################################################

gam.fit.all <-  train(wage ~ age+year+ns(education), 
                     data = Wage.trn, 
                     method = "gam",
                     # tuneGrid = data.frame(df =20),
                     tuneLength=10,
                     preProcess = c("center","scale"),
                     trControl = ctrl)
summary(gam.fit.all)
plot(gam.fit.all)

with(Wage,plot(wage~age))
plot.Gam(gam.fit.all$finalModel, se=TRUE ,col ="blue ",residuals = TRUE)


library (gam)
gam.m3 = gam(wage~s(year,4)+ s(age,5) + education,data = Wage)
x11(width = 16,height = 10)
par(mfrow=c(3,1))
par(mar=c(5.1,7.1,4.1,2.1)) # Change margins
plot(gam.m3,se=TRUE,col="blue",cex=2,cex.axis=2,cex.lab=2)
preds <- predict(gam.m3,newdata=Wage)
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage) # combine with loess
plot(gam.lo,se=TRUE,col="green")
