library(fpp2)
library(gam)
library(TSA)
library(lmtest)  #contains coeftest function

######################################################################################
library(forecast)

economy <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/spanish-economy.csv')
# economy <- read.csv('datasets/spanish-economy.csv')
ggplot(economy,aes(x=Year,y=Population)) + geom_point() + 
  geom_smooth(method='gam',formula=y~splines::ns(x,2))

ggplot(economy,aes(x=Population,y=log10(GDP))) + geom_point() + 
  geom_smooth(method='gam',formula=y~splines::ns(x,2)) +xlab("Population") +ylab("log10(GDP)") 

ggplot(economy,aes(x=Year,y=log10(GDP))) + geom_point() + 
  geom_smooth(method='gam',formula=y~splines::ns(x,2))



i <- economy$Year<2000
ggplot(economy[i,],aes(x=Population,y=log10(GDP))) + geom_point() + 
  geom_smooth(method='lm',formula=y~x)+xlab("Population") +ylab("log10(GDP)")

economy2000 <- economy[i,]
fit.lm <- lm(log10(GDP)~Population,economy2000)
summary(fit.lm)
ggtsdisplay(fit.lm$residuals)
ggtsdisplay(diff(fit.lm$residuals))

fit.res <- Arima(fit.lm$residuals,order=c(1,1,1))
checkresiduals(fit.res)
autoplot(fit.res)
ggtsdisplay(fit.res$residuals)

fit.lm2 <- with(economy2000,Arima(log10(GDP),xreg=Population,order=c(0,0,0)))
summary(fit.lm)
summary(fit.lm2)
library(lmtest)
fit.drm <- with(economy2000,Arima(log10(GDP),xreg=Population,order=c(1,1,1)))
coeftest(fit.lm)
coeftest(fit.lm2)
coeftest(fit.drm)
checkresiduals(fit.lm)
checkresiduals(fit.lm2)
checkresiduals(fit.drm)

economy2000$lm <- fit.lm$fitted.values
economy2000$drm <- fit.drm$fitted
ggplot(economy2000,aes(x=lm,y=log10(GDP))) + geom_point() + 
  geom_smooth(method='lm',formula=y~x)

ggplot(economy2000,aes(x=drm,y=log10(GDP))) + geom_point() + 
  geom_smooth(method='lm',formula=y~x)
#geom_smooth(method=gam,formula=y~gam)

gridExtra::grid.arrange(autoplot(forecast(fit.lm2,xreg=economy$Population[41:51])),
                        autoplot(forecast(fit.drm,xreg=economy$Population[41:51])))

gridExtra::grid.arrange(autoplot(forecast(fit.lm2,xreg=economy$Population[-(1:40)])) +
                          autolayer(ts(log10(economy$GDP)),series="Real data"),
                        autoplot(forecast(fit.drm,xreg=economy$Population[-(1:40)])) +
                          autolayer(ts(log10(economy$GDP)),series="Real data"))





######################################################################################




data <- uschange[,1:2]
autoplot(data)
autoplot(data,facets = TRUE)
ggplot(as.data.frame(data),aes(x=Income,y=Consumption)) + geom_point() +
   geom_smooth(method=lm,formula=y~x)

Consumption <- data[,1]
Income <- data[,2]

#### Fit using TSA
TF.fit <- TSA::arima(Consumption,
                     order=c(1,0,0),
                     #seasonal = list(order=c(1,0,0),period=24),
                     xtransf = Income,
                     transfer = list(c(0,9)), #List with (r,s) orders
                     include.mean = TRUE,
                     method="ML")
summary(TF.fit) # summary of training errors and estimated coefficients
barplot(coef(TF.fit)[-(1:2)],las=2,col='darkblue')
coeftest(TF.fit) # statistical significance of estimated coefficients
ggtsdisplay(TF.fit$residuals)

#### Fit arima noise with selected
xlag = Lag(x,0)   # b
xlag[is.na(xlag)]=0
arima.fit <- arima(y,
                   order=c(0,0,1),
                   #seasonal = list(order=c(0,0,0),period=24),
                   xtransf = xlag,
                   transfer = list(c(2,0)), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima.fit)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 50)


### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x)
########

# Check fitted
autoplot(y, series = "Real")+
  forecast::autolayer(fitted(arima.fit), series = "Fitted")

