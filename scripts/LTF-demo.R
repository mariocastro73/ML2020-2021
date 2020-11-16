library(fpp2)
library(gam)
library(TSA)
library(lmtest)  #contains coeftest function

######################################################################################
economy <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/spanish-economy.csv')

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
ggtsdisplay(fit.lm$residuals) # Clearly needs differencing
ggtsdisplay(diff(fit.lm$residuals)) # It looks like MA(1) and maybe AR with small phi1.
# Let's try directly auto arima
fit.res.aa <- auto.arima(fit.lm$residuals)
coeftest(fit.res.aa) # Both significant, si we'll keep that noise term

fit.lm2 <- with(economy2000,Arima(log10(GDP),xreg=Population,order=c(0,0,0)))
summary(fit.lm)
summary(fit.lm2) # Same as above
fit.ltf <- with(economy2000,Arima(log10(GDP),xreg=Population,order=c(1,1,1)))

library(lmtest)
coeftest(fit.lm)
coeftest(fit.lm2)
coeftest(fit.ltf)
checkresiduals(fit.lm)
checkresiduals(fit.lm2)
checkresiduals(fit.ltf)
ggtsdisplay(fit.ltf$residuals)

economy2000$lm <- fit.lm$fitted.values
economy2000$ltf <- fit.ltf$fitted
ggplot(economy2000,aes(x=lm,y=log10(GDP))) + geom_point() + 
  geom_smooth(method='lm',formula=y~x)

ggplot(economy2000,aes(x=ltf,y=log10(GDP))) + geom_point() +  
  geom_smooth(method='lm',formula=y~x) # Much better!

gridExtra::grid.arrange(autoplot(forecast(fit.lm2,xreg=economy$Population[41:51])),
                        autoplot(forecast(fit.ltf,xreg=economy$Population[41:51])))

gridExtra::grid.arrange(autoplot(forecast(fit.lm2,xreg=economy$Population[-(1:40)])) +
                          autolayer(ts(log10(economy$GDP)),series="Real data"),
                        autoplot(forecast(fit.ltf,xreg=economy$Population[-(1:40)])) +
                          autolayer(ts(log10(economy$GDP)),series="Real data"))
# So it captures better the ups and downs. Somehow imputes the heaps to correlations and not to 
# a sustained trend. Cool!


######################################################################################
# LTF: Complete model (lagged regressors)
######################################################################################
library(TSA)

y <- log10(economy2000[,2])
Population <- economy2000[,6]
library(quantmod)  # to create the Lags
xlag = Lag(Population,0)   # b
xlag[is.na(xlag)]=0

# Let's fit the same model as above (fit.ltf) but using the library TSA (more flexible)
# fit.TF <- TSA::arima(y, # To distinguish from base-R arima function or, best, use arimax
fit.TF <- arimax(y,
                 order=c(1,1,1),
                 #seasonal = list(order=c(1,0,0),period=24),# Uncomment for seasonal data
                 xtransf = xlag,
                 transfer = list(c(0,0)), #List with (r,s) orders
                 include.mean = TRUE,
                 method="ML") # More stable than the default one

checkresiduals(fit.ltf)
checkresiduals(fit.TF) # Exactly same result
autoplot(fit.TF)
economy2000$TF <- fitted(fit.TF) # The syntax is slightly different
ggplot(economy2000,aes(x=TF,y=ltf)) + geom_point() # See, exactly the same fit.

# What if we have some suspicions that lagged predictors might matter. 
# Then we start the procedure from scratch
y <- log10(economy2000[,2])
Population <- economy2000[,6]
library(quantmod)  # to create the Lags
xlag = Lag(Population,0)   # b
xlag[is.na(xlag)]=0

fit.TF <- arimax(y,
                 order=c(1,0,0),
                 #seasonal = list(order=c(1,0,0),period=24),# Uncomment for seasonal data
                 xtransf = xlag,
                 transfer = list(c(0,8)), #List with (r,s) orders
                 include.mean = TRUE,
                 method="ML") # More stable than the default one
coeftest(fit.TF) 
# All the lagged coefficients are non-significant, so b=s=r=0. 
# So we were right with the previous model, no lagged regressors are required.
