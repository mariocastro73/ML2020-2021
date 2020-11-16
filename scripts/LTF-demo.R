library(fpp2)
library(gam)
library(TSA)
library(lmtest)  #contains coeftest function
library(forecast)

######################################################################################


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

######################################################################################
### Now let's play with another dataset 
######################################################################################

?insurance
y <- insurance[,1] # Sales
x <- insurance[,2] # TV.advert
xlag <- Lag(x,0)
fit.TF <- arimax(y,
                 order=c(1,0,0), # ARIMA noise p=1, d=q=0
                 #seasonal = list(order=c(1,0,0),period=24), # Uncomment for seasonal data
                 xtransf = xlag, # Lagged predictor (not in this case, b=0)
                 transfer = list(c(0,8)), # List with (r,s) orders
                 include.mean = TRUE, # The coefficient "c" of the model
                 method="ML")

summary(fit.TF) # summary of training errors and estimated coefficients
coeftest(fit.TF) # statistical significance of estimated coefficients
barplot(coef(fit.TF)[-(1:2)])
# The first coefficient is significant, so b=0.
# There is no pattern of decay, so r=0.
# 0 significant coefficient (the index of T1-MA*) so s=0.
autoplot(fit.TF)
ggtsdisplay(fitted(fit.TF),lag=50) 
# Clearly AR(1) or MA(1) and something weird with lags=10 and 11 (might be related to the 
# a seasonal contribution, 12 month). First, we'll fit the new model and then check those again.
# Finally, looking at ACF, it seems that no differencing is required.

fit.TF2 <- arimax(y,
                  order=c(0,0,1), # ARIMA noise q=1, d=0 (no differencing required)
                  xtransf = xlag, # Lagged predictor (not in this case, b=0, as we discussed above)
                  transfer = list(c(0,0)), # List with (r,s) orders. We determined that s=0 and r=0
                  include.mean = TRUE, # The coefficient "c" of the model
                  method="ML")
fit.TF3 <- arimax(y,
                  order=c(1,0,0), # ARIMA noise p=1, d=0 (no differencing required)
                  xtransf = xlag, # Lagged predictor (not in this case, b=0, as we discussed above)
                  transfer = list(c(0,0)), # List with (r,s) orders. We determined that s=0 and r=0
                  include.mean = TRUE, # The coefficient "c" of the model
                  method="ML")
coeftest(fit.TF2) # statistical significance of estimated coefficients
coeftest(fit.TF3) # statistical significance of estimated coefficients
summary(fit.TF2)
summary(fit.TF3) # Better fit in terms of RMSE, MAE, ...
# The first coefficient is significant, so b=0.
# There is no pattern of decay, so r=0.
# Only 1 significant coefficient (T1-MA0) so s=0 
autoplot(fit.TF3)
ggtsdisplay(fitted(fit.TF3),lag=50) 

df <- data.frame(ads=x,sales=y,fit=fitted(fit.TF3))
ggplot(df,aes(x=fit,y=sales)) + geom_point() +
  geom_smooth(method='lm') # Pretty decent fit


# Compare with a basic linear regression
AIC(fit.TF3)
AIC((fit.lm <- lm(y~x)))
ggtsdisplay(residuals(fit.TF3))
ggtsdisplay(residuals(fit.lm))


# Check fitted
autoplot(y, series = "Real")+
  autolayer(fitted(fit.TF3), series = "LTF") +
  autolayer(ts(fit.lm$fitted.values,frequency=12,start=c(2002,1)), series = "LM")  # Fits systematically wors

# You can try to add P=1 to account for the apparent seasonal autoregressive component.
forecast(fit.TF3)
