source('scripts/simLTF.R')
library(fpp2)


set.seed(1234)
x <- rnorm(1000)

y <- sim.LTF.ARIMA(x)
ccf(y,x)
plot(x,y)
basic.fit <- lm(y~x)
ggtsdisplay(basic.fit$residuals)

xlag <- Lag(x,0)
fit.TF <- arimax(y,
  order=c(0,0,1), # ARIMA noise p=1, d=q=0
  #seasonal = list(order=c(1,0,0),period=24), # Uncomment for seasonal data
  xtransf = xlag, # Lagged predictor (not in this case, b=0)
  transfer = list(c(0,10)), # List with (r,s) orders
  include.mean = TRUE, # The coefficient "c" of the model
  method="ML")
print(coeftest(fit.TF))
barplot(coef(fit.TF)[-(1:2)],las=2,col='darkblue')

######################################################################################

y <- sim.LTF.ARIMA(x,b=1)
ccf(y,x)
xlag <- Lag(x,0)
fit.TF <- arimax(y,
                 order=c(0,0,1), # ARIMA noise p=1, d=q=0
                 #seasonal = list(order=c(1,0,0),period=24), # Uncomment for seasonal data
                 xtransf = xlag, # Lagged predictor (not in this case, b=0)
                 transfer = list(c(0,10)), # List with (r,s) orders
                 include.mean = TRUE, # The coefficient "c" of the model
                 method="ML")
print(coeftest(fit.TF))
barplot(coef(fit.TF)[-(1:2)],las=2,col='darkblue')

######################################################################################

y <- sim.LTF.ARIMA(x,b=2)
ccf(y,x)
xlag <- Lag(x,0)
fit.TF <- arimax(y,
                 order=c(0,0,1), # ARIMA noise p=1, d=q=0
                 #seasonal = list(order=c(1,0,0),period=24), # Uncomment for seasonal data
                 xtransf = xlag, # Lagged predictor (not in this case, b=0)
                 transfer = list(c(0,10)), # List with (r,s) orders
                 include.mean = TRUE, # The coefficient "c" of the model
                 method="ML")
print(coeftest(fit.TF))
barplot(coef(fit.TF)[-(1:2)],las=2,col='darkblue')

source('scripts/simLTF.R')
library(fpp2)


set.seed(1234)
x <- rnorm(1000)

y <- sim.LTF.ARIMA(x)
ccf(y,x)
plot(x,y)
basic.fit <- lm(y~x)
ggtsdisplay(basic.fit$residuals)

xlag <- Lag(x,0)
fit.TF <- arimax(y,
                 order=c(0,0,1), # ARIMA noise p=1, d=q=0
                 #seasonal = list(order=c(1,0,0),period=24), # Uncomment for seasonal data
                 xtransf = xlag, # Lagged predictor (not in this case, b=0)
                 transfer = list(c(0,10)), # List with (r,s) orders
                 include.mean = TRUE, # The coefficient "c" of the model
                 method="ML")
print(coeftest(fit.TF))
barplot(coef(fit.TF)[-(1:2)],las=2,col='darkblue')

######################################################################################

y <- sim.LTF.ARIMA(x,b=1)
ccf(y,x)
xlag <- Lag(x,0)
fit.TF <- arimax(y,
                 order=c(0,0,1), # ARIMA noise p=1, d=q=0
                 #seasonal = list(order=c(1,0,0),period=24), # Uncomment for seasonal data
                 xtransf = xlag, # Lagged predictor (not in this case, b=0)
                 transfer = list(c(0,10)), # List with (r,s) orders
                 include.mean = TRUE, # The coefficient "c" of the model
                 method="ML")
print(coeftest(fit.TF))
barplot(coef(fit.TF)[-(1:2)],las=2,col='darkblue')

######################################################################################

y <- sim.LTF.ARIMA(x,delta=.9)
ccf(y,x)
xlag <- Lag(x,0)
fit.TF <- arimax(y,
                 order=c(0,0,1), # ARIMA noise p=1, d=q=0
                 #seasonal = list(order=c(1,0,0),period=24), # Uncomment for seasonal data
                 xtransf = xlag, # Lagged predictor (not in this case, b=0)
                 transfer = list(c(0,10)), # List with (r,s) orders
                 include.mean = TRUE, # The coefficient "c" of the model
                 method="ML")
print(coeftest(fit.TF))
barplot(coef(fit.TF)[-(1:2)],las=2,col='darkblue')

######################################################################################

y <- sim.LTF.ARIMA(x,delta=.5)
ccf(y,x)
xlag <- Lag(x,0)
fit.TF <- arimax(y,
                 order=c(0,0,1), # ARIMA noise p=1, d=q=0
                 #seasonal = list(order=c(1,0,0),period=24), # Uncomment for seasonal data
                 xtransf = xlag, # Lagged predictor (not in this case, b=0)
                 transfer = list(c(0,10)), # List with (r,s) orders
                 include.mean = TRUE, # The coefficient "c" of the model
                 method="ML")
print(coeftest(fit.TF))
barplot(coef(fit.TF)[-(1:2)],las=2,col='darkblue')

######################################################################################

y <- sim.LTF.ARIMA(x,delta=c(.6,-.6))
ccf(y,x)
fit.TF <- arimax(y,
                 order=c(0,0,1), # ARIMA noise p=1, d=q=0
                 #seasonal = list(order=c(1,0,0),period=24), # Uncomment for seasonal data
                 xtransf = xlag, # Lagged predictor (not in this case, b=0)
                 transfer = list(c(0,10)), # List with (r,s) orders
                 include.mean = TRUE, # The coefficient "c" of the model
                 method="ML")
print(coeftest(fit.TF))
barplot(coef(fit.TF)[-(1:2)],las=2,col='darkblue')


