#################################################################################
##############        Forecasting:     ARIMA         ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function

## Set working directory ---------------------------------------------------------------------------------------------

## Load custom functions ---------------------------------------------------------------
source("ForecastingTools.R")

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read_excel("CarRegistrations.xls")
fdata <- CarRegistrations
#Convert to time series object
y <- ts(fdata$CarReg,start = 1960, frequency = 12)
#for daily data
autoplot(y)


## Identification and fitting frocess -------------------------------------------------------------------------------------------------------

#Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y,12)
#Lambda <- BoxCox.lambda(y) #other option
z <- BoxCox(y,Lambda)
autoplot(z)

#Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(z,lag.max = 100)

#If differencing is needed
Bz <- diff(z,differences = 1)
ggtsdisplay(Bz,lag.max = 100) #differences contains the order of differentiation

#Seasonal Differentiation
#If differencing is needed
B12Bz <- diff(Bz, lag = 12, differences = 1)

#ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(B12Bz,lag.max = 100)

#Fit seasonal model with estimated order
arima.fit <- Arima(y,
                   order=c(0,0,0),
                   seasonal = list(order=c(0,0,0),period=12),
                   lambda = Lambda,
                   include.constant = FALSE)
summary(arima.fit) #summary of training errors and estimated coefficients
coeftest(arima.fit) #statistical significance of estimated coefficients
autoplot(arima.fit) #root plot

#Check residuals
checkresiduals_ICAI(arima.fit, bins = 100)
#If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 100)


#Check fitted forecast
autoplot(y, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")


#Perform future forecast
y_est <- forecast(arima.fit, h=12)
autoplot(y_est)


