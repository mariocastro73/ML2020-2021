# 1) Plot the series and search for possible outliers. 
# 2) Stabilize the variance by transforming the data (Box-Cox).
# 3) Analyse the stationarity of the transformed series. 
# 4) If the series is not stationary, then we use differencing. 
# 5) Identify the seasonal model by analyzing the seasonal coefficients of the ACF and PACF
# 6) Identify the regular component by exploring the ACF and PACF of the residuals of the seasonal model.
# 7) Check the significance of the coefficients
# 8) Analyze the residuals:
# 9) Compare different models using AIC or SBC (M=p+q+P+Q):


library(forecast)
y <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/elec-demand.csv')
y <- ts(y$x,start=1975,frequency = 12)

ggtsdisplay(y) # Seasonal differencing is mandatory
# ggtsdisplay(y,lag=200)# Let's do a zoom

BoxCox.lambda(y) # Close to 1, so do nothing

y.sdiff <- diff(y, lag = 12, differences = 1)
ggtsdisplay(y.sdiff)# Requires regular differencing

y.rdiff <- diff(y, lag = 1, differences = 1)
ggtsdisplay(y.rdiff,lag=200) # Requires seasonal differencing
# Do both
y.rdiff.sdiff <- diff(y.rdiff, lag = 12, differences = 1) 
ggtsdisplay(y.rdiff.sdiff) # Sweet!

### Part II

# Fitting
arima.fit <- Arima(y, order=c(0,1,0),
                   seasonal = list(order=c(0,1,1),period=12),
                   lambda = NULL,
                   include.constant = TRUE)

ggtsdisplay(arima.fit$residuals)
ggtsdisplay(arima.fit$residuals,lag=13)

arima.fit2 <- Arima(y, order=c(0,1,1),
                   seasonal = list(order=c(0,1,1),period=12),
                   lambda = NULL, 
                   include.constant = TRUE)
ggtsdisplay(arima.fit2$residuals)
ggtsdisplay(arima.fit2$residuals,lag=80)
autoplot(arima.fit)
autoplot(arima.fit2)
summary(arima.fit)
summary(arima.fit2)

library(lmtest)
coeftest(arima.fit)
coeftest(arima.fit2)

autoplot(y) +
  autolayer(arima.fit2$fitted,series="Fit")

library(ggplot2)
df <- data.frame(y=y,x=arima.fit2$fitted)
ggplot(df,aes(x=x,y=y)) + geom_point() + 
  geom_smooth(method='lm',formula=y~x)

