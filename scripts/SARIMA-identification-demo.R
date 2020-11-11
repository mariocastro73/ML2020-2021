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

