library(forecast)

# ARIMA(1,1,1)
set.seed(123)
series <- arima.sim(list(order=c(1,1,1),ar=c(-0.4),ma=c(0.9)),n=100)
ggtsdisplay(series)
