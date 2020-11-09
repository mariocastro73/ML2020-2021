library(forecast)

# ARIMA(1,1,1)
set.seed(123)
series <- arima.sim(list(order=c(1,1,1),ar=c(-0.4),ma=c(0.9)),n=100)
ggtsdisplay(series) # Look a the smooth shape of ACF

# Another (real) example: Google stock market
dgoog200 <- diff(goog200) # Differencing
ggtsdisplay(goog200)
ggtsdisplay(dgoog200)
fit <- arima(dgoog200,order=c(1,0,0))
checkresiduals(fit)
autoplot(forecast(fit))


# Playing with the stock market
library(quantmod)
ibex <- new.env()
getSymbols("^IBEX", env = ibex, src = "yahoo", from = as.Date("2015-01-04"), to = as.Date("2020-11-06"))
ibex <- ibex$IBEX
ibex <- ibex$IBEX.Adjusted
ggtsdisplay(ibex)
ggtsdisplay(diff(ibex))


# Auto.arima 
fit.aa <- auto.arima(ibex)
summary(fit.aa)
