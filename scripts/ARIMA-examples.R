library(forecast)
library(fpp2)

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
# Compare using autoarima
summary(auto.arima(goog200))
summary(auto.arima(diff(goog200))) # Same fit!!! Autoarima detects the "derivative"

# Playing with the stock market
library(quantmod)
ibex <- new.env()
getSymbols("^IBEX", env = ibex, src = "yahoo", from = as.Date("2015-01-04"), to = as.Date("2020-11-06"))
ibex <- ibex$IBEX
ibex <- ibex$IBEX.Adjusted
ggtsdisplay(ibex)
ggtsdisplay(diff(ibex),lag=100) # Looks like pure noise

# Auto.arima 
ggtsdisplay(ibex)
fit.aa <- auto.arima(ibex)
summary(fit.aa)
autoplot(fit.aa) # Pure (integrated) noise!!!!
ggtsdisplay(diff(ibex),lag=60)

# Another example: Excess mortality (MOMO)
tmp <- read.csv('https://momo.isciii.es/public/momo/data')
table(tmp$nombre_ambito)
data <- tmp$defunciones_observadas[tmp$nombre_ambito %in% "Madrid, Comunidad de"]
data <- ts(tail(data,313+365),start=2019,frequency=365) # Only 2018-2020
ggtsdisplay(data)

d2019 <- head(data,365)
d2020 <- tail(data,313)
ggtsdisplay(data)
ggtsdisplay(d2019)
ggtsdisplay(d2020)
summary(fit.aa <- auto.arima(d2019))
autoplot(fit.aa)
checkresiduals(fit.aa)
# Hand-made
ggtsdisplay(d2019)
ggtsdisplay(diff(d2019))
fit.111 <- Arima(d2019,order=c(1,1,1))
fit.010 <- Arima(d2019,order=c(0,1,0))
checkresiduals(fit.aa)
checkresiduals(fit.111)
checkresiduals(fit.010)

autoplot(fit.111)

autoplot(d2019,series='Madrid mortality 2019') +
  autolayer(fitted(fit.111),series="Arima(1,1,1)") +
  autolayer(fitted(fit.010),series="Arima(0,1,0)") +
  autolayer(fitted(fit.aa),series="AutoArima (3,1,3)")

library(psych)
pairs.panels(data.frame(d2019,fitted(fit.111),fitted(fit.010),fitted(fit.aa)))
pairs.panels(data.frame(d2019,fitted(fit.111),fitted(fit.aa)))

autoplot(d2019,series="2019") +
  autolayer(d2020,series="2020")

# Old methods
fit.es <- ses(d2019,h=313)
autoplot(fitted(fit.es),series="Exponential smoothing") +
  autolayer(d2019,series="Data")
autoplot(forecast(fit.es),series="Exponential smoothing") +
  autolayer(data,series="Data")
summary(fit.es)
pairs.panels(data.frame(d2019,fitted(fit.es))) # Same correlation as ARIMA

autoplot(dec <- decompose(ts(d2019,frequency = 7)))
checkresiduals(remainder(dec))
