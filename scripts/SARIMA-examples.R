library(forecast)
library(fpp2)

# SARIMA(1,1,1)(1,1,1)4
set.seed(123)
ggtsdisplay(euretail)
ggtsdisplay(diff(euretail))
ggtsdisplay(diff(diff(euretail),lag=4)) # (1-B^4)*(1-B)

print(euretail,calendar = TRUE)
fit <- Arima(euretail,order=c(1,1,1),seasonal=c(1,1,1))
summary(fit)
fit.aa <- auto.arima(euretail,trace=TRUE)

autoplot(euretail,series="Data") +
  autolayer(fit$fitted,series="SARIMA(1,1,1)(1,1,1)[4]") +
  autolayer(fit.aa$fitted,series="Auto arima")

cor(data.frame(euretail,fit$fitted,fit.aa$fitted))

summary(fit)
summary(fit.aa)
checkresiduals(fit)
checkresiduals(fit.aa)
library(gridExtra)

grid.arrange(autoplot(fit),autoplot(fit.aa))

# Forecasting
gridExtra::grid.arrange(autoplot(forecast(fit.aa)), autoplot(forecast(fit)))


# Another examle:Corticosteroid drug sales in Australia
ggtsdisplay(h02) # To be continued....
