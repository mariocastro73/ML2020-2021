library(fpp2)
ggtsdisplay(livestock)

e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=TRUE, h=1)
fore.arima <- function(x, h){forecast(auto.arima(x), h=h)}
e4 <- tsCV(livestock, fore.arima, h=1)
fore.stl <- function(x, h){forecast(ets(x), h=h)}
e5 <- tsCV(livestock, fore.stl, h=1)
# Compare MSE:
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)
mean(e4^2, na.rm=TRUE)
mean(e5^2, na.rm=TRUE)

# Compare MAE:
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)
mean(abs(e4), na.rm=TRUE)
mean(abs(e5), na.rm=TRUE)

# The winner is ETS
summary(fit.ets <- ets(livestock))
autoplot(fit.ets)
autoplot(forecast(fit.ets)) 
checkresiduals(fit.ets$residuals)
  
  

