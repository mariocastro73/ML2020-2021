library(gridExtra)

# Can you tell the difference?
set.seed(123)
plot1 <- autoplot(arima.sim(list(order=c(2,0,0),ar=c(.1,-.3)),n=300),ylab = "Time series")
plot2 <- autoplot(arima.sim(list(order=c(0,0,2),ma=c(.1,-.3)),n=300),ylab = "Time series")
plot3 <- autoplot(arima.sim(list(order=c(2,0,2),ar=c(.1,-.3),ma=c(.1,-.3)),n=300),ylab = "Time series")
gridExtra::grid.arrange(plot1,plot2,plot3)
# Understanding the blue-lines
set.seed(123)
series <- arima.sim(list(order=c(0,0,0)),n=100) # Pure noise: 100 points
acf1 <- ggAcf(series)
series <- arima.sim(list(order=c(0,0,0)),n=1000) # Pure noise:1000 points
acf2 <- ggAcf(series)
series <- arima.sim(list(order=c(0,0,0)),n=10000) # Pure noise:10000 points
acf3 <- ggAcf(series)
grid.arrange(acf1,acf2,acf3)

# Using ACF to identify MA(q)
set.seed(123)
ma1 <- arima.sim(list(ma=0.5),n=200)
ggma1 <- ggAcf(ma1)
ma2 <- arima.sim(list(ma=c(0.5,0.4)),n=200)
ggma2 <- ggAcf(ma2)
ma3 <- arima.sim(list(ma=c(0.3,0.8,0.8)),n=200)
ggma3 <- ggAcf(ma3)
ma4 <- arima.sim(list(ma=c(0.6,0.3,0.8,0.7)),n=200)
ggma4 <- ggAcf(ma4)
grid.arrange(ggma1,ggma2,ggma3,ggma4,nrow=2)

#  ACF doesn't work for AR(p)
set.seed(123)
ar1 <- arima.sim(list(ar=0.8),n=200)
ggar1 <- ggAcf(ar1)
ar2 <- arima.sim(list(ar=c(0.5,0.4)),n=200)
ggar2 <- ggAcf(ar2)
ar3 <- arima.sim(list(ar=c(0.7,-0.5,0.6)),n=200)
ggar3 <- ggAcf(ar3)
ar4 <- arima.sim(list(ar=c(0.6,-0.3,0.8,-0.7)),n=200)
ggar4 <- ggAcf(ar4)
grid.arrange(ggar1,ggar2,ggar3,ggar4,nrow=2)

# PACF works for AR(p)
set.seed(123)
ar1 <- arima.sim(list(ar=0.8),n=200)
ggar1 <- ggAcf(ar1)
ggPar1 <- ggPacf(ar1)
grid.arrange(ggar1,ggPar1,nrow=2)

# PACF works for AR(p)
set.seed(123)
ar1 <- arima.sim(list(ar=0.8),n=200)
ggar1 <- ggPacf(ar1)
ar2 <- arima.sim(list(ar=c(0.5,0.4)),n=200)
ggar2 <- ggPacf(ar2)
ar3 <- arima.sim(list(ar=c(0.7,-0.6,0.6)),n=200)
ggar3 <- ggPacf(ar3)
ar4 <- arima.sim(list(ar=c(0.9,-0.3,0.8,-0.7)),n=200)
ggar4 <- ggPacf(ar4)
grid.arrange(ggar1,ggar2,ggar3,ggar4,nrow=2)

# Residuals should behave like random noise both for ACF and PACF
set.seed(123)
series <- arima.sim(list(order=c(0,0,0)),n=1000)
grid.arrange(ggAcf(series),ggPacf(series),nrow=2)


# Residual analysis and significance
set.seed(123)
ARMA11 <- arima.sim(list(ar=0.8,ma=0.8),sd=0.25,n=150)
autoplot(ARMA11,main='ARMA11')
ggar1 <- ggAcf(ARMA11)
ggPar1 <- ggPacf(ARMA11)
grid.arrange(ggar1,ggPar1,nrow=1)
######################################################################################
# Easier way to plot
######################################################################################
ggtsdisplay(ARMA11)

# Let's fit some models
fit1 <- arima(ARMA11,order=c(1,0,0))
checkresiduals(fit1)
Box.test(fit1$residuals,lag=10, fitdf=0, type="Lj")
fit2 <- arima(ARMA11,order=c(0,0,1))
checkresiduals(fit2)
Box.test(fit2$residuals)
fit3 <- arima(ARMA11,order=c(1,0,1))
checkresiduals(fit3)
Box.test(fit3$residuals,lag=10, fitdf=0, type="Lj")
summary(fit3)
# Significance test
library(lmtest)
coeftest(fit3)

# Unit root
autoplot(fit3)

autoplot(ARMA11,series='Data') +
  autolayer(fitted(fit3),series="Fitted")
# Compare with other models
autoplot(ARMA11,series='Data') +
  autolayer(fitted(fit1),series="Fitted")
autoplot(ARMA11,series='Data') +
  autolayer(fitted(fit2),series="Fitted")

