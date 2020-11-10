library(forecast)
library(gridExtra)


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
fit1 <- arima(ARMA11,order=c(1,0,0))  # Base-R arima function
checkresiduals(fit1)
fit1 <- Arima(ARMA11,order=c(1,0,0)) # We will use Arima with capital "A" as it allows including "c"
checkresiduals(fit1)
Box.test(fit1$residuals,lag=10, fitdf=0, type="Lj")
# Intermission: How likely are outliers if residuals are normal?
1-pnorm(1.96) # Probability of larger than 1.96*sigma
pnorm(-1.96) #  Probability of lower than -1.96
(1-pnorm(1.96)+pnorm(-1.96))*100 # 5%

1-pnorm(4) # Probability of larger than 4*sigma
pnorm(-4) #  Probability of lower than -4
(1-pnorm(4)+pnorm(-4))*100 # 5%

# Let's fit another model
fit2 <- Arima(ARMA11,order=c(0,0,1))
checkresiduals(fit2)
ggPacf(fit2$residuals)
Box.test(fit2$residuals)
fit3 <- Arima(ARMA11,order=c(1,0,1))
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


autoplot(forecast(fit3,h = 10))

# Scenario generation
print(fit3)
set.seed(123)
gridExtra::grid.arrange(autoplot(ARMA11,series="Original",ylab=""),
  autoplot(arima.sim(list(order=c(1,0,1),ar=0.7228,ma=0.9254),sd=sd(fit3$residuals),n=150),series="Sim1",ylab=""),
  autoplot(arima.sim(list(order=c(1,0,1),ar=0.7228,ma=0.9254),sd=sd(fit3$residuals),n=150),series="Sim2",ylab=""),
  autoplot(arima.sim(list(order=c(1,0,1),ar=0.7228,ma=0.9254),sd=sd(fit3$residuals),n=150),series="Sim3",ylab=""))

# More on stability and stationarity: the kpss test
library(urca)
summary(ur.kpss(ARMA11))

