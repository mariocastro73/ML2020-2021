library(forecast)
library(gridExtra)
########################################################
y <- h02
fit.ets <- ets(y)
autoplot(fit.ets)


summary(fit.ets)
checkresiduals(fit.ets$residuals)

fit.arima <- auto.arima(y)
summary(fit.arima)
checkresiduals(fit.arima$residuals)

fit.dec <- stl(y,s.window='periodic')
summary(fit.dec)
autoplot(fit.dec)
autoplot(seasadj(fit.dec),series="Seas. adj.") +
  autolayer(y,series="Data")
ggtsdisplay(seasadj(fit.dec))
  
ggtsdisplay(remainder(fit.dec),plot.type = 'hist')

data <- data.frame(y=y,ets=fit.ets$fitted,arima=fit.arima$fitted)
grid.arrange(
  ggplot(data,aes(x=ets,y=y)) + geom_point() + 
    geom_smooth(method='lm',formula=y~x),
  ggplot(data,aes(x=arima,y=y)) + geom_point() + 
    geom_smooth(method='lm',formula=y~x))

grid.arrange(autoplot(forecast(fit.ets,h=20)),
             autoplot(forecast(fit.dec,h=20)),
             autoplot(forecast(seasadj(fit.dec),h=20)),
             autoplot(forecast(fit.arima,h=20)),nrow=2)

# Combining methods
fit.ets.adj <- ets(seasadj(fit.dec))
autoplot(fit.ets.adj)
autoplot(forecast((fit.ets.adj)))

########################################################


economy <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/spanish-economy.csv')
print(economy$Year)
y <- ts(economy$Population/1e6,frequency = 1,start=1960)
autoplot(y)

fit.ets <- ets(y)
autoplot(fit.ets)


fit.arima <- auto.arima(y)
summary(fit.ets)
checkresiduals(fit.ets$residuals)

summary(fit.arima)
checkresiduals(fit.arima$residuals)

grid.arrange(autoplot(forecast(fit.ets)),
autoplot(forecast(fit.arima)))

data <- data.frame(y=y,ets=fit.ets$fitted,arima=fit.arima$fitted)
grid.arrange(
  ggplot(data,aes(x=ets,y=y)) + geom_point() + 
    geom_smooth(method='lm',formula=y~x),
  ggplot(data,aes(x=arima,y=y)) + geom_point() + 
    geom_smooth(method='lm',formula=y~x))


### another example
y <- ausbeer
autoplot(y)
fit.ets <- ets(y)
autoplot(fit.ets)

fit.dec <- stl(y,s.window = 'periodic')
autoplot(fit.dec)
summary(fit.dec)

fit.arima <- auto.arima(y)
summary(fit.arima)

checkresiduals(fit.ets$residuals)
checkresiduals(remainder(fit.dec))
checkresiduals(fit.arima$residuals)

data <- data.frame(y=y,ets=fit.ets$fitted,arima=fit.arima$fitted)

grid.arrange(
  ggplot(data,aes(x=ets,y=y)) + geom_point() + 
    geom_smooth(method='lm',formula=y~x),
  ggplot(data,aes(x=arima,y=y)) + geom_point() + 
    geom_smooth(method='lm',formula=y~x))
#geom_smooth(method=gam,formula=y~gam)

grid.arrange(autoplot(forecast(fit.ets,h=20)),
             autoplot(forecast(fit.dec,h=20)),
             autoplot(forecast(fit.arima,h=20)))

### another example-redux
y <- ausbeer
BoxCox.lambda(y)
y <- log(y)
autoplot(y)
fit.ets <- ets(y)
autoplot(fit.ets)
fit.dec <- stl(y,s.window=12)
autoplot(fit.dec)

fit.arima <- auto.arima(y)

checkresiduals(fit.ets$residuals)
checkresiduals(remainder(fit.dec))
checkresiduals(fit.arima$residuals)


RMSE(fit.ets$residuals)
RMSE(remainder(fit.dec))
RMSE(fit.arima$residuals)
MAE(fit.ets$residuals)
MAE(remainder(fit.dec))
MAE(fit.arima$residuals)

# Plot to compare
data <- data.frame(y=y,ets=fit.ets$fitted,arima=fit.arima$fitted)
grid.arrange(
  ggplot(data,aes(x=ets,y=y)) + geom_point() + 
    geom_smooth(method='lm',formula=y~x),
  ggplot(data,aes(x=arima,y=y)) + geom_point() + 
    geom_smooth(method='lm',formula=y~x))
#geom_smooth(method=gam,formula=y~gam)

grid.arrange(autoplot(forecast(fit.ets,h=20)),
             autoplot(forecast(fit.dec,h=20)),
             autoplot(forecast(fit.arima,h=20)))

autoplot(fit.ets)


