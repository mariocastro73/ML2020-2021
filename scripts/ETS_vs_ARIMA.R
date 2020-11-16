######################################################################################
library(forecast)
library(gridExtra)
######################################################################################


autoplot(h02)
BoxCox.lambda(h02)

y <- log(h02)
autoplot(y)

fit.stl <- stl(y,s.window='periodic')
autoplot(fit.stl)
autoplot(seasadj(fit.stl),series="Seas. adj.") +
  autolayer(y,series="Data")
ggtsdisplay(remainder(fit.stl),plot.type = 'hist')

fit.ets <- ets(y)
autoplot(fit.ets)

summary(fit.ets)
checkresiduals(fit.ets$residuals)

fit.arima <- auto.arima(y)
summary(fit.arima)
checkresiduals(fit.arima$residuals)
fit.arima <- Arima(y,order = c(1,1,0),seasonal = c(0,1,2))
summary(fit.arima)

data <- data.frame(y=y,ets=fit.ets$fitted,arima=fit.arima$fitted)
grid.arrange(
  ggplot(data,aes(x=ets,y=y)) + geom_point() + 
    geom_smooth(method='lm',formula=y~x),
  ggplot(data,aes(x=arima,y=y)) + geom_point() + 
    geom_smooth(method='lm',formula=y~x))

grid.arrange(autoplot(forecast(fit.ets,h=20)),
             autoplot(forecast(fit.stl,h=20)),
             autoplot(forecast(seasadj(fit.stl),h=20)),
             autoplot(forecast(fit.arima,h=20)),nrow=2)
f.s <- function(x,h) forecast(fit.stl,h=h)
f.a <- function(x,h) forecast(fit.arima,h=h)
f.e <- function(x,h) forecast(fit.ets,h=h)
hmax=2
e.a <- na.omit(tsCV(y,f.a,h=hmax))[,hmax]
e.e <- na.omit(tsCV(y,f.e,h=hmax))[,hmax]
e.s <- na.omit(tsCV(y,f.s,h=hmax))[,hmax]
summary(data.frame(e.a^2,e.e^2,e.s^2))



# Combining methods
fit.ets.adj <- ets(seasadj(fit.stl))
autoplot(fit.ets.adj)
autoplot(forecast((fit.ets.adj)))


### another example

BoxCox.lambda(ausbeer)
y <- log(ausbeer)
y <- BoxCox(ausbeer,BoxCox.lambda(ausbeer))
autoplot(y)
fit.stl <- stl(y,s.window='periodic')
autoplot(fit.stl)

fit.ets <- ets(y)
autoplot(fit.ets)
summary(fit.ets)
ggtsdisplay(fit.ets$residuals)

fit.arima <- auto.arima(y)
summary(fit.arima)
ggtsdisplay(fit.arima$residuals)

f.s <- function(x,h) forecast(fit.stl,h=h)
f.a <- function(x,h) forecast(fit.arima,h=h)
f.e <- function(x,h) forecast(fit.ets,h=h)
hmax=2
e.a <- na.omit(tsCV(y,f.a,h=hmax))[,hmax]
e.e <- na.omit(tsCV(y,f.e,h=hmax))[,hmax]
e.s <- na.omit(tsCV(y,f.s,h=hmax))[,hmax]
summary(data.frame(e.a^2,e.e^2,e.s^2))


grid.arrange(autoplot(forecast(fit.ets,h=20)),
             autoplot(forecast(fit.stl,h=20)),
             autoplot(forecast(fit.arima,h=20)))




########################################################
# Bonus track
########################################################

economy <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/spanish-economy.csv')
print(economy$Year)
y <- ts(economy$Population,frequency = 1,start=1960)
autoplot(y)

fit.ets <- ets(y)
autoplot(fit.ets)
summary(fit.ets)
checkresiduals(fit.ets$residuals)

fit.arima <- auto.arima(y)
summary(fit.arima)
checkresiduals(fit.arima$residuals)

grid.arrange(autoplot(forecast(fit.ets)),
             autoplot(forecast(fit.arima)))


e.s <- na.omit(tsCV(y,f.s,h=hmax))[,hmax]
summary(data.frame(e.a^2,e.e^2,e.s^2))
f.e <- function(x,h) forecast(fit.ets,h=h)
f.a <- function(x,h) forecast(fit.arima,h=h)
hmax=2
e.a <- na.omit(tsCV(y,f.a,h=hmax))[,hmax]
e.e <- na.omit(tsCV(y,f.e,h=hmax))[,hmax]
summary(data.frame(e.a^2,e.e^2))
