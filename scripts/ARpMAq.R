library(forecast)

######################################################################################
# Homemade AR(p) and MA(q) processes
######################################################################################

# Correlation only: AR(1)
set.seed(123)
phi1 <- -0.9
c <- 0
yt <- 1
series <- c()
for(t in 1:100) {
  yt <- c+phi1*yt+rnorm(1)
  series <- c(series,yt)
}
series <- ts(series)
autoplot(series)
ggAcf(series)
# Easier alternative
set.seed(123)
phi1 <- -0.9
autoplot(ar1 <- arima.sim(list(ar=phi1),n=100))
ggAcf(ar1)

# To show the exponential decay (AR(1))
set.seed(123)
phi1 <- 0.8
autoplot(ar1 <- arima.sim(list(ar=phi1),n=100))
ggAcf(ar1)

# Correlation and drift
set.seed(123)
phi1 <- -0.9
c <- 20
yt <- 1
series <- c()
for(t in 1:100) {
  yt <- c+phi1*yt+rnorm(1)
  series <- c(series,yt)
}
series <- ts(series)
autoplot(series)
ggAcf(series)

# MA(1)
set.seed(123)
theta1 <- 0.8
c <- 20
epst <- rnorm(1)
yt <- 0
series <- c()
for(t in 1:100) {
  epsnew <- rnorm(1)
  yt <- c+ epsnew + theta1*epst
  epst <- epsnew
  series <- c(series,yt)
}
series <- ts(series)
autoplot(series)
ggAcf(series)

# Super simple MA(2)
set.seed(123)
series <- arima.sim(list(ma=c(1,.8)),n=100)
autoplot(series)
ggAcf(series)

# Super sumple ARMA11
set.seed(123)
series <- arima.sim(list(ar=c(-0.9),ma=c(0.9)),n=100)
autoplot(series)
ggAcf(series)

# Super sumple ARIMA11 (but ARIMA will be the topic of other videos)
set.seed(123)
series <- arima.sim(list(order=c(1,1,1),ar=c(-0.4),ma=c(0.9)),n=100)
autoplot(series)
ggAcf(series)


