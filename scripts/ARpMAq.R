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
