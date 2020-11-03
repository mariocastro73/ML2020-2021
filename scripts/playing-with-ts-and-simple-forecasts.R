# Load the libraries
library(forecast)
library(fpp2)

# Let's create a periodic timeseries
t <- 1:140
y <- 2*sin(2*pi*t/7)+rnorm(140,0,.5)
plot(y,type='l')
data <- ts(y,frequency=7) # If you use frenquency=12, it would consider that the data is periodic on a yearly basis
print(data,calendar = TRUE) # The parameter 'calendar' asigns meaning to the index

autoplot(data) +
  autolayer(naive(data,h=14),PI=TRUE) # Simple naive method (same as rwf(data,h=14))

autoplot(data) +
  autolayer(snaive(data,h=14),PI=TRUE)# Seasonal naive 

autoplot(data) +
  autolayer(rwf(data,h=14,drift=TRUE),PI=TRUE)# Random-walk with drift

autoplot(data) +
  autolayer(meanf(data,h=14),PI=TRUE) # Average or mean value of the whole time series

autoplot(data) + # Put everything together with legend and withot predictive intervals
  autolayer(naive(data,h=14),PI=FALSE,series = "Naive") +
  autolayer(snaive(data,h=14),PI=FALSE,series="Seas. Naive") +
  autolayer(rwf(data,h=14,drift=TRUE),PI=FALSE,series="Drift") +
  autolayer(meanf(data,h=14),PI=FALSE,series = "Mean")


########################################################
# Another experiment with mock data, using pure noise
########################################################

t <- 1:140
y <- cumsum(rnorm(140,0,.5))
plot(y,type='l')
data <- ts(y,frequency=7) # In this case, we should remove seasoality if we think that the underlying data is not periodic
print(data,calendar = TRUE)

autoplot(data) +
  autolayer(naive(data,h=14),PI=TRUE)

autoplot(data) +
  autolayer(snaive(data,h=14),PI=TRUE)

autoplot(data) +
  autolayer(rwf(data,h=14,drift=TRUE),PI=TRUE)

autoplot(data) +
  autolayer(meanf(data,h=14),PI=TRUE)

autoplot(data) + # compare them all
  autolayer(naive(data,h=14),PI=FALSE,series = "Naive") +
  autolayer(snaive(data,h=14),PI=FALSE,series="Seas. Naive") +
  autolayer(rwf(data,h=14,drift=TRUE),PI=FALSE,series="Drift") +
  autolayer(meanf(data,h=14),PI=FALSE,series = "Mean")
