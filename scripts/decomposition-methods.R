library(tseries)
library(forecast)
library(smooth)

data(AirPassengers)
AP <- AirPassengers
# Take a look at the class of the dataset AirPassengers
class(AP)

plot(AP,ylab='Air passengers',xlab='Years')
y <- log(AP)
plot(y)
plot(dec <- decompose(y))
######################################################################################
# Mock data
######################################################################################

years <- 1:144
seasonal <- -11*sin(years/12*2*pi)+5.7*sin(years/12*4*pi)-2*sin(years/12*6*pi)+
                   2.5*sin(years/12*8*pi)+1.6*sin(years/12*10*pi)
trend <- 40000-(years-1)^2
noise <- rnorm(length(years),0,1000)
y <- trend+seasonal+noise
data <- ts(y,start=2009,frequency = 12)
plot(data)

plot(dec <- decompose(data))

# x11()
par(mar=c(5.1,7.1,4.1,2.1)) # Change margins
plot(years,y,type='l',cex.axis=2,cex.lab=2)
plot(years,trend,type='l',cex.axis=2,cex.lab=2)
plot(years,seasonal,type='l',cex.axis=2,cex.lab=2)
plot(years,noise,type='l',cex.axis=2,cex.lab=2)

######################################################################################
# Temperatures
######################################################################################

temp <- read.csv('datasets/DAILY_DEMAND.csv',sep=';')
demand <- ts(temp$DEM[1:365],frequency = 7,start=2007)
x11()
# plot(retiro)
plot(dec <- decompose(demand))
temp$fecha[(8*7)] # Low demand in agust (see trend part of the series)
temp$fecha[(27*7)] # High electricity demand in january (cold)


# Looking at the random part, it's clear thet the model couldn't catch the spikes in january (among other minor ones)

library(seasonalview)
m <- seas(AirPassengers)
view(m)


# Adjustments
library(forecast)
library(fpp2)
df <- cbind(Monthly = milk,
            DailyAverage = milk/monthdays(milk))
autoplot(df, facet=TRUE) 