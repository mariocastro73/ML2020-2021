library(tseries)
library(forecast)
library(smooth)
library(forecast)
library(fpp2)

# Take a look at the class of the dataset AirPassengers (from base library datasets)
class(AirPassengers)

plot(AirPassengers,ylab='Air passengers',xlab='Years')
autoplot(AirPassengers)
autoplot(decompose(AirPassengers))
autoplot(decompose(AirPassengers,type='multiplicative'))
dec <- decompose(AirPassengers)
autoplot(dec$random)
checkresiduals(dec$random)
dec <- decompose(AirPassengers,type='multiplicative')
autoplot(dec$random)
checkresiduals(dec$random)

AP <- log(AirPassengers)
autoplot(AP)
autoplot(dec <- decompose(AP))
autoplot(decompose(AP,type='multiplicative'))
autoplot(dec$random)
checkresiduals(dec$random)


# Looking at the random part, it's clear thet the model couldn't catch the spikes in january (among other minor ones)
# Advanced decomposition methods
library(seasonalview) # Monthly data
print(elecequip)
autoplot(elecequip) 
autoplot(decompose(elecequip))
autoplot(seas(elecequip))
autoplot(seas(elecequip,x11=""))
dec <- seas(elecequip)
checkresiduals(remainder(dec))
dec <- seas(elecequip,x11="")
checkresiduals(remainder(dec))

autoplot(dec <- stl(elecequip, s.window="periodic", robust=TRUE))
checkresiduals(remainder(dec))

autoplot(dec <- stl(elecequip, s.window="periodic", t.window = 31, robust=TRUE))
checkresiduals(remainder(dec))

autoplot(dec <- stl(elecequip, s.window="periodic", t.window = 101, robust=TRUE))
checkresiduals(remainder(dec))


# Adjustments
df <- cbind(Monthly = milk,
            DailyAverage = milk/monthdays(milk))
autoplot(df, facet=TRUE) 

autoplot(milk,series='Data')+ 
  autolayer(seasadj(decompose(milk)),series='Seasonally adjusted')



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
autoplot(data)

autoplot(dec <- decompose(data))
checkresiduals(dec$random)



# x11()
par(mar=c(5.1,7.1,4.1,2.1)) # Change margins
par(mfrow=c(4,1))
plot(years,y,type='l',cex.axis=2,cex.lab=2)
plot(years,trend,type='l',cex.axis=2,cex.lab=2)
plot(years,seasonal,type='l',cex.axis=2,cex.lab=2)
plot(years,noise,type='l',cex.axis=2,cex.lab=2)
