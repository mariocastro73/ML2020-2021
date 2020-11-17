library(fpp2)
library(gam)
library(TSA)
library(lmtest)  #contains coeftest function

## Set working directory ---------------------------------------------------------------------------------------------


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/LTF-data.dat",header = TRUE, sep = "")
ggplot(fdata,aes(x=x,y=y)) + geom_point() # OMG!
# Convert to time series object
x <- ts(fdata$x)
y <- ts(fdata$y)
fdata_ts <- ts(fdata)
autoplot(fdata_ts, facets = TRUE)

# Create time series and scale values 
ggtsdisplay(y)
ggtsdisplay(x)
ggCcf(y,x)

## Identification and fitting process -------------------------------------------------------------------------------------------------------

#### Fit initial FT model with large s
# This arimax function belongs to the TSA package
TF.fit0 <- arimax(y,
                order=c(1,0,0),
                # seasonal = list(order=c(1,1,0),period=24),
                xtransf = x,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
coeftest(TF.fit0) # statistical significance of estimated coefficients
barplot(coef(TF.fit0)[-(1:2)],las=2)
# b=1
# r=2 (damped)
# s=0 (before the decay there are 0 non-null coefficients)
xlag = Lag(x,1)   # b
xlag[is.na(xlag)]=0
TF.fit1 <- arimax(y,
                 order=c(1,0,0),
                 # seasonal = list(order=c(1,1,0),period=24),
                 xtransf = xlag,
                 transfer = list(c(2,0)), #List with (r,s) orders
                 include.mean = TRUE,
                 method="ML")
coeftest(TF.fit1) # statistical significance of estimated coefficients
# The intercept is 0
ggtsdisplay(TF.fit1$residuals)
auto.arima(TF.fit1$residuals)
#### Fit arima noise with selected

TF.fit2 <- arima(y,
                   order=c(3,0,1),
                   xtransf = xlag,
                   transfer = list(c(2,0)), #List with (r,s) orders
                   include.mean = FALSE, # The intercept is 0
                   method="ML")
coeftest(TF.fit2) # statistical significance of estimated coefficients
ggtsdisplay(TF.fit2$residuals)
res <- TF.fit2$residuals
auto.arima(res) # Pure noise


# Check fitted
autoplot(y, series = "Real")+
  forecast::autolayer(fitted(TF.fit2), series = "Fitted")

ggplot(data.frame(X=fitted(TF.fit2),Y=y),aes(x=X,y=Y)) + geom_point() + 
   geom_smooth(method='lm',formula=y~x)

library(polynom)
print(delta <- polynomial(c(1,-0.6809,-0.2496)))
print(omega <- polynomial(-0.32))
