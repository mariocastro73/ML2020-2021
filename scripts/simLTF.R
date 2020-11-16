#########################################################
# Mario Castro Ponce
# Mon Nov 16 15:08:26 2020 ------------------------------
#########################################################
library(fpp2)
library(Hmisc)
library(quantmod)

simLTF <- function(x,omega=1,delta=NULL,b=0){
  #x <- input time series
  #omega <- vector with numerator coefficient values
  #delta <- vector with denominator coefficient values
  #b <- lag value
  
  x <- Lag(x,b)
  x[is.na(x)]<-0
  
  # AR regression terms
  if (!is.null(delta)) {
    xfilt <-
      filter(x,
             filter = delta,
             method = 'recursive',
             sides = 1)
  } else {
    xfilt <- x # In case there are no -AR coefficients
  }
  # MA regression terms
  xfilt <-
    filter(xfilt,
           filter = omega,
           method = 'convolution',
           sides = 2)
  return(ts(xfilt))
}

#########################################################
# Mario Castro Ponce
# Mon Nov 16 15:08:26 2020 ------------------------------
#########################################################
library(fpp2)
library(Hmisc)
library(quantmod)

sim.LTF.ARIMA <- function(x,omega=1,delta=NULL,b=0,p=NULL,d=0,q=NULL){
  #x <- input time series
  #omega <- vector with numerator coefficient values
  #delta <- vector with denominator coefficient values
  #b <- lag value
  
  x <- Lag(x,b)
  x[is.na(x)]<-0
  
  # AR regression terms
  if (!is.null(delta)) {
    xfilt <-
      filter(x,
             filter = delta,
             method = 'recursive',
             sides = 1)
  } else {
    xfilt <- x # In case there are no -AR coefficients
  }
  # MA regression terms
  xfilt <-
    filter(xfilt,
           filter = omega,
           method = 'convolution',
           sides = 2)

  
    noise <- arima.sim(model = list(order=c(length(p),d,length(q))),n=length(x))
  y <- as.numeric(xfilt+noise)
  y[is.na(y)] <- 0
  return(ts(y))
}

