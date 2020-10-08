library(ggplot2)
library(psych)
library(plot3D)
library(lattice)

set.seed(123)
n <- 50
x <- runif(n)
y <- .3+x+rnorm(n,0,.2)
data <- data.frame(x=x,y=y)
g <- ggplot(data,aes(x,y)) + geom_point() +geom_smooth()
print(g)
# pairs.panels(data)

plot(data,pch=19,cex=.7,ylim=c(0,2))
abline(0,1,col='orange',lwd=2)
abline(0.5,1,col='orange',lwd=2)
abline(1,1,col='orange',lwd=2)
plot(data,pch=19,cex=.7,ylim=c(0,2))
abline(.3,1,col='red',lwd=4)
sse <- function(w0)  # sse: Sum of Squared Errors
  {
  with(data,sum((y-(w0+1*x))^2))
}
sse(0)
sse(0.5)
sse(1)
# Brute force
w0 <- seq(0,.8,by=0.001) # Sample 801 values of w0
error <- sapply(w0,sse) # Create a vector with the errors (love sapply!)
plot(w0,error,type='l',ylim=c(1.5,8)) # Plot the curve
index.min <- which.min(error) # Find the index of the minimum error
points(w0[index.min],error[index.min],pch=19,col=2,cex=2) # plot it
title(w0[index.min]) # Show the value as a title

# Gradient descent
w0 <- 0 # Initial guess
lr <- .005 #learning rate 
gradient <- -1 # Initial guess
sse(w0) # Show the error
points(w0,sse(w0),pch=19,col='blue') # Plot our first guess
epoch <- 1
cat("w0:",w0,"old:",old," gradient:",gradient,"epoch:",0) # Print stuff


old <- w0 # Keep the previous value of the weight
old.e <- sse(w0) # Keep the previous value of the error
w0 <- old - lr*gradient# Update the new weight
gradient <- (sse(w0)-old.e)/(w0-old) # Compute the "gradient"
points(w0,sse(w0),pch=19,col=epoch) # Plot the new value
cat("w0:",w0,"old:",old," gradient:",gradient,"epoch:",epoch) # Print stuff
epoch <- epoch + 1 

points(w0,sse(w0),pch=19,col='green')
cat("Number of steps until the method 'converged':",epoch)

sse2 <- function(w0,w1) 
{
  with(data,sum((y-(w0+w1*x))^2))
}

grid <- expand.grid(w0=seq(0,.8,by=0.01),w1=seq(-2,3,by=0.01))
error <- mapply(sse2,grid$w0,grid$w1)
data2d <- data.frame(w0=grid$w0,w1=grid$w1,error=error)
with(data2d,scatter3D(w0,w1,error,phi=10,theta = 50))

