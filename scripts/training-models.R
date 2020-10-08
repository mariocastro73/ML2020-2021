library(ggplot2)
library(plot3D)

set.seed(123)
n <- 50
x <- runif(n)
noise <- 0.2 # The standard deviation of the noise added to the data
y <- 0.3 + x + rnorm(n,0,noise) # Slope: 1 Intercept: 0.3
data <- data.frame(x=x,y=y)
g <- ggplot(data,aes(x,y)) + geom_point() + geom_smooth()
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
cat("w0:",w0," gradient:",gradient,"epoch:",0) # Print stuff

save.error <- c()

old <- w0 # Keep the previous value of the weight
old.e <- sse(w0) # Keep the previous value of the error
w0 <- old - lr*gradient# Update the new weight
# Remember the definition of derivative?
# f'(x)= (f(x2)-f(x1))/(x2-x1) when x2 -> x1
gradient <- (sse(w0)-old.e)/(w0-old) # Compute the "gradient"
points(w0,sse(w0),pch=19,col=epoch) # Plot the new value
save.error <- rbind(save.error,c(epoch=epoch,error=sse(w0))) # Plotting later
cat("w0:",w0,"old:",old," gradient:",gradient,"epoch:",epoch) # Print stuff
epoch <- epoch + 1 


points(w0,sse(w0),pch=19,col='green',cex=1.5)
cat("Number of steps until the method 'converged':",epoch," error:",sse(w0))
cat("Exact (unavoidable) error:",noise^2*n) # Variance of n gaussian numbers
plot(save.error,type='o',pch=18)

######### What happens if we fit 2 weights?

sse.twoweights <- function(w0,w1) 
{
  with(data,sum((y-(w0+w1*x))^2))
}

grid <- expand.grid(w0=seq(0,.8,by=0.01),w1=seq(-2,3,by=0.01))
error <- mapply(sse.twoweights,grid$w0,grid$w1) # like sapply but for 2D
data2d <- data.frame(w0=grid$w0,w1=grid$w1,error=error)
with(data2d,scatter3D(w0,w1,error,phi=10,theta = 50))

