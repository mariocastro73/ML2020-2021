##########################################################
# EXPLORATORY NON-PARAMETRIC REGRESSION IN R WITH GGPLOT #
##########################################################

require(smooth)
library(ggpubr)
library(ggplot2)

########################################################
set.seed(999)
n <- 120
t <- seq(0,3.5*pi/2,length=n)
class <- as.factor(sample(c("A","B","C"),n/3,replace = TRUE))
data <- data.frame(Predictor=t,Output=sin(t)-.5*sin(as.numeric(class)*t)+rnorm(n,0,.35),class=class)
#################################################################
# ggplot do all this stuff for us with the function "geom_smooth"
#################################################################

# Linear regression
gg.lm <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='lm') + facet_grid(class ~.)
print(gg.lm)
# Quadratic regression
gg.qua <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='lm',formula= y ~ poly(x,2)) + facet_grid(class ~.)
print(gg.qua)
# Cubic regression
gg.cub <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='lm',formula= y ~ poly(x,3)) + facet_grid(class ~.)
print(gg.cub)
# Loess
gg.lo <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='loess') + facet_grid(class ~.)
print(gg.lo)

# Natural splines
gg.ns2 <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::ns(x, 2)) + facet_grid(class ~.)
print(gg.ns2)

gg.ns3 <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::ns(x, 3)) + facet_grid(class ~.)
print(gg.ns3)

gg.bs2 <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::bs(x, 2)) + facet_grid(class ~.)
print(gg.bs2)
gg.ns30 <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::ns(x, 30)) + facet_grid(class ~.)
print(gg.ns30)
gg.bs30 <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::bs(x, 30)) + facet_grid(class ~.)
print(gg.bs30)

x11(width = 16,height = 10)
ggarrange(gg.lm,gg.qua,gg.cub,gg.lo,gg.ns2,gg.ns3,gg.bs2,gg.ns30,gg.bs30,
          labels = c("Linear", "Quadratic", "Cubic","LOESS","NatSpline2","NatSpline3","Spline2","NatSpline30","Spline30"))

