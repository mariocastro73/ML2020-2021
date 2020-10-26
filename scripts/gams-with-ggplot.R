require(smooth)
library(ggpubr)
library(ggplot2)

########################################################
set.seed(999)
t <- seq(0,3.5*pi/2,length=100)
#################################################################
# ggplot do all this stuff for us with the function "geom_smooth"
#################################################################

# Linear regression
gg.lm <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+geom_smooth(method='lm')
print(gg.lm)
# Quadratic regression
gg.qua <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='lm',formula= y ~ poly(x,2))
print(gg.qua)
# Cubic regression
gg.cub <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='lm',formula= y ~ poly(x,3))
print(gg.cub)
# Loess
gg.lo <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='loess')
# Automatic GAM
gg.gam <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='gam')
print(gg.gam)
# Natural splines
gg.ns2 <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::ns(x, 2)) 
print(gg.ns2)
gg.ns3 <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::ns(x, 3)) 
print(gg.ns3)
gg.bs2 <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::bs(x, 2)) 
print(gg.bs2)
gg.bs30 <- ggplot(data,aes(x=Predictor,y=Output))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::bs(x, 30)) 
print(gg.bs30)

x11(width = 16,height = 10)
ggarrange(gg.lm,gg.qua,gg.cub,gg.lo,gg.gam,gg.ns2,gg.ns3,gg.bs2,gg.bs30,
          labels = c("Linear", "Quadratic", "Cubic","LOESS","GAM","NatSpline2","NatSpline3","Spline2","Spline3"))

