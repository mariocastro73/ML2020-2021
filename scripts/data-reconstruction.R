library(FactoMineR)
library(factoextra)

traffic <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/TrafficData.dat")
data <- traffic[,5:28]
matplot(t(data),type="l")
res.pca <- PCA(data,graph=FALSE)
fviz_screeplot(res.pca)
res.pca <- PCA(data,ncp = 2)
res.pca$ind$coord

corrplot(res.pca$var$coord)
# Reconstructing 1 day
plot(as.numeric(data[,18]),type='l') # 6pm traffic
orig <- as.numeric(data[,18])
plot(orig,type='l')
plot(res.pca$ind$coord[,1],type='l')
y <- res.pca$ind$coord[,1:2] %*% res.pca$var$coord[18,] 
plot(y,orig,xlab='Reconstructed',ylab='Original')
abline(fit <- lm(orig~y),col=2,lwd=3)
summary(fit)

# Reconstructing variance around the mean
plot(colMeans(data),ylim=c(-10,700),pch=19,xlab='Hour of the day',ylab='Mean daily traffic (2016)')
lines(colMeans(data)+res.pca$var$coord[,1]*50,col=2)
lines(colMeans(data)-res.pca$var$coord[,1]*50,col=4)
lines(colMeans(data)+res.pca$var$coord[,2]*50,col=2,lty=3)
lines(colMeans(data)-res.pca$var$coord[,2]*50,col=4,lty=3)
legend('topleft',legend=c("Mean","+PC1","-PC1","+PC2","-PC2"),pch=c(19,rep(-1,4)),col=c(1,2,4,2,4),lty=c(-1,1,1,3,3))
