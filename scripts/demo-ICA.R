traffic <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/TrafficData.dat")
data <- traffic[,5:28]

#ICA
library(fastICA)
data.ica <- fastICA(data, n.comp = 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
                    method = "R", row.norm = FALSE, maxit = 200,
                    tol = 0.0001, verbose = TRUE)
res.pca <- PCA(data)
par(mfrow=c(2,2))
plot(data.ica$S[,1],type='l',main="ICA1")
plot(res.pca$ind$coord[,1],type='l',main="PC1")
plot(data.ica$S[,2],type='l',main="ICA2")
plot(res.pca$ind$coord[,2],type='l',main="PC2")

df_plot <- data.frame(time=1:366,IC1 = data.ica$S[,1], IC2 =data.ica$S[,2], DAY=traffic$WEEKDAY)
ggplot(df_plot)+geom_point(aes(x=IC1,y=IC2, color=DAY))
library(gridExtra)
grid.arrange(ggplot(df_plot,aes(x=time,y=IC1, color=DAY))+geom_line(),
ggplot(df_plot,aes(x=time,y=IC2, color=DAY))+geom_line())

