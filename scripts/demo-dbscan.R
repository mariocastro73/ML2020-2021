# library(fpc)
library(factoextra)
library(dbscan)


data <- read.table("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/Aggregation.dat",header = FALSE, sep = "", stringsAsFactors = FALSE)
ggplot(data,aes(x=V1,y=V2)) + geom_point()

kNNdistplot(data,k=5)
abline(h=1.4)
eps <- 1.4
db <-dbscan(data[,-3], eps = eps, minPts = 5)
fviz_cluster(db,data[,-3],geom="point")
kNNdistplot(data,k=6)
abline(h=1.4)
eps <- 1.5
db <-dbscan(data[,-3], eps = eps, minPts = 6)
fviz_cluster(db,data[,-3],geom="point")
data$cluster <- as.factor(db$cluster)
ggplot(data,aes(x=V1,y=V2,col=cluster)) + geom_point()
