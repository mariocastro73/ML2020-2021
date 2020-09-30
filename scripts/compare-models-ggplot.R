library(caret)
library(MLeval)
library(ggplot2)
library(gridExtra)

data <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/data-for-knn.csv')

train <- createDataPartition(data[,"Y"],p=0.8,list=FALSE)
data.trn <- data[train,]
data.tst <- data[-train,]

ctrl  <- trainControl(method  = "cv",number  = 10,
                      # summaryFunction = multiClassSummary,
                      classProbs = TRUE,
                      savePredictions = TRUE) 

fit.5 <- train(Y ~ ., data = data.trn, method = "knn",trControl = ctrl,
                 tuneGrid = data.frame(k=5))
fit.45 <- train(Y ~ ., data = data.trn, method = "knn",trControl = ctrl,
                 tuneGrid = data.frame(k=45))
fit.200 <- train(Y ~ ., data = data.trn, method = "knn",trControl = ctrl,
                 tuneGrid = data.frame(k=200))

# grid$Prob <- predict(fit.knn,grid,type = 'prob')$YES

# Decision tree
fit.rpart <- train(Y ~ ., data = data.trn, method = "rpart",
  trControl = ctrl, 
  preProcess = c("center","scale"), 
 tuneLength = 10)

# Decision tree
fit.rf100 <- train(Y ~ ., data = data.trn, method = "rf",
  trControl = ctrl, 
  preProcess = c("center","scale"),
  ntree=100)
fit.rf500 <- train(Y ~ ., data = data.trn, method = "rf",
  trControl = ctrl, 
  preProcess = c("center","scale"),
  ntree=500)
fit.rf1000 <- train(Y ~ ., data = data.trn, method = "rf",
  trControl = ctrl, 
  preProcess = c("center","scale"),
  ntree=1000)
####### Logistic regression
fit.glm <- train(Y ~ ., data = data.trn, method = "glm",
                    trControl = ctrl, 
                    preProcess = c("center","scale"),
                    family=binomial())
fit.glm2 <- train(Y ~ X1+X2+I(X1^2), data = data.trn, method = "glm",
                    trControl = ctrl, 
                    preProcess = c("center","scale"),
                    family=binomial())

### Plot stuff
r1 <- range(data$X1)
r2 <- range(data$X2)
ngrid <- 250
dx1 <- seq(r1[1],r1[2],length=ngrid)
dx2 <- seq(r2[1],r2[2],length=ngrid)
grid <- expand.grid(X1=dx1,X2=dx2)
grid$Y5 <- predict(fit.5,grid)
grid$Y45 <- predict(fit.45,grid)
grid$Y200 <- predict(fit.200,grid)
grid$Ytree <- predict(fit.rpart,grid)
grid$Yrf100 <- predict(fit.rf100,grid)
grid$Yrf1000 <- predict(fit.rf1000,grid)
grid$Yrf500 <- predict(fit.rf500,grid)
grid$Yglm <- predict(fit.glm,grid)
grid$Yglm2 <- predict(fit.glm2,grid)

g <- ggplot(grid,aes(x=X1,y=X2))  + scale_colour_brewer()
g + geom_point(aes(col=Y5))
p1 <- g + geom_point(aes(col=Y45))
print(p1)
g + geom_point(aes(col=Y200))
g + geom_point(aes(col=Ytree))
g + geom_point(aes(col=Yrf100))
g + geom_point(aes(col=Yrf500))
g + geom_point(aes(col=Yrf1000))
g + geom_point(aes(col=Yglm))
p2 <- g + geom_point(aes(col=Yglm2))
print(p2)


models <- list(knn5=fit.5,knn45=fit.45,knn200=fit.200,
               tree=fit.rpart,
               rf100=fit.rf100,rf500=fit.rf500,rf1000=fit.rf1000,
               logreg=fit.glm,logreg2=fit.glm2)
dotplot(resamples(models))
grid.arrange(p1,p2)
evalm(models,gnames=c("knn4","knn45","knn200","tree","rf100","rf500","rf1000","logreg","logreg2"))
