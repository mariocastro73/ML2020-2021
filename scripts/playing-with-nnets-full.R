#################################################################################
##############     Example MLP for Classification   ############################
#################################################################################

## Load libraries --------------------------------------------------------------------------------
library(caret)
library(ggplot2)
library(NeuralNetTools) ##Useful tools for plotting and analyzing neural networks
library(nnet)
library(NeuralSens)

## Read an example dataset
data <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/data-for-knn.csv')

## Add a noisy input variable
data$X3 <- rnorm(nrow(data))

str(data)
summary(data)

## Exploratory analysis -------------------------------------------------------------------------------------
ggplot(data) + geom_point(aes(x = X1, y = X2, color = Y))
# plot using base-R
# with(data,plot(X1,X2,col=Y))


## Divide the data into training and test sets ---------------------------------------------------
set.seed(150) #For replication
# create random split
train <- createDataPartition(data[,"Y"],p=0.8,list=FALSE)
data.trn <- data[train,]
data.tst <- data[-train,]

ctrl  <- trainControl(method  = "cv",number  = 10, 
                      summaryFunction = multiClassSummary, # Multiple metrics
                      classProbs=T,# Required for the ROC curves
                      savePredictions = T) # Required for the ROC curves

set.seed(150) #For replication (I've added the same in all "trains" so you can just run that part independently)
# Hidden layer with one neuron
fit.mlp <- train(Y ~ ., data = data.trn, 
                 method = "nnet",
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 maxit = 250,    # Maximum number of iterations
                 tuneGrid = data.frame(size = 1, decay = 0),
                 # tuneGrid = data.frame(size = 0, decay = 0),skip=TRUE, # Technically, this is log-reg
                 metric = "Accuracy")

########################################################
# Grid for evaluating the model  (don't need to understand this, just use it for 2D plots)
np_grid <- 150 #number of discretization points in each dimension
np.X1 <- seq(from = min(data$X1), to = max(data$X1), length.out = np_grid)
np.X2 <- seq(from = min(data$X2), to = max(data$X2), length.out = np_grid)
grid_X1_X2 <- expand.grid(X1 = np.X1, X2 = np.X2)
grid_X1_X2$X3 <- rnorm(nrow(grid_X1_X2))

## Plot a 2D graph with the results of the model -------------------------------------------------
grid_X1_X2$pred = predict(fit.mlp, type = "raw", newdata = grid_X1_X2) # predicted probabilities for class YES
ggplot(grid_X1_X2) + 
  geom_point(aes(x = X1, y = X2, color = pred), show.legend = TRUE) +
  geom_point(data = data[data$Y == "YES",], aes(x = X1, y = X2), color = "black", size = 1) +
  geom_point(data = data[data$Y == "NO",], aes(x = X1, y = X2), color = "white", size = 1) 

# Try decay = 0 and 2 neurons: are 2 neurons enough?
set.seed(150) #For replication
fit.mlp <- train(Y ~ ., data = data.trn, 
                 method = "nnet",
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 maxit = 250,    # Maximum number of iterations
                 tuneGrid = data.frame(size = 2, decay = 0),
                 metric = "Accuracy")


## Plot a 2D graph with the results of the model -------------------------------------------------
grid_X1_X2$pred <- predict(fit.mlp, type = "raw", newdata = grid_X1_X2) # predicted probabilities for class YES
ggplot(grid_X1_X2) +geom_point(aes(x = X1, y = X2, color = pred)) +
  geom_point(data = data[data$Y == "YES",], aes(x = X1, y = X2), color = "black", size = 1) +
  geom_point(data = data[data$Y == "NO",], aes(x = X1, y = X2), color = "white", size = 1)
#accuracy measures
mlp_pred <-  predict(fit.mlp,data.trn) # Accuracy of training
confusionMatrix(data = mlp_pred, reference = data.trn$Y, positive = "YES")
# A trick to extract only the Accuracy
confusionMatrix(data = mlp_pred, reference = data.trn$Y, positive = "YES")$overall[1] 
mlp_pred <-  predict(fit.mlp, newdata = data.tst) # Accuracy of testing
confusionMatrix(data = mlp_pred, reference = data.tst$Y, positive = "YES")
confusionMatrix(data = mlp_pred, reference = data.tst$Y, positive = "YES")$overall[1]

# Sensitivity analysis: how do you interpret the results? next step?
plotnet(fit.mlp$finalModel) #Plot the network
SensAnalysisMLP(fit.mlp) #Statistical sensitivity analysis
varImp(fit.mlp)

# Try decay = 0 and 10 neurons: a reasonable number of neurons 
set.seed(150) #For replication
fit.mlp <- train(Y ~ ., data = data.trn, 
                 method = "nnet",
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 maxit = 250,    # Maximum number of iterations
                 tuneGrid = data.frame(size = 10, decay = 0),
                 metric = "Accuracy")


## Plot a 2D graph with the results of the model -------------------------------------------------
grid_X1_X2$pred <- predict(fit.mlp, type = "raw", newdata = grid_X1_X2) # predicted probabilities for class YES
ggplot(grid_X1_X2) + geom_point(aes(x = X1, y = X2, color = pred)) +
  geom_point(data = data[data$Y == "YES",], aes(x = X1, y = X2), color = "black", size = 1) +
  geom_point(data = data[data$Y == "NO",], aes(x = X1, y = X2), color = "white", size = 1)

#SensAnalysisMLP(fit.mlp) #Statistical sensitivity analysis

#accuracy measures
mlp_pred <-  predict(fit.mlp, data.trn)
confusionMatrix(data = mlp_pred, reference = data.trn$Y, positive = "YES")$overall[1] 
mlp_pred <-  predict(fit.mlp,data.tst)
confusionMatrix(data = mlp_pred, reference = data.tst$Y, positive = "YES")$overall[1] 

SensAnalysisMLP(fit.mlp)
varImp(fit.mlp)

# Try decay = 3 and 10 neurons: too much weight decay
set.seed(150) #For replication
fit.mlp <- train(Y ~ ., data = data.trn, 
                 method = "nnet",
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 maxit = 250,    # Maximum number of iterations
                 tuneGrid = data.frame(size = 10, decay = 3),
                 metric = "Accuracy")

## Plot a 2D graph with the results of the model -------------------------------------------------
grid_X1_X2$pred <- predict(fit.mlp, type = "raw", newdata = grid_X1_X2) # predicted probabilities for class YES
ggplot(grid_X1_X2) + geom_point(aes(x = X1, y = X2, color = pred)) +
  geom_point(data = data[data$Y == "YES",], aes(x = X1, y = X2), color = "black", size = 1) +
  geom_point(data = data[data$Y == "NO",], aes(x = X1, y = X2), color = "white", size = 1)

#accuracy measures
mlp_pred <-  predict(fit.mlp, data.trn)
confusionMatrix(data = mlp_pred, reference = data.trn$Y, positive = "YES")$overall[1] 
mlp_pred <-  predict(fit.mlp,data.tst)
confusionMatrix(data = mlp_pred, reference = data.tst$Y, positive = "YES")$overall[1] 

# Try decay = 0 and 50 neurons: too complex model
set.seed(150) #For replication
fit.mlp <- train(Y ~ ., data = data.trn, 
                 method = "nnet",
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 maxit = 2500,    # Maximum number of iterations
                 tuneGrid = data.frame(size = 50, decay = 0),
                 metric = "Accuracy")


## Plot a 2D graph with the results of the model -------------------------------------------------
grid_X1_X2$pred <- predict(fit.mlp, type = "raw", newdata = grid_X1_X2) # predicted probabilities for class YES
ggplot(grid_X1_X2) + geom_point(aes(x = X1, y = X2, color = pred)) +
  geom_point(data = data[data$Y == "YES",], aes(x = X1, y = X2), color = "black", size = 1) +
  geom_point(data = data[data$Y == "NO",], aes(x = X1, y = X2), color = "white", size = 1)

#accuracy measures
mlp_pred <-  predict(fit.mlp, data.trn)
confusionMatrix(data = mlp_pred, reference = data.trn$Y, positive = "YES")$overall[1] 
mlp_pred <-  predict(fit.mlp,data.tst)
confusionMatrix(data = mlp_pred, reference = data.tst$Y, positive = "YES")$overall[1] 


# Try decay = 0.1 and 50 neurons: the excess of complexity is corrected by weight decay
set.seed(150) #For replication
fit.mlp <- train(Y ~ ., data = data.trn, 
                 method = "nnet",
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 maxit = 2500,    # Maximum number of iterations
                 tuneGrid = data.frame(size = 50, decay = 0.1),
                 metric = "Accuracy")


## Plot a 2D graph with the results of the model -------------------------------------------------
grid_X1_X2$pred <- predict(fit.mlp, type = "raw" , newdata = grid_X1_X2) # predicted probabilities for class YES
ggplot(grid_X1_X2) + geom_point(aes(x = X1, y = X2, color = pred)) +
  geom_point(data = data[data$Y == "YES",], aes(x = X1, y = X2), color = "black", size = 1) +
  geom_point(data = data[data$Y == "NO",], aes(x = X1, y = X2), color = "white", size = 1)

#SensAnalysisMLP(fit.mlp) #Statistical sensitivity analysis
#accuracy measures
mlp_pred <-  predict(fit.mlp, data.trn)
confusionMatrix(data = mlp_pred, reference = data.trn$Y, positive = "YES")$overall[1] 
mlp_pred <-  predict(fit.mlp,data.tst)
confusionMatrix(data = mlp_pred, reference = data.tst$Y, positive = "YES")$overall[1] 
SensAnalysisMLP(fit.mlp)
varImp(fit.mlp)

# Try MAXIT=25, decay = 0 and 50 neurons: test the effect of early stopping
set.seed(150) #For replication
fit.mlp <- train(Y ~ ., data = data.trn, 
                 method = "nnet",
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 maxit = 25,    # Maximum number of iterations
                 tuneGrid = data.frame(size = 50, decay = 0),
                 metric = "Accuracy")

## Plot a 2D graph with the results of the model -------------------------------------------------
grid_X1_X2$pred <- predict(fit.mlp, type = "raw", newdata = grid_X1_X2) # predicted probabilities for class YES
ggplot(grid_X1_X2) + geom_point(aes(x = X1, y = X2, color = pred)) +
  geom_point(data = data[data$Y == "YES",], aes(x = X1, y = X2), color = "black", size = 1) +
  geom_point(data = data[data$Y == "NO",], aes(x = X1, y = X2), color = "white", size = 1)

#accuracy measures
mlp_pred <-  predict(fit.mlp, data.trn)
confusionMatrix(data = mlp_pred, reference = data.trn$Y, positive = "YES")$overall[1] 
mlp_pred <-  predict(fit.mlp,data.tst)
confusionMatrix(data = mlp_pred, reference = data.tst$Y, positive = "YES")$overall[1] 


## OPTIMIZATION OF THE #NEURONS AND WEIGHT DECAY PARAMETER USING CROSS-VALIDATION
## Initialize trainControl -----------------------------------------------------------------------
ctrl <- trainControl(method = "cv",                        #k-fold cross-validation
                     number = 8,                           #Number of folds
                     summaryFunction = defaultSummary,     #Performance summary for comparing models in hold-out samples.
                     classProbs = TRUE)                    #Compute class probs in Hold-out samples

#----
# Try tuneGrid = 4: th eoptimal values are not reached
set.seed(150) #For replication
fit.mlp <- train(Y ~ ., data = data.trn, 
                 method = "nnet",
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 maxit = 250,    # Maximum number of iterations
                 tuneLength = 4,
                 metric = "Accuracy")
fit.mlp #information about the resampling settings
ggplot(fit.mlp)

# Expand the grid
set.seed(150) #For replication
Gfit.mlp = train(Y ~ ., data = data.trn, 
                 method = "nnet",
                 preProcess = c("center","scale"),
                 maxit = 250,    # Maximum number of iterations
                 tuneGrid = expand.grid(size = seq(5,25,length.out = 5), decay=10^seq(-9,1,by=1)),
                 trControl = ctrl, 
                 metric = "Accuracy")

Gfit.mlp #information about the resampling settings
ggplot(Gfit.mlp) + scale_x_log10()

Gfit.mlp$finalModel #information about the model trained
summary(Gfit.mlp$finalModel) #information about the network and weights
plotnet(Gfit.mlp$finalModel) #Plot the network
SensAnalysisMLP(Gfit.mlp) #Statistical sensitivity analysis

## Plot a 2D graph with the results of the model -------------------------------------------------
grid_X1_X2$pred <- predict(Gfit.mlp, type = "raw", newdata = grid_X1_X2) # predicted probabilities for class YES
ggplot(grid_X1_X2) + geom_point(aes(x = X1, y = X2, color = pred)) +
  geom_point(data = data[data$Y == "YES",], aes(x = X1, y = X2), color = "black", size = 1) +
  geom_point(data = data[data$Y == "NO",], aes(x = X1, y = X2), color = "white", size = 1)
#accuracy measures
mlp_pred = predict(Gfit.mlp, type = "raw", newdata = data.trn[,c("X1","X2","X3")])
confusionMatrix(data = mlp_pred, reference = data.trn$Y, positive = "YES")$overall[1] 
mlp_pred = predict(Gfit.mlp, type = "raw", newdata = data.tst[,c("X1","X2","X3")])
confusionMatrix(data = mlp_pred, reference = data.tst$Y, positive = "YES")$overall[1] 


