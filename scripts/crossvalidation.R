library(MLTools) # Optional: just for plotting
library(caret)

# Let's play a little bit with knn
data <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/data-for-knn.csv')
set.seed(1234)

# Use the caret package 
train <- createDataPartition(data[,"Y"],p=0.8,list=FALSE) # Y is the "target" class
data.trn <- data[train,] # Check with str that nrows of data.trn is 80% of the original "data"
data.tst <- data[-train,] # And this, just 20%

ctrl  <- trainControl(method  = "cv",number  = 10) # The focus of this video: 10-fold cross-validation

fit.cv <- train(Y ~ ., data = data.trn, method = "knn", # k nearest neighbors
  trControl = ctrl,  # Add the control
  preProcess = c("center","scale"),  # preprocess the data (center=> -mean(); scale= /standard.deviation)
  tuneGrid =data.frame(k=seq(5,100,by=15))) # Try only these values in the CV step
  # tuneLength = 25) # Use 25 sequential numbers instead

pred <- predict(fit.cv,data.tst) # predict the output classes
confusionMatrix(table(data.tst[,"Y"],pred)) 
print(fit.cv) # Plot the results. See at the end the chosen value of "k"
plot(fit.cv) # Plot the Cross-validation output

library(lattice) 
data.tst$probs <- predict(fit.cv,data.tst,type = 'prob')$YES 
histogram(~probs|Y,data.tst)# See how certain the prediction was




Plot2DClass(data.trn[,1:2], #Input variables of the model
            data.trn$Y,     #Output variable
            fit.knn,#Fitted model with caret
            var1 = "X1", var2 = "X2",
            selClass = "YES")


# Including additional metrics for CV  (beyond Accuracy)
ctrl  <- trainControl(method  = "cv",number  = 10, summaryFunction = multiClassSummary)
fit.knn.cv <- train(Y ~ ., data = data.trn, method = "knn",
                    trControl = ctrl, 
                    preProcess = c("center","scale"), 
                    tuneLength = 30)
print(fit.knn.cv)
plot(fit.knn.cv)
plot(fit.knn.cv,metric = "Specificity")
plot(fit.knn.cv,metric = "Sensitivity")
plot(fit.knn.cv,metric = "Kappa")

# Partition tree
library(rpart.plot)

ctrl  <- trainControl(method  = "cv",number  = 10)
fit.tree <- train(Y ~ ., data = data.trn, method = "rpart",
                trControl = ctrl, 
                preProcess = c("center","scale"),
                tuneLength=20)
print(fit.tree)
plot(fit.tree)
rpart.plot(fit.tree$finalModel)
Plot2DClass(data.trn[,1:2], #Input variables of the model
            data.trn$Y,     #Output variable
            fit.tree,#Fitted model with caret
            var1 = "X1", var2 = "X2",
            selClass = "YES")
