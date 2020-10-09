# ensure the results are repeatable
set.seed(1234)
# load the library
library(mlbench)
library(caret)
library(klaR)

# load the data
data(PimaIndiansDiabetes)
data  <-  PimaIndiansDiabetes
str(data)
# Tip #1: Eye inspection and correlations
library(psych)
pairs.panels(data)
# I see a lot of skewed distributions 
# not today but we should preprocess using Box Cox, for instance
nam <- colnames(data)
par(mfrow=c(3,3))
for(i in 1:8) plot(data[,i]~ data$diabetes,col=2:3,xlab='diabetes',ylab=nam[i])
# Again, look at those outliers!!!! Not today, not today, .... BUT YOU SHOULD!
par(mfrow=c(1,1))

train.index <- createDataPartition(data[,"diabetes"],p=0.8,list=FALSE)
data.trn <- data[train.index,]
data.tst <- data[-train.index,]

ctrl  <- trainControl(method  = "cv",number  = 10, 
        summaryFunction = multiClassSummary,
        classProbs=T,# Required for the ROC curves
        savePredictions = T) # Required for the ROC curves

fit.nnet <- train(diabetes ~ ., data = data.trn, method = "nnet",
  trControl = ctrl, 
  preProcess = c("center","scale"), 
  tuneGrid =expand.grid(decay=c(0,.0001,.001,.01,.1),size=seq(3,25,by=4)))

pred <- predict(fit.nnet,data.tst)
confusionMatrix(table(data.tst[,"diabetes"],pred))
print(fit.nnet)
plot(fit.nnet)
library(NeuralNetTools)
plotnet(fit.nnet,cex=.7)
library(MLeval)
evalm(list(fit.nnet))

# Tip #3 (importance metrics)
set.seed(123)
fit.rpart <- train(diabetes ~ ., data = data.trn, method = "rpart",
                  trControl = ctrl, 
                  preProcess = c("center","scale"), 
                  tuneGrid=data.frame(cp=c(0,0.0001,0.001,0.01,0.1,1)))

print(fit.rpart)
print(fit.rpart$finalModel)
# Inspection informs us that glucose, age and mass look  more important
# 1) root 615 215 neg (0.65040650 0.34959350)  
# 2) glucose< 0.7317214 478 113 neg (0.76359833 0.23640167)  
# 4) age< -0.3156849 269  34 neg (0.87360595 0.12639405) *
#   5) age>=-0.3156849 209  79 neg (0.62200957 0.37799043)  
# 10) mass< -0.6807759 43   2 neg (0.95348837 0.04651163) *
#   11) mass>=-0.6807759 166  77 neg (0.53614458 0.46385542)  
# 22) glucose< -0.4497808 66  19 neg (0.71212121 0.28787879) *
#   23) glucose>=-0.4497808 100  42 pos (0.42000000 0.58000000)  

# Let's train a Random Forest
fit.rf <- train(diabetes ~ ., data = data.trn, method = "rf",
                   trControl = ctrl, 
                   preProcess = c("center","scale"), 
                   mtree=1000)
plot(varImp(fit.nnet))
plot(varImp(fit.rpart))
plot(varImp(fit.rf))

# The three methods agree: glucose, age and mass.
# Tip #4 Sensitivity analysis
library(NeuralSens)
x11()
SensAnalysisMLP(fit.nnet)
# Same conclusion: glucose, age and mass

########################################################
# Recursive Feature extraction
########################################################
set.seed(1234)
# split into training and validation datasets
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- data[ind==1,]
validationData <- data[ind==2,]
trainData <- trainData[complete.cases(trainData),]
validationData <- validationData[complete.cases(validationData),]
# Next we will use the rfe method of the caret package, setting up using the rfeControl method.

# define the control using a random forest selection function
control <- rfeControl(functions=nbFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(trainData[,1:8], 
               trainData[,9], 
               sizes=c(1:8), 
               rfeControl=control)
# Let’s see the results.
print(results)
predictors(results)
## [1] "glucose" "age"     "mass"
# plot the results
plot(results, type=c("g", "o"))

# We have a veredict: Glucose, age and mass
# NOTE: I mentioned above that the distributions of some variables are
# very skewed, so I would take this results with a pinch of salt.
# EXERCISE: Repeat again and you'll probably reach different conclusions

fit.reduced <- train(diabetes ~ glucose+age+mass, data = data.trn, 
                method = "nnet",
                trControl = ctrl, 
                preProcess = c("center","scale"), 
                tuneLength=10)
pred <- predict(fit.reduced,data.tst)
confusionMatrix(table(data.tst[,"diabetes"],pred))

fit.reduced2 <- train(diabetes ~ ., data = data.trn, method = "rpart",
                trControl = ctrl, 
                preProcess = c("center","scale"), 
                tuneLength=10)
pred <- predict(fit.reduced2,data.tst)
confusionMatrix(table(data.tst[,"diabetes"],pred))
fit.reduced3 <- train(diabetes ~ ., data = data.trn, method = "rf",
                trControl = ctrl, 
                mtree=1000,
                preProcess = c("center","scale"))

pred <- predict(fit.reduced3,data.tst)
confusionMatrix(table(data.tst[,"diabetes"],pred))

models <- list(nnet=fit.nnet,tree=fit.rpart,rf=fit.rf,nnet2=fit.reduced,tree2=fit.reduced2,rf2=fit.reduced3)
evalm(models,gnames=c('net','tree','rf','net2','tree2','rf2'))

# Now the full
dotplot(resamples(models))
res <- resamples(models)
summary(res)

