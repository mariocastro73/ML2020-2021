library(caret)
library(rpart.plot)
data <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/carseats.csv')
################################################################################
# A  data set containing sales of child car seats at 400 different stores.
# Sales: Unit sales (in thousands) at each location
# CompPrice: Price charged by competitor at each location
# Income: Community income level (in thousands of dollars)
# Advertising: Local advertising budget for company at each location (in thousands of dollars)
# Population: Population size in region (in thousands)
# Price: Price company charges for car seats at each site
# ShelveLoc: A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site
# Age: Average age of the local population
# Education: Education level at each location
# Urban: A factor with levels No and Yes to indicate whether the store is in an urban or rural location
# US: A factor with levels No and Yes to indicate whether the store is in the US or not

str(data)
summary(data)
set.seed(666)
train <- createDataPartition(data[,"High"],p=0.8,list=FALSE)
data.trn <- data[train,]
data.tst <- data[-train,]

ctrl  <- trainControl(method  = "cv",number  = 10) #, summaryFunction = multiClassSummary

# random forest

fit.cv <- train(High ~ ., data = data.trn, method = "rf",
                trControl = ctrl, 
                tuneLength = 50)

print(fit.cv)
plot(fit.cv)

pred <- predict(fit.cv,data.tst)
confusionMatrix(table(data.tst[,"High"],pred))


print(varImp(fit.cv))
plot(varImp(fit.cv))





fit.cv <- train(High ~., data = data.trn, method = "rpart",
                trControl = ctrl, 
                tuneLength = 20)

pred <- predict(fit.cv,data.tst)
confusionMatrix(table(data.tst[,"High"],pred))
print(fit.cv)
plot(fit.cv)
rpart.plot(fit.cv$finalModel,fallen.leaves = F)
print(varImp(fit.cv))
plot(varImp(fit.cv))
