library(caret)

heart <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/heart.csv")
str(heart)
set.seed(5432)
# 1.Preprocess the dataset according to this criteria
# a)Scale the numerical variables [x*=(x-mean)/standard.deviation].
# We can do this manually, computing the mean and sd for every numerical value or use
# the function preProcess = c("center","scale"), in the caret train call. I'll do that below

# b)Check for NAs
summary(heart) # Everyting is fine

# c)Check if categorical variables are “factors”
heart$sex <- as.factor(heart$sex)
heart$cp <- as.factor(heart$cp)
heart$target <- as.factor(ifelse(heart$target==0,"risk","healthy"))
# d)Check if the target class is reasonably balanced
# 
table(heart$target)
# Not bad
# health  risk 
# 138 165

# 2.Divide the dataset in a proportion 75% for training and 25% for validation.
train <- createDataPartition(heart[,"target"],p=0.75,list=FALSE)
heart.trn <- heart[train,]
heart.tst <- heart[-train,]

# 3.Use the “class” package to create a knn classifier. I'm going to cheat and use caret
fit.knn <- train(target ~ ., data = heart.trn, method = "knn",
                trControl = ctrl, 
                preProcess = c("center","scale"),  # here is where I implement step 1.a)
                tuneGrid =data.frame(k=5))

# 4.Compute the Confusion Matrix for k=5
pred.knn <- predict(fit.knn,heart.tst)
confusionMatrix(table(heart.tst$target,pred.knn))
# 72%  TRAINING accuracy

# 5.Try different values of the hyperparameter “k” (odd values) to determine the one which maximizes the accuracy. (Hint: you can save some sweat by using a “for” loop. Check the documentation on “Control flow”). 
#    Instead of doing this I'll use cross validation (same for the next step)
ctrl  <- trainControl(method  = "cv",number  = 10, summaryFunction = multiClassSummary)
# Adding multiClass summary allows to extract automatically the values for the specificity and sensitivity

fit.knn.cv <- train(target ~ ., data = heart.trn, method = "knn",
                trControl = ctrl, 
                preProcess = c("center","scale"), 
                tuneGrid =data.frame(k=seq(5,100,by=2)))
# 6.For the optimal “k”, calculate the sensitivity and specificity of the classifier. 
print(fit.knn.cv) 
# The optimal value is k=59. Probably it'll be around that for you 
plot(fit.knn.cv)

pred.knn.cv <- predict(fit.knn.cv,heart.tst)
confusionMatrix(table(heart.tst$target,pred.knn.cv))

# 7.Build a logistic regression classifier using the function “glm”
#    I'll cheat again and use caret
fit.glm <- train(target ~ ., data=heart.trn,method='glm',family=binomial(),
                 trControl=ctrl,
                 preProcess=c("center","scale"))

# 8.Compute the Confusion matrix for a threshold at 0.5
pred.glm <- predict(fit.glm,heart.tst)
confusionMatrix(table(heart.tst$target,pred.glm)) 
# Interestingly, almost as good as knn but log-reg is more interpretable

# 9.Try different values of the the threshold (from 0.1 to 0.9) to determine the one which maximizes the accuracy.
pred.glm.probs <- predict(fit.glm,heart.tst,type='prob')

# You can use a loop but I'll use the advanced sapply method
thres <- seq(0.1,0.9,by=0.1)
myfunc <- function(thres) {
  predicted <- as.factor(ifelse(pred.glm.probs$risk>thres,"risk","healthy")) 
  return(length(which(predicted == heart.tst$target))/nrow(heart.tst)*100) # Sum the elements in the diagonal
}
accuracy <- sapply(thres,myfunc)
plot(thres,accuracy)
 


# 10.For the optimal “threshold”, calculate the sensitivity and specificity of the classifier.
optimal.thres <- 0.5 # The optimal
predicted <- as.factor(ifelse(pred.glm.probs$risk>optimal.thres,"healthy","risk")) 
confusionMatrix(table(heart.tst$target,predicted))
# Sensitivity : 0.7742          
# Specificity : 0.7727  

# 11.Discuss critically the meaning of the coefficients of the fit obtained with coef.
exp(coef(fit.glm$finalModel))
1/exp(coef(fit.glm$finalModel))

# As we have standardized the numerical parameters (with the preProcess call) the odds for a 
# patient with mean age (54.366, see the summary above), nean reating bps (131.6) etc etc...
# for cp=0 and sex = male (class=0) to be healthy is 1.3 (56%)
# As the standard deviation for age is 9 years, every 9 yearse the odds increase by a factor 1.1
# On the other hand, women have 0.38 less odds of heart attack. I leave for you further interpretation

# 12.Discuss which classifier is best according to accuracy, sensitivity and specificty.

confusionMatrix(table(heart.tst$target,pred.knn))
confusionMatrix(table(heart.tst$target,pred.knn.cv))
confusionMatrix(table(heart.tst$target,pred.glm))

# Accuracy is almost the same in knn with CV and glm, so I'll prefer the second method as it's easier
# to interpret.
