## Load libraries --------------------------------------------------------------------------------
library(caret)
library(ggplot2)
library(corrplot)
library(psych)


library(ISLR) #Load dataset library from Introduction to Statistical Learning book
data(Hitters)
?Hitters #Description of the dataset
str(Hitters)
# I prefer to reorder the variables to make analysis more intuitive (especially for PCR and PLS)
Hitters <- Hitters[,c(19,1:7,16:18,8:15,20)]
str(Hitters)
########################################################
## Part 0: Exploratory analysis ------------------------
########################################################

summary(Hitters)
# Salary has NA's and it's our output, so I'm going to remove them
Hitters <- na.omit(Hitters) #Eliminate NA
x11(width = 16,height = 10) # Bigger window for plotting pairs
pairs.panels(Hitters)
boxplot(Hitters$CAtBat)
boxplot(Hitters$CAtBat)$out
Hitters <- Hitters[Hitters$CAtBat<14053,]
pairs.panels(Hitters)

hist(Hitters$Salary)
BoxCoxTrans(Hitters$Salary) # 0.1 and we're talking about money, log10 is my go-to
Hitters$Salary <- log10(Hitters$Salary) 
boxplot(Hitters$Salary) # Good! No weird things there
# I won't apply BoxCox or YeoJohnson as the variables are integer numbers
# Actually, for variables that are "counts" typically, they are well described
# by Poisson distributions and I prefer to leave them like that

# Optional: Look more visually at the correlations
numvars <- sapply(Hitters, class) %in% c("integer","numeric")
C <- cor(Hitters[,numvars])
corrplot::corrplot(C, method = "circle",diag = FALSE)
# Look at the blocks!!! Beautiful how our brain works!

########################################################
# Prepare the data
# Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
#create random 80/20 % split
p.split <- 0.8 #split probability (only 262 observations, so keep it large)
trainIndex <- createDataPartition(Hitters$Salary,     #output variable. createDataPartition creates proportional partitions
                                  p = p.split,
                                  list = FALSE) #Avoid output as a list
#obtain training and validation sets
Hitters.trn = Hitters[trainIndex,]
Hitters.trn_eval <- Hitters.trn #For storing evaluations of the models (optional)
Hitters.tst = Hitters[-trainIndex,]

## Initialize trainControl -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions

########################################################
# PART I: Start with regression and do things manually
########################################################

#==================================================================
## FIRST model with all
#==================================================================
set.seed(150) #For replication
lm.fit = train(form = Salary ~ ., 
               data = Hitters.trn, 
               method = "lm", #Linear model
               tuneGrid = data.frame(intercept = TRUE), 
               # preProcess = c("center","scale"), # No scaling or centering (Poisson data)
               trControl = ctrl_tune, 
               metric = "RMSE")

summary(lm.fit)  #information about the model trained
lm.fit #information about the resampling settings

#Evaluate the model with training sets and diagnosis
lm_pred1 <-  predict(lm.fit,  newdata = Hitters.trn)  
lm_pred1.tst <-  predict(lm.fit,  newdata = Hitters.tst)  
pairs.panels(data.frame(lm_pred1,Hitters.trn$Salary))
pairs.panels(data.frame(lm_pred1.tst,Hitters.tst$Salary)) # Could be better

Hitters.trn_eval$res <-  lm.fit$finalModel$residuals  
x11(width = 16,height = 10)
# It's misleading to compare residuals with the output
# also numerical because it's hard to see trends in the categorical ones
pairs.panels(Hitters.trn_eval[,numvars][,-1])  # I see nonlinearities, but that's for another day

## Let's remove stuff by hand
summary(lm.fit)

# pvalues suggest removing all but Years,Errors, Division. Maybe too much....
#Identify correlated variables
library(car)
vif(lm.fit$finalModel)
barplot(vif(lm.fit$finalModel),las=2,col='darkblue')
abline(h=10,col=2,lwd=2)
which(vif(lm.fit$finalModel)>10)
which(vif(lm.fit$finalModel)>40)
which(vif(lm.fit$finalModel)>100)

#==================================================================
## SECOND model removing by hand
#==================================================================
set.seed(150) #For replication
# lm2.fit = train(form = Salary ~ HmRun+Runs+RBI+Walks+Years+League+Division+PutOuts+Assists+Errors+NewLeague,
lm2.fit = train(form = Salary ~ . -CAtBat-CHits-CHmRun-CRuns-CRBI-CWalks,
                data = Hitters.trn, 
                method = "lm", #Linear model
                tuneGrid = data.frame(intercept = TRUE), 
                # preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
lm2.fit #information about the resampling settings
lm.fit # compare with full model
summary(lm2.fit)  #compare with full model
summary(lm.fit)  #information about the model trained. We've lost accuracy and increased error. 
# Pretty good!

#Identify correlated variables
vif(lm2.fit$finalModel)
barplot(vif(lm2.fit$finalModel),las=2,col='darkblue')
abline(h=10,col=2) # Almost there! But this looks like trial and error


#==================================================================
## THIRD model with automatic selection
#==================================================================
## As there is no tuning parameter for linear regression, Cross-Validation is not necessary for variable selection.
## For other methods, the trainControl specified before can be used
ctrl_none <- trainControl(method = "none",                     
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions

## The control function is similar to trainControl()
## Specifies the cross validation method used for selecting the optimum number of variables
ctrl_rfe <- rfeControl(method = "cv",
                       number = 10,
                       verbose = FALSE,
                       functions = caretFuncs)
## rfe() function instead of train
set.seed(150)
lm.RFE <- rfe(form = Salary~.,
              data = Hitters.trn,
              # Arguments passed to train() function
              method = "lm",
              # preProcess = c("center","scale"),
              trControl = ctrl_none,
              # Arguments for rfe
              sizes = 1:(ncol(Hitters.trn)-1), #The number of features that should be retained
              metric = "Rsquared",
              rfeControl = ctrl_rfe)
# Grab a coffee and wait a bit.....
lm.RFE # Cross validation results and variable selection. 
# Great, from 19 to 11. Not bad
plot(lm.RFE,metric = "RMSE",type='b',cex=2)
lm.RFE$fit #Final caret train() object
lm.RFE$fit$finalModel #Final model trained
summary(lm.RFE$fit$finalModel)

#identify correlated variables
vif(lm.RFE$fit$finalModel)
barplot(vif(lm.RFE$fit$finalModel),las=2,col='darkblue')
abline(h=10,col=2,lwd=2) # We could now still remove Hits or AtBat manually

#==================================================================
## FOURTH model using RIDGE REGULARIZATION
#==================================================================
#######Ridge regression
set.seed(150) #For replication
#With categorical variables, formula method should be used
ridge.fit = train(form = Salary~.,
                  data = Hitters.trn, 
                  method = "glmnet",
                  nlambda = 1000, # Number of lambda values for glmnet function 
                  tuneGrid = expand.grid(
                    lambda = 10^ seq(-3,1, length =50), 
                    alpha = 0),
                  # preProcess = c("center","scale"),
                  trControl = ctrl_tune, 
                  metric = "RMSE")
ridge.fit #information about the resampling
plot(ridge.fit)

#Plot the evolution of the coefficients as a function of lambda
plot(ridge.fit$finalModel,xvar='lambda')
abline(v=log(ridge.fit$finalModel$lambdaOpt),lty=2)
#Coefs for high value of lambda
ridge.fit$finalModel$lambda[1]
coef(ridge.fit$finalModel)[-1,1]# Zero (look at the scale)

#Coefs for low value of lambda
ridge.fit$finalModel$lambda[1000]
coef(ridge.fit$finalModel)[-1,1000]
barplot(coef(ridge.fit$finalModel)[-1,1000],las=2)

#######lasso regression
set.seed(150) #For replication
#With categorical variables, formula method should be used
lasso.fit = train(form = Salary~.,
                  data = Hitters.trn, 
                  method = "glmnet",
                  nlambda = 1000, # Number of lambda values for glmnet function 
                  tuneGrid = expand.grid(
                    lambda = 10^seq(-3,0, length =50), 
                    alpha = 1),
                  # preProcess = c("center","scale"),
                  trControl = ctrl_tune, 
                  metric = "RMSE")
lasso.fit #information about the resampling
plot(lasso.fit)
#Plot the evolution of the coefficients as a function of lambda
x11(width = 16,height = 10)
plot(lasso.fit$finalModel,xvar='lambda')
legend('topright',legend=names(Hitters[,-1]),col=1:19,lty=1)
abline(v=log(lasso.fit$finalModel$lambdaOpt),lty=2)
#Coefs for high value of lambda
lasso.fit$finalModel$lambda[5]
coef(lasso.fit$finalModel)[,5] # 0!!
#Coefs for low value of lambda
lasso.fit$finalModel$lambda[550]
coef(lasso.fit$finalModel)[-1,550]
barplot(coef(lasso.fit$finalModel)[-1,550],las=2)

#-------------------------------------------------------------------------------------------------
#--------------------------- Dimension reduction methods ------------------------------------------
#-------------------------------------------------------------------------------------------------

#######Principal component regression
set.seed(150) #For replication
#With categorical variables, formula method should be used
pcr.fit = train(form = Salary~.,
                data = Hitters.trn, 
                method = "pcr",
                tuneGrid = data.frame(ncomp = 1:(ncol(Hitters.trn)-1)),
                # preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
pcr.fit #information about the resampling
plot(pcr.fit)
#Variance explained
summary(pcr.fit$finalModel)
pcr.fit$finalModel$loadings

#######Partial least squares regression
set.seed(150) #For replication
#With categorical variables, formula method should be used
plsr.fit = train(form = Salary~.,
                 data = Hitters.trn, 
                 method = "pls",
                 tuneGrid = data.frame(ncomp = 1:(ncol(Hitters.trn)-1)),
                 preProcess = c("center","scale"),
                 trControl = ctrl_tune, 
                 metric = "RMSE")
plsr.fit #information about the resampling
plot(plsr.fit)

summary(plsr.fit$finalModel)
plsr.fit$finalModel$loadings
par(mfrow=c(2,1))
barplot(plsr.fit$finalModel$loadings[,1],las=2) # PLS1 1 is related to player performance
barplot(plsr.fit$finalModel$loadings[,2],las=2) # PLS2 distinguishes between year 86 and history and league characteristics
par(mfrow=c(1,1))



#-------------------------------------------------------------------------------------------------
#--------------------------- Cross-Validation results ------------------------------------------
#-------------------------------------------------------------------------------------------------
transformResults <- resamples(all.fits <- list(
  lm=lm.fit,
  lm2 = lm2.fit,
  lm_rfe=lm.RFE,
  ridge = ridge.fit,
  lasso = lasso.fit,
  pcr = pcr.fit,
  pls = plsr.fit))
summary(transformResults) # Lasso and PLS win if we look at Rsquared and RMSE 
dotplot(transformResults,metric = "RMSE")
dotplot(transformResults,metric = "Rsquared")
dotplot(transformResults,metric = c("RMSE","Rsquared"))


# Testing (manually computed RMSE)
myfunc <- function(fit) {
  summary(sqrt((predict(fit,Hitters.tst)-Hitters.tst$Salary)^2)) # RMSE for testing
}
lapply(all.fits,myfunc)
# pls lasso and ridge win, but differences are small


# Finally, let's see what about variable importance
plot(varImp(lm.fit))
plot(varImp(lm2.fit))
print(varImp(lm.RFE))
plot(varImp(ridge.fit))
plot(varImp(lasso.fit))
# All methods agree that your performance in the
# past is not a good predictor of the future salary, but strangely enough, 
# how many years you've been playing is (Years)
# Also the division and the league, of course
# Finally, hits and errors "weigh" the same! Fun!
plot(varImp(pcr.fit)) # Applicable but not useful as the predictors are mixed
plot(varImp(plsr.fit))# Applicable but not useful as the predictors are mixed
