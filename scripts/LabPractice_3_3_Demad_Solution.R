## Load libraries --------------------------------------------------------------------------------
library(caret)
library(gam)
library(splines)
library(corrplot)
library(psych)

## LoadData 
fdata <- read.csv("DAILY_DEMAND.csv", sep = ";")
#Plot relation between temperature and demand
ggplot(fdata)+geom_point(aes(x=TEMP, y=DEM))
#We can see that there is a non-linear relation between them.


# Set variable types -----------------------------------------
fdata$WD = as.factor(fdata$WD);
fdata$fecha <- NULL
str(fdata)

#correlation plot of numeric variables
numvars <- sapply(fdata, class) %in% c("integer","numeric")
C <- cor(fdata[,numvars])
corrplot(C, method = "number")
pairs.panels(fdata)


## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
ratioTR = 0.8 #Percentage for training
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$DEM,     #output variable. createDataPartition creates proportional partitions
                                  p = ratioTR, #split probability
                                  list = FALSE, #Avoid output as a list
                                  times = 1) #only one partition
#obtain training and validation sets
fTR = fdata[trainIndex,]
fTS = fdata[-trainIndex,]


## Initialize trainControl -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions

## Linear Regression -------------------------------------------------------------------------------------------
set.seed(150) #For replication
lm.fit = train(form = DEM ~ WD + TEMP, 
               data = fTR, 
               method = "lm", #Linear model
               preProcess = c("center","scale"),
               trControl = ctrl_tune, 
               metric = "RMSE")
lm.fit #information about the resampling settings
summary(lm.fit)  #information about the model trained


#Evaluate the model with training sets and diagnosis
fTR_eval = fTR
fTR_eval$lm_pred = predict(lm.fit,  newdata = fTR)  
par(mfrow=c(2,2))
plot(lm.fit$finalModel)
par(mfrow=c(1,1))
plot(lm.fit$finalModel$residuals~fTR$WD)
plot(lm.fit$finalModel$residuals~fTR$TEMP)

#Relation between DEM and TEMP
ggplot(fTR_eval)+geom_point(aes(x=TEMP, y = DEM))+geom_point(aes(x=TEMP, y = lm_pred,  color=WD))



#-------------------------------------------------------------------------------------------------
#----------------------- polynomial regression  ------------------------------------------------
#-------------------------------------------------------------------------------------------------
set.seed(150) #For replication
poly.fit = train(form = DEM ~ WD + poly(TEMP, degree = 2, raw = TRUE),
               data = fTR, 
               method = "lm", #Linear model
               preProcess = c("center","scale"),
               trControl = ctrl_tune, 
               metric = "RMSE")
poly.fit #information about the resampling settings
summary(poly.fit)  #information about the model trained


#Evaluate the model with training sets and diagnosis
fTR_eval$poly_pred = predict(poly.fit,  newdata = fTR)  

par(mfrow=c(2,2))
plot(poly.fit$finalModel)
par(mfrow=c(1,1))
plot(poly.fit$finalModel$residuals~fTR$WD)
plot(poly.fit$finalModel$residuals~fTR$TEMP)
#Non-linear effect seen in TEMP

#Relation between DEM and TEMP
ggplot(fTR_eval)+geom_point(aes(x=TEMP, y = DEM))+geom_point(aes(x=TEMP, y = poly_pred,  color=WD))




#-------------------------------------------------------------------------------------------------
#----------------------- GAM with splines   ------------------------------------------------
#-------------------------------------------------------------------------------------------------

set.seed(150) #For replication
gam.fit = train(form = DEM ~ WD + TEMP,
                data = fTR, 
                method = "gamSpline",
                tuneGrid = data.frame(df = seq(2,10,2)),
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
gam.fit #information about the resampling settings
ggplot(gam.fit)
summary(gam.fit)  #information about the model trained
#Plot the fitted splines
#Careful, if library "car" is loaded, this plot gives an error
plot(gam.fit$finalModel, se=TRUE ,col ="blue ")


#Evaluate the model with training sets and diagnosis
fTR_eval$gam_pred = predict(gam.fit,  newdata = fTR)  
res
plot(gam.fit$finalModel$residuals~fTR$WD)
plot(gam.fit$finalModel$residuals~fTR$TEMP)
# Looks flat now

#Relation between DEM and TEMP
ggplot(fTR_eval)+geom_point(aes(x=TEMP, y = DEM))+geom_point(aes(x=TEMP, y = gam_pred,  color=WD))




#-------------------------------------------------------------------------------------------------
#----------------------- GAM with splines & Holidays   ------------------------------------------------
#-------------------------------------------------------------------------------------------------
# POSSIBLE IMPROVEMENTS
# - Holidays (How?)
fTR$WD[(fTR_eval$DEM-fTR_eval$gam_pred)<(-80)] <- 7

set.seed(150) #For replication
gam.fit = train(form = DEM ~ WD + TEMP,
                data = fTR, 
                method = "gamSpline",
                tuneGrid = data.frame(df = seq(2,10,2)),
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
gam.fit #information about the resampling settings
ggplot(gam.fit)
summary(gam.fit)  #information about the model trained
#Plot the fitted splines
#Careful, if library "car" is loaded, this plot gives an error
plot(gam.fit$finalModel, se=TRUE ,col ="blue ")


#Evaluate the model with training sets and diagnosis
fTR_eval$gam_pred = predict(gam.fit,  newdata = fTR)  
with(fTR_eval,plot(gam_pred-DEM ~ TEMP)) # Nice although there is still heterocedasticity

#Relation between DEM and TEMP
ggplot(fTR_eval)+geom_point(aes(x=TEMP, y = DEM))+geom_point(aes(x=TEMP, y = gam_pred,  color=WD))

