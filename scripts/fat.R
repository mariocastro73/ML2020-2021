# http://jse.amstat.org/v13n2/datasets.mills.html
# http://jse.amstat.org/v4n1/datasets.johnson.html
# Variable description
# 1: Case Number
# 2: Percent body fat using Berzak's equation,
# 3: Percent body fat using Siri's equation,
# 4: Density (gm/cm^3)
# 5: Age (yrs)
# 6: Weight (lbs)
# 7: Height (inches)
# 8: Adiposity index = Weight/Height^2 (kg/m^2) (aka BMI)
# 9: Fat Free Weight
# 10: Neck circumference (cm)
# 11: Chest circumference (cm)
# 12: Abdomen circumference (cm) "at the umbilicus
# 13: Hip circumference (cm)
# 14: Thigh circumference (cm)
# 15: Knee circumference (cm)
# 16: Ankle circumference (cm)
# 17: Extended biceps circumference (cm)
# 18: Forearm circumference (cm)
# 19: Wrist circumference (cm) "distal to the

set.seed(1234) # Not necessary as lm is fitted mathematically, not "trained"
bmi <- read.table('http://jse.amstat.org/datasets/fat.dat.txt')
str(bmi)
nam <- c("Case","Body.Fat","Siri","Density","Age","Weight","Height",
         "BMI","Fat.Free.Weight","Neck","Chest","Abdomen","Hip",
         "Thigh","Knee","Ankle","Biceps","Forearm","Wrist")
colnames(bmi) <- nam
str(bmi)
summary(bmi)

par(mfrow=c(3,3)) 
for(i in 2:10) hist(bmi[,i],xlab=nam[i],main="",col='skyblue')
for(i in 11:18) hist(bmi[,i],xlab=nam[i],main="",col='skyblue')
par(mfrow=c(2,2))
tmp <- boxplot(bmi$Height) # Boxplot has a "return" inside
tmp$out
tmp <- boxplot(bmi$Hip)
tmp$out
tmp <- boxplot(bmi$Ankle)
tmp$out
tmp <- boxplot(bmi$Siri)
tmp$out
# More elegant solution would involve using %in% or caret
bmi <- bmi[bmi$Height>29.5 & bmi$Hip<116.1 & bmi$Ankle<29.6 & bmi$Siri<47.5,]
par(mfrow=c(3,3)) 
for(i in 2:10) hist(bmi[,i],xlab=nam[i],main="",col='skyblue')
for(i in 11:18) hist(bmi[,i],xlab=nam[i],main="",col='skyblue')

with(bmi,plot(Siri~Body.Fat)) # Both formulas are equivalent


# Regression using good-old "lm"
par(mfrow=c(1,1))
with(bmi,plot(BMI ~ Body.Fat))
fit1 <- lm(Body.Fat ~ BMI,bmi)
s1 <- summary(fit1) # summary has a "return" inside
print(s1$r.squared) # Print R^2 alone

# Use lm built-in plot (Always!)
hist(fit1$residuals,col='skyblue',xlab='residuals')
par(mfrow=c(2,2))
plot(fit1)  # Pretty good overall.
pred <- predict(fit1,bmi) # Do some predictions, etc...


# Fit with caret
library(caret)
fit2 <- train(Body.Fat ~ BMI,bmi,method='lm')
summary(fit2)
hist(fit2$finalModel$residuals,col='skyblue',xlab='residuals',15)

########################################################
# Multiple regression:  What if add more regressors?   #
########################################################
library(psych)
data <- bmi[,c("Body.Fat","BMI","Age","Abdomen","Thigh")]
pairs.panels(data) 

fit.multi <- lm(Body.Fat ~ .,data)
summary(fit1)
summary(fit.multi) # Much better!!!
data <- bmi[,c("Body.Fat","Age","Abdomen")]
fit.multi2 <- lm(Body.Fat ~ .,data)
summary(fit.multi2) # Much better!!!

# But don't cellebrate yet, let's see some plots
par(mfrow=c(2,2))
plot(fit.multi2) # good!

# What variables follow the linear regression assumptions?
data$residuals <- fit.multi2$residuals
pairs.panels(data[,-1])


#### Extra: the origins of BMI
# Let's try some fits with weight and height
library(ggplot2)
g <- ggplot(bmi)
g + geom_point(aes(Height,Body.Fat))
g + geom_point(aes(Weight,Body.Fat)) # Much better
g + geom_point(aes(Weight,Height)) # But see the correlation between them!
g + geom_point(aes(BMI,Body.Fat)) # Better, but why?

print(summary(lm(Body.Fat ~ BMI,bmi))) # Looks banana-shaped
print(summary(lm(Body.Fat ~ Weight+Height,bmi))) # Worse fit than BMI
# What if we consider log (log-normality)
print(summary(lm(Body.Fat ~ log(BMI),bmi))) # Slightly better. 
# What if we use Height and Weight with log
print(summary(fit <- lm(Body.Fat ~ log(Weight)+log(Height),bmi))) # Better but look at the coefficients

coef(fit)
coef(fit)/44.22 # Almost a ratio of -2 for height
# Remember that log(x1)-2*log(x2)=log(x1/x2^2) -> The formula for BMI!
par(mfrow=c(2,2))
plot(fit) # Passes the tests
