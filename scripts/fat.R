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
