# http://jse.amstat.org/v13n2/datasets.mills.html
# http://jse.amstat.org/v4n1/datasets.johnson.html
# Variable description
# 1: Case Number
# 2: Percent body fat using Brozek's equation,
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



bmi <- read.table('http://jse.amstat.org/datasets/fat.dat.txt')
str(bmi)
nam <- c("Case","Brozek","Siri","Density","Age","Weight","Height",
         "BMI","Fat.Free.Weight","Neck","Chest","Abdomen","Hip",
         "Thigh","Knee","Ankle","Biceps","Forearm","Wrist")
colnames(bmi) <- nam
str(bmi)
summary(bmi)

par(mfrow=c(3,3)) 
for(i in 2:10) hist(bmi[,i],xlab=nam[i],main="",col='skyblue')
for(i in 11:18) hist(bmi[,i],xlab=nam[i],main="",col='skyblue')
par(mfrow=c(2,2))
tmp <- boxplot(bmi$Height)
tmp$out
tmp <- boxplot(bmi$Hip)
tmp$out
tmp <- boxplot(bmi$Ankle)
tmp$out
tmp <- boxplot(bmi$Siri)
tmp$out
bmi <- bmi[bmi$Height>29.5 & bmi$Hip<116.1 & bmi$Ankle<33.7 & bmi$Siri<47.5,]
par(mfrow=c(3,3)) 
for(i in 2:10) hist(bmi[,i],xlab=nam[i],main="",col='skyblue')
for(i in 11:18) hist(bmi[,i],xlab=nam[i],main="",col='skyblue')

with(bmi,plot(Siri~Brozek))
library(psych)
pairs.panels(bmi[,c("Siri","BMI","Weight","Height")])

# Regression using good-old "lm"
fit1 <- lm(Siri ~ BMI,bmi)
summary(fit1)

# Fit with caret
library(caret)
fit2 <- train(Siri ~ BMI,bmi,method='lm')
summary(fit2)

pred <- predict(fit2,bmi) # Predictions
par(mfrow=c(1,1))
with(bmi,plot(Siri~BMI))
lines(bmi$BMI,pred,col=2,lwd=3)
hist(fit1$residuals,col='skyblue',xlab='residuals',15)
hist(fit2$finalModel$residuals,col='skyblue',xlab='residuals',15)

# Use lm built-in plot
par(mfrow=c(2,2))
plot(fit1)  # Pretty good overall.
