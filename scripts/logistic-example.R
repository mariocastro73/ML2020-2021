# Dataset to predict admision to college 
admision <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/college.csv")
str(admision)
summary(admision)  # The range of grep and gpa (the two numerical features) is so
# different, so I'll normalize the data later
admision$rank <- as.factor(admision$rank)
# GPA (Grade Point Average)
# GRE (Graduate Record Examination)
# Rank: 4: Top universities

library(psych)
pairs.panels(admision) # Have glimpse of the dataset (gpa is a bit skewed, so we could correct it)
#PlotDataframe(admision, output.name = "admit") # Included in MLTools
# It seems that gre and gpa are slightly correlated. That is expected but might create
# some misinterpration of the data. Let's move on

# Class imbalance
table(admision$admit) # There is some class imbalance, but the dataset is small, so I'll keep all the cases
# We should check this in a thorough analysis
table(admision$rank) # There is some imbalance but, this is a predictor and all the values are far from 0

# Before doing the regression, let's scale the data (I will scale gre and gpa and not
# standardize it because both are positive features I prefer not to. This is personal
# and standadizin by (x-mean)/standard.deviation is the most common approach). 
print(max(admision$gre))
print(max(admision$gpa))
# Advanced tip
# sapply(admision[,2:3],max)
admision$gre <- admision$gre/max(admision$gre)
admision$gpa <- admision$gpa/max(admision$gpa)
summary(admision)

# Let's split the data
p <- 0.8 # Training: 80% of the dataset; Validation: 20%
train <- sample.int(nrow(admision),p*nrow(admision))
train.data <- admision[train,]
val.data <- admision[-train,]

# Logistic regression
# Formula: admit ~ gre + gpa + rank
# There is no need to convert admit to factor.
fit <- glm(admit ~ gre + gpa + rank, data = train.data, family = "binomial") # Fit
#fit <- glm(admit ~ ., data = admision, family = "binomial") # Simpler syntax
summary(fit) # Print the results
## odds ratios only
exp(coef(fit))

# The intercept means that, for grep=gpa=0, the odds of being admitted in a Rank 1 
# university is 54:1 against
# Every point in the (scaled) gre increases your odds by a 600% percent. 
# That sounds a lot, but it means that it increases only
exp(1.5061/8)
# 1.21 => 21% increase for every 100 points (remember that the original scale was 0..800)
# For gpa
exp(3.6647/4)
# In this case, it means a 150% increase for every gpa point. this is a lot...

1/exp(coef(fit)) # To help to interpret the values lower than 1

# Note how the odds ratio decreases dramatically for every "step" in the rank
# For the same values of gre and gpa, applying to a rank 2 vs a rank 1 university 
# reduces your odds by a factor of 1.6. Also, and this is more striking, applying for
# a rank 4 university reduces your odds by 4.4 (340%!!!) with respect to a rank 3.

# Let's see how it predicts
pred <- predict(fit,val.data,type='response') # type='response' gives probabilities
val.data$prediction <- pred
pairs.panels(val.data) # Looks nice. See how the prediction correlates with the features

# Let's now calculate the Confusion matrix
pred.class <- ifelse(pred>0.5,1,0) # Threshold at 0.5
cm <- table(col=val.data$admit,pred.class) # Confusion matrix (true vs predicted)
print(cm)
sum(diag(cm))/sum(colSums(cm)) # accuracy (true's/all)=> 69%
my.statistics(val.data$admit,pred.class) # Borrowed from knn-example.R (see https://github.com/mariocastro73/ML2020-2021/blob/master/scripts/knn-example.R)
# Sensitivity sucks but specificity is 98% so False positives are extremely low but
# you can't predict if all those negatives deserve it or not.
