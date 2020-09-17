# na.omit by with function
Titanic <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/Titanic.csv")
View(Titanic)
str(Titanic)
summary(Titanic)

Titanic2 <- Titanic[!is.na(Titanic$Age),]
View(Titanic2)
# na.omit
Titanic3 <- na.omit(Titanic)
View(Titanic3)

# with: void repetive typig Titanic$adsfdsa

Titanic3$Survived <- as.factor(Titanic3$Survived)
plot(Titanic3$Age ~ Titanic3$Survived)
with(Titanic3,plot(Age~Survived))
with(Titanic3,
     {
         plot(Age~Survived);
         plot(Age~Sex);
         table(Survived,Sex);
     } 
)

# Custom function
c(mean(Titanic3$Age),sd(Titanic3$Age))

my.first.function <- function(v) {
     return(c(mean(v),sd(v)))
}

my.first.function(Titanic3$Age)
my.first.function(runif(1000,3,10))

# by 
my.first.function(Titanic3$Age[Titanic3$Survived==0])
my.first.function(Titanic3$Age[Titanic3$Survived==1])

by(Titanic3$Age,Titanic3$Survived,my.first.function)
with(Titanic3,
     by(Age,Survived,my.first.function))

# apply lapply mapply sapply 
