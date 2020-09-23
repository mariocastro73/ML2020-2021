taxi <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/taxi.csv")
str(taxi)
summary(taxi)
plot(taxi$pickup_longitude)
boxplot(taxi$pickup_longitude)
hist(taxi$pickup_longitude)
taxi2 <- taxi[taxi$pickup_longitude < -60,]
hist(taxi2$pickup_longitude)
summary(taxi2)
hist(taxi2$pickup_latitude)
taxi2 <- taxi2[taxi2$pickup_latitude>40 & taxi2$pickup_latitude<41,]
hist(taxi2$pickup_latitude)
summary(taxi2)
hist(log10(taxi2$fare_amount))
sort(taxi2$fare_amount)
taxi2 <- taxi2[taxi2$fare_amount >0.01,]
hist(taxi2$fare_amount)
hist(log10(taxi2$fare_amount))
summary(taxi2)

data <- data.frame(x=c(1,2,NA),y=c(10,21,33))
summary(data)
str(taxi)
Titanic[1:10,]
summary(taxi2$fare_amount)


fa.mean <- mean(taxi2$fare_amount)
fa.sd <- sd(taxi2$fare_amount)
x <- (taxi2$fare_amount-fa.mean)/fa.sd
pa.mean <- mean(taxi2$passenger_count)
pa.sd <- sd(taxi2$passenger_count)
x <- (taxi2$fare_amount-fa.mean)/fa.sd
y <- (taxi2$passenger_count-pa.mean)/pa.sd

mean(x)
sd(x)
mean(y)
sd(y)
hist(y)
taxi2$fare_amount <- x
taxi2$passenger_count <- y
summary(taxi2)
str(taxi2)

pago <- c(0,1,0,1,1,0)
as.factor(pago)
as.factor(ifelse(pago==0,"No","Si"))

library(psych)
pairs.panels(taxi2)

table(as.factor(taxi$passenger_count))

table(Titanic$Sex)
female <- which(Titanic$Sex=='female')
male <- which(Titanic$Sex=='male')
length(female)
length(male)
head(Titanic,10)

male.down <- sample(male,length(female))
length(male.down)
female.up <- sample(female,length(male),replace=TRUE)
length(female.up)
