taxi <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/taxi.csv")

str(taxi)
summary(taxi)
head(sort(taxi$fare_amount),25)

taxi <- taxi[taxi$fare_amount>2,]

boxplot(taxi$fare_amount)
hist(taxi$fare_amount)
# log10()
log10(1000000)

taxi$log.fare <- log10(taxi$fare_amount)
hist(taxi$log.fare)
info <- boxplot(taxi$log.fare)
str(info)
info$stats
10^info$stats

taxi.filtered <- taxi[taxi$fare_amount<40,]
summary(taxi.filtered)

hist(taxi.filtered2$dropoff_longitude)
boxplot(taxi$dropoff_longitude)

                                
