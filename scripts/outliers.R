# import the taxi dataset
taxi <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/taxi.csv")


str(taxi) # See its structure
summary(taxi) # Summary (cheap but amazing trick)
head(sort(taxi$fare_amount),25) # Lets show the first 25 rows of the sorted variable fare_amount

taxi <- taxi[taxi$fare_amount>2,] # Overwrite the taxi dataframe with those rows with fare_amount >2 dollars

boxplot(taxi$fare_amount) # Do a basic boxplot
hist(taxi$fare_amount) # And a histogram 
# log10() # Log10 gives you the order of magnitude of a number
log10(1000000)

taxi$log.fare <- log10(taxi$fare_amount) # Create a new variable
hist(taxi$log.fare) # Histogram for the new variable
info <- boxplot(taxi$log.fare) # Superpower: Extract info from the boxplot
str(info) 
info$stats # This gives you the most relevant descriptors. The third one is the median of the distribution (see ?boxplot)
10^info$stats # Convert back to dollars

taxi.filtered <- taxi[taxi$fare_amount<40,] # Create a new dataframe without the outliers
summary(taxi.filtered)

hist(taxi.filtered2$dropoff_longitude) # As an exercise, play with longitude and latitude
boxplot(taxi$dropoff_longitude)

                                
