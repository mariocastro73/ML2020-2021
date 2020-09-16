# 1.	Let us begin with the basics. Create a vector with values 2002, 2004, 2006, 2008 using c and seq functions.
v <- c(2002, 2004, 2006, 2008)
v <- seq(2002,2008,by = 2)


# 2.	Use function length to get the size of the vector.
length(v)


# 3.	Try the different methods for selecting the elements of the vector.
v[2]
v[3:4]
v[-1]


# 4.	Load the data set usedcars.csv into a variable named fdata. It contains actual data about used cars advertised for sale on a popular U.S. website. Source: B. Lantz (2015). Machine Learning with R. Second edition. PACKT
# ----- FIRST SET WORKING DIRECTORY ----
fdata <- read.table(file = "usedcars.csv", #Name of text file.
            sep = ",",                       #Separation character.
            header = TRUE,                   #If column names are in the first row.
            na.strings = "NA",               #Character to be marked as missing value.
            stringsAsFactors = FALSE)         #¿convert string to factors?


# 5.	Use str and summary functions on fdata. What types of variables are in the dataset? What are the average values of the numeric variables?
str(fdata)
summary(fdata)


# 6.	Use View and head functions on fdata.
View(fdata)
head(fdata)


# 7.	Access the elements number 5 to 20 of variable color.
fdata$color[5:20]


# 8.	Create a new dataset removing row numbers 10 and 100.
fdata2 <- fdata[-c(10,100), 1:2 ]


# 9.	Create a new dataset only with columns year, price and mileage.
fdata2 <- fdata[,c(1,3,4)] #Option 1
fdata2 <- fdata[,c("year","price","mileage")] #Option 2


# 10.	Obtain statistics for variables year and price.
summary(fdata$year)
summary(fdata$price)
#functions mean(), var(), quantile(), etc could also be used


# 11.	Use function by() to calculate statistics filtering by classes.
by(fdata$price, fdata$transmission, mean)
by(fdata$price, fdata$transmission, sd)
by(fdata$price, fdata$color, summary)


# 12.	Filter from this dataset the rows that have a year that matches the values of the vector created in step 1.
index_year <- fdata$year %in% v #option 1
fdata2 <- fdata[index_year,] 

fdata2 <- fdata[fdata$year %in% v,] #option 2

#check if filter is correct
table(fdata2$year)


# 13.	Create a new column in the dataset named PM resulting from multiplying the values of price and mileage in each row.
fdata$PM <- fdata$price * fdata$mileage


# 14.	Plot the price values with a solid line.
plot(fdata$price,type = "l")


# 15.	Plot a scatterplot between variables mileage (x axis) and price (y axis).
plot(fdata$mileage,fdata$price)


# 16.	Plot a boxplot of mileage values.
boxplot(fdata$mileage)
#EXTRA: Plot a boxplot of mileage values and prices.
boxplot(fdata[,c("mileage","price")])
#EXTRA: Plot a boxplot of prices dividided by classes in transmission.
boxplot(mileage ~ transmission, data = fdata)


# 17.	Plot a histogram of the prices data. Can you change the number of bins? Can you plot the probability densities for each bar?
hist(fdata$mileage)
hist(fdata$mileage,breaks = 30)
hist(fdata$mileage,breaks = 30,freq = FALSE)
