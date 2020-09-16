####################################################
########### Data wrangling example #################
####################################################

## Load Titanic dataset --------------------------------------------------------
# read.table is a function that allows to read multiple kinds of text files. It admits the following arguments:
Titanic <- read.table(file = "Titanic.csv", #Name of text file.
           sep = ",",                       #Separation character.
           header = TRUE,                   #If column names are in the first row.
           na.strings = "NA",               #Character to be marked as missing value.
           stringsAsFactors = FALSE)         #¿convert string to factors?
#See structure 
str(Titanic)
#Print first few rows
head(Titanic) 

## FACTORS AND CONVERSIONS --------------------------------------------------------
## Identify each type of variable in the dataset. Make the necessary conversions.

# summarize the data read to explore range of values and missing data.
summary(Titanic)
#"Name" is character, but should not be considered as categorical data.
#"PClass" and "Sex" are character variables, but they only contain a small number of classes
unique(Titanic$PClass) #unique function returns a vector with duplicate elements removed  
table(Titanic$Sex) #table counts unique elements.
#Therefore, they should be considered as categorical data.
Titanic$PClass <- as.factor(Titanic$PClass)
Titanic$Sex <- as.factor(Titanic$Sex)
str(Titanic) #Labels are created
summary(Titanic) #Summary gives the number of cases
#Contingency table
table(Titanic$PClass, Titanic$Sex)
#"Survived" and "SexCode" are numerical variables, but they only contain a small number of classes
table(Titanic$Survived)
table(Titanic$SexCode)
#Therefore, they should be considered as categorical data.
Titanic$Survived <- as.factor(Titanic$Survived)
str(Titanic)
#some functions do not admit names of factors starting with numbers. We can change the factor names:
levels(Titanic$Survived) <- c("NO","YES")
str(Titanic)
#Another useful function when having a variable with 0 and 1 is ifelse()
Titanic$SexCode <- as.factor(ifelse(Titanic$SexCode, "F","M")) #caret does not handle class level names as numbers
str(Titanic)

## MISSING VALUES --------------------------------------------------------
## Identify missing values and eliminate those observations from the dataset.
# The summary function provides the number of missing values (if any) found for each variable.
summary(Titanic)
# Define a new dataset only with correctly observed data
Titanic_noNA <- Titanic[!is.na(Titanic$Age),]
# In addition, the function na.omit() directly obtains the dataset without NA in all variables.
Titanic_noNA <- na.omit(Titanic)
summary(Titanic_noNA)


## BASIC DATA MANIPULATION --------------------------------------------------------
## Create two datasets, one for each passenger Sex 
## Then, join again the two datasets only with Age, Sex and Survived variables. 

#We can create an index variable that has 1s when the sex is female
#The %in% command is equivalent to using the function match()
index_Sex <- Titanic$Sex %in% "female"
#Create the two datasets
Titanic_F <- Titanic[index_Sex,]
Titanic_M <- Titanic[!index_Sex,]
#Join the two datasets only with age, sex and survived. We can use the rbind function
Titanic_new <- rbind(Titanic_F[,c("Age","Sex","Survived")],Titanic_M[,c("Age","Sex","Survived")])


