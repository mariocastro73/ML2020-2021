Titanic <- read.csv("https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/Titanic.csv")
Titanic$Survived <- as.factor(ifelse(Titanic$Survived=="0","No","Yes"))
Titanic$Sex <- as.factor(Titanic$Sex)
Titanic$PClass <- as.factor(Titanic$PClass)
Titanic <- Titanic[,c("Survived","PClass","Sex","Age")]
str(Titanic)

with(Titanic,{
  print(table(Sex))
  print(table(PClass))
  print(table(Survived))
}
)

# sample
x <- 1:10
sample(x,10) # Permutation
sample(x,4) # Permutation
sample(x,11) # Permutation (error)
sample(x,100,replace = TRUE) # Permutation with replacement

# R base
table(Titanic$Survived) 
Yes <- which(Titanic$Survived=="Yes") # List of all the entries with Survived=Yes
No <- which(Titanic$Survived=="No")# List of all the entries with Survived=No
length(Yes)
length(No)
down <- c(sample(No,length(Yes)),Yes) # "Paste" the indices
length(down)
Titanic.down <- Titanic[down,] # Create new dataframe with the indices in "down"
str(Titanic.down)
table(Titanic.down$Survived)
up <- c(sample(Yes,length(No),replace=TRUE),No)
length(up)
Titanic.up <- Titanic[up,]
str(Titanic.up)
table(Titanic.up$Survived)

# library(caret)
Titanic.down2 <- downSample(Titanic[,-1],Titanic$Survived,yname="Survived")
str(Titanic.down2)
table(Titanic.down2$Survived)
Titanic.up2 <- upSample(Titanic[,-1],Titanic$Survived,yname="Survived")
str(Titanic.up2)
table(Titanic.up2$Survived)
