#installing packages
install.packages("tidyverse")
install.packages("ggvis")
#importing libraries
library('tidyverse')
library('ggplot2')
library('ggvis')
#importing CSV file.
setwd("C:\\Users\\HP\\Desktop\\r project\\archive (1)")
bike_buyers <- read.csv('bike_buyers.csv', header=TRUE,na.strings='')
view(bike_buyers)
#This dataset has details of 1000 users from different backgrounds and whether or not they buy a bike.

#top 6 rows.
head(bike_buyers)
#bottom 6 rows
tail(bike_buyers)

#structure of a data.
class(bike_buyers)
str(bike_buyers)
#summary about data
summary(bike_buyers)
#checking null values in data.
colSums(is.na(bike_buyers))
##in data there are several column which contains the null values.


#Viewing distribution in attributes with NA values.
hist(bike_buyers$Income)
#the distribution is left skewed.

hist(bike_buyers$Children, breaks = 20)

hist(bike_buyers$Cars, breaks = 15,col="green")
hist(bike_buyers$Age)
#the distribution is left skewed.

#the distribution of Income and Age is left-skewed. We will impute median values
median(na.omit((bike_buyers$Income)))
median(na.omit((bike_buyers$Age)))


bike_buyers_clean <- bike_buyers
colSums(is.na(bike_buyers_clean))

bike_buyers_clean$Income[is.na(bike_buyers_clean$Income)]<-median(na.omit((bike_buyers$Income)))

bike_buyers_clean$Age[is.na(bike_buyers_clean$Age)]<-median(na.omit((bike_buyers$Age)))
colSums(is.na(bike_buyers_clean))
#imputing with mean values.
bike_buyers_clean$Cars[is.na(bike_buyers_clean$Cars)]<-mean(bike_buyers$Cars, na.rm = TRUE)
  
bike_buyers_clean$Children[is.na(bike_buyers_clean$Children)]<-mean(bike_buyers$Children, na.rm = TRUE)

get_mode <- function(x) {                 
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

# Marital Status replaced with Mode
bike_buyers_clean$Marital.Status[is.na(bike_buyers_clean$Marital.Status)]<-get_mode(bike_buyers$Marital.Status)

# Gender replaced with Mode
bike_buyers_clean$Gender[is.na(bike_buyers_clean$Gender)]<-get_mode(bike_buyers$Gender)

# Children replaced with Mode
bike_buyers_clean$Children[is.na(bike_buyers_clean$Children)] <-get_mode(bike_buyers$Children)

# Home Owner replaced with Mode
bike_buyers_clean$Home.Owner[is.na(bike_buyers_clean$Home.Owner)] <-get_mode(bike_buyers$Home.Owner)

colSums(is.na(bike_buyers_clean))
#hence all the null values is been treated.

#Save clean dataframe.
write.csv(bike_buyers_clean,"bike_buyers_clean.csv", quote = FALSE, row.names = TRUE)
bike_buyers <- bike_buyers_clean
#comparisom categorical attributes using bar plot.
counts <- table(bike_buyers$Cars, bike_buyers$Gender)
counts
barplot(counts, main = '',xlab="gender",ylab = "count of cars")
##males have comparatively more cars than female but diffrence is slighter.


#two numeric columns using plot().
plot(bike_buyers$Age,bike_buyers$Income,type= "p")
##income between 40 to 60 age group is better than any others.
##income ratio is very low before 60s age group.

plot(bike_buyers$Age,bike_buyers$Children,type= "p")


hist(bike_buyers$Age,xlab = "age",col = "blue",border = "red", xlim = c(0,100), ylim = c(0,5),breaks = 5)
##Exploring ggplot library.
#histogram
ggplot(bike_buyers, aes(x = Age)) +geom_histogram()
#plot
ggplot(bike_buyers,aes(y = Age, x = Gender)) +geom_point()

p5 <- ggplot(bike_buyers, aes(x = Age, y = Occupation))
p5 + geom_line(aes(color = Age))

boxplot(bike_buyers$Income, main = 'Income Boxplot')
#Outlier Treatment
OutVals = boxplot(bike_buyers$Income)$out
print(OutVals)
which(bike_buyers$Income %in% OutVals)
x = bike_buyers$Income [!(bike_buyers$Income %in% OutVals) ]
boxplot(x)


boxplot(bike_buyers$Age, main = 'age Boxplot')
#Outlier Treatment
OutVals = boxplot(bike_buyers$Age)$out
print(OutVals)
which(bike_buyers$Age %in% OutVals)
x = bike_buyers$Age [!(bike_buyers$Age %in% OutVals) ]
boxplot(x)

boxplot(bike_buyers$Children, main = 'children Boxplot')
dim(bike_buyers)




