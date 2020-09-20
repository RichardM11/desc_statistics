rm(list=ls(all=TRUE)) #clean environment

library() #install needed packages

#Checking folder
getwd()
setwd("C:/Users..")

## Entering data from CSV
mydata <- read.table("file_name", header=TRUE, sep="\t")

mydata <- read.csv("c:\mydata\mydatafile.csv", header=TRUE)

## Entering data from txt
mydata <- read.table("C:/myfolder/abc.txt",header=TRUE, sep="\t", na.strings = "-9")

## Entering data from excel
mydata <- read_excel("mydata.xlsx", sheet=1)  #it changes if it is xls

## Entering data from R
load("mydata.RData")
load("mydata.rda")

## EXPLORING DATA
summary(mydata) # Provides basic descriptive statistics and frequencies.
edit(mydata) # Open data editor
str(mydata) # Provides the structure of the dataset
names(mydata) # Lists variables in the dataset
head(mydata) # First 6 rows of dataset
head(mydata, n=10)# First 10 rows of dataset
head(mydata, n= -10) # All rows but the last 10
tail(mydata) # Last 6 rows
tail(mydata, n=10) # Last 10 rows
tail(mydata, n= -10) # All rows but the first 10
mydata[1:10, ] # First 10 rows
mydata[1:10,1:3] # First 10 rows of data of the first 3 variables
search() # Shows the loaded packages
library() # Shows the installed packages

## Treating Missing Data

rowSums(is.na(mydata)) # Number of missing per row
colSums(is.na(mydata)) # Number of missing per column/variable
rowMeans(is.na(mydata))*length(mydata) # No. of missing per row (another way)
# length = num. of variables/elements in an object

# Convert to missing data
mydata[mydata$age=="& ","age"] <- NA # NOTE: Notice hidden spaces.
mydata[mydata$age==999,"age"] <- NA

# The function na.omit() returns the object with listwise deletion of missing values.
m <- as.matrix(DF)
na.omit(m)

# Creating a new dataset without missing data
mydata1 <- na.omit(mydata)

#Replacing a value
mydata1[mydata1$SAT==1787,"SAT"] <- 1800
mydata1[mydata1$Country=="Bulgaria","Country"] <- "US"

#Rename values from a variable
mydata$variable <- revalue(mydata$variable, c("1"="xxx", "2"="yyy")) #require plyr package

# Use factor() for nominal data
mydata$sex <- factor(mydata$sex, levels = c(1,2), labels = c("male", "female"))
# Use ordered() for ordinal data
mydata$var2 <- ordered(mydata$var2, levels = c(1,2,3,4), labels = c("Strongly agree", "Somewhat
agree", "Somewhat disagree", "Strongly disagree"))

#############Reordering Labels
levels(mydata$Major)
# Syntax for reorder(categorical variable, numeric variable, desired statistic)
mydata$Major = with(mydata, reorder(Major,Read,mean)) # Order goes from low to high
levels(mydata$Major)
attr(mydata$Major, 'scores') # Reorder creates an attribute called 'scores' (with the statistic
# used to reorder the labels, in this case the mean values.

##Subsetting variables
mydata2 <- mydata[,1:14] # Selecting the first 14 variables
mydata2 <- mydata[c(1:14)]

##Subsetting observations
mydata2 <- mydata2[1:30,] # Selecting the first 30 observations
mydata3a <- mydata[which(mydata$Gender=='Female' & mydata$SAT > 1800),]


##Subsetting using subset command
mydata3 <- subset(mydata2, Age >= 20 & Age <= 30)
mydata4 <- subset(mydata2, Age >= 20 & Age <= 30, select=c(ID, First, Last, Age))
mydata5 <- subset(mydata2, Gender=="Female" & Status=="Graduate" & Age >= 30)
mydata6 <- subset(mydata2, Gender=="Female" & Status=="Graduate" & Age == 30)



##### Frequency Analysis
table(mydata$variable) #show absolute values
prop.table(mydata$variable) #show proportional values
addmargins(table(mydata$variable)) # Adding row/col margins

# Two-way tables
readgender <- table(mydata$Read,mydata$Gender)
readgender
prop.table(readgender,1) # Row proportions
prop.table(readgender,2) # Column proportions
round(prop.table(readgender,1), 2) # Round col prop to 2 digits
round(100*prop.table(readgender,1), 2) # Round col prop to 2 digits (percents)

chisq.test(readgender) # Do chisq test Ho: no relathionship
fisher.test(readgender) # Do fisher'exact test Ho: no relationship

# 3-way crosstabs
table3 <- xtabs(~Read+Major+Gender, data=mydata)
table3
ftable(table3a)

##PAREI NA PAG28