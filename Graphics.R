rm(list=ls(all=TRUE)) #clean environment

library("readxl")
library("dplyr")
library("ggplot2")
library("xlsx")
if(!require(ggthemes)){install.packages('ggthemes');library(ggthemes)}

getwd()
setwd("...")



### HISTOGRAMS

library(car)
head(Prestige)
hist(Prestige$income)
hist(Prestige$income, col="green")
with(Prestige, hist(income)) # Histogram of income with a nicer title.

with(Prestige, hist(income, breaks="FD", col="green"))
box()

hist(Prestige$income, breaks="FD")

# Conditional histograms (BREAKS)
par(mfrow=c(1, 2))
hist(mydata$SAT[mydata$Gender=="Female"], breaks="FD", main="Female", xlab="SAT",col="green")
hist(mydata$SAT[mydata$Gender=="Male"], breaks="FD", main="Male", xlab="SAT", col="green")
# Braces indicate a compound command allowing several commands with 'with' command
par(mfrow=c(1, 1))
with(Prestige, {
  hist(income, breaks="FD", freq=FALSE, col="green")
  lines(density(income), lwd=2)
  lines(density(income, adjust=0.5),lwd=1)
  rug(income)
})

# Histograms overlaid
hist(mydata$SAT, breaks="FD", col="green")
hist(mydata$SAT[mydata$Gender=="Male"], breaks="FD", col="gray", add=TRUE)
legend("topright", c("Female","Male"), fill=c("green","gray"))
# Check
satgender <- table(mydata$SAT,mydata$Gender)
satgender

##Histogram with normal curve overlay
x <- rnorm(100)
hist(x, freq=F)
curve(dnorm(x), add(T)
      h <- hist(x, plot=F)
      ylim <- range(0. h$density, dnorm(0))
      hist(x, freq=F, ylim=ylim)
      curve(dnorm(x), add=T)
      
##Scatterplots
#Useful to 1) study the mean and variance functions in the regression of y on x p.128;
#2)to identify outliers and leverage points.
      
# plot(x,y)
plot(mydata$SAT) # Index plot
plot(mydata$Age, mydata$SAT)
plot(mydata$Age, mydata$SAT, main="Age/SAT", xlab="Age", ylab="SAT", col="red")
abline(lm(mydata$SAT~mydata$Age), col="blue")

# regression line (y~x)
lines(lowess(mydata$Age, mydata$SAT), col="green") # lowess line (x,y)
identify(mydata$Age, mydata$SAT, row.names(mydata))
# On row.names to identify. "All data frames have a row names attribute, 
#a character vector of length
#the number of rows with no duplicates nor missing values." 
# "Use attr(x, "row.names") if you need an integer value.)"
library/base/html/row.names.html
mydata$Names <- paste(mydata$Last, mydata$First)
row.names(mydata) <- mydata$Names
plot(mydata$SAT, mydata$Age)
identify(mydata$SAT, mydata$Age, row.names(mydata))

# Rule on span for lowess, big sample smaller (~0.3), small sample bigger (~0.7)
library(car)
scatterplot(SAT~Age, data=mydata)
scatterplot(SAT~Age, id.method="identify", data=mydata)
scatterplot(SAT~Age, id.method="identify", boxplots= FALSE, data=mydata)
scatterplot(prestige~income, span=0.6, lwd=3, id.n=4, data=Prestige)
# By groups
scatterplot(SAT~Age|Gender, data=mydata)
scatterplot(SAT~Age|Gender, id.method="identify", data=mydata)
scatterplot(prestige~income|type, boxplots=FALSE, span=0.75, data=Prestige)
scatterplot(prestige~income|type, boxplots=FALSE, span=0.75, col=gray(c(0,0.5,0.7)), data=Prestige)
# For multiple variables
scatterplotMatrix(~ prestige + income + education + women, span=0.7, id.n=0, data=Prestige)
pairs(Prestige) # Pariwise plots. Scatterplots of all variables in the dataset
pairs(Prestige, gap=0, cex.labels=0.9) # gap controls
#3D Scatterplot (requires package car)
library(car)
scatter3d(prestige ~ income + education, id.n=3, data=Duncan)
#Scatterplot (for categorical data)
plot(vocabulary ~ education, data=Vocab)
plot(jitter(vocabulary) ~ jitter(education), data=Vocab)
plot(jitter(vocabulary, factor=2) ~ jitter(education, factor=2), data=Vocab)
# cex makes the point half the size, p. 134
plot(jitter(vocabulary, factor=2) ~ jitter(education, factor=2), col="gray", cex=0.5, data=Vocab)
with(Vocab, {
  abline(lm(vocabulary ~ education), lwd=3, lty="dashed")
  lines(lowess(education, vocabulary, f=0.2), lwd=3)
})
