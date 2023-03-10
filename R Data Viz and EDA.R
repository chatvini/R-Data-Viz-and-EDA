#---------------------- Exploratory Data Analysis and Viz ----------------------#

# Case study - 
# A retailer does various marketing investments and campaigns
# (offline and online) to boost sales of its products along with adjusting prices
# As a marketing budgeting manager for the retailer, the client would like
# to understand 
# a.) Is price an important factor to drive sales? 
# b.) How spends and other campaigns are performing? 
# c.) Where should the budget be diverted if a particular channel is not performing

# Steps involved in solving a bp
# 1. Understand the BP
# 2. Approach/ what kind of data is req
# 3. Get the data
# 4. Data understanding>>Look at the data, dim, quality (missing/ outliers), data types...
# 5. Data manipulating >> filtering, order, adding/ removing columns, groups, duplicates, treating missing..
# 6. EDA and Viz >> Univariate and Bivariate analysis

## Install important libraries

install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)  #data manipulation
library(ggplot2)  #data visualization

setwd("/Users/macmojave/Desktop/IPBA/")


##Loading Data
mmix<-read.csv("MMix.csv",header=TRUE,stringsAsFactors=FALSE, na.strings = "")
View(mmix)

#use na.strings
 
is.na(mmix$PromoType)
mmix<-read.csv("MMix.csv",na.strings= "No Promo")

sum(is.na(mmix$PromoType))


##mmix <- MMix
##Checking Data Characteristics
#dimensions
dim(mmix)
#structure of data
str (mmix)
#top and bottom records of data
head(mmix)

tail(mmix)

# Data cleaning
#Removing USD from TV, Radio and Instore spend values
mmix$TV <- gsub("USD ", "", mmix$TV)
class(mmix$TV)
mmix$TV <- as.numeric(mmix$TV)

mmix$Radio <- gsub("USD ", "", mmix$Radio)
class(mmix$Radio)
mmix$Radio <- as.numeric(mmix$Radio)

mmix$InStore <- gsub("USD ", "", mmix$InStore)
class(mmix$InStore)
mmix$InStore <- as.numeric(mmix$InStore)
?gsub

#Dv = Sales

# #checking missing values
colSums(is.na(mmix))

# #Treating missing values

mmix$PromoType[is.na(mmix$PromoType)] <- "NO_OFFER"

# Change to factor variable
mmix$PromoType <- as.factor(mmix$PromoType)


#--------------------------------Exploratory Analysis -------------------------------------

#------ Univariate analysis----------

#summary statistics
summary(mmix$NewVolSales)

summary(mmix)
#Viz

#for continuous - hist and boxplot

#histograms - binned representation of cont. variable acc to freq.

#checking sales distribution graphically
hist(mmix$NewVolSales)

#try main, col, xlab, ylab, labels
hist(mmix$NewVolSales, xlab = "Sales", ylab = "Freq", main = "Hist." , labels = TRUE, col = "Red", border = "black" )

hist(mmix$BasePrice, xlab = "Sales", ylab = "Freq", main = "Hist." , labels = TRUE, col = "Red", border = "black" ) #normal
hist(mmix$Radio, xlab = "Sales", ylab = "Freq", main = "Hist." ,  
     labels = TRUE, col = "Red", border = "black" ) #bimodal curve
hist(mmix$TV, xlab = "Sales", ylab = "Freq", main = "Hist." , 
     labels = TRUE, col = "Red", border = "black" )
hist(mmix$InStore, xlab = "Sales", ylab = "Freq", main = "Hist." , 
     labels = TRUE, col = "Red", border = "black" )

#boxplots
boxplot(mmix$NewVolSales)


# Likewise
hist(mmix$BasePrice)
hist(mmix$InStore)
hist(mmix$Radio)
hist(mmix$TV)

## Univariate for categorical variables
#to calcu frequency

table(mmix$PromoType)/nrow(mmix)
table(mmix$Facebook)

##Bivariate analysis 

#Correlations
cor(mmix$NewVolSales,mmix$BasePrice)
cor(mmix$NewVolSales,mmix$Radio)
cor(mmix$NewVolSales,mmix$TV)
cor(mmix$NewVolSales,mmix$InStore)
?cor

#How to get correlation matrix
cor(mmix[,c(1,2,3,4,5,7,8,9)])

#corplot library to get correlation plot

## you can change the type (method) to kendall, spearman or pearson (default)

#Viz for continuous variables
qplot(mmix$BasePrice, mmix$NewVolSales)
qplot(mmix$InStore, mmix$NewVolSales)
qplot(mmix$Radio, mmix$NewVolSales)
qplot(mmix$TV, mmix$NewVolSales)

#qplot deprecated so using plot
plot(mmix$BasePrice, mmix$NewVolSales,pch = as.numeric(mmix$PromoType))
plot(mmix$InStore, mmix$NewVolSales)
plot(mmix$Radio, mmix$NewVolSales)
plot(mmix$TV, mmix$NewVolSales)

?qplot
# You can also use plot function to plot the same with type = 'p'

plot(mmix$BasePrice, mmix$NewVolSales, main = "Sales vs Price",
     xlab = 'Price', ylab = 'Sales', 
     pch = 1, type = 'p', col ='red')

#multivariate 
plot(mmix$BasePrice, mmix$NewVolSales, main = "Sales vs Price",
     xlab = 'Price', ylab = 'Sales', 
     pch = as.numeric(mmix$PromoType), type = 'p', 
     col = as.numeric(mmix$PromoType))

#Viz for categorical variables
plot(mmix$NewVolSales, mmix$Facebook)  #this does not give any good interpretation

?qplot

#Facebook campaign
boxplot(mmix$NewVolSales~ mmix$Facebook, col = "red")

#Twitter campaign
boxplot(mmix$NewVolSales~ mmix$Twitter, col = "red")

#Website campaign
boxplot(mmix$NewVolSales~ mmix$WebCamp, col = "red")

#Online campaign
boxplot(mmix$NewVolSales~ mmix$Online, col = "red")

#Inserts
boxplot(mmix$NewVolSales~ mmix$Inserts, col = "blue")

#Promotion type
boxplot(mmix$NewVolSales~ mmix$PromoType, col = "blue")

# Use of subset in boxplot to plot for a subset of data
boxplot(mmix$NewVolSales ~ mmix$Inserts, main = "Sales by PromoType",
        xlab = 'PromoType', ylab = 'Sales', col = 'red',
        subset = mmix$Year == 2018, varwidth = TRUE)
# subset	
# an optional vector specifying a subset of observations to be used 
# for plotting.
# subset => subset = mmix$Year == 2018

#varwidth	
# if varwidth is TRUE, the boxes are drawn with widths proportional 
# to the square-roots of the number of observations in the groups.

par(mfrow=c(1,3))

# Use of subset in boxplot to plot for a subset of data
boxplot(mmix$NewVolSales ~ mmix$Year, main = "Sales BOGO",
        xlab = 'Promo', ylab = 'Sales', col = 'red',
        subset = mmix$PromoType == "BOGO", varwidth = TRUE)

# Use of subset in boxplot to plot for a subset of data
boxplot(mmix$NewVolSales ~ mmix$Year, main = "Sales 10 $ OFF",
        xlab = 'Promo', ylab = 'Sales', col = 'red',
        subset = mmix$PromoType == "CPN_10$OFF", varwidth = TRUE)

# Use of subset in boxplot to plot for a subset of data
boxplot(mmix$NewVolSales ~ mmix$Year, main = "Sales when no Promotion",
        xlab = 'Promo', ylab = 'Sales', col = 'red',
        subset = mmix$PromoType == "NO_OFFER", varwidth = TRUE)

dev.off()

# Will you stop here and deliver these results to the business and say that 
# TV and Radio spends do not work at all


install.packages("zoo")
library(zoo) # for rolling averages
library(tidyverse)

# So create a variable for lagged TV spend using rollmean
mmix$TV_lag <- rollmean(mmix$TV, k = 4, fill = NA, align =  "right")

cor(mmix$NewVolSales, mmix$TV_lag, use = 'complete.obs')
?cor


mmix$Radio_lag <- rollmean(mmix$Radio, k = 14, fill = NA, align =  "right")

cor(mmix$NewVolSales, mmix$Radio_lag, use = 'complete.obs')

#qplot(mmix$Radio_lag, mmix$NewVolSales)

#qplot(mmix$TV_lag, log(mmix$NewVolSales))



## Let's continue and finish rest of the topics

# Find relationship b/w categorical variables using frequency/ counts 
# What is your interpretation here?
table(mmix$PromoType, mmix$Qtr)

a<-table(mmix$PromoType, mmix$Qtr)

prop.table(a) # overall


prop.table(a,1) # rows


prop.table(a,2) # columns

# Stacked bar charts
barplot(prop.table(a,2), 
        col=colors()[c(23,21)] , 
        border="white", 
        font.axis=2, 
        beside=F, 
        legend=rownames(data), 
        xlab="Qtr", 
        font.lab=2)


# Check this out
# https://www.r-graph-gallery.com/index.html

# Use ggplot library to plot stacked bar 
# gives (frequency plot when position = 'stack') (default)

# position arg - stack, dodge, fill
mmix %>% 
  ggplot(aes(x = factor(Qtr), fill = factor(PromoType) )) +
  geom_bar() 
?geom_bar

mmix %>% 
  ggplot(aes(x = factor(Qtr), fill = factor(PromoType) )) +
  geom_bar(position = "dodge") 
?geom_bar

# Use ggplot library to plot stacked bar 
# gives (freqeuncy plot when position = 'fill')
mmix %>% 
  ggplot(aes(x = factor(Qtr), fill = factor(PromoType) )) +
  geom_bar(position  = "fill") 

mmix %>% 
  ggplot(aes(x = factor(PromoType), fill = factor(Qtr) )) +
  geom_bar(position  = "fill") 


# Use of facet_grid
mmix %>% 
  ggplot(aes(x = factor(PromoType), fill = factor(Qtr) )) +
  geom_bar(position  = "fill") +
  facet_grid(.~Year)

# Creating Boxplots - univariate
ggplot(mmix, aes (y = NewVolSales)) +
  geom_boxplot()

ggplot(mmix, aes (y = NewVolSales)) +
  geom_boxplot() +
  facet_grid(.~Year)

# Creating Boxplots - bivariate and multivariate
ggplot(mmix, aes (x= PromoType, y = NewVolSales)) +
  geom_boxplot() +
  facet_grid(.~Year)


# Creating Boxplots - multivariate Trellis plots
ggplot(mmix, aes (x= PromoType, y = NewVolSales)) +
  geom_boxplot() +
  facet_grid(Qtr~Year)

# Scatter plots
ggplot(mmix, aes (x= BasePrice, y = NewVolSales)) +
  geom_point(aes(col = Year))


1##### --------EXTRAS-------- (FOR SELF PRACTICE AND LEARNING)----

#checking outliers
x<-boxplot(mmix$NewVolSales)
boxplot(mmix$NewVolSales)

# Check outliers for Independent Variables
#boxplot(Radio)
#boxplot(TV)
#boxplot(InStore)
#boxplot(Base.Price)

# Outlier treatment - Way 1
#To get list of outliers
list_out<- x$out
list_out
#gives the positions in the data where outliers are present
index<-which(mmix$NewVolSales %in% list_out)

#---Shortlist the outliers from the dataset and replace
mmix$NewVolSales[index]

#na.rm=TRUE --> making sure missing values are removed before calculating mean
mean_sales<-mean(mmix$NewVolSales,na.rm=TRUE)
mmix$NewVolSales[index]<-mean_sales

# Outlier treatment - way 2
qn = quantile(mmix$NewVolSales, c(0.05, 0.95), na.rm = TRUE)
# qn = quantile(mmix$NewVolSales, c(0.25, 0.75), na.rm = TRUE)

summary(mmix$NewVolSales)

y<- IQR(mmix$NewVolSales)
q1 <- quantile(mmix$NewVolSales, 0.25)
q3 <- quantile(mmix$NewVolSales, 0.75)

?within
mmix<- within(mmix, { NewVolSales1 = ifelse(NewVolSales < (q1-1.5*y), qn[1], NewVolSales)
NewVolSales1 = ifelse(NewVolSales > (q3+1.5*y), qn[2], NewVolSales)})

# Check summary statistics
summary(NewVolSales)
summary(mmix$NewVolSales1)
qn[1]
qn[2]


## Log transformations ----
# skewness reduction when log transformed
hist(log(mmix$NewVolSales))
hist(log(mmix$Base.Price))
hist(log(mmix$InStore))
hist(log(mmix$Radio))

##What is the use of log variables?
##To make a variable in scale with the other variable
qplot(mmix$InStore, log(mmix$NewVolSales))
qplot(mmix$Radio, log(mmix$NewVolSales))

qplot(log(mmix$InStore), log(mmix$NewVolSales))
qplot(log(mmix$Radio), log(mmix$NewVolSales))

##Creating New Variables

#Data Transformations
mmix$LnSales<-log(mmix$NewVolSales)
mmix$LnPrice<-log(mmix$BasePrice)
mmix$OfflineSpend<-mmix$Radio+mmix$TV+mmix$InStore

summary(mmix$OfflineSpend)
summary(mmix$NewVolSales)

qplot(mmix$NewVolSales, mmix$OfflineSpend)

# Creating price buckets
summary(mmix$BasePrice)

#Create price buckets - converting numeric variable into categorical
mmix$Price_Bkt[mmix$BasePrice < 15.03]<-"Low"
mmix$Price_Bkt[mmix$BasePrice >= 15.03 & mmix$BasePrice < 15.33]<-"Avg"
mmix$Price_Bkt[mmix$BasePrice >= 15.33 & mmix$BasePrice < 15.64]<-"High"
mmix$Price_Bkt[mmix$BasePrice >= 15.64]<-"V.High"

#Create price buckets - converting numeric variable into categorical
mmix$Price_Bkt[mmix$Base.Price < 15.03]<-"1Low"
mmix$Price_Bkt[mmix$Base.Price >= 15.03 & mmix$Base.Price < 15.64]<-"2Avg"
mmix$Price_Bkt[mmix$Base.Price >= 15.64]<-"3V.High"

mmix$Spend_Bkt[mmix$OfflineSpend < 391]<-"1LowSpend"
mmix$Spend_Bkt[mmix$OfflineSpend >= 391 & mmix$OfflineSpend < 510]<-"2AvgSpend"
mmix$Spend_Bkt[mmix$OfflineSpend >= 510]<-"3HighSpend"

mmix$Sales_Bkt[mmix$NewVolSales < 19049]<-"1LowSales"
mmix$Sales_Bkt[mmix$NewVolSales >= 19049 & mmix$NewVolSales < 20943]<-"2AvgSales"
mmix$Sales_Bkt[mmix$NewVolSales >= 20943]<-"3HighSales"

# Idea of lag impact of a promotion spend
?lag

cor(mmix$NewVolSales,mmix$Radio)
qplot(mmix$Radio, mmix$NewVolSales)

#Let us find if radio spend has a Lagged impact on sales
#Today's spend on a radio promotion might lead to sales n days down the line
cor(mmix$NewVolSales,lag(mmix$Radio,7), use = "complete.obs")

#what happens if I see correlation b/w log transformed sales and lag radio

#Create a lag variable in data
mmix$Radio_lag <- lag(mmix$Radio, 7)

qplot(mmix$Radio_lag, log(mmix$NewVolSales)) # this is still skewed radio spend
#let us do log transformation
qplot(log(mmix$Radio_lag), log(mmix$NewVolSales))

# lag of TV spend

cor(mmix$TV, mmix$NewVolSales)
qplot(mmix$TV, mmix$NewVolSales)

# TV Lag 8,11, 15
cor(lag(mmix$TV,15), mmix$NewVolSales, use = "complete.obs")

# Create a variable for lagged TV spend
mmix$TV_lag <- lag(mmix$TV, 15)

# Let us plot lagged value v/s sales (direct and log transformed)
qplot(mmix$TV_lag, log(mmix$NewVolSales))
qplot(log(mmix$TV_lag), log(mmix$NewVolSales)) # why?

hist(mmix$TV_lag)
