
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
#load data
options(scipen=999)
data<-read_excel("card_transactions.xlsx")

# Using only records with Transtype=P
data=data%>%
  filter(Transtype=="P") #total record number changed into 96353

# Removing Outliers (The record with the highest amount (reason: used wrong currency type))
data%>%
  select(Recordnum,Amount)%>%
  arrange(-Amount) #Recordnum=52594 is the outlier

data=data[data$Recordnum!=52594,] #total record number changeed into 96352

# Filling "blank" into MerchantState and MerchantZip
colnames(data)[c(5,6,7)]=c('MerchantDes','MerchantState','MerchantZip')
data$MerchantState[is.na(data$MerchantState)]='Blank'
data$MerchantZip[is.na(data$MerchantZip)]='Blank'

# Filling serial number into Merchantnum
length(data$Merchantnum[is.na(data$Merchantnum)]) #total number of NAs in Merchantnum is 3198
data$Merchantnum[is.na(data$Merchantnum)]=seq(1,3198,1)

# write.csv(data,"cleaned_data.csv")

