# install the package

#install.packages('readxl')
#install.packages('tidyverse')
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("imputeTS")
#install.packages("corrplot")


#load libraries
library("readxl")
library("tidyverse")
library("lubridate")
#library("dplyr")
library("imputeTS")
#library("ggplot2")
library("corrplot")


# set YOUR current R working directory to the specific path.
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path) 
#will print out the current directory.
getwd()  

  
# Read data from different files
data_months <- read.csv("data/dataset/months.csv")
cash_rate_data <- read.csv("data/dataset/Cash rate -RBA.csv")
loan_commitments <- read.csv("data/dataset/New loan commitments total housing.csv")
labour_force_data <- read_excel("data/dataset/Labour Force Australia.xlsx", sheet = "Data1")
dwellings_mean_price <- read_excel("data/dataset/Mean price of residential dwellings_Australia.xlsx", sheet = "Data1")
residential_dwellings_number <- read_excel("data/dataset/Mean price of residential dwellings_Australia.xlsx", sheet = "Data1")
earnings <- read_excel("data/dataset/Average Weekly Earnings.xlsx", sheet = "Data1")
property_price_index <- read_excel("data/dataset/Residential Property Price Index.xlsx", sheet = "Data1")

#view the object type
class(loan_commitments)
class(labour_force_data)

# Retrieve relevant columns
loan_commitments <- loan_commitments[,c(1,2)]
labour_force_data <- labour_force_data[,c(1,67)]
dwellings_mean_price <- dwellings_mean_price[,c(1,37)]
residential_dwellings_number <- residential_dwellings_number[,c(1,46)]
earnings <- earnings[,c(1,9)]
property_price_index <- property_price_index[,c(1,10)]

#rename columns
colnames(loan_commitments)
loan_commitments <- rename(loan_commitments, Date = "X",Total_loans = "Total..ex..refinancing....b.")
colnames(labour_force_data)
labour_force_data <- rename(labour_force_data, Date = "...1", Unemployment_rate= "Unemployment rate ;  Persons ;...67")
colnames(dwellings_mean_price)
dwellings_mean_price <- rename(dwellings_mean_price, Date = "...1", Mean_price = "Mean price of residential dwellings ;  Australia ;")
colnames(residential_dwellings_number)
residential_dwellings_number <- rename(residential_dwellings_number, Date = "...1", dwellings_number = "Number of residential dwellings ;  Australia ;")
colnames(earnings)
earnings <- rename(earnings, Date = "...1", total_earnings = "Earnings; Persons; Full Time; Adult; Total earnings ;")
colnames(property_price_index)
property_price_index <- rename(property_price_index, Date = "...1", price_index = "Residential Property Price Index ;  Weighted average of eight capital cities ;")

#Skip details rows
#labour_force_data <- labour_force_data[-c(1:9),]
#dwellings_mean_price <- dwellings_mean_price[-c(1:9),]

#change date format
str(data_months)
data_months$Date <- dmy(data_months$Date)
cash_rate_data$Date <- dmy(cash_rate_data$Date)
loan_commitments$Date <- dmy(loan_commitments$Date)

#labour_force_data$Date <- dmy(labour_force_data$Date)
#dwellings_mean_price$Date <- dmy(dwellings_mean_price$Date)
#residential_dwellings_number$Date <- dmy(residential_dwellings_number$Date)
#data_months$Date <- format(data_months$Date, "%y-%m-%d")
#earnings$Date <- dmy(earnings$Date)
#property_price_index$Date <- dmy(property_price_index$Date)


#create dataset
# Merge dataframes with dplyr
merge_dataset <- left_join(data_months, cash_rate_data, by='Date') %>%
                    left_join(., labour_force_data, by='Date') %>%
                    left_join(., dwellings_mean_price, by='Date') %>%
                    left_join(., residential_dwellings_number, by='Date') %>%
                    left_join(., earnings, by='Date') %>%
                    left_join(., property_price_index, by='Date') %>%
                    left_join(., loan_commitments, by='Date')

## UNDERSTANDING THE DATA

# The following graph shows loan commitments has dropped from mid 2017 and the pattern

ggplot(merge_dataset) +
  geom_line(aes(x=Date, y=Total_loans)) 

ggplot(merge_dataset) +
  geom_line(aes(x=Date, y=Cash)) 

ggplot(merge_dataset) +
  geom_line(aes(x=Date, y=Unemployment_rate)) 