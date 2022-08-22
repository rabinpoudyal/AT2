library("readxl")
library("tidyverse")
library(lubridate)


# READ DATA FROM DIFFERENT FILES
data_months <- read.csv("~/uts/STDS/assignment2/data/dataset/months.csv")
cash_rate_data <- read.csv("~/uts/STDS/assignment2/data/dataset/Cash rate -RBA.csv")
loan_commitments <- read.csv("~/uts/STDS/assignment2/data/dataset/New loan commitments total housing.csv")
labour_force_data <- read_excel("~/uts/STDS/assignment2/data/dataset/Labour Force Australia.xlsx")
resident_mean_price <- read_excel("~/uts/STDS/assignment2/data/dataset/Mean price of residential dwellings_Australia.xlsx")

# RETRIVE RELEVANT COLUMNS AND FORM DATA FRAME
data_months$Date <- dmy(data_months$Date)
#data_months$Date <- format(data_months$Date, "%y-%m-%d")

dataset <- data.frame("Date" = data_months$Date)
cash_rate_data$Date <- dmy(cash_rate_data$Date)
cash_rate <- filter(cash_rate_data, cash_rate_data$Date >= "2018-01-01")


# MERGE DATASETS
new_dataset <- merge(dataset, cash_rate, by="Date")