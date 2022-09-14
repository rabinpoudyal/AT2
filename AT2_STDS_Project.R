#install packages: run once

#install.packages('readxl')
#install.packages('tidyverse')
#install.packages("lubridate")
#install.packages("imputeTS")
#install.packages("corrplot")
#install.packages("datasets")
#install.packages("DGLMExtPois")
#install.packages("bbmle")

library("readxl")
library("tidyverse")
library("lubridate")
library("imputeTS")
library("corrplot")
library("readr")
library("dplyr")
library("ggplot2")
library("cowplot")

#load(ggplot2)
#require(ggplot)
require(ggplot2)

library(tidyverse)
library(magrittr)
library(GGally)
library(ggpubr)
library(datasets)
library(COMPoissonReg)
library(DGLMExtPois)
library(bbmle)



# set working directory.
path = "C:/github_project/"
setwd(path) 
getwd()

# Read data from different files
data_months <- read.csv(
  "AT2/data/dataset/months.csv")
cash_rate_data <- read.csv(
  "AT2/data/dataset/Cash rate -RBA.csv")
loan_commitments <- read.csv(
  "AT2/data/dataset/New loan commitments total housing.csv")
labour_force_data <- read_excel(
  "AT2/data/dataset/Labour Force Australia.xlsx", 
  sheet = "Data1")
dwellings_mean_price <- read_excel(
  "AT2/data/dataset/Mean price of residential dwellings_Australia.xlsx", 
  sheet = "Data1")
residential_dwellings_number <- read_excel(
  "AT2/data/dataset/Mean price of residential dwellings_Australia.xlsx", 
  sheet = "Data1")
earnings <- read_excel(
  "AT2/data/dataset/Average Weekly Earnings.xlsx", 
  sheet = "Data1")
property_price_index <- read_excel(
  "AT2/data/dataset/Residential Property Price Index.xlsx", 
  sheet = "Data1")

# Retrieve relevant columns
loan_commitments <- loan_commitments[,c(1,2)]
labour_force_data <- labour_force_data[,c(1,67)]
dwellings_mean_price <- dwellings_mean_price[,c(1,37)]
residential_dwellings_number <- residential_dwellings_number[,c(1,46)]
earnings <- earnings[,c(1,9)]
property_price_index <- property_price_index[,c(1,10)]


#rename columns
colnames(loan_commitments)
loan_commitments <- rename(
  loan_commitments, Date = "X",
  Total_loans = "Total..ex..refinancing....b.")

colnames(labour_force_data)
labour_force_data <- rename(
  labour_force_data, Date = "...1", 
  Unemployment_rate= "Unemployment rate ;  Persons ;...67")

colnames(dwellings_mean_price)
dwellings_mean_price <- rename(
  dwellings_mean_price, Date = "...1", 
  Mean_price = "Mean price of residential dwellings ;  Australia ;")

colnames(residential_dwellings_number)
residential_dwellings_number <- rename(
  residential_dwellings_number, Date = "...1", 
  dwellings_number = "Number of residential dwellings ;  Australia ;")

colnames(earnings)
earnings <- rename(
  earnings, Date = "...1", 
  total_earnings = "Earnings; Persons; Full Time; Adult; Total earnings ;")

colnames(property_price_index)
property_price_index <- rename(
  property_price_index, Date = "...1", 
  price_index = 
    "Residential Property Price Index ;  Weighted average of eight capital cities ;")

#change date format
data_months$Date <- dmy(data_months$Date)
cash_rate_data$Date <- dmy(cash_rate_data$Date)
loan_commitments$Date <- dmy(loan_commitments$Date)


# Merge dataframes
merge_dataset <- left_join(data_months, cash_rate_data, by='Date') %>%
  left_join(., labour_force_data, by='Date') %>%
  left_join(., dwellings_mean_price, by='Date') %>%
  left_join(., residential_dwellings_number, by='Date') %>%
  left_join(., earnings, by='Date') %>%
  left_join(., property_price_index, by='Date') %>%
  left_join(., loan_commitments, by='Date')


#dataset skipping missing values
missing_val_dataset <- merge_dataset
missing_val_dataset$Cash <- na_ma(missing_val_dataset$Cash, weighting = "simple", k = 2)
missing_val_dataset <- missing_val_dataset %>% fill(total_earnings)
missing_val_dataset <- subset(missing_val_dataset, select=-c(price_index))
missing_val_dataset <- missing_val_dataset %>% drop_na()
missing_val_dataset <- filter(missing_val_dataset, Date >= "2012-01-01")

# outliers analysis
summary(missing_val_dataset) #dataset skipping missing values



##################################### filling missing values

# outliers analysis
summary(merge_dataset) #dataset filling missing values

#check missing values count
colSums(is.na(merge_dataset))

#Perform cash imputation with 3 simple moving average 
merge_dataset$Cash <- na_ma(merge_dataset$Cash, weighting = "simple", k = 2)

#replacing missing data from top to bottom
merge_dataset <- merge_dataset %>% fill(Mean_price)
merge_dataset <- merge_dataset %>% fill(dwellings_number)
merge_dataset <- merge_dataset %>% fill(total_earnings)
merge_dataset <- merge_dataset %>% fill(price_index)


#using data from 2012
merge_dataset <- filter(merge_dataset, Date >= "2012-01-01")

################################ dataset we are going to use

#saving the data // chose one of the datasets to the EDA and the model
clean_dataset <- merge_dataset
#clean_dataset <- missing_val_dataset


################################## EDA

#check the distribution of the cash data
ggplot(clean_dataset) +
  aes(x = Cash) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

#correlations of the variables
corr_matrix = cor(clean_dataset[, c(2:8)])
corrplot(corr_matrix, method = 'number', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)

#total loan by date with cash rate as colour 
clean_dataset %>%
  ggplot(aes(Date, Total_loans, color = Cash)) + 
  geom_point(size = 2, alpha = 2) +
  xlab("Date") +
  ylab("Total loans")

#4 plots in a grid
q1 <- ggplot(clean_dataset, aes(y=Total_loans, Date)) + geom_line()
q2 <- ggplot(clean_dataset, aes(y=Cash, Date)) + geom_line()
q3 <- ggplot(clean_dataset, aes(y=Unemployment_rate, Date)) + geom_line()
q4 <- ggplot(clean_dataset, aes(y=total_earnings, Date)) + geom_line()

plot_grid(q1, q2, q3, q4)


# ggplot - relationship with total loans

ggplot(clean_dataset, aes(x=Mean_price, y=Total_loans)) + geom_point() + labs(title="Mean price vs total loans", x="Mean price", y="Total loans") + theme_classic()

ggplot(clean_dataset, aes(x=Mean_price, y=dwellings_number)) + geom_point() + labs(title="Mean price vs total loans", x="Mean price of dwellings", y="Total loans") + theme_classic()

ggplot(clean_dataset, aes(x=dwellings_number, y=Total_loans)) + geom_point() + labs(title="Total Loans vs dwellings number", x="Number of Dwellings", y="Total Loans") + theme_classic()

ggplot(clean_dataset, aes(x=price_index, y=Total_loans)) + geom_point() + labs(title="Total Loans vs price index", x="Price Index", y="Number of Total Loans") + theme_classic()

############ ggplot - relationship for hypothesis test

#Relationship between Cash rate and Total loans
clean_dataset %>%
  ggplot(aes(Cash, Total_loans )) + geom_point(mapping = aes(x =Cash , y = Total_loans)) +geom_smooth(color ="Red") +labs(title = "Decreasing Cash Rate resulting in increased \nnumber of loans from 2012-2022 ")

#Relationship between total earnings and Total loans
clean_dataset %>%
  ggplot(aes(total_earnings, Total_loans )) + geom_point(mapping = aes(x =total_earnings , y = Total_loans, color = Date)) +geom_smooth(color ="Pink") +labs(title = "Increasing Total Earnings results in increase \nin number of loans from 2012-2022 ")


#Relationship between cash rate and mean price
clean_dataset %>%
  ggplot(aes(Cash, Mean_price)) +
  geom_point(size = 2, alpha = 2) + labs(title = 'Relationship of Cash rate and Mean Price', subtitle = 'November 2011 - June 2022') +
  xlab("Cash rate (months)") +
  ylab("Mean price")

#Relationship between total earnings and dwelling numbers
clean_dataset %>%
  ggplot(aes(total_earnings, dwellings_number)) +
  geom_point(size = 2, alpha = 2) + labs(title = 'Relationship of Total earnings and Number of Residential Dwellings', subtitle = 'November 2011 - June 2022') +
  xlab("Total earnings (months)") +
  ylab("Dwellings number")

#Relationship between total earnings and mean price
clean_dataset %>%
  ggplot(aes(total_earnings, Mean_price)) +
  geom_point(size = 2, alpha = 2) + labs(title = 'Relationship of Total earnings and Mean price of residential dwellings', subtitle = 'November 2011 - June 2022') +
  xlab("Total earnings (months)") +
  ylab("Mean Price")

################################## Models

##save data to perform the model
data <- clean_dataset

# correlation - distributions
ggpairs(data)

#view columns names
colnames(data)

#formulas approaches for the models
formula_mod1 = Total_loans ~ Mean_price + price_index + Cash + dwellings_number + Unemployment_rate + total_earnings
formula_mod2 = Total_loans ~ Mean_price + Cash + dwellings_number + Unemployment_rate + total_earnings
formula_mod3 = Total_loans ~ Mean_price + Cash + dwellings_number
formula_mod4 = Total_loans ~ Cash
formula_mod5 = Total_loans ~ dwellings_number
formula_mod6 = Total_loans ~ Mean_price
formula_mod7 = Total_loans ~ Cash + dwellings_number

############ poisson model

pois_mod = glm(
  formula = formula_mod2,
  family = poisson, 
  data = data)

summary(pois_mod)

#estimated variance against the mean
plot(log(fitted(pois_mod)),log((data$Total_loans-fitted(pois_mod))^2),
     xlab=expression(hat(mu)),
     ylab=expression((y-hat(mu))^2),
     pch=20,col="blue")
abline(0,1) ## 'variance = mean' line

#Dispersion parameter 
dp = sum(residuals(pois_mod,type ="pearson")^2)/pois_mod$df.residual
dp #under-dispersion

# other way to extract the dispersion parameter manually - declare the function:
dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}


#check how much the coefficient estimations are affected by
summary(pois_mod,dispersion = dp) 


########### quasipoisson model

#Allow Dispersion Estimation
qpoi_mod = glm(formula = formula_mod2,
               family=quasipoisson, data)
summary(qpoi_mod) 
#for formula_mod2: 3 of the predictors’ coefficients are significant
#for formula_mod2: dispersion parameter is estimated to be 0.1051395


#We can also use the residual deviance to test whether the null hypothesis is true 
#(model provides an adequate fit for the data)
#p-value = 1 - pchisq(deviance, degrees of freedom)
pvalue = 1 - pchisq(12.648, 120)
pvalue #p-value of 1 showing that there is a significant evidence to support the null hypothesis.


#check dispersion
(summary(qpoi_mod)$dispersion)

#checking AIC quasi family
(qAIC(qpoi_mod, dispersion= dfun(qpoi_mod),nobs=length(data$Total_loans)))


######### COM-Poisson model

### formula.nu = Total_loans ~ 1 ????? not sure 

com_model = glm.CMP(formula.mu = formula_mod2,
                    formula.nu = Total_loans ~ 1,
                    data=data)
#Summary of the model
summary(com_model) 

print(com_model)
coef(com_model)

# To see termination condition of the optimization process
com_model$nloptr$message

# To see number of iterations of the optimization process
com_model$nloptr$iterations



#################### double Poisson Model


#################### Gamma-count Model




################### references

#http://cursos.leg.ufpr.br/rmcd/applications.html - 
#https://stats.stackexchange.com/questions/108995/interpreting-residual-and-null-deviance-in-glm-r  - interpret the Null and Residual Deviance
#https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf  -  quasi AIC
# https://cran.r-project.org/web/packages/DGLMExtPois/DGLMExtPois.pdf  - com-poisson
# https://www.rdocumentation.org/packages/DGLMExtPois/versions/0.2.0/topics/glm.CMP  - com-poisson


