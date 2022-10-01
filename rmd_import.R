library("readxl")
library("tidyverse")
library("lubridate")
library("imputeTS")
library("corrplot")
library("readr")
library("dplyr")
library("ggplot2")
library("cowplot")

require(ggplot2)

library(tidyverse)
library(magrittr)
library(GGally)
library(ggpubr)
library(datasets)
library(COMPoissonReg)
library(DGLMExtPois)
library(bbmle)

library(car)


# Read data from different files
data_months <- read.csv(
  "data/dataset/months.csv")
cash_rate_data <- read.csv(
  "data/dataset/Cash rate -RBA.csv")
loan_commitments <- read.csv(
  "data/dataset/New loan commitments total housing.csv")
labour_force_data <- read_excel(
  "data/dataset/Labour Force Australia.xlsx", 
  sheet = "Data1")
dwellings_mean_price <- read_excel(
  "data/dataset/Mean price of residential dwellings_Australia.xlsx", 
  sheet = "Data1")
residential_dwellings_number <- read_excel(
  "data/dataset/Mean price of residential dwellings_Australia.xlsx", 
  sheet = "Data1")
earnings <- read_excel(
  "data/dataset/Average Weekly Earnings.xlsx", 
  sheet = "Data1")
property_price_index <- read_excel(
  "data/dataset/Residential Property Price Index.xlsx", 
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

#saving the data // choose one of the datasets to the EDA and the model
clean_dataset <- merge_dataset
#clean_dataset <- missing_val_dataset


################################## EDA



#correlations of the variables
corr_matrix = cor(clean_dataset[, c(2:8)])



##save data to perform the model
data <- clean_dataset


#formulas - approaches for the models
formula1 = Total_loans ~ Mean_price + price_index + Cash + dwellings_number + Unemployment_rate + total_earnings
formula2 = Total_loans ~ Mean_price + Cash + dwellings_number + Unemployment_rate + total_earnings #full
formula3 = Total_loans ~ Cash + Unemployment_rate + total_earnings + Mean_price + price_index
formula4 = Total_loans ~ Mean_price + Cash + dwellings_number + Unemployment_rate
formula5 = Total_loans ~ Mean_price + Cash + dwellings_number + total_earnings
formula6 = Total_loans ~ Mean_price + Cash + dwellings_number
formula7 = Total_loans ~ Mean_price + Cash
formula8 = Total_loans ~ Cash + dwellings_number
formula9 = Total_loans ~ Mean_price
formula10 = Total_loans ~ Cash
formula11 = Total_loans ~ dwellings_number
formula12 = Total_loans ~ total_earnings
formula13 = Total_loans ~ Unemployment_rate
formula14 = log(Total_loans) ~ Mean_price + Cash + dwellings_number + total_earnings
formula15 = Total_loans ~ Mean_price + Cash + Unemployment_rate
formula16 = Total_loans ~ Cash + Unemployment_rate
formula17 = Total_loans ~ Mean_price + Cash  + Unemployment_rate + total_earnings

#//formulas to highlight formula2 and formula16

####### multilinear regression

linear_mod <- lm(formula16, data = data)

#The confidence interval of the model 
confint(linear_mod)


#independence of the predictors// there is no correlation between the residuals, e.g. the residuals are independent
#perform Durbin-Watson test//https://www.statology.org/durbin-watson-test-r/
durbinWatsonTest(linear_mod)
#p-value is 0. Since this p-value is less than 0.05, we can reject the null hypothesis and conclude that the residuals in this regression model are autocorrelated

#seems we have heteroscedasticity - the results of the analysis become hard to trust/  to declare that a term in the model is statistically significant, when in fact it is not.
#heteroscedasticity. This means that the variability in the response is changing as the predicted value increases. observations with larger errors will have more pull or influence on the fitted model
#common sol is to calculate log or square root transformation/ formula_mod14 still have heteroscedasticity
#We can also use the Non- Constant Error Variance (NVC) Test
ncvTest(linear_mod)


car::vif(linear_mod)



#################Gaussian
#GLM with gaussian family
gauss_mod <- glm(formula = formula16 , data = data, family = "gaussian")




#Multicollinearity
car::vif(gauss_mod)

########################## Gamma

#training the model gamma
gamma_mod = glm(
  formula = formula16,
  family = Gamma, 
  data = data)

summary(gamma_mod)
#glm gamma_full AIC 468
#GLM Gamma formula1 AIC 525
#GLM Gamma formula2 AIC 466
#GLM Gamma formula4 AIC 624.29

#Plotting the 4 graphs necessary for assumption check
# par(mfrow = c(2, 2))
# plot(gamma_mod)

#Checking for multicollinearity
car::vif(gamma_mod)


##################### Inverse gaussian
#Inverse Gauss AIC 486

inv_mod = glm(
  formula = formula16,
  family = inverse.gaussian, 
  data = data)

summary(inv_mod)

#Plotting the 4 graphs necessary for assumption check

#Checking for multicollinearity
car::vif(inv_mod)

################## PCA
#//https://www.datacamp.com/tutorial/pca-analysis-r //https://www.r-bloggers.com/2016/02/principal-component-analysis-using-r/
#https://www.analyticsvidhya.com/blog/2016/03/pca-practical-guide-principal-component-analysis-python/
#model applying PCA
pca <- prcomp(data[,c(2:6)], center = TRUE,scale. = TRUE)

summary(pca) #Importance of components
attributes(pca)

pca$scale #use for normalization

#extracting the new features
x <- pca$x

#joining the whole data
#data_pca <- data.frame(data[,c(1:2)], x[,c(1:4)], data[,c("Total_loans")] )

data_pca <- data.frame(x[,c(1:5)], data[,c("Total_loans")] )


#rename column
colnames(data_pca)
data_pca <- rename(
  data_pca, 
  Total_loans = "data...c..Total_loans...")
#,Date = "data...c.1.1..")

# correlations between the principal components and the original variables
#//https://online.stat.psu.edu/stat505/lesson/11/11.4
#//https://reader.elsevier.com/reader/sd/pii/S0169743901002039?token=6588695C5B8FF0C572ADC0B7AE588ACC1A2D170382E23C05124DEB9FFA67F85EC4129732D75758328800CE44D1682D93&originRegion=us-east-1&originCreation=20220928072020
#https://www.sciencedirect.com/science/article/pii/S0169743901002039
full_data <- data.frame(data[,c(2:6)], x[,c(1:5)])



#formulas pca
formula_pca1 = Total_loans ~ Cash  + PC1 + PC2 + PC3 + PC4
formula_pca2 = Total_loans ~ Cash  + PC2 + PC3 + PC4
formula_pca3 = Total_loans ~ PC1 + PC2 + PC3 + PC4 + PC5 # selected formula
formula_pca4 = ln(Total_loans) ~ PC1 + PC2 + PC3 + PC4 + PC5

############gamma + pca // selected model
#training model
gamma_mod_pca = glm(
  formula = formula_pca3,
  family = Gamma, 
  data = data_pca)

# summary(gamma_mod_pca)

#Checking for multicollinearity
# car::vif(gamma_mod_pca)

#########inverse gaussian + pca
inv_mod_pca = glm(
  formula = formula_pca3,
  family = inverse.gaussian, 
  data = data_pca)


#Checking for multicollinearity
# car::vif(inv_mod_pca)

