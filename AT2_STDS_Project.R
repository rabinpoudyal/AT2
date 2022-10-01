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

library(car)



# set working directory.
path = "C:/Users/amedi/OneDrive/Escritorio/UTS/STDS_statistical_thinking_for_data_science/AT2/project_report/"
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

#saving the data // choose one of the datasets to the EDA and the model
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
  ggplot(aes(Cash, Total_loans )) + geom_point(mapping = aes(x =Cash , y = Total_loans)) +geom_smooth(color ="Blue") +labs(title = "Decreasing Cash Rate resulting in increased \nnumber of loans from 2012-2022 ")

#Relationship between total earnings and Total loans
clean_dataset %>%
  ggplot(aes(total_earnings, Total_loans )) + geom_point(mapping = aes(x =total_earnings , y = Total_loans, color = Date)) +geom_smooth(color ="Red") +labs(title = "Increasing Total Earnings results in increase \nin number of loans from 2012-2022 ")


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
summary(linear_mod)

#The confidence interval of the model 
confint(linear_mod)

#Assumption of linear regression // https://www.statology.org/linear-regression-assumptions/

#linearity of the data - Residual vs Fitted plot
plot(linear_mod, 1) 
#the residuals are not randomly scattered around the center line of zero
#seems the variance is not constant
#error should not be approx normally distributed


#independence of the predictors// there is no correlation between the residuals, e.g. the residuals are independent
#perform Durbin-Watson test//https://www.statology.org/durbin-watson-test-r/
durbinWatsonTest(linear_mod)
#p-value is 0. Since this p-value is less than 0.05, we can reject the null hypothesis and conclude that the residuals in this regression model are autocorrelated


# residuals errors have constant variance - Homoscedasticity
plot(linear_mod, 3)
#seems we have heteroscedasticity - the results of the analysis become hard to trust/  to declare that a term in the model is statistically significant, when in fact it is not.
#heteroscedasticity. This means that the variability in the response is changing as the predicted value increases. observations with larger errors will have more pull or influence on the fitted model
#common sol is to calculate log or square root transformation/ formula_mod14 still have heteroscedasticity
#We can also use the Non- Constant Error Variance (NVC) Test
ncvTest(linear_mod)

#normality - residuals are normally distributed
#q-q plot/ quantile-quantile plot/ to determine whether or not the residuals of a model follow a normal distribution
#If the points on the plot roughly form a straight diagonal line, then the normality assumption is met
#we also can use formal statistical tests like Shapiro-Wilk, Kolmogorov-Smironov, Jarque-Barre, or D’Agostino-Pearson / these tests are sensitive to large sample sizes
plot(linear_mod, 2)
#check distribution of the residuals
hist(residuals(linear_mod), col = "steelblue")
#left skewed


#independence of observations
#identify influential observations
#Residuals vs. Leverage Plot - cook's distance //https://www.statology.org/residuals-vs-leverage-plot/
#to the extent to which the coefficients in the regression model would change if a particular observation was removed from the dataset.
plot(linear_mod, 4)
#there are influential points/ Influential observations could indicate that the model you specified does not provide a good fit to the data

#to sum up
par(mfrow = c(2, 2))
plot(linear_mod)

#Multicollinearity analysis// https://www.statology.org/multicollinearity-regression/
#if we have highly correlated variable with each other, multicollinearity is likely to be a problem
#main goals of regression analysis is to isolate the relationship between each predictor variable and the response variable
#when we run a regression analysis, we interpret each regression coefficient as the mean change in the response variable, assuming all of the other predictor variables in the model are held constant
#The precision of the coefficient estimates are reduced, which makes the p-values unreliable. This makes it difficult to determine which predictor variables are actually statistically significant.
#variance inflation factor (VIF)//http://www.sthda.com/english/articles/39-regression-model-diagnostics/160-multicollinearity-essentials-and-vif-in-r/#:~:text=For%20a%20given%20predictor%20(p,one%20(absence%20of%20multicollinearity).
car::vif(linear_mod)


###### GLM / https://www.statology.org/interpret-glm-output-in-r/

########################## Assumption glm 
#// https://www.youtube.com/watch?v=XGhGeWRFw2Y&ab_channel=StatsTree

####Assumption about the structure
#reponse (y) is a continuous variable
#(y) is linearly related to x's   plot 1/ residuals vs predicted ****

####Assumption about the residuals
#normality of the residuals/errors N(0,sigma)  plot 2/q-q plot ***
#residuals errors have constant variance - Homoscedasticity  sigma=sigma plot 1/ residuals vs predicted ****
#independence of the errors/residuals cov(Ei,Ej)=0 plot 1/ residuals vs predicted ****
#non-collinearity of the predictors / independence of observations: cov(xi,xj)=0 plot4 (cook's distance: [check if a particular observation has influence on the final result])


#################Gaussian
#GLM with gaussian family
gauss_mod <- glm(formula = formula16 , data = data, family = "gaussian")


#Calculating residuals
residuals <- resid(gauss_mod)

#Residual Plot fitted vs residuals
plot(fitted(gauss_mod), residuals)

#Q-q plot and density plot
qqnorm(residuals)
plot(density(residuals))

par(mfrow = c(2, 2))
plot(gauss_mod)

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
par(mfrow = c(2, 2))
plot(gamma_mod)

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
par(mfrow = c(2, 2))
plot(inv_mod)

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

corr_matrix = cor(full_data)
corrplot(corr_matrix, method = 'number', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)


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

summary(gamma_mod_pca)

#Plotting the 4 graphs necessary for assumption check
par(mfrow = c(2, 2))
plot(gamma_mod_pca)

#Checking for multicollinearity
car::vif(gamma_mod_pca)

#########inverse gaussian + pca
inv_mod_pca = glm(
  formula = formula_pca3,
  family = inverse.gaussian, 
  data = data_pca)

summary(inv_mod_pca)

#Plotting the 4 graphs necessary for assumption check
par(mfrow = c(2, 2))
plot(inv_mod_pca)

#Checking for multicollinearity
car::vif(inv_mod_pca)



####### hypotesis test

#Coefficient P-values significance
#https://www.statology.org/multiple-linear-regression/
#https://www.statology.org/multiple-linear-regression-r/


######################### for count data
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



################### references

#http://cursos.leg.ufpr.br/rmcd/applications.html - 
#https://stats.stackexchange.com/questions/108995/interpreting-residual-and-null-deviance-in-glm-r  - interpret the Null and Residual Deviance
#https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf  -  quasi AIC
# https://cran.r-project.org/web/packages/DGLMExtPois/DGLMExtPois.pdf  - com-poisson
# https://www.rdocumentation.org/packages/DGLMExtPois/versions/0.2.0/topics/glm.CMP  - com-poisson


################### references Daniel distribution
#https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
#https://www.scribbr.com/statistics/akaike-information-criterion/