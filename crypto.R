##Data preparation ########################################################################################################
require (xts) # attache the package to convert our dataframes into an xts (time-series) object for simplicity in calculations
require(zoo)

library(tidyquant)

ticker = "^SP600"
start_date = "2016-04-24"
end_date = "2023-04-24"

options("getSymbols.warning4.0"=TRUE)
options("getSymbols.yahoo.warning"=TRUE)

# example
getSymbols(ticker, from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting SP600 prices

chart_Series(SP600) #plotting the series
data.xts = SP600$SP600.Close

return = diff (log(data.xts)) # Log return calculation
return = return [-1] # removing the first empty observation, received after return calculation
summary (return)

#####################################################################################
##Multivariate Analysis - SP600 vs Bitcoin
# import a time series of different frequency
BTC = read.csv("BTC-USD.csv")
BTC$Date = as.Date(BTC$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
BTC.xts = xts(BTC['Close'], order.by=BTC$Date)  # converting a data frame into xts onject (time-series object)
head(BTC.xts$Close)
head(SP600$SP600.Close)
plot(BTC$Close, main = "Bitcoin weekly Close Price", type = "l", ylab = "Price")

btc_return = diff (log(BTC.xts)) # Log return calculation
btc_return = btc_return [-1] # removing the first empty observation, received after return calculation
summary (btc_return)

boxplot(btc_return,  horizontal=TRUE, main = "Bitcoin return Boxplot", ylab = "Return")

plot(btc_return, main = "Bitcoin weekly returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# boxplot
btc_return2 = diff (log(BTC$Close)) # return as a vector
boxplot(btc_return2, horizontal=TRUE, main="Bitcoin Return")

hist(btc_return) # bild a histograme of the return

### Distributions

## Location
mean (btc_return) # calculate mean of the return
median(btc_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(btc_return)

# Variance
var(btc_return)
# Standard deviation
sd(btc_return)
# Median Absolute Deviation -> compare with Std Dev
mad (btc_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (BTC.xts$Close)
kurtosis(BTC.xts$Close)

# Estimates Based on Percentiles
quantile (btc_return)
quantile (btc_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (btc_return, tauseq)

IQR (btc_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
btc_return.density = density(btc_return) # estimate kernel density
plot (btc_return.density) # plot the estimated density

#QQ
qqnorm(btc_return)
qqline(btc_return)

## Shapiro-Wilk test 
btc_return2 = fortify.zoo(btc_return)
head(btc_return)
head(btc_btc_return2)
shapiro.test(btc_return2$Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.


library (moments)
agostino.test(btc_return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (btc_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (btc_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(btc_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(btc_return2$Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (btc_return2$Close, plnorm)


#################################################################################

btc_together =  merge(SP600$SP600.Close,BTC.xts$Close, all=FALSE) # merging 2 dataframes
head(btc_together)

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
btc_return.together = Return.calculate( btc_together , method = "log")
plot (btc_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
btc_return.together.df = fortify(btc_return.together) # convert the  xts object into a dataframe
btc_return.together.df = btc_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= btc_return.together.df$SP600.Close
Y= btc_return.together.df$Close

# Correlation 
require (fBasics) # attache the required package


#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)
# The correlation between x and y is reported as 0.14. 
# Since the p-value is lower than the critical value of 0.05 we reject the null hypothesis 
# of zero correlation. The function also reports the 95% confidence interval as 0.1036790 to 0.1736413. It doesnt crosses zero. 


#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")
#The Spearman rank correlation between x and y is .1. 

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")

#####################################################################################
##Multivariate Analysis - SP600 vs Namecoin (NMC)
# import a time series of different frequency
NMC = read.csv("NMC-USD.csv")
NMC$Date = as.Date(NMC$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
NMC.xts = xts(NMC['Close'], order.by=NMC$Date)  # converting a data frame into xts onject (time-series object)

plot(NMC$Close, main = "Namecoin weekly Close Price", type = "l", ylab = "Price")

nmc_return = diff (log(NMC.xts)) # Log return calculation
nmc_return = nmc_return [-1] # removing the first empty observation, received after return calculation
summary (nmc_return)

boxplot(nmc_return,  horizontal=TRUE, main = "Namecoin return Boxplot", ylab = "Return")

plot(nmc_return, main = "Namecoin weekly returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# boxplot
nmc_return2 = diff (log(NMC$Close)) # return as a vector
boxplot(nmc_return2, horizontal=TRUE, main="Namecoin Return")

hist(nmc_return) # bild a histograme of the return

### Distributions

head(nmc_return)
## Location
mean (nmc_return) # calculate mean of the return
median(nmc_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(nmc_return)

# Variance
var(nmc_return)
# Standard deviation
sd(nmc_return)
# Median Absolute Deviation -> compare with Std Dev
mad (nmc_return)


#skewness
library(e1071) # the library to calculate skewness
skewness (NMC.xts$Close)
kurtosis(NMC.xts$Close)

# Estimates Based on Percentiles
quantile (nmc_return)
quantile (nmc_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (nmc_return, tauseq)

IQR (nmc_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
nmc_return.density = density(nmc_return) # estimate kernel density
plot (nmc_return.density) # plot the estimated density

#QQ
qqnorm(nmc_return)
qqline(nmc_return)

nmc_return2 = fortify.zoo(nmc_return)
head(nmc_return)
head(nmc_return2)
shapiro.test(nmc_return2$SP600.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.


library (moments)
agostino.test(nmc_return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (nmc_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (nmc_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(nmc_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(nmc_return2$SP600.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (nmc_return2$SP600.Close, plnorm)


nmc_together =  merge(SP600$SP600.Close,NMC.xts$Close, all=FALSE) # merging 2 dataframes
head(nmc_together)

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
nmc_return.together = Return.calculate( nmc_together , method = "log")
plot (nmc_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
nmc_return.together.df = fortify(nmc_return.together) # convert the  xts object into a dataframe
nmc_return.together.df = nmc_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= nmc_return.together.df$SP600.Close
Y= nmc_return.together.df$Close

# Correlation 
require (fBasics) # attache the required package


#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)
# The correlation between x and y is reported as 0.14. 
# Since the p-value is lower than the critical value of 0.05 we reject the null hypothesis 
# of zero correlation. The function also reports the 95% confidence interval as 0.1036790 to 0.1736413. It doesnt crosses zero. 


#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")
#The Spearman rank correlation between x and y is .1. 

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")


#####################################################################################
##Multivariate Analysis - SP600 vs Litecoin (LTC)
# import a time series of different frequency
LTC = read.csv("LTC-USD.csv")
LTC$Date = as.Date(LTC$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
LTC.xts = xts(LTC['Close'], order.by=LTC$Date)  # converting a data frame into xts onject (time-series object)

plot(LTC$Close, main = "Litecoin weekly Close Price", type = "l", ylab = "Price")

ltc_return = diff (log(LTC.xts)) # Log return calculation
ltc_return = return [-1] # removing the first empty observation, received after return calculation
summary (ltc_return)

boxplot(ltc_return,  horizontal=TRUE, main = "Litecoin return Boxplot", ylab = "Return")

plot(ltc_return, main = "Litecoin weekly returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

# boxplot
ltc_return2 = diff (log(LTC$Close)) # return as a vector
boxplot(ltc_return2, horizontal=TRUE, main="Litecoin Return")

hist(ltc_return) # bild a histograme of the return

### Distributions

## Location
mean (ltc_return) # calculate mean of the return
median(ltc_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(ltc_return)

# Variance
var(ltc_return)
# Standard deviation
sd(ltc_return)
# Median Absolute Deviation -> compare with Std Dev
mad (ltc_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (LTC.xts$Close)
kurtosis(LTC.xts$Close)

# Estimates Based on Percentiles
quantile (ltc_return)
quantile (ltc_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (ltc_return, tauseq)

IQR (ltc_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
ltc_return.density = density(ltc_return) # estimate kernel density
plot (ltc_return.density) # plot the estimated density

#QQ
qqnorm(ltc_return)
qqline(ltc_return)

ltc_return2 = fortify.zoo(ltc_return)
head(ltc_return)
head(ltc_return2)
shapiro.test(ltc_return2$SP600.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.


library (moments)
agostino.test(ltc_return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (ltc_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (ltc_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(ltc_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(ltc_return2$SP600.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (ltc_return2$SP600.Close, plnorm)


ltc_together =  merge(SP600$SP600.Close,LTC.xts$Close, all=FALSE) # merging 2 dataframes
head(ltc_together)

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
ltc_return.together = Return.calculate( ltc_together , method = "log")
plot (ltc_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
ltc_return.together.df = fortify(ltc_return.together) # convert the  xts object into a dataframe
ltc_return.together.df = ltc_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= ltc_return.together.df$SP600.Close
Y= ltc_return.together.df$Close

# Correlation 
require (fBasics) # attache the required package


#Pearson's product moment correlation coefficient t-test
cor.test (X,Y, method="pearson",alternative="two.sided",conf.level = 0.95)
correlationTest(X,Y)
# The correlation between x and y is reported as 0.14. 
# Since the p-value is lower than the critical value of 0.05 we reject the null hypothesis 
# of zero correlation. The function also reports the 95% confidence interval as 0.1036790 to 0.1736413. It doesnt crosses zero. 


#spearman rank correlation test
cor.test(X,Y, method="spearman",alternative="two.sided")
#The Spearman rank correlation between x and y is .1. 

# KENDALL'S TAU CORRELATION COEFFICIENT TEST
cor.test(X,Y,method="kendal",alternative="two.sided")

####################################################################################
## Causality ##

##Causality based on VAR
##SP600 vs BTC
library(vars)
head(btc_return.together.df)
data = cbind(btc_return.together.df$SP600.Close, btc_return.together.df$Close) # Combine the time series into a matrix
# Set the maximum number of lags to consider
max_lags = 10

# Calculate selection criteria for different lag orders
lag_selection = VARselect(data, lag.max = max_lags, type = "both")

# View the selection criteria
print(lag_selection)

# Extract the optimal lag order based on the AIC or BIC criterion
optimal_lag_aic = lag_selection$selection["AIC(n)"]
optimal_lag_bic = lag_selection$selection["SC(n)"]

# Estimate the VAR model with the optimal lag order
model_aic = VAR(data, p = optimal_lag_aic)
model_bic = VAR(data, p = optimal_lag_bic)

#To test for causality within a VAR model, you can use the causality() function:
causal_test = causality(model_aic, cause = "y1")
causal_test

##SP600 vs LTC
library(vars)
head(ltc_return.together.df)
data = cbind(ltc_return.together.df$SP600.Close, ltc_return.together.df$Close) # Combine the time series into a matrix
# Set the maximum number of lags to consider
max_lags = 10

# Calculate selection criteria for different lag orders
lag_selection = VARselect(data, lag.max = max_lags, type = "both")

# View the selection criteria
print(lag_selection)

# Extract the optimal lag order based on the AIC or BIC criterion
optimal_lag_aic = lag_selection$selection["AIC(n)"]
optimal_lag_bic = lag_selection$selection["SC(n)"]

# Estimate the VAR model with the optimal lag order
model_aic = VAR(data, p = optimal_lag_aic)
model_bic = VAR(data, p = optimal_lag_bic)

#To test for causality within a VAR model, you can use the causality() function:
causal_test = causality(model_aic, cause = "y1")
causal_test

##SP600 vs NMC
library(vars)
head(nmc_return.together.df)
data = cbind(nmc_return.together.df$SP600.Close, nmc_return.together.df$Close) # Combine the time series into a matrix
# Set the maximum number of lags to consider
max_lags = 10

# Calculate selection criteria for different lag orders
lag_selection = VARselect(data, lag.max = max_lags, type = "both")

# View the selection criteria
print(lag_selection)

# Extract the optimal lag order based on the AIC or BIC criterion
optimal_lag_aic = lag_selection$selection["AIC(n)"]
optimal_lag_bic = lag_selection$selection["SC(n)"]

# Estimate the VAR model with the optimal lag order
model_aic = VAR(data, p = optimal_lag_aic)
model_bic = VAR(data, p = optimal_lag_bic)

#To test for causality within a VAR model, you can use the causality() function:
causal_test = causality(model_aic, cause = "y1")
causal_test

#########################################################
##Volatility

library (tseries)
bitcoin.garch.1 <- garch(Y, order =c(1,1))
summary (bitcoin.garch.1)


library (rugarch)
bitcoin.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
bitcoin.garch.2 = ugarchfit(bitcoin.garch.spec, Y)
summary(bitcoin.garch.2)
plot (bitcoin.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

# BMW 
library(rugarch)
data(bmw, package="evir")
arma.garch.norm = ugarchspec(mean.model=list(armaOrder=c(1,0)), variance.model=list(garchOrder=c(1,1)))
bmw.garch.norm = ugarchfit(data=bmw, spec=arma.garch.norm)

show(bmw.garch.norm)
length (bmw)

plot (bmw.garch.norm)

#testing distribution
library(MASS)
e = residuals(bmw.garch.norm, standardize=TRUE)
fitdistr(e,"t") #Maximum-likelihood fitting of univariate distributions, allowing parameters to be held fixed if desired.

#student distribution
arma.garch.t = ugarchspec(mean.model=list(armaOrder=c(1,0)),variance.model=list(garchOrder=c(1,1)), distribution.model = "std")
bmw.garch.t = ugarchfit(data=bmw,spec=arma.garch.t)
show(bmw.garch.t)





