
# Stock Benchmark: S&P600
# Plot the chat for S&P600 from 2016-04-24 to 2023-04-24\
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

boxplot(return,  horizontal=TRUE, main = "SP600 return Boxplot", ylab = "Return")

plot(return, main = "S&P 600 weekly returns", xlab = "year", type = "l", ylab = "log return") # plot the graph


### Distributions

## Location
mean (return) # calculate mean of the return
mean (return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(return)

# Variance
var(return)
# Standard deviation
sd(return)
# Median Absolute Deviation -> compare with Std Dev
mad (return)

#skewness
library(e1071) # the library to calculate skewness
skewness (return2$SP600.Close)
kurtosis(return2$SP600.Close)

# Estimates Based on Percentiles
quantile (return)
quantile (return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (return, tauseq)

IQR (return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
return.density = density(return) # estimate kernel density
plot (return.density) # plot the estimated density

#QQ
qqnorm(return)
qqline(return)


## Shapiro-Wilk test 
return2 = fortify.zoo(return)
shapiro.test(return2$SP600.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.


library (moments)
agostino.test(return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(return2$SP600.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (return2$SP600.Close, plnorm)

####################################################################################
# Stock candidate 1: Moderna Inc (MRNA), Biotechnoligy company, Sector: Healthcare
# Get ticket of MRNA
getSymbols("MRNA", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting MRNA prices

chart_Series(MRNA) #plotting the series
mrna_data.xts = MRNA$MRNA.Close

mrna_return = diff (log(mrna_data.xts)) # Log return calculation
mrna_return = mrna_return [-1] # removing the first empty observation, received after return calculation
summary (mrna_return)

plot(mrna_return, main = "Moderna weekly returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (mrna_return) # calculate mean of the return
mean (mrna_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(mrna_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (mrna_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(mrna_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(mrna_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(mrna_return)

# Variance
var(mrna_return)
# Standard deviation
sd(mrna_return)
# Median Absolute Deviation -> compare with Std Dev
mad (mrna_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (mrna_return$MRNA.Close)
kurtosis(mrna_return$MRNA.Close)

# Estimates Based on Percentiles
quantile (mrna_return)
quantile (mrna_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (mrna_return, tauseq)

IQR (mrna_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
mrna_return.density = density(mrna_return) # estimate kernel density
plot(mrna_return.density) # plot the estimated density

#QQ
qqnorm(mrna_return)
qqline(mrna_return)

## Shapiro-Wilk test 
mrna_return2 = fortify.zoo(mrna_return)
shapiro.test(mrna_return2$MRNA.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(mrna_return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (mrna_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (mrna_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(mrna_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(mrna_return2$MRNA.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (mrna_return2$MRNA.Close, plnorm)
####################################################################################
# Stock candidate 2: Microsoft (MSFT), Biotechnoligy company, Sector: Healthcare
# Get ticket of MSFT
getSymbols("MSFT", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting MRNA prices

chart_Series(MSFT) #plotting the series
msft_data.xts = MSFT$MSFT.Close

msft_return = diff (log(msft_data.xts)) # Log return calculation
msft_return = msft_return [-1] # removing the first empty observation, received after return calculation
summary (msft_return)

boxplot(msft_return,  horizontal=TRUE, main = "Microsoft return Boxplot", ylab = "Return")

plot(return, main = "Microsoft weekly returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (msft_return) # calculate mean of the return
mean (msft_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(msft_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (msft_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(msft_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(msft_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(msft_return)

# Variance
var(msft_return)
# Standard deviation
sd(msft_return)
# Median Absolute Deviation -> compare with Std Dev
mad (msft_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (msft_return$MSFT.Close)
kurtosis(msft_return$MSFT.Close)

# Estimates Based on Percentiles
quantile (msft_return)
quantile (msft_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (msft_return, tauseq)

IQR (msft_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
msft_return.density = density(msft_return) # estimate kernel density
plot(msft_return.density) # plot the estimated density

#QQ
qqnorm(msft_return)
qqline(msft_return)

## Shapiro-Wilk test 
msft_return2 = fortify.zoo(msft_return)
shapiro.test(msft_return2$MSFT.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(msft_return, alternative = "two.sided") # D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (msft_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (msft_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(msft_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(msft_return2$MSFT.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (msft_return2$MSFT.Close, plnorm)

####################################################################################
# Stock candidate 3: T-Mobile US,Inc. (TMUS)
# Get ticket of TMUS
getSymbols("TMUS", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting TMUS prices

chart_Series(TMUS) #plotting the series
tmus_data.xts = TMUS$TMUS.Close

tmus_return = diff (log(tmus_data.xts)) # Log return calculation
tmus_return = tmus_return [-1] # removing the first empty observation, received after return calculation
summary (tmus_return)

boxplot(tmus_return,  horizontal=TRUE, main = "T-Mobile US return Boxplot", ylab = "Return")

plot(tmus_return, main = "T-Mobile US,Inc weekly returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (tmus_return) # calculate mean of the return
mean (tmus_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(tmus_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (tmus_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(tmus_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(tmus_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(tmus_return)

# Variance
var(tmus_return)
# Standard deviation
sd(tmus_return)
# Median Absolute Deviation -> compare with Std Dev
mad (tmus_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (tmus_return$TMUS.Close)
kurtosis(tmus_return$TMUS.Close)

# Estimates Based on Percentiles
quantile (tmus_return)
quantile (tmus_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (tmus_return, tauseq)

IQR (tmus_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
tmus_return.density = density(tmus_return) # estimate kernel density
plot(tmus_return.density) # plot the estimated density

#QQ
qqnorm(tmus_return)
qqline(tmus_return)

## Shapiro-Wilk test 
tmus_return2 = fortify.zoo(tmus_return)
shapiro.test(tmus_return2$TMUS.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(tmus_return, alternative = "two.sided") #	D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (tmus_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (tmus_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(tmus_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(tmus_return2$TMUS.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (tmus_return2$TMUS.Close, plnorm)

####################################################################################
# Stock candidate 4: UnitedHealth Group Incorporated. (UNH)
# Get ticket of UNH
getSymbols("UNH", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting UNH prices

chart_Series(UNH) #plotting the series
unh_data.xts = UNH$UNH.Close

unh_return = diff (log(unh_data.xts)) # Log return calculation
unh_return = unh_return [-1] # removing the first empty observation, received after return calculation
summary (unh_return)
boxplot(unh_return,  horizontal=TRUE, main = "UnitedHealth Group Incorporated return Boxplot", ylab = "Return")

plot(unh_return, main = "UnitedHealth Group Incorporated", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (unh_return) # calculate mean of the return
mean (unh_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(unh_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (unh_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(unh_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(unh_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(unh_return)

# Variance
var(unh_return)
# Standard deviation
sd(unh_return)
# Median Absolute Deviation -> compare with Std Dev
mad (unh_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (unh_return$UNH.Close)
kurtosis(unh_return$UNH.Close)

# Estimates Based on Percentiles
quantile (unh_return)
quantile (unh_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (unh_return, tauseq)

IQR (unh_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
unh_return.density = density(unh_return) # estimate kernel density
plot(unh_return.density) # plot the estimated density

#QQ
qqnorm(unh_return)
qqline(unh_return)

## Shapiro-Wilk test 
unh_return2 = fortify.zoo(unh_return)
shapiro.test(unh_return2$UNH.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(unh_return, alternative = "two.sided") #	D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (unh_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (unh_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(unh_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(unh_return2$UNH.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (unh_return2$UNH.Close, plnorm)

####################################################################################
# Stock candidate 5: Lam Research Corporation. (LRCX)
# Get ticket of LRCX
getSymbols("LRCX", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting LRCX prices

chart_Series(LRCX) #plotting the series
lrcx_data.xts = LRCX$LRCX.Close

lrcx_return = diff (log(lrcx_data.xts)) # Log return calculation
lrcx_return = lrcx_return [-1] # removing the first empty observation, received after return calculation
summary (lrcx_return)
boxplot(lrcx_return,  horizontal=TRUE, main = "Lam Research Corporation return Boxplot", ylab = "Return")

plot(lrcx_return, main = "Lam Research Corporation weekly returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (lrcx_return) # calculate mean of the return
mean (lrcx_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(lrcx_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (lrcx_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(lrcx_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(lrcx_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(lrcx_return)

# Variance
var(lrcx_return)
# Standard deviation
sd(lrcx_return)
# Median Absolute Deviation -> compare with Std Dev
mad (lrcx_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (lrcx_return$LRCX.Close)
kurtosis(lrcx_return$LRCX.Close)

# Estimates Based on Percentiles
quantile (lrcx_return)
quantile (lrcx_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (lrcx_return, tauseq)

IQR (lrcx_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
lrcx_return.density = density(lrcx_return) # estimate kernel density
plot(lrcx_return.density) # plot the estimated density

#QQ
qqnorm(lrcx_return)
qqline(lrcx_return)

## Shapiro-Wilk test 
lrcx_return2 = fortify.zoo(lrcx_return)
shapiro.test(lrcx_return2$LRCX.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(lrcx_return, alternative = "two.sided") #	D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (lrcx_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (lrcx_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(lrcx_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(lrcx_return2$LRCX.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (lrcx_return2$LRCX.Close, plnorm)

####################################################################################
# Stock candidate 6: ServiceNow (NOW), Biotechnoligy company, Sector: Healthcare
# Get ticket of NOW
getSymbols("NOW", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting NOW prices

chart_Series(NOW) #plotting the series
now_data.xts = NOW$NOW.Close

now_return = diff (log(now_data.xts)) # Log return calculation
now_return = now_return [-1] # removing the first empty observation, received after return calculation
summary (now_return)
boxplot(now_return,  horizontal=TRUE, main = "ServiceNow return Boxplot", ylab = "Return")

plot(now_return, main = "ServiceBow", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (now_return) # calculate mean of the return
mean (now_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(now_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (now_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(now_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(now_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(now_return)

# Variance
var(now_return)
# Standard deviation
sd(now_return)
# Median Absolute Deviation -> compare with Std Dev
mad (now_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (now_return$NOW.Close)
kurtosis(now_return$NOW.Close)

# Estimates Based on Percentiles
quantile (now_return)
quantile (now_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (now_return, tauseq)

IQR (now_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
now_return.density = density(now_return) # estimate kernel density
plot(now_return.density) # plot the estimated density

#QQ
qqnorm(now_return)
qqline(now_return)

## Shapiro-Wilk test 
now_return2 = fortify.zoo(now_return)
shapiro.test(now_return2$NOW.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(now_return, alternative = "two.sided") #	D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (now_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (now_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(now_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(now_return2$NOW.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (now_return2$NOW.Close, plnorm)

####################################################################################
# Stock candidate 7: Broadcom Inc (AVGO)
# Get ticket of AVGO
getSymbols("AVGO", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting AVGO prices

chart_Series(AVGO) #plotting the series
avgo_data.xts = AVGO$AVGO.Close

avgo_return = diff (log(avgo_data.xts)) # Log return calculation
avgo_return = avgo_return [-1] # removing the first empty observation, received after return calculation
summary (avgo_return)

boxplot(avgo_return,  horizontal=TRUE, main = "Broadcom Inc return Boxplot", ylab = "Return")

plot(avgo_return, main = "Broadcom Inc", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (avgo_return) # calculate mean of the return
mean (avgo_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(avgo_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (avgo_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(avgo_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(avgo_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(avgo_return)

# Variance
var(avgo_return)
# Standard deviation
sd(avgo_return)
# Median Absolute Deviation -> compare with Std Dev
mad (avgo_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (avgo_return$AVGO.Close)
kurtosis(avgo_return$AVGO.Close)

# Estimates Based on Percentiles
quantile (avgo_return)
quantile (avgo_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (avgo_return, tauseq)

IQR (avgo_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
avgo_return.density = density(avgo_return) # estimate kernel density
plot(avgo_return.density) # plot the estimated density

#QQ
qqnorm(avgo_return)
qqline(avgo_return)

## Shapiro-Wilk test 
avgo_return2 = fortify.zoo(avgo_return)
shapiro.test(avgo_return2$AVGO.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(avgo_return, alternative = "two.sided") #	D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (avgo_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (avgo_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(avgo_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(avgo_return2$AVGO.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (avgo_return2$AVGO.Close, plnorm)

####################################################################################
# Stock candidate 8: Masteredcard Incorporated (MA), Biotechnoligy company, Sector: Healthcare
# Get ticket of MA
getSymbols("MA", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting MA prices

chart_Series(MA) #plotting the series
ma_data.xts = MA$MA.Close

ma_return = diff (log(ma_data.xts)) # Log return calculation
ma_return = ma_return [-1] # removing the first empty observation, received after return calculation
summary (ma_return)

boxplot(ma_return,  horizontal=TRUE, main = "Masteredcard Incorporated return Boxplot", ylab = "Return")

plot(ma_return, main = "Mastercard Incorporated", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (ma_return) # calculate mean of the return
mean (ma_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(ma_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (ma_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(ma_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(ma_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(ma_return)

# Variance
var(ma_return)
# Standard deviation
sd(ma_return)
# Median Absolute Deviation -> compare with Std Dev
mad (ma_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (ma_return$MA.Close)
kurtosis(ma_return$MA.Close)

# Estimates Based on Percentiles
quantile (ma_return)
quantile (ma_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (ma_return, tauseq)

IQR (ma_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
ma_return.density = density(ma_return) # estimate kernel density
plot(ma_return.density) # plot the estimated density

#QQ
qqnorm(ma_return)
qqline(ma_return)

## Shapiro-Wilk test 
ma_return2 = fortify.zoo(ma_return)
shapiro.test(ma_return2$MA.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(ma_return, alternative = "two.sided") #	D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (ma_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (ma_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(ma_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(ma_return2$MA.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (ma_return2$MA.Close, plnorm)

####################################################################################
# Stock candidate 9: Addvanced Micro Devices, Inc. (AMD), Biotechnoligy company, Sector: Healthcare
# Get ticket of AMD
getSymbols("AMD", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting AMD prices

chart_Series(AMD) #plotting the series
amd_data.xts = AMD$AMD.Close

amd_return = diff (log(amd_data.xts)) # Log return calculation
amd_return = amd_return [-1] # removing the first empty observation, received after return calculation
summary (amd_return)

boxplot(amd_return,  horizontal=TRUE, main = "Addvanced Micro Devices,Inc. return Boxplot", ylab = "Return")

plot(amd_return, main = "Advanced Micro Devices", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (amd_return) # calculate mean of the return
mean (amd_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(amd_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (amd_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(amd_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(amd_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(amd_return)

# Variance
var(amd_return)
# Standard deviation
sd(amd_return)
# Median Absolute Deviation -> compare with Std Dev
mad (amd_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (amd_return$AMD.Close)
kurtosis(amd_return$AMD.Close)

# Estimates Based on Percentiles
quantile (amd_return)
quantile (amd_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (amd_return, tauseq)

IQR (amd_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
amd_return.density = density(amd_return) # estimate kernel density
plot(amd_return.density) # plot the estimated density

#QQ
qqnorm(amd_return)
qqline(amd_return)

## Shapiro-Wilk test 
amd_return2 = fortify.zoo(amd_return)
shapiro.test(amd_return2$AMD.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(amd_return, alternative = "two.sided") #	D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (amd_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (amd_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(amd_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(amd_return2$AMD.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (amd_return2$AMD.Close, plnorm)

####################################################################################
# Stock candidate 10: Tesla (TSLA)
# Get ticket of TSLA
getSymbols("TSLA", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting TSLA prices

chart_Series(TSLA) #plotting the series
tsla_data.xts = TSLA$TSLA.Close

tsla_return = diff (log(tsla_data.xts)) # Log return calculation
tsla_return = tsla_return [-1] # removing the first empty observation, received after return calculation
summary (tsla_return)

boxplot(tsla_return,  horizontal=TRUE, main = "Tesla return Boxplot", ylab = "Return")

plot(tsla_return, main = "T-Mobile US,Inc weekly returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (tsla_return) # calculate mean of the return
mean (tsla_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(tsla_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (tsla_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(tsla_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(tsla_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(tsla_return)

# Variance
var(tsla_return)
# Standard deviation
sd(tsla_return)
# Median Absolute Deviation -> compare with Std Dev
mad (tsla_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (tsla_return$TSLA.Close)
kurtosis(tsla_return$TSLA.Close)

# Estimates Based on Percentiles
quantile (tsla_return)
quantile (tsla_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (tsla_return, tauseq)

IQR (tsla_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
tsla_return.density = density(tsla_return) # estimate kernel density
plot(tsla_return.density) # plot the estimated density

#QQ
qqnorm(tsla_return)
qqline(tsla_return)

## Shapiro-Wilk test 
tsla_return2 = fortify.zoo(tsla_return)
shapiro.test(tsla_return2$TSLA.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(tsla_return, alternative = "two.sided") #	D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (tsla_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (tsla_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(tsla_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(tsla_return2$TSLA.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (tsla_return2$TSLA.Close, plnorm)

####################################################################################
# Stock candidate 11: NVIDIA Corporation (NVDA)
# Get ticket of NVDA
getSymbols("NVDA", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting NVDA prices

chart_Series(NVDA) #plotting the series
nvda_data.xts = NVDA$NVDA.Close

nvda_return = diff (log(nvda_data.xts)) # Log return calculation
nvda_return = nvda_return [-1] # removing the first empty observation, received after return calculation
summary (nvda_return)

boxplot(nvda_return,  horizontal=TRUE, main = "NVIDIA Corporation return Boxplot", ylab = "Return")

plot(nvda_return, main = "NVIDIA Corporation weekly returns", xlab = "year", type = "l", ylab = "log return") # plot the graph

### Distributions

## Location
mean (nvda_return) # calculate mean of the return
mean (nvda_return, trim=0.1) # calculate trimmed mean (10 per cent is trimmed) or the return
floor (0.1*nrow(nvda_return)) # 261 extreme values from each of the tails are trimmed that equals to 10% of the observations

mean (nvda_return, trim=0.2) # calculate trimmed mean (20 per cent is trimmed) or the return
floor (0.2*nrow(nvda_return)) # 522 extreme values from  each of the tiles are trimmed that equals to 20% of the observations

#weighted mean
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)  # Create example data: for instance grades per session test
w1 <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1)  # Create example weights
weighted.mean(x1, w1) # calculate weighted mean
mean (x1) # compare with the mean value

median(nvda_return) # calculate the median value of the return

#Compare mean and median -> skew

## Estimates of Variability

# Mean absolute deviation
library (DescTools) # Tools for Descriptive Statistics
MeanAD(nvda_return)

# Variance
var(nvda_return)
# Standard deviation
sd(nvda_return)
# Median Absolute Deviation -> compare with Std Dev
mad (nvda_return)

#skewness
library(e1071) # the library to calculate skewness
skewness (nvda_return$NVDA.Close)
kurtosis(nvda_return$NVDA.Close)

# Estimates Based on Percentiles
quantile (nvda_return)
quantile (nvda_return, probs=c(0.05, 0.95)) # values in the 5th and 95th quantiles

tauseq = seq(.1,.95,.1) # generate sequesnce of numbers that will be used as quantiles
quantile (nvda_return, tauseq)

IQR (nvda_return) # IQRx = Qx(0,75) - Qx(0.25) (interquartile range)

# kernel density
nvda_return.density = density(nvda_return) # estimate kernel density
plot(nvda_return.density) # plot the estimated density

#QQ
qqnorm(nvda_return)
qqline(nvda_return)

## Shapiro-Wilk test 
nvda_return2 = fortify.zoo(nvda_return)
shapiro.test(nvda_return2$NVDA.Close) # Shapiro-Wilk test, use a vector as the input, not a dataframe
# From the output, the p-value < 0.05 implying that the distribution of the data is significantly different from normal distribution. 
# In other words, we can assume the non-normality.

library (moments)
agostino.test(nvda_return, alternative = "two.sided") #	D'Agostino skewness test

# Anscombe-Glynn test of kurtosis
anscombe.test (nvda_return, alternative = "two.sided" ) # Anscombe-Glynn test of kurtosis

#Bonett-seier test of kurtosis
bonett.test (nvda_return, alternative = "two.sided" )

# Kolmogorov-Smirnov
library (fBasics)
ks.test(nvda_return, "pnorm")

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(nvda_return2$NVDA.Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
ad.test (nvda_return2$NVDA.Close, plnorm)



###############################################################################
## Multivariate analysis
## Benchmark vs MSFT
# merging 2 dataframes
together =  merge(SP600$SP600.Close,MSFT$MSFT.Close, all=FALSE) 
# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
return.together = Return.calculate( together , method = "log")
plot (return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
return.together.df = fortify(return.together) # convert the  xts object into a dataframe
return.together.df = return.together.df [-1,]
head(return.together.df)

## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= return.together.df$SP600.Close
Y= return.together.df$MSFT.Close


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

#################################################################################
## Benchmark vs TMUS
# merging 2 dataframes
tmus_together =  merge(SP600$SP600.Close,TMUS$TMUS.Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
tmus_return.together = Return.calculate( tmus_together , method = "log")
plot (tmus_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
tmus_return.together.df = fortify(tmus_return.together) # convert the  xts object into a dataframe
tmus_return.together.df = tmus_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= tmus_return.together.df$SP600.Close
Y= tmus_return.together.df$TMUS.Close


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

#################################################################################
## Benchmark vs UNH
# merging 2 dataframes
unh_together =  merge(SP600$SP600.Close,UNH$UNH.Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
unh_return.together = Return.calculate(unh_together , method = "log")
plot (unh_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
unh_return.together.df = fortify(unh_return.together) # convert the  xts object into a dataframe
unh_return.together.df = unh_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= unh_return.together.df$SP600.Close
Y= unh_return.together.df$UNH.Close


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

#################################################################################
## Benchmark vs LRCX
# merging 2 dataframes
lrcx_together =  merge(SP600$SP600.Close,LRCX$LRCX.Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
lrcx_return.together = Return.calculate( lrcx_together , method = "log")
plot (lrcx_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
lrcx_return.together.df = fortify(lrcx_return.together) # convert the  xts object into a dataframe
lrcx_return.together.df = lrcx_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= lrcx_return.together.df$SP600.Close
Y= lrcx_return.together.df$LRCX.Close


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

#################################################################################
## Benchmark vs NOW
# merging 2 dataframes
now_together =  merge(SP600$SP600.Close,NOW$NOW.Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
now_return.together = Return.calculate( now_together , method = "log")
plot (now_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
now_return.together.df = fortify(now_return.together) # convert the  xts object into a dataframe
now_return.together.df = now_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= now_return.together.df$SP600.Close
Y= now_return.together.df$NOW.Close


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

#################################################################################
## Benchmark vs AVGO
# merging 2 dataframes
avgo_together =  merge(SP600$SP600.Close,AVGO$AVGO.Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
avgo_return.together = Return.calculate( avgo_together , method = "log")
plot (avgo_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
avgo_return.together.df = fortify(avgo_return.together) # convert the  xts object into a dataframe
avgo_return.together.df = avgo_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= avgo_return.together.df$SP600.Close
Y= avgo_return.together.df$AVGO.Close


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

#################################################################################
## Benchmark vs MA
# merging 2 dataframes
ma_together =  merge(SP600$SP600.Close,MA$MA.Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
ma_return.together = Return.calculate( ma_together , method = "log")
plot (ma_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
ma_return.together.df = fortify(ma_return.together) # convert the  xts object into a dataframe
ma_return.together.df = ma_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= ma_return.together.df$SP600.Close
Y= ma_return.together.df$MA.Close


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

#################################################################################
## Benchmark vs AMD
# merging 2 dataframes
amd_together =  merge(SP600$SP600.Close,AMD$AMD.Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
amd_return.together = Return.calculate( amd_together , method = "log")
plot (amd_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
amd_return.together.df = fortify(amd_return.together) # convert the  xts object into a dataframe
amd_return.together.df = amd_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= amd_return.together.df$SP600.Close
Y= amd_return.together.df$AMD.Close


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

#################################################################################
## Benchmark vs TSLA
# merging 2 dataframes
tsla_together =  merge(SP600$SP600.Close,TSLA$TSLA.Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
tsla_return.together = Return.calculate( tsla_together , method = "log")
plot (tsla_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
tsla_return.together.df = fortify(tsla_return.together) # convert the  xts object into a dataframe
tsla_return.together.df = tsla_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= tsla_return.together.df$SP600.Close
Y= tsla_return.together.df$TSLA.Close


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

#################################################################################
## Benchmark vs NVDA
# merging 2 dataframes
nvda_together =  merge(SP600$SP600.Close,NVDA$NVDA.Close, all=FALSE) 

# Calculate log return
require (PerformanceAnalytics) # the package to calculate return automaticaly 
nvda_return.together = Return.calculate( nvda_together , method = "log")
plot (nvda_return.together)



#convert into dataframes for further usage
require (ggplot2) # to use fortify below
nvda_return.together.df = fortify(nvda_return.together) # convert the  xts object into a dataframe
nvda_return.together.df = nvda_return.together.df [-1,]


## End of data preparation ###########################################################################

# for further simplicity let's assign returns to X and Y
X= nvda_return.together.df$SP600.Close
Y= nvda_return.together.df$NVDA.Close


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

################################################################################
## Two samole t-test
#Two sample t-test for the difference in sample means
t.test(X,Y, alternative = "two.sided", var.equal=TRUE)



set.seed("119933") # to ensure the same result of random data generation
par(mfrow=c(1,2)) # to build one  figures in one row and two columns  
x1 = rlnorm(20,meanlog=1,sdlog=2) # generate random values from the Log Normal Distribution, mean of the distribution on the log scale
hist(x1) # build the histograms 

x2 = rlnorm(20,meanlog=3,sdlog=2)
hist(x2)

boxplot(list(x1,x2),main="(a) no transformation")
boxplot(list(log(x1),log(x2)),main="(b) log transformation")
t.test(x1,x2,equal.var=F)
t.test(log(x1),log(x2))

####################################################################################
## Causality ##

library(yfR)
#  options 
my_ticker = c("^SP600","MSFT","TMUS","UNH","LRCX","NOW","AVGO","MA","AMD","TSLA","NVDA") 

# get  data
df_yf = yf_get(tickers = my_ticker, 
               first_date = start_date,
               last_date = end_date)
head(df_yf)

unique.tikers = unique(df_yf$ticker)

SP600= df_yf[df_yf$ticker==unique.tikers[1], ] 
MSFT= df_yf[df_yf$ticker==unique.tikers[2], ] 
TMUS= df_yf[df_yf$ticker==unique.tikers[3], ] 
UNH= df_yf[df_yf$ticker==unique.tikers[4], ] 
LRCX= df_yf[df_yf$ticker==unique.tikers[5], ] 
NOW= df_yf[df_yf$ticker==unique.tikers[6], ] 
AVGO= df_yf[df_yf$ticker==unique.tikers[7], ] 
MA= df_yf[df_yf$ticker==unique.tikers[8], ] 
AMD= df_yf[df_yf$ticker==unique.tikers[9], ] 
TSLA= df_yf[df_yf$ticker==unique.tikers[10], ] 
NVDA= df_yf[df_yf$ticker==unique.tikers[11], ] 


library (lmtest)
grangertest(SP600$price_close ~ MSFT$price_close, order = 1)
grangertest(SP600$price_close ~ TMUS$price_close, order = 1)
grangertest(SP600$price_close ~ UNH$price_close, order = 1)
grangertest(SP600$price_close ~ LRCX$price_close, order = 1)
grangertest(SP600$price_close ~ NOW$price_close, order = 1)
grangertest(SP600$price_close ~ AVGO$price_close, order = 1)
grangertest(SP600$price_close ~ MA$price_close, order = 1)
grangertest(SP600$price_close ~ AMD$price_close, order = 1)
grangertest(SP600$price_close ~ TSLA$price_close, order = 1)
grangertest(SP600$price_close ~ NVDA$price_close, order = 1)


# The null hypothesis for the Granger causality test is that X does not Granger-cause Y.
# If the p-value is less than or equal to alpha, you reject the null hypothesis and conclude that X Granger-causes Y (i.e., past values of X help predict Y).
# If the p-value is greater than alpha, you fail to reject the null hypothesis and conclude that there is not enough evidence to suggest that X Granger-causes Y.



#causality based on VAR
library(vars)
data = cbind(return.together.df$SP600.Close, return.together.df$weekdays.cryptos.xts) # Combine the time series into a matrix
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




## Causality based on DAG
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("graph")
BiocManager::install("RBGL")
BiocManager::install("Rgraphviz")

library(pcalg)
library(graph)


# Load the required packages
library(pcalg)
library(ggm)

# Set the seed for reproducibility
set.seed(42)

# Number of observations
n = 1000

# Simulate data for six variables
X1 = rnorm(n)
X2 =  0.6 * X1 + rnorm(n, sd=0.8)
X3 = 0.7 * X1 - 0.3 * X2 + rnorm(n, sd=0.9)
X4 = 0.5 * X2 + rnorm(n, sd=1.0)
X5 = -0.8 * X3 + rnorm(n, sd=1.1)
X6 = 0.4 * X4 + 0.4 * X5 + rnorm(n, sd=1.2)

# Combine variables into a data frame
data  = data.frame(X1, X2, X3, X4, X5, X6)

# Compute the covariance matrix
cov_matrix = cor(data)

# Estimate the DAG structure using the PC algorithm
suffStat = list(C = cov_matrix, n = n)
dag = pc(suffStat, indepTest = gaussCItest, alpha = 0.05, labels = colnames(data))

# Plot the DAG
plot(dag, main="Estimated DAG")

###########
data = cbind(SP600$price_close, SP600$volume, MSFT$price_close, MSFT$volume, 
             TMUS$price_close, TMUS$volume, UNH$price_close, UNH$volume, 
             LRCX$price_close, LRCX$volume, NOW$price_close, NOW$volume, 
             AVGO$price_close, AVGO$volume, MA$price_close, MA$volume, 
             AMD$price_close, AMD$volume, TSLA$price_close, TSLA$volume, 
             NVDA$price_close, NVDA$volume)
colnames(data) = c("BM_p", "BM_v", "MSFT_p", "MSFT_v", 
                   "TMUS_p", "TMUS_v", "UNH_p", "UNH_v",
                   "LRCX_p", "LRCX_v", "NOW_p", "NOW_v",
                   "AVGO_p", "AVGO_v", "MA_p", "MA_v",
                   "AMD_p", "AMD_v", "TSLA_p", "TSLA_v", 
                   "NVDA_p", "NVDA_v")
head(data)

# Estimate the DAG:
suffStat = list(C = cor(data), n = nrow(data))
dag = pc(suffStat, indepTest = gaussCItest, alpha = 0.05, labels = colnames(data))

# Plot the estimated DAG:
library(Rgraphviz)
plot(dag)

####################################################################################
## Spillover

####################################################################################
## Volatility
## GARCH model
# X return SP600
# Y return MSFT

library (tseries)
msft.garch.1 <- garch(Y, order =c(1,1))
summary (msft.garch.1)

library (rugarch)
msft.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
msft.garch.2 = ugarchfit(msft.garch.spec, Y)
summary(msft.garch.2)
plot (msft.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

##Fitting ARMA+GARCH Models
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


# ARMA-APARCH-t
arma.aparch.t = ugarchspec(mean.model=list(armaOrder=c(1,0)), 
                           variance.model=list(model="apARCH",
                                               garchOrder=c(1,1)))

msft.aparch.t = ugarchfit(data=Y, spec=arma.aparch.t)
show(msft.aparch.t)
plot (msft.aparch.t)


####################################################################################
## Volatility
## GARCH model
# X return SP600
# Y return TMUS

library (tseries)
tmus.garch.1 <- garch(Y, order =c(1,1))
summary (tmus.garch.1)

library (rugarch)
tmus.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
tmus.garch.2 = ugarchfit(tmus.garch.spec, Y)
summary(tmus.garch.2)
plot (tmus.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

####################################################################################
## Volatility
## GARCH model
# X return SP600
# Y return UNH

library (tseries)
unh.garch.1 <- garch(Y, order =c(1,1))
summary (unh.garch.1)

library (rugarch)
unh.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
unh.garch.2 = ugarchfit(unh.garch.spec, Y)
summary(unh.garch.2)
plot (unh.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

####################################################################################
## Volatility
## GARCH model
# X return SP600
# Y return LRCX

library (tseries)
lrcx.garch.1 <- garch(Y, order =c(1,1))
summary (lrcx.garch.1)

library (rugarch)
unh.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
unh.garch.2 = ugarchfit(unh.garch.spec, Y)
summary(unh.garch.2)
plot (unh.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

####################################################################################
## Volatility
## GARCH model
# X return SP600
# Y return NOW

getSymbols("NOW", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting MRNA prices
chart_Series(NOW)
now_data.xts = NOW$NOW.Close

now_return = diff (log(now_data.xts)) # Log return calculation
now_return = now_return [-1] # removing the first empty observation, received after return calculation
summary (now_return)

library (tseries)
now.garch.1 <- garch(Y, order =c(1,1))
summary (now.garch.1)

library (rugarch)
now.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
now.garch.2 = ugarchfit(now.garch.spec, Y)
summary(now.garch.2)
plot (now.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

####################################################################################
## Volatility
## GARCH model
# X return SP600
# Y return AVGO

getSymbols("AVGO", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting MRNA prices
chart_Series(AVGO)
avgo_data.xts = AVGO$AVGO.Close

avgo_return = diff (log(avgo_data.xts)) # Log return calculation
avgo_return = avgo_return [-1] # removing the first empty observation, received after return calculation
summary (avgo_return)

library (tseries)
avgo.garch.1 <- garch(Y, order =c(1,1))
summary (avgo.garch.1)

library (rugarch)
avgo.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
avgo.garch.2 = ugarchfit(avgo.garch.spec, Y)
summary(avgo.garch.2)
plot (avgo.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

####################################################################################
## Volatility
## GARCH model
# X return SP600
# Y return MA

getSymbols("MA", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting MRNA prices
chart_Series(MA)
ma_data.xts = MA$MA.Close

ma_return = diff (log(ma_data.xts)) # Log return calculation
ma_return = ma_return [-1] # removing the first empty observation, received after return calculation
summary (ma_return)

library (tseries)
ma.garch.1 <- garch(Y, order =c(1,1))
summary (ma.garch.1)

library (rugarch)
ma.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
ma.garch.2 = ugarchfit(ma.garch.spec, Y)
summary(ma.garch.2)
plot (ma.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

####################################################################################
## Volatility
## GARCH model
# X return SP600
# Y return AMD

getSymbols("AMD", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting MRNA prices
chart_Series(AMD)
amd_data.xts = AMD$AMD.Close

amd_return = diff (log(amd_data.xts)) # Log return calculation
amd_return = amd_return [-1] # removing the first empty observation, received after return calculation
sumamary (amd_return)

library (tseries)
amd.garch.1 <- garch(Y, order =c(1,1))
summary (amd.garch.1)

library (rugarch)
amd.garch.spec = ugarchspec(mean.model=list(aramdOrder=c(0,0)), distribution="norm") 
amd.garch.2 = ugarchfit(amd.garch.spec, Y)
sumamdry(amd.garch.2)
plot (amd.garch.2)

library(fGarch) # library for GARCH models
sumamdry(garchFit(~garch(1,1), X ))

####################################################################################
## Volatility
## GARCH model
# X return SP600
# Y return TSLA

getSymbols("TSLA", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting MRNA prices
chart_Series(TSLA)
tsla_data.xts = TSLA$TSLA.Close

tsla_return = diff (log(tsla_data.xts)) # Log return calculation
tsla_return = tsla_return [-1] # removing the first empty observation, received after return calculation
summary (tsla_return)

library (tseries)
tsla.garch.1 <- garch(Y, order =c(1,1))
summary (tsla.garch.1)

library (rugarch)
tsla.garch.spec = ugarchspec(mean.model=list(artslaOrder=c(0,0)), distribution="norm") 
tsla.garch.2 = ugarchfit(tsla.garch.spec, Y)
summary(tsla.garch.2)
plot (tsla.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

####################################################################################
## Volatility
## GARCH model
# X return SP600
# Y return NVDA

getSymbols("NVDA", from = start_date,
           to = end_date, periodicity = "weekly", warnings = FALSE,
           auto.assign = TRUE) # getting MRNA prices
chart_Series(NVDA)
nvda_data.xts = NVDA$NVDA.Close

nvda_return = diff (log(nvda_data.xts)) # Log return calculation
nvda_return = nvda_return [-1] # removing the first empty observation, received after return calculation
summary (nvda_return)

library (tseries)
nvda.garch.1 <- garch(Y, order =c(1,1))
summary (nvda.garch.1)

library (rugarch)
nvda.garch.spec = ugarchspec(mean.model=list(arnvdaOrder=c(0,0)), distribution="norm") 
nvda.garch.2 = ugarchfit(nvda.garch.spec, Y)
summary(nvda.garch.2)
plot (nvda.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

# ARMA-APARCH-t
arma.aparch.t = ugarchspec(mean.model=list(armaOrder=c(1,0)), 
                           variance.model=list(model="apARCH",
                                               garchOrder=c(1,1)))

nvda.aparch.t = ugarchfit(data=Y, spec=arma.aparch.t)
show(nvda.aparch.t)
plot (nvda.aparch.t)

## ARMA+GARCH Models
library(rugarch)
arma.garch.norm = ugarchspec(mean.model=list(armaOrder=c(1,0)), variance.model=list(garchOrder=c(1,1)))
nvda.garch.norm = ugarchfit(data=nvda_return, spec=arma.garch.norm)

show(nvda.garch.norm)
length (nvda_return)

plot (nvda.garch.norm)





