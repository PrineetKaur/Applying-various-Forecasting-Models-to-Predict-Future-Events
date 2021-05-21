library(tseries)
library(forecast)
library(portes)
library(readxl)

setwd('C:/Users/pbhurji/OneDrive - IESEG/Desktop/Course Materials/Semester2/8) Forecasting/Forecasting - Individual Assignment - Prineet Kaur Bhurji')

##########################################################################
#### Exercise 1 - Bankruptcies in Belgium (January 2000 to March 2021)####
##########################################################################

###############
### 1) & 2) ###
###############

# Reading and Plotting the data
data1 <- read_excel("DataSets2021.xlsx", sheet="Bankrupt")
Bankruptcies <- ts(data1[2], frequency = 12, start = c(2000,1))
Bankruptcies
plot(Bankruptcies)

# Using Log Transformation
Bankruptcies_log <- log(Bankruptcies)
plot(Bankruptcies_log)

# Plotting the Seasonal and Monthly Trend Graphs
seasonplot(Bankruptcies_log, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal Plot",
           ylab="Bankruptcies in Belgium",
           col=rainbow(25), 
           pch=20)

monthplot(Bankruptcies_log, main="Seasonal Subseries Plot", ylab = "Bankruptcies in Belgium",
          xlab="Month", type="l")

# Plotting the ACF & PACF (Autocorrelation & Partial Autocorrelation)
lag.plot(Bankruptcies_log)
tsdisplay(Bankruptcies_log)

##########
### 3) ###
##########

# Splitting the Data into Train and Test Set
train <- window(Bankruptcies_log, end=c(2017,12))
test <- window(Bankruptcies_log, start=c(2018,1))

# Specifying the Length of the Test Set
len_test <- length(test)

# Applying Seasonal Naive Method on Train set
snm <- snaive(train, h=len_test)
plot(snm)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(snm)
residuals_snm <- residuals(snm)
hist(residuals_snm)
tsdisplay(residuals_snm)
Box.test(residuals_snm, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_snm[-c(1:12),], lags = seq(1, 24, 1), order = 0)

# Evaluating Forecast Accuracy
accuracy(snm, test)[,c(2,3,4,5,6,7)]


##########
### 4) ###
##########

# Applying STL Decomposition

## Using different t.window values in stl function
stl1 <- stl(train[,1], s.window="periodic", t.window = 12, robust = TRUE)
stl1_pred <- predict(stl1, h=len_test)
checkresiduals(stl1_pred)

stl2 <- stl(train[,1], s.window="periodic", t.window = 6, robust = TRUE)
stl2_pred <- predict(stl2, h=len_test)
checkresiduals(stl2_pred)

stl3 <- stl(train[,1], s.window="periodic", t.window = 3, robust = TRUE)
stl3_pred <- predict(stl3, h=len_test)
checkresiduals(stl3_pred)

## Using rwdrift function
rwdrift <- rwf(train, drift = TRUE,h=len_test)
checkresiduals(rwdrift)

# Applying Naive Method to the STL data
stl_nm <- forecast(stl2, method = "naive") 
plot(stl_nm)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(stl_nm)
residuals_stl_nm <- residuals(stl_nm)
hist(residuals_stl_nm)
tsdisplay(residuals_stl_nm)
Box.test(residuals_stl_nm, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_stl_nm[-1], lags = seq(1, 24, 1), order = 0)

# Evaluating Forecast Accuracy
accuracy(stl_nm, test)[,c(2,3,4,5,6,7)]

##########
### 5) ###
##########

# Forecasting using ETS (Using only the methods that involve Trend and Seasonal components)  

### ETS1 ###
ets1 <- ets(train, model = "ANA", ic = c("aic"))
ets1
ets1 <- forecast(ets1, h=len_test)
plot(ets1)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets1)
residuals_ets1 <- residuals(ets1)
hist(residuals_ets1)
tsdisplay(residuals_ets1)
Box.test(residuals_ets1, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets1, lags = seq(1, 24, 1), order = length(ets1$model$par))

# Evaluating Forecast Accuracy
accuracy(ets1, test)[,c(2,3,4,5,6,7)]

### ETS2 ###
ets2 <- ets(train, model = "ANAd", ic = c("aic"))
ets2
ets2 <- forecast(ets2, h=len_test)
plot(ets2)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets2)
residuals_ets2 <- residuals(ets2)
hist(residuals_ets2)
tsdisplay(residuals_ets2)
Box.test(residuals_ets2, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets2, lags = seq(1, 24, 1), order = length(ets2$model$par))

# Evaluating Forecast Accuracy
accuracy(ets2, test)[,c(2,3,4,5,6,7)]

### ETS3 ###
ets3 <- ets(train, model = "AAA", ic = c("aic"))
ets3
ets3 <- forecast(ets3, h=len_test)
plot(ets3)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets3)
residuals_ets3 <- residuals(ets3)
hist(residuals_ets3)
tsdisplay(residuals_ets3)
Box.test(residuals_ets3, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets3, lags = seq(1, 24, 1), order = length(ets3$model$par))

# Evaluating Forecast Accuracy
accuracy(ets3, test)[,c(2,3,4,5,6,7)]

### ETS4 ###
ets4 <- ets(train, model = "AAAd", ic = c("aic"))
ets4
ets4 <- forecast(ets4, h=len_test)
plot(ets4)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets4)
residuals_ets4 <- residuals(ets4)
hist(residuals_ets4)
tsdisplay(residuals_ets4)
Box.test(residuals_ets4, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets4, lags = seq(1, 24, 1), order = length(ets4$model$par))

# Evaluating Forecast Accuracy
accuracy(ets4, test)[,c(2,3,4,5,6,7)]

### ETS5 ###
ets5 <- ets(train, model = "MNA", ic = c("aic"))
ets5
ets5 <- forecast(ets5, h=len_test)
plot(ets5)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets5)
residuals_ets5 <- residuals(ets5)
hist(residuals_ets5)
tsdisplay(residuals_ets5)
Box.test(residuals_ets5, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets5, lags = seq(1, 24, 1), order = length(ets5$model$par))

# Evaluating Forecast Accuracy
accuracy(ets5, test)[,c(2,3,4,5,6,7)]

### ETS6 ###
ets6 <- ets(train, model = "MNAd", ic = c("aic"))
ets6
ets6 <- forecast(ets6, h=len_test)
plot(ets6)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets6)
residuals_ets6 <- residuals(ets6)
hist(residuals_ets6)
tsdisplay(residuals_ets6)
Box.test(residuals_ets6, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets6, lags = seq(1, 24, 1), order = length(ets6$model$par))

# Evaluating Forecast Accuracy
accuracy(ets6, test)[,c(2,3,4,5,6,7)]

### ETS7 ###
ets7 <- ets(train, model = "MAA", ic = c("aic"))
ets7
ets7 <- forecast(ets7, h=len_test)
plot(ets7)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets7)
residuals_ets7 <- residuals(ets7)
hist(residuals_ets7)
tsdisplay(residuals_ets7)
Box.test(residuals_ets7, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets7, lags = seq(1, 24, 1), order = length(ets7$model$par))

# Evaluating Forecast Accuracy
accuracy(ets7, test)[,c(2,3,4,5,6,7)]

### ETS8 ###
ets8 <- ets(train, model = "MAAd", ic = c("aic"))
ets8
ets8 <- forecast(ets8, h=len_test)
plot(ets8)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets8)
residuals_ets8 <- residuals(ets8)
hist(residuals_ets8)
tsdisplay(residuals_ets8)
Box.test(residuals_ets8, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets8, lags = seq(1, 24, 1), order = length(ets8$model$par))

# Evaluating Forecast Accuracy
accuracy(ets8, test)[,c(2,3,4,5,6,7)]

### Applying Auto ETS to get the Best Model Automatically ###
ets_auto <- ets(train, ic = c("aic"))
ets_auto
plot(ets_auto)
etsf_auto <- forecast(ets_auto, h=len_test)
plot(etsf_auto)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets_auto)
residuals_ets_auto <- residuals(ets_auto)
hist(residuals_ets_auto)
tsdisplay(residuals_ets_auto)
Box.test(residuals_ets_auto, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets_auto, lags = seq(1, 24, 1), order = length(ets_auto$model$par))

# Evaluating Forecast Accuracy
accuracy(etsf_auto, test)[,c(2,3,4,5,6,7)]

##########
### 6) ###
##########

# Forecasting using ARIMA Models and auto ARIMA procedure

# Applying differences to the data and identification of autocorrelations
tsdisplay(train, main = "Belgian Bankruptcy Index")
tsdisplay(diff(train, 12), main = "Seasonality differenced Belgian Bankruptcy Index")

# Applying ARIMA Models manually
arima1 <- Arima(train, order = c(1,0,1), seasonal = c(2,1,1))
tsdisplay(residuals(arima1))

arima2 <- Arima(train, order = c(3,0,1), seasonal = c(0,1,1))
tsdisplay(residuals(arima2))

arima3 <- Arima(train, order = c(3,0,1), seasonal = c(3,1,1))
tsdisplay(residuals(arima3))

arima4 <- Arima(train, order = c(3,1,1), seasonal = c(3,1,1))
tsdisplay(residuals(arima4))

arima5 <- Arima(train, order = c(3,0,3), seasonal = c(3,1,1))
tsdisplay(residuals(arima5))

arima6 <- Arima(train, order = c(3,0,3), seasonal = c(3,1,3))
tsdisplay(residuals(arima6))

# Applying auto ARIMA
arima_auto <- auto.arima(train, allowdrift = FALSE, stepwise = FALSE, approximation = FALSE)
arima_auto
arimaf_auto <- forecast(arima_auto, h=len_test)
plot(arimaf_auto)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(arima_auto)
residuals_arima_auto <- residuals(arima_auto)
hist(residuals_arima_auto)
tsdisplay(residuals_arima_auto)
Box.test(residuals_arima_auto, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_arima_auto, lags = seq(1, 24, 1), order = length(arima_auto$model$par))

# Evaluating Forecast Accuracy
accuracy(arimaf_auto, test)[,c(2,3,4,5,6,7)]

##################
### 7), 8), 9) ###
##################

# SNAIVE STL is best in our case. Kindly refer to Report for complete details!

# Generating Forecast over the Complete data using the Final model

Final_Model <- stl(Bankruptcies_log[,1], s.window="periodic", t.window = 6, robust = TRUE)

# Applying Naive Method to the STL data
Final_Forecast <- forecast(Final_Model, method = "naive") 
plot(Final_Forecast)

# Evaluating Final Forecast Accuracy
accuracy(Final_Forecast)[,c(2,3,4,5,6,7)]


###############################################################################
#### Exercise 2 - Electricity End Use in US (January 1973 to January 2021) ####
###############################################################################

# Reading and Plotting the data
data2 <- read_excel("Table_7.6_Electricity_End_Use.xlsx", sheet="Data_Formatted")
Electricity_usage <- ts(data2[2], frequency = 12, start = c(1973,1))
Electricity_usage
plot(Electricity_usage)

# Using Log Transformation
Electricity_log <- log(Electricity_usage)
plot(Electricity_log)

# Plotting the Seasonal and Monthly Trend Graphs
seasonplot(Electricity_log, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal Plot",
           ylab="Total Electricity Used",
           col=rainbow(25), 
           pch=20)

monthplot(Electricity_log, main="Seasonal Subseries Plot", ylab = "Total Electricity Used",
          xlab="Month", type="l")

# Plotting the ACF & PACF (Autocorrelation & Partial Autocorrelation)
lag.plot(Electricity_log)
tsdisplay(Electricity_log)

# Splitting the Data into Train and Test Set
train <- window(Electricity_log, end=c(2005,12))
test <- window(Electricity_log, start=c(2006,1))

# Specifying the Length of the Test Set
len_test <- length(test)

# Applying Seasonal Naive Method on Train set
snm <- snaive(train, h=len_test)
plot(snm)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(snm)
residuals_snm <- residuals(snm)
hist(residuals_snm)
tsdisplay(residuals_snm)
Box.test(residuals_snm, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_snm[-c(1:12),], lags = seq(1, 24, 1), order = 0)

# Evaluating Forecast Accuracy
accuracy(snm, test)[,c(2,3,4,5,6,7)]

### Seasonal auto ARIMA ###

# Applying auto ARIMA
arima_auto <- auto.arima(train, allowdrift = FALSE, stepwise = FALSE, approximation = FALSE)
arima_auto
arimaf_auto <- forecast(arima_auto, h=len_test)
plot(arimaf_auto)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(arima_auto)
residuals_arima_auto <- residuals(arima_auto)
hist(residuals_arima_auto)
tsdisplay(residuals_arima_auto)
Box.test(residuals_arima_auto, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_arima_auto, lags = seq(1, 24, 1), order = length(arima_auto$model$par))

# Evaluating Forecast Accuracy
accuracy(arimaf_auto, test)[,c(2,3,4,5,6,7)]

# Applying Seasonal Naive Method (Best Model) on the Complete set
Final_forecast <- snaive(Electricity_log, h=len_test)
plot(Final_forecast)

# Evaluating Final Forecast Accuracy
accuracy(Final_Forecast)[,c(2,3,4,5,6,7)]

####################################################################################################
######################################## END OF ASSIGNMENT #########################################
####################################################################################################

