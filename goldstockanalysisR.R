#import libraries

#for data manipulation and visualization
library(tidyverse) 
#path management
library(here)
#for creating data splits
library(rsample)

#to handle time series data structures
library(xts)
library(zoo)

#working with dates
library(lubridate)
#toolbox for timeseries data
library(tsbox)
#for modeling, testing and validating trading strategies
library(quantmod)
#for evaluating performance of models
library(yardstick)
#deep learning
library(keras)
#for predictive modeling
library(caret)
#for forecasting time series data
library(prophet)
#Technical Trading Rules for financial analysis
library(TTR)
#provides functions for time series decomposition
library(forecast)

library(dplyr)

library(tseries)




##EXPLORATORY DATA ANALYSIS

#Visualizing distribution of variables
# Histogram for 'Close'
hist(df$Close, main = "Distribution of Closing Prices", xlab = "Closing Price", col = "blue", breaks = 30)
hist(df$Volume, main = "Distribution of Volume of Gold", xlab = "Volume", col = "red", breaks = 30)
hist(df$Open, main = "Distribution of Opening Prices", xlab = "Opening Price", col = "blue", breaks = 30)
hist(df$High, main = "Distribution of Highest Prices Recorded", xlab = "Highest Price", col = "black", breaks = 30)
hist(df$Low, main = "Distribution of Lowest Prices Recorded", xlab = "Lowest Price", col = "black", breaks = 30)

# Convert 'Date' to Date type
df$Date <- as.Date(df$Date)

# Time series plot for 'Close'
plot(df$Date, df$Close, type = "l", col = "blue", xlab = "Date", ylab = "Closing Price", main = "Time Series Plot of Closing Prices")

# Time series plot for 'Volume'
plot(df$Date, df$Open, type = "l", col = "green", xlab = "Date", ylab = "Volume", main = "Time Series Plot of Opening Prices")

plot(df$Date, df$High, type = "l", col = "black", xlab = "Date", ylab = "Volume", main = "Time Series Plot of Highest Prices")
plot(df$Date, df$Low, type = "l", col = "black", xlab = "Date", ylab = "Volume", main = "Time Series Plot of Lowest Prices")


# Correlation matrix
correlation_matrix <- cor(df[, c("Close", "Volume", "Open", "High", "Low")])
print(correlation_matrix)

##there is undeniably high correlation between The closing prices, opening prices, highest and lowest recorded price on that day 
##visualize using heat map 
library(corrplot)
corrplot(correlation_matrix, method = "color")




##seasonal decomposition for Closing and Opening prices--trend, seasonality & remainder

# Convert 'Date' to a Date object
df$Date <- as.Date(df$Date)

# Create a time series object (ts_data)
ts_data <- ts(df$Close, frequency = 365)  # Adjust the frequency as needed

# Apply seasonal decomposition using stl
decomposition <- stl(ts_data, s.window = "periodic")

# Plot the decomposed components
plot(decomposition)

# Access the components (trend, seasonal, and remainder)
trend_component <- decomposition$time.series[, "trend"]
seasonal_component <- decomposition$time.series[, "seasonal"]
remainder_component <- decomposition$time.series[, "remainder"]




###ARIMA MODEL
#check for stationarity
#Augmented Dickey Fuller Test
#H0:the time series is non-stationary
#HA:the ts is stationary
adf_test_result <- adf.test(ts_data)
print(adf_test_result)

df$Month <- month(df$Date, label = TRUE)

ggplot(df, aes(x = Month, y = Close)) +
  geom_boxplot() +
  labs(title = "Seasonal Subseries Plot", x = "Month", y = "Closing Price")

#the p value is ,0.4138,greater than the significance level of 0.05.
#Therefore there is enough evidence to fail to reject the null hypothesis , suggesting that the the ts is non stationary
##difference the series to achieve stationarity

ts_data_diff <- diff(ts_data)

# Plot the differenced series
plot(ts_data_diff, main = "Differenced Series", xlab = "Time", ylab = "Differences")

adf_test_result <- adf.test(ts_data_diff)
print(adf_test_result)

#pvalue is 0.01 , it is less than 0.05, meaning we reject the null hypothesis, the ts is stationary

#lets try differencing a second time for a lower pvalue
# Second differencing
ts_data_diff2 <- diff(ts_data_diff)

# Plot the second differenced series
plot(ts_data_diff2, main = "Second Differenced Series", xlab = "Time", ylab = "Differences")

# Augmented Dickey Fuller Test for the second differenced series
library(tseries)
adf_test_result_diff2 <- adf.test(ts_data_diff2)
print(adf_test_result_diff2)





#determine ARIMA model order
acf(ts_data_diff, lag.max = 20)
pacf(ts_data_diff, lag.max = 20)

#p: order of the AR model
#d: degree of differencing
#q: order of the MA component
##differencing was done once so d=1, cutoff point for PACF = 17, so p = 16, cutoff point for ACF = 18 , so q=17

#fit model
#Fit ARIMA model with automatic order selection using AIC
# Fit ARIMA model with automatic order selection using AIC
arima_model_auto <- forecast::auto.arima(ts_data)

# Forecast with the model
forecast_values <- forecast(arima_model_auto, h = 10)

# Plot the forecast
plot(forecast_values)

# Extract forecasted time points
forecasted_time_points <- time(forecast_values)

# Align with the original dataset
forecasted_rows <- df[df$Date %in% forecasted_time_points, ]

# Extract actual closing prices
actual_closing_prices <- forecasted_rows$Close

# Calculate MSE
forecasted_values <- forecast_values$`Point Forecast`

# Calculate squared differences
squared_diff <- (actual_closing_prices - forecasted_values)^2

# Calculate Mean Squared Error (MSE)
mse <- mean(squared_diff)

# Print MSE
cat("Mean Squared Error (MSE):", mse, "\n")












##############################################################################################################################################################################################################


