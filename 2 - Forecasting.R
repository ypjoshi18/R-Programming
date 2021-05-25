# ---------------------------------------------
# 1. Create a time series forecast
# 2. Select the 'best' time series forecast
# 3. Forecast all stores and find average MAPE
# ---------------------------------------------
# ---------------------------------------------

# Function to calculate MAPE (Mean Absolute Percent Error)
MAPE <- function(actuals, predicted) {
  MAPE <- mean((abs(actuals - predicted)/predicted)*100)
}

fcst.dates <- c('2012-10-05', '2012-10-12', '2012-10-19', '2012-10-26')

library(zoo)

# Use a loop to run the forecasting code for each store
for (i in 1:length(stores)) {
  
  # Extract data for one store and create ts object
  ts <- tsdat.10 %>%
    filter(Store.No == stores[i]) %>%
    select(Total.POS) %>%
    na.trim.ts(side = 'left')
  ts.short <- ts(ts[1:(length(ts)-4)], frequency = 1, start = c(year(min(tsdat.10$Date)),week(min(tsdat.10$Date))))
  ts.long <- ts(ts, frequency = 1, start = c(year(min(tsdat.10$Date)),week(min(tsdat.10$Date))))
  
  # Create ARIMA forecast and store holdout MAPE and out-of-sample forecast values
  arima.short <- auto.arima(ts.short, test = 'adf', ic = 'aic')
  arima.holdout.mape <- MAPE(ts[(length(ts)-3):length(ts)], as.vector(forecast(arima.short, h = 4)$mean))
  arima.fcst <- forecast(ts.long, model = arima.short, h = 4)$mean
  
  # Create ESM forecast and store MAPE
  esm.short <- ets(ts.short, model = 'ZZN', ic = 'aic', allow.multiplicative.trend = TRUE)
  esm.holdout.mape <- MAPE(ts[(length(ts)-3):length(ts)], as.vector(forecast(esm.short, h = 4)$mean))
  esm.fcst <- forecast(ts.long, model = esm.short, h = 4, use.initial.values = TRUE)$mean
  
  # Select better model
  if (arima.holdout.mape < esm.holdout.mape) {
    fcst <- cbind(fcst.dates, rep(stores[i],4), rep('ARIMA', 4), as.vector(arima.fcst), rep(arima.holdout.mape,4))
  } else {
    fcst <- cbind(fcst.dates, rep(stores[i],4), rep('ESM', 4), as.vector(esm.fcst), rep(esm.holdout.mape,4))
  }
  
  # Collect all forecasts in one data set
  if (i == 1) {
    ts.fcst <- fcst
  } else {
    ts.fcst <- rbind(ts.fcst,fcst)
  }
  
}
ts.fcst <- as.data.frame(ts.fcst)
colnames(ts.fcst) <- c('Date', 'Store.No', 'Model', 'TS.Predict', 'Holdout.MAPE')
ts.fcst$Date <- as.Date(ts.fcst$Date, format = '%Y-%m-%d')
ts.fcst$Store.No <- as.character(ts.fcst$Store.No)
ts.fcst$TS.Predict <- as.numeric(as.character(ts.fcst$TS.Predict))
ts.fcst$Holdout.MAPE <- as.numeric(as.character(ts.fcst$Holdout.MAPE))

# Calculate average MAPE
ts.avg.mape <- mean(as.numeric(ts.fcst[,5]))

plot(arima.short$x, col = 'red')
lines(arima.short$fitted, col = 'blue')
plot(forecast(arima.short, h = 4))

plot(esm.short$x, col = 'red')
lines(esm.short$fitted, col = 'blue')
plot(forecast(esm.short, h = 4))
