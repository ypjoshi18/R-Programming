# ---------------------------------------------
# 1. Compile results
# 2. Create ensemble forecast
# 3. Evaluate overall method
# ---------------------------------------------
# ---------------------------------------------

# ---------------------------------------------
# 1. Compile results
# ---------------------------------------------
### Compile Actuals and Predicted values
xgb.evaluate <- gb.future %>%
  mutate(XGB.Predict = predict(mod1, newdata = gb.future[,-1])) %>%
  select(Date, Store.No, XGB.Predict) %>%
  left_join(actuals[,c(1,2,5)], by = c('Date', 'Store.No'))



# ---------------------------------------------
# 2. Create ensemble forecast
# ---------------------------------------------
# Ensemble forecast
ts.avg.mape # from 80 holdout observations
xgb.holdout.mape

final.fcst <- xgb.evaluate %>%
  left_join(ts.fcst[,c(1,2,4)], by = c('Date', 'Store.No')) %>%
  mutate(Final.Prediction = (1.15*XGB.Predict + 0.85*TS.Predict)/2) # weights determined by the holdout MAPEs



# ---------------------------------------------
# 3. Evaluate overall method
# ---------------------------------------------
# Check MAPEs
final.mape <- MAPE(final.fcst$Total.POS.Actuals, final.fcst$Final.Prediction) # Ensemble
ts.mape <- MAPE(final.fcst$Total.POS.Actuals, final.fcst$TS.Predict) #TS only
xgb.mape <- MAPE(xgb.evaluate$Total.POS.Actuals, xgb.evaluate$XGB.Predict) #XGB only

# Plot final predicted values against actuals
plot(final.fcst$Total.POS.Actuals, final.fcst$Final.Prediction, main = 'Ensemble')
plot(final.fcst$Total.POS.Actuals, final.fcst$TS.Predict, main = 'Time Series') 
plot(final.fcst$Total.POS.Actuals, final.fcst$XGB.Predict, main = 'XGB') 

