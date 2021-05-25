# ---------------------------------------------
# 1. Create lagged POS values
# 2. Create Store.No and Format indicators
# 3. Run XGB model w/ holdout validation
# ---------------------------------------------
# ---------------------------------------------


# Pull in actuals to be processed with training data
actuals <- read.csv('C:/Users/joshi/OneDrive/Desktop/16108514/Data analysis/POS_Actuals.csv', header = T, stringsAsFactors = F)
str(actuals)

actuals$Store.No <- as.character(actuals$Store.No)
actuals$Date <- as.Date(actuals$Date, format = "%m/%d/%Y")

xgb.actuals <- actuals %>%
  select(-Total.POS.Actuals) %>%
  mutate(Total.POS = NA)

# ---------------------------------------------
# 1. Create lagged POS values
# ---------------------------------------------
gbdat.00 <- tsdat.10 %>%
  select(Date, Store.No, Format, Sq.Ft, Store.Holiday, Total.POS) %>%
  rbind(xgb.actuals) %>%
  arrange(Store.No) %>%
  group_by(Store.No) %>%
  mutate(Lag1 = lag(Total.POS),
         Lag2 = lag(Lag1),
         Lag3 = lag(Lag2),
         Lag4 = lag(Lag3),
         Lag5 = lag(Lag4),
         Lag6 = lag(Lag5),
         Lag7 = lag(Lag6),
         Lag8 = lag(Lag7)) %>%
  ungroup()

# ---------------------------------------------
# 2. Create Store.No and Format indicators
# ---------------------------------------------
# Create Store.No indicator
for (i in 1:length(stores)) {
  store <- stores[i]
  gbdat.00[[paste('Store',store,sep = '')]] <- ifelse(gbdat.00$Store.No == store, 1, 0)
}

# Creat Format indicator
formats <- unique(gbdat.00$Format)
for (i in 1:length(formats)) {
  format <- formats[i]
  gbdat.00[[paste('Format',format,sep = '')]] <- ifelse(gbdat.00$Format == format, 1, 0)
}



# ---------------------------------------------
# 3. Run XGB model w/ holdout validation
# ---------------------------------------------
# Clean up data frame and for use in xgboost
### For training the model
gb.past <- gbdat.00 %>%
  select_if(is.numeric) %>%
  filter(!is.na(Total.POS) & !is.na(Lag8))


### For predicted values
gb.future <- gbdat.00 %>%
  filter(is.na(Total.POS))
gb.future$Lag3 <- ifelse(is.na(gb.future$Lag3), gb.future$Lag4, gb.future$Lag3)
gb.future$Lag2 <- ifelse(is.na(gb.future$Lag2), gb.future$Lag3, gb.future$Lag2)
gb.future$Lag1 <- ifelse(is.na(gb.future$Lag1), gb.future$Lag2, gb.future$Lag1)


### Split to training and testing sets
gb.part <- sample(nrow(gb.past), size = nrow(gb.past)-80)
gbtrain <- gb.past[gb.part,]
gbtest <- gb.past[-gb.part,]

# Create model and capture test MAPE
mod1 <- train(Total.POS ~ .,
              data = gbtrain,
              na.action = na.exclude,
              method = 'xgbLinear',
              metric = 'RMSE',
              trControl = trainControl('cv', number = 10))

xgb.holdout.mape <- MAPE(gbtest$Total.POS, predict(mod1, newdata = gbtest))

# Plot holdout values and predicted holdout values
plot(gbtest$Total.POS, predict(mod1, newdata = gbtest))
abline(a=0, b=1, col = 'red')



