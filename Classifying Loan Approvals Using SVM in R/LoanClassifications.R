loan_data00 <- read.csv("C:/Users/joshi/OneDrive/Desktop/16110378/resource/Documents/LoanData_Train.csv")
str(loan_data00)

loan_data00$Credit_History <- as.character(loan_data00$Credit_History)

library(tidyverse)
# Distributions of the numeric variables
num_vars <- loan_data00 %>%
  select_if(is.numeric)
names(num_vars)

summary(num_vars)
# LoanAmount has 22 missing values
# Loan_Amount_Term has 14 missing values

sort(unique(loan_data00$Loan_Amount_Term))
loan_data00$Loan_Amount_Term <- as.character(loan_data00$Loan_Amount_Term)
num_vars <- num_vars %>% select(-Loan_Amount_Term)

# Distribution of the categorical variables
cat_vars <- loan_data00 %>%
  select_if(is.character) %>%
  select(-c(Loan_ID,Loan_Status))
names(cat_vars)

apply(cat_vars, 2, table)

for(i in 1:length(cat_vars)) {
  print(table(loan_data00$Loan_Status, loan_data00[,names(cat_vars)[i]]))
}


# Imputation for categorical variables
cat_props <- as.data.frame(table(loan_data00$Gender[which(!is.na(loan_data00$Gender))]) / 
                             length(loan_data00$Gender[which(!is.na(loan_data00$Gender))]))
cat_props

values <- as.character(cat_props$Var1)
props <- cat_props$Freq
values
props
sample(values, size = 1, prob = props)


for (i in 1:length(cat_vars)) {
  # Select column and only non-missing values
  var <- loan_data00[which(!is.na(loan_data00[,names(cat_vars)[i]])),names(cat_vars)[i]]
  
  # Capture proportions
  cat_props <- as.data.frame(table(var) / length(var))
  values <- as.character(cat_props$var)
  props <- cat_props$Freq
  
  # Impute missing values based on proportions
  loan_data00[,names(cat_vars)[i]] <- ifelse(is.na(loan_data00[,names(cat_vars)[i]]),
                                             sample(values,1,prob = props),
                                             loan_data00[,names(cat_vars)[i]])
  
  # Health check
  print(sum(is.na(loan_data00[,names(cat_vars)[i]])))
}

apply(cat_vars, 2, table)



# Imputation for numerical variables
summary(num_vars)

## look at boxplots of Loan Amount by each categorical variable
for (i in 1:length(cat_vars)) {
  depvar <- loan_data00[,names(cat_vars)[i]]
  means <- loan_data00 %>%
    select(LoanAmount, names(cat_vars)[i]) %>%
    group_by_(names(cat_vars)[i]) %>%
    summarise(Mean = mean(LoanAmount, na.rm = T))
  print(means)
  
  boxplot(loan_data00$LoanAmount ~ depvar, main = names(cat_vars)[i], ylab = 'LoanAmount')
  points(1:length(unique(depvar)), means$Mean, pch = 19, col = 'red')
}

# Impute missing values for Loan Amount, based on groupings from Dependents
loan_data10 <- loan_data00 %>%
  group_by(Dependents) %>%
  mutate(LoanAmount = ifelse(is.na(LoanAmount),
                             round(mean(LoanAmount, na.rm = T)),
                             LoanAmount)) %>%
  ungroup() %>%
  as.data.frame()

summary(loan_data10$LoanAmount)



# Feature engineering and other processing
loan_data20 <- loan_data10 %>%
  mutate(TotalIncome = ifelse(CoapplicantIncome == 0, ApplicantIncome, ApplicantIncome+CoapplicantIncome),
         CoapplicantIncome = ifelse(CoapplicantIncome == 0, 0, 1),
         LoanRatio = round(LoanAmount / TotalIncome, 3))

hist(loan_data20$TotalIncome)
hist(loan_data20$LoanAmount)
loan_data20$TotalIncome <- log(loan_data20$TotalIncome)
loan_data20$LoanAmount <- log(loan_data20$LoanAmount)


# standardize all numeric variables
loan_data20$TotalIncome <- as.vector(scale(loan_data20$TotalIncome))
loan_data20$LoanAmount <- as.vector(scale(loan_data20$LoanAmount))
loan_data20$LoanRatio <- as.vector(scale(loan_data20$LoanRatio))


# convert all categorical variables to 0/1 indicators
loan_data30 <- loan_data20 %>%
  select(c(Gender,Married,Dependents,Education,Self_Employed,Loan_Amount_Term,
           Credit_History,Property_Area,Loan_Status,TotalIncome,LoanAmount,LoanRatio)) 

# Convert loan term back to numeric
loan_data30$Loan_Amount_Term <- as.numeric(loan_data30$Loan_Amount_Term)

for (j in 1:length(names(loan_data30))) {
  if (!is.numeric(loan_data30[,names(loan_data30)[j]])) {
    levels <- unique(loan_data30[,names(loan_data30)[j]])
    
    if (length(levels) > 2) {
      for (k in 1:length(levels)) {
        loan_data30[,paste(names(loan_data30)[j],levels[k],sep='')] <- ifelse(loan_data30[,names(loan_data30)[j]] == levels[k],1,0)
      }
    } else {
      loan_data30[,paste(names(loan_data30)[j],levels[1],sep='')] <- ifelse(loan_data30[,names(loan_data30)[j]] == levels[1],1,0)
    }
  } else {
    loan_data30[,names(loan_data30)[j]] <- loan_data30[,names(loan_data30)[j]]
  }
}

# convert response to a factor
loan_data30$Loan_StatusY <- as.factor(loan_data30$Loan_StatusY)

# final feature selection for input data
loan_data_final <- loan_data30 %>%
  select(-c(Gender,Married,Dependents,Education,Self_Employed,Credit_History,Property_Area,Loan_Status))




# Initial modeling
set.seed(123)
sample.index <- sample(nrow(loan_data_final), 0.8*nrow(loan_data_final), replace = F)
train <- loan_data_final[sample.index,]
test <- loan_data_final[-sample.index,]


library(e1071)
svm.model <- svm(Loan_StatusY ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'linear',
                 )
svm.model

# predict the testing data
pred.new <- predict(svm.model, newdata = test[,-17])

# confusion matrix
results <- table(test[,17],pred.new)
results

# misclassification rate
1 - sum(diag(results)) / sum(results)


# visualize the results
plotdata <- as.data.frame(cbind(LoanRatio = test$LoanRatio, 
                                TotalIncome = test$TotalIncome,
                                Actual = test[,17], 
                                Predicted = pred.new)) %>%
  mutate(Result = ifelse(Actual == Predicted,'TRUE','FALSE'))

plot(plotdata$TotalIncome, plotdata$LoanRatio, xlab = 'TotalIncome', ylab = 'LoanRatio')
points(plotdata$TotalIncome, plotdata$LoanRatio, pch = 19, col = ifelse(plotdata$Result == 'TRUE', 'red', 'green'))



# Feature selection
library(Boruta)

feature.selection <- Boruta(Loan_StatusY ~ ., data = train) # runs random forests in the background
feature.selection


# new model w/ the 4 confirmed important variables
svm4 <- svm(Loan_StatusY ~ Credit_History1 + LoanAmount + LoanRatio + TotalIncome,
            data = train,
            type = 'C-classification',
            kernel = 'linear')
pred4 <- predict(svm4, newdata = test[,-17])
results4 <- table(test[,17], pred4)
results
1 - sum(diag(results4)) / sum(results4)




svm5 <- svm(Loan_StatusY ~ Credit_History1 + LoanRatio,
            data = train,
            type = 'C-classification',
            kernel = 'linear')
pred5 <- predict(svm5, newdata = test[,-17])
results5 <- table(test[,17], pred5)
results
1 - sum(diag(results5)) / sum(results5)


attStats(feature.selection)








