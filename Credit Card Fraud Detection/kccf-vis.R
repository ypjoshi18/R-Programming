kccf <- read.csv("E:/Datasets/creditcard.csv", header = T, sep = ",")
kccf$Class <- as.factor(kccf$Class)

#Visualization of data
kccf.true <- kccf[kccf$Class == 0,]
kccf.false <- kccf[kccf$Class == 1,]
library(ggplot2)
ggplot() + 
  geom_density(data=kccf.true,
               aes(x=Time), color="blue",
               fill="blue", alpha=0.12) +
  geom_density(data=kccf.false,
               aes(x=Time), color="red",
               fill="red", alpha=0.12)
          
##  Get an idea of the imbalance in class distribution
                    
summary(kccf$Class)
                    
                    
##  Explore to see if there are any missing values in the data
                  
nrow(kccf[!complete.cases(kccf), ])
colnames(kccf)[colSums(is.na(kccf)) > 0]

summary(kccf$Class)
