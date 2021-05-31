                    ##     Read the data; format and retrieve dimensions
                    ##     ---------------------------------------------
                    kccf <- read.csv("E:/Datasets/creditcard.csv", header = T, sep = ",")
                    kccf$Class <- as.factor(kccf$Class)
                    
                    rows <- nrow(kccf)  #  no. of rows in the data
                    cols <- ncol(kccf)  #  no. of cols in the data
                    
                    ##     Shuffle all data // Train-Test data split
                    ##     ----------------    - - - - - - - - - - - 
                    set.seed(39)         #  set seed for a good luck :)
                    
                    kccf       <- kccf[sample(rows),1:cols]
                    
                    ntr        <- as.integer(round(0.8*rows))
                    
                    kccf.train <- kccf[1:ntr,       1:cols]    #   training data set (with class)
                    kccf.test  <- kccf[(ntr+1):rows, -cols]    #   test data input
                    kccf.testc <- kccf[(ntr+1):rows,  cols]    #   test data class
                    
                    rm(kccf)                          #   keep your workspace clean
                    
                    kccf.testc <- as.data.frame(kccf.testc)
                    colnames(kccf.testc)[1] <- c("Class")
                    
                    
                    ##     Modelling with Decision Tree
                    ##     ----------------------------
                    
                    library(rpart)       #  for constructing Decision Trees

                    # Build the tree (on training data set) 
                    Start.time <- Sys.time()
                    kccf.tree  <- rpart(Class ~.,
                                        data = kccf.train,
                                        method = "class")
                    print(Sys.time() -  Start.time)
                    
                    # Make predictions
                    kccf.pred <- predict(kccf.tree, kccf.test)
                    
                    kccf.testc$Pred <- 0L
                    kccf.testc$Pred[kccf.pred[,2] > 0.5] <- 1L
                    kccf.testc$Pred <- factor(kccf.testc$Pred)
                    
                    
                    ##      Performance metrics : TP/FN/... & (AUC-ROC)
                    ##      -------------------------------------------
                    
                    library(caret)       #  for accuracy measures: Spec/Sens/...
                    
                    confusionMatrix(kccf.testc$Pred, kccf.testc$Class, positive = "1")
                    
                    
                    library(pROC)        #  for Area Under the (ROC) Curve
                    
                    kccf.testc$Class <- ordered(kccf.testc$Class, levels=c("0", "1"))
                    kccf.testc$Pred <- ordered(kccf.testc$Pred, levels=c("0", "1"))
                    
                    auc(kccf.testc$Class, kccf.testc$Pred, positive = "1")
                    
                    
                    library(rpart.plot)  #   for visualizing the trees
                    
                    # View results graphically
                    rpart.plot(kccf.tree, cex = 0.66, extra = 3,
                               type = 5, box.palette = "BuRd")
                    
                    # Other metrics (MCC or F-score)
                    
                    library(MLmetrics)   #   for Precision, Recall and F1-score
                    
                    Precision(kccf.testc$Class, kccf.testc$Pred, positive = "1")
                    Recall(kccf.testc$Class, kccf.testc$Pred, positive = "1")
                    
                    F1_Score(kccf.testc$Class, kccf.testc$Pred, positive = "1")
                    
                    library(mltools)     #   for Matthews Correlation Coefficient
                    
                    mcc(kccf.testc$Class, kccf.testc$Pred)
                    