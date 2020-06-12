library(tidyverse)
library(glmnet)
library(randomForest)
library(dplyr)
library(performanceEstimation)
library(corrplot)
library(caret)
library(regclass)
library(tuneRanger)
library(mlr)
library(glmnetUtils)
library(Hmisc)
library(xtable)
library(ranger)
# Load data
load("classification.RData")

# Prepare data ####
X_train <- scale(X_train) # Centering and standardize
X_valid <- scale(X_valid)
Xdata <- as.data.frame(X_train) 
Xdata_valid <- as.data.frame(X_valid)
Ydata <- as.factor(y_train)
Ydata_valid <- as.factor(y_valid)
fulldata <- cbind(
  Ydata,
  Xdata
) %>% as.data.frame()
fulldata$Ydata <- as.factor(Ydata)

# Explanatory data analysis ####

# Frequencies of outcomes
summary(y_train) # About 1/4 are ones
par(mfrow =c(1,2))
barplot(table(Ydata),ylim = c(0,300)) # Highly unbalanced dataset
title(main = "Train data")
barplot(table(Ydata_valid),ylim = c(0,300))
title(main = "Validation data")
prop.table(table(y_train)) # Percentage of each class
prop.table(table(y_valid))

# Correlation between predictors
corr <- cor(X_train, method = "pearson")
corr[upper.tri(corr, diag = TRUE)] <- 0
C <- lapply(X = seq(1,dim(corr)[1]), function(X){
  length(which(corr[X,] > 0.7))
})
C <- unlist(C) %>% sort(decreasing = TRUE)
corrdata <- data.frame(
  X0 = length(which(C > 0)),
  X1 = length(which(C > 1)),
  X2 = length(which(C > 2)),
  X5 = length(which(C > 5)),
  X10 = length(which(C > 10)),
  X15 = length(which(C > 15)),
  X20 = length(which(C > 20))
)
xtable(corrdata)

# RF tuneranger ####
set.seed(123)
fulldata.task = makeClassifTask(data = fulldata, target = "Ydata")
estimateTimeTuneRanger(fulldata.task)
res <- tuneRanger(fulldata.task, measure = list(acc))
# optimal parameters: mtry = 203, nodesize = 13.

# RF, balancing data by under sampling n=76+76 ####

# Constructing balanced subset
set.seed(123)
n_1 <- length(which(y_train == 1))
index_1 <- which(y_train == 1)
index_0 <- which(y_train == 0) %>% sample(size = n_1)
y_train_balanced <- c(y_train[index_0], y_train[index_1])
X_train_balanced <- X_train[c(index_0, index_1),]
Ydata_balanced <- as.factor(y_train_balanced)
# Run RF on balanced data
RF_76 <- randomForest(Ydata_balanced~., mtry = 203, ntree = 800 ,data = X_train_balanced, importance = TRUE)
pred_76 <- predict(RF_76,Xdata_valid, type = "class")
conf_76 <- confusionMatrix(pred_76, reference = Ydata_valid)

# RF, balancing by inputs ####

# Run RF algorithm
set.seed(123)
RF1 <- randomForest(Ydata~., mtry = 203, nodesize = 13, data = Xdata,ntree = 800,sampsize = c('0' = 50, '1' = 50),importance = TRUE)
pred_RF1 <- predict(RF1,Xdata_valid, type = "class") # predict on validation data
conf_RF1 <- confusionMatrix(pred_RF1, reference = Ydata_valid) 

# Removing seemingly unnecessary variables
to_remove <- unique(c(which(RF1$importance[,3] <= 0),which(RF1$importance[,4] <=0)))
RF_data <- Xdata[,-to_remove] # Remove 407 variables -> 393 variables left

# Second fit
set.seed(123)
RF2 <- randomForest(Ydata~., mtry = 203, nodesize = 13, data = RF_data,ntree = 800,sampsize = c('0' = 50, '1' = 50),importance = TRUE)
pred_RF2 <- predict(RF2,Xdata_valid, type = "class") 
conf_RF2 <- confusionMatrix(pred_RF2, reference = Ydata_valid) # Improved results.

# Removing variables a second time
to_remove2 <- unique(c(which(RF2$importance[,3] <= 0),which(RF2$importance[,4] <=0)))
RF_data2 <- RF_data[,-to_remove2]
set.seed(123)
RF3 <- randomForest(Ydata~., mtry = 203, nodesize = 13, data = RF_data2,ntree = 800,sampsize = c('0' = 50, '1' = 50),importance = TRUE)
pred_RF3 <- predict(RF3,Xdata_valid, type = "class") 
conf_RF3 <- confusionMatrix(pred_RF3, reference = Ydata_valid) # worse results.

# Results RF ####

# Plot illustrating different types of balancing
par(mfrow = c(2,1))
plot(RF1, main = "Balanced sampling in algorithm", col = c("black", "red", "green"))
plot(RF_76, main = "Algorithm used on balanced data", col = c("black", "red", "green"))
# Extracting metrics
metrics_RF2 <- c(conf_RF2$overall[1], conf_RF2$byClass[c(1,2,5,11)])
metrics_RF1 <- c(conf_RF1$overall[1], conf_RF1$byClass[c(1,2,5,11)])
#Extracting top 100 variables in accuracy contribution
RF2_acc <- sort(RF2$importance[,3], decreasing = TRUE)[1:100] %>%names()
RF_top_variables <- lapply(seq(1,length(RF2_acc)), function(x){
  which(colnames(Xdata) == RF2_acc[x])
}) %>% unlist()

# GLMNET ####

# Run CV-algorithm
cv_alpha <- c(seq(0,1, length.out = 11))
set.seed(123)
GL1 <- cva.glmnet(X_train,t(y_train),alpha = cv_alpha, family = "binomial", cv.type = "min", type = "class")
#Extract the confusion matrix for the models
confs_GL1 <- lapply(X = seq(1,11), function(X){
  pred_GL1 <- predict(GL1$modlist[[X]], X_valid, s = GL1$modlist[[X]]$lambda.min, type = "class")
  confusionMatrix(as.factor(pred_GL1), reference = Ydata_valid)
})
GL1_metrics <- lapply(seq(1,11), function(x){
  c(confs_GL1[[x]]$overall[1], confs_GL1[[x]]$byClass[c(1,2,5,11)])
})
# Results Elastic net ####

# Minlossplot for CV-object
par(mfrow = c(1,1))
minlossplot(GL1, cv.type = "min", main = "Elastic net CV loss")
# Coefficient profile plot for alpha = 0.2 & 0.9
par(mfrow = c(2,1))
plot(GL1$modlist[[10]]$glmnet.fit, xvar= "lambda", main = "Alpha   =     0.9")
abline(v = log(GL1$modlist[[10]]$lambda.min))
plot(GL1$modlist[[4]]$glmnet.fit, xvar= "lambda", main = "Alpha   =     0.2")
abline(v = log(GL1$modlist[[4]]$lambda.min))
# Two of the best models according to plot minlossplot, alpha = 0.2 & 0.9
unlist(GL1_metrics[[4]])
unlist(GL1_metrics[[10]])
GL1_variables <- which(coef(GL1$modlist[[10]], s= GL1$modlist[[10]]$lambda.min) != 0)
nrvar1 <- length(GL1_variables)
nrvar2 <- length(which(coef(GL1$modlist[[4]], s= GL1$modlist[[4]]$lambda.min) != 0))
# Comparison RF and GLMNET ####

# Looking at shared predictors
shared_100 <- lapply(seq(1, length(GL1_variables)), function(x){
  which(RF_top_variables == GL1_variables[x])
})       
most_imp_variables <- GL1_variables[which(shared_100 != 0)]
