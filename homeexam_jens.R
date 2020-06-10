library(tidyverse)
library(glmnet)
library(randomForest)
library(dplyr)
library(ROSE)
library(performanceEstimation)
library(corrplot)
library(caret)
library(regclass)

# Load data
load("classification.RData")

# Prepare data
Xdata <- as.data.frame(X_train)
Xdata_valid <- as.data.frame(X_valid)
Ydata <- as.factor(y_train)
Ydata_valid <- as.factor(y_valid)
Xdata <- scale(Xdata) # Centering and standardize

# Explanatory data analysis ####
str(Xdata) # Explanatory variables have very different magnitudes
summary(y_train) # About 1/4 are ones
barplot(table(Ydata),ylim = c(0,300)) # Highly unbalanced dataset
prop.table(table(y_train)) # Percentage of each class

corrmatrix <- cor(Xdata) - diag(800)
length(which(abs(corrmatrix) > 0.7))/2 # 1407 high correlations 

corr <- cor(X_train, method = "pearson")
highlyCorrelated <- findCorrelation(corr, cutoff=0.75)


# No balancing RF ####
set.seed(123)
RF <- randomForest(Ydata~., mtry = sqrt(p), data = Xdata, importance = TRUE)
pred_valid <- predict(RF,Xdata_valid, type = "class") # No ones.
mean(pred_valid == Ydata_valid)
T1 <- table(pred_valid,Y_data_valid)
confusionMatrix(pred_valid, reference = Y_data_valid)
accuracy.meas(Y_data_valid,predicted = pred_valid)

# Balancing data by under sampling 76+76 ####
set.seed(123)
n_1 <- length(which(y_train == 1))
index_1 <- which(y_train == 1)
index_0 <- which(y_train == 0) %>% sample(size = n_1)
y_train_balanced <- c(y_train[index_0], y_train[index_1])
X_train_balanced <- X_train[c(index_0, index_1),]
Ydata_balanced <- as.factor(y_train_balanced)

RF_76 <- randomForest(Ydata_balanced~., mtry = sqrt(p), ntree = 800 ,data = X_train_balanced, importance = TRUE)

pred_valid_76 <- predict(RF_76,Xdata_valid, type = "class")
mean(pred_valid_76 == Ydata_valid)
T2 <- table(pred_valid_76,Y_data_valid)
accuracy.meas(Y_data_valid,predicted = pred_valid_76)


# Balancing data by under sampling 50+50 ####
set.seed(123)
n_2 <- 50
index_1 <- which(y_train == 1) %>% sample(size = n_2)
index_0 <- which(y_train == 0) %>% sample(size = n_2)
y_train_balanced <- c(y_train[index_0], y_train[index_1])
X_train_balanced <- X_train[c(index_0, index_1),]
Ydata_balanced <- as.factor(y_train_balanced)

RF_50 <- randomForest(Ydata_balanced~., mtry = sqrt(p), data = X_train_balanced, importance = TRUE)
which(importance(RF_50)[,3] == 0) # Variables with little importance

pred_valid_50 <- predict(RF_50,Xdata_valid, type = "class")
mean(pred_valid_50 == Ydata_valid)
T3 <- table(pred_valid_50,Y_data_valid)
accuracy.meas(Y_data_valid,predicted = pred_valid_50)



# Balancing samplesize in formula ####
set.seed(123)
RF_form <- randomForest(Ydata~., mtry = sqrt(p), data = Xdata,ntree = 800,replace = TRUE,classwt = NULL,sampsize = c('0' = 50, '1' = 50),importance = TRUE, proximity = TRUE)
pred_valid_form <- predict(RF_form,Xdata_valid, type = "class") 
mean(pred_valid_form == Ydata_valid)
T4 <- table(pred_valid_form,Y_data_valid)
accuracy.meas(Y_data_valid,predicted = pred_valid_form)



# Accuracy / Balanced accuracy ####
acc1 <- (T1[1,1]+T1[2,2])/175
acc2 <- (T2[1,1]+T2[2,2])/175
acc3 <- (T3[1,1]+T3[2,2])/175
acc4 <- (T4[1,1]+T4[2,2])/175

bal_acc1 <- (T1[1,1]/(T1[1,1]+T1[2,1]) + T1[2,2]/(T1[2,2]+T1[1,2])) /2
bal_acc2 <- (T2[1,1]/(T2[1,1]+T2[2,1]) + T2[2,2]/(T2[2,2]+T2[1,2])) /2
bal_acc3 <- (T3[1,1]/(T3[1,1]+T3[2,1]) + T3[2,2]/(T3[2,2]+T3[1,2])) /2
bal_acc4 <- (T4[1,1]/(T4[1,1]+T4[2,1]) + T4[2,2]/(T4[2,2]+T4[1,2])) /2

# RF balance input
newdata <- as.data.frame(cbind(Ydata, X_train))
subset_SMOTE <- SMOTE(as.factor(newdata[,1])~.,newdata, perc.over = 100)


# GLMNET analysis ####
set.seed(123)
X_train_glm <- scale(X_train) #Standardize + Center
X_valid_glm <- scale(X_valid)
GL <- glmnet(X_train_glm,Ydata, alpha = 1, family = "binomial") # l_1 norm
plot(GL, xvar="lambda")
GL_cv <- cv.glmnet(X_train,t(y_train))
opt_lambda <- GL_cv$lambda.min # 0.017..
plot(GL_cv)
coef(GL, s = opt_lambda)
which(coef(GL, s = opt_lambda)!= 0)

pred_glm <- predict(GL, X_valid_glm,s = opt_lambda, type = "class")
T5 <- table(pred_glm,Y_data_valid)
acc5 <- (T5[1,1]+T5[2,2])/175
bal_acc5 <- (T5[1,1]/(T5[1,1]+T5[2,1]) + T5[2,2]/(T5[2,2]+T5[1,2])) /2
confusionMatrix(as.factor(pred_glm), reference = Y_data_valid)



