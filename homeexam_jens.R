library(tidyverse)
library(glmnet)
library(randomForest)

# Load data
load("classification.RData")

# Explanatory data analysis
set.seed(123)
Xdata <- as.data.frame(X_train)
Xdata_valid <- as.data.frame(X_valid)
Ydata <- as.factor(y_train)
Ydata_valid <- as.factor(y_valid)
p <- dim(Xdata)[2]
n1 <-dim(Xdata)[1]
str(Xdata) # Explanatory variables have different magnitudes
summary(y_train) # About 1/4 are ones


# RF analysis ####
RF <- randomForest(Ydata~., mtry = sqrt(p), data = Xdata, importance = TRUE)
pred <- predict(RF, Xdata, type = "class")
table(pred,Ydata)
pred_valid <- predict(RF,Xdata_valid, type = "class")
mean(pred_valid == Ydata_valid)
T1 <- table(pred_valid,Y_data_valid)
# RF balanced data
n_ones <- length(which(y_train == 1))
subzero <- sample_n(Xdata[which(y_train== 0),], size = n_ones)
subones <- Xdata[which(y_train == 1),]
X_subset <- rbind(subzero,subones)
Y_subset <- as.factor(c(rep(0,n_ones), rep(1,n_ones)))
RF_balanced <- randomForest(Y_subset~., mtry = sqrt(p), data = X_subset, importance = TRUE)
pred_valid_bal <- predict(RF_balanced, Xdata_valid, type = "class")
mean(pred_valid_bal == Ydata_valid)
T2 <- table(pred_valid_bal, Ydata_valid)
# RF balanced n = 50+50
subzero <- sample_n(Xdata[which(y_train== 0),], size = 50)
subones <- sample_n(Xdata[which(y_train== 1),], size = 50)
X_subset <- rbind(subzero,subones)
Y_subset <- as.factor(c(rep(0,50), rep(1,50)))
RF_balanced_50 <- randomForest(Y_subset~., mtry = sqrt(p), data = X_subset, importance = TRUE)
pred_valid_bal_50 <- predict(RF_balanced, Xdata_valid, type = "class")
mean(pred_valid_bal_50 == Ydata_valid)
table(pred_valid_bal_50, Ydata_valid)
# Balanced accuracy
bal_acc1 <- (T1[1,1]/(T1[1,1]+T1[2,1]) + T1[2,2]/(T1[2,2]+T1[1,2])) /2
bal_acc_bal <- (T2[1,1]/(T2[1,1]+T2[2,1]) + T2[2,2]/(T2[2,2]+T2[1,2])) /2

# GLMNET analysis ####
GL <- glmnet(X_train,Ydata, alpha = 1, family = "binomial") # l_1 norm
plot(GL, xvar="lambda")
GL_cv <- cv.glmnet(X_train,t(y_train))
GL_cv$lambda.min # 0.017..
plot(GL_cv)
