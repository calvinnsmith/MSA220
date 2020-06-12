# loading required packages
library(tidyverse)
library(caret)
library(glmnet)
library(randomForest)
library(car)
library(tuneRanger)

# loading data
load("classification.RData")

### Forming the data before we start ####
colnames(X_train) <- colnames(X_train, do.NULL = FALSE, prefix = "col")
colnames(X_valid) <- colnames(X_train)
X_train <- scale(X_train)
X_valid <- scale(X_valid)

# creating training data and testing data
train.data <- data.frame(as.factor(y_train),X_train)
colnames(train.data)[1] <- "y"

test.data <- data.frame(as.factor(y_valid),X_valid)
colnames(test.data)[1] <- "y"


### exploratory data analysis ####

# unbalanced data
length(which(y_train == 0)) # 247 zeros


# correlation matrix
co_mat <- cor(X_train)
co_mat <- tbl_df(co_mat)
co_mat[lower.tri(co_mat, diag = TRUE)] <- NA
co_mat[abs(co_mat) < 0.7] <- NA
S <- which(abs(co_mat) >= 0.7, arr.ind = T)
final <- cbind(S,abs(co_mat[S]))
colnames(final) <- c("var1","var2","corr")
final <- data.frame(final) %>% arrange(var2)
example <-filter(final, var2 == 768)


# fitting a logistic regression model to training data
model.glm <- glm(y~.,family = "binomial",data = train.data, singular.ok = TRUE)
# lineaerly dependent variables
ld.vars <-  attributes(alias(model.glm)$Complete)$dimnames[[1]]

set.seed(123)
#### Penalized Logistic Regression (elastic net) ####

# finding optimal lambda and alpha using cross-validation
elastic <- train(
  X_train,as.factor(y_train), method = "glmnet", metric = "Accuracy",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10)
# fitting final model
mod.elastic <- glmnet(X_train, as.factor(y_train), alpha = elastic$bestTune$alpha, family = "binomial",
                lambda = elastic$bestTune$lambda)

#non-zero coefficients and 10 biggest
mod.elastic.coef <- data.frame(which(mod.elastic$beta != 0),mod.elastic$beta[which(mod.elastic$beta != 0)])
colnames(mod.elastic.coef) <- c("var","beta")
mod.elastic.coef <- arrange(mod.elastic.coef, beta)
coef.ten.big <- mod.elastic.coef[63:72,] %>% arrange(desc(beta))

# computing predictions for the final model on the validation data set
predictions <- elastic %>% predict(X_valid)

#confusion marix and evaluation scores
c_elastic <- confusionMatrix(predictions,as.factor(y_valid))


#### Random Forest #####

#  random forest algorithm 
set.seed(500)
rf <- randomForest(y~., data = train.data,ntree = 600,mtry = 100,replace = TRUE,strata = as.factor(train.data$y),sampsize = c('0' = 66, '1' = 76),nodesize = 5,importance = TRUE, proximity = TRUE)
plot(rf, main = "OOB Error rate vs no. trees")

# predictions
rf.pred = predict(rf, newdata=test.data)
c_rf <- confusionMatrix(rf.pred,as.factor(y_valid))

# variable importance 
geeny <- importance(rf, type = 2)
acc <- data.frame(importance(rf, type =1))
acc$index <- seq(1,800)
varImpPlot(rf, sort = TRUE, type = 1, n.var = 20)












