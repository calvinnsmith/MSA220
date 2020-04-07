library(tidyverse)
library(dplyr)
library(latex2exp)
library(MASS)
library(glmnet)
library(nnet)
library(FNN)


##### FIRST DATASET #### 

data <- read.csv("MedGPA.csv") # reading gpa data
data <- as_tibble(data) %>%
  select(Accept,Apps,GPA)

#### normalizing data #####
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data$Apps <- normalize(data$Apps)
data$GPA <- normalize(data$GPA)

#### PLOTTING GPA VS Apps ####
boundary <- c(2.4,4.2)
boundary1 <- c(0,25)

ggplot() +
  geom_point(
    aes(x = GPA, y = Apps, colour = Accept, fill = Accept),
    data = data, size = 1.5) +
  scale_colour_manual("Accept", values = cbPalette[-1]) +
  scale_fill_manual("Accept", values = cbPalette[-1], guide = FALSE) +
  scale_x_continuous(
    "GPA",lim = boundary, expand = c(0, 0)) +
  scale_y_continuous("Applications",lim = boundary1, expand = c(0, 0)) +
  #theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-10,0,5,0),
    plot.margin = margin(0,0,0,0),
    panel.grid = element_blank(),
    axis.title = element_text(size = 9),
    legend.title = element_text(size = 9))


#### k-fold cross validation ####
set.seed(123)
k <- 5 #number of folds
n <- nrow(data)/k
data <- data[sample(nrow(data),nrow(data),replace=F),] #randomly shuffled dataset
logistic <- numeric(k)
lda <- numeric(k)
knn <- numeric(k)
count <- 0
for (i in seq(1,nrow(data),n)){
  count <- count + 1
  test_data <- data[i:(i+n-1),]
  train_data <- data[-(i:(i+n-1)),]
  
  ### LOGISTIC REGRESSION ###
  glm_fit <- glm(Accept ~ GPA + Apps, data = train_data, family = binomial(link = "logit"))
  glm_probs <- predict(glm_fit,newdata = test_data)
  glm_pred <- as.factor(ifelse(1 / (1 + exp(-unname(glm_probs))) <= 0.5, "A", "D"))
  logistic[count] <- mean(glm_pred != test_data$Accept)
  ### LDA ###
  lda_fit <- MASS::lda(Accept ~ GPA + Apps, data = train_data)
  lda_pred <- predict(lda_fit,newdata = test_data,type = 'response')$class
  lda[count] <- mean(lda_pred != test_data$Accept) 
  ### KNN ###
  knn_pred <- FNN::knn(
    train_data[,2:3],   # training data (variables)
    test_data[,2:3],          # test data (variables)
    train_data$Accept,   # training data (classes)
    k = 5)             # k
    knn[count] <- mean(knn_pred != test_data$Accept)
}
std <- c(sd(logistic)/5,sd(knn)/5,sd(lda)/5) %>% round(.,3) 
means <- c(mean(logistic),mean(knn),mean(lda)) %>% round(.,3)
cv_res_acceptance <- rbind(means,std)  
colnames(cv_res_acceptance) <- c("logistic","knn(k=5)","lda")
##### PLOTTING #####
h <- 0.05 #Step-width
x1_arr <- seq(0, 25, by = h)
x2_arr <- seq(2.6, 4, by = h)
data_test <- expand.grid(x1_arr, x2_arr)
colnames(data_test) <- c("Apps", "GPA")
data_test <- as_tibble(data_test)

# Fit LDA model
fit_lda <- MASS::lda(Accept ~ GPA + Apps, data)
class_pred_lda <- predict(fit_lda, data_test)$class

# Fit logistic model
fit_logistic <- glm(
  Accept ~ GPA + Apps ,data = data, family = binomial(link = "logit"))
pred_logistic <- predict(fit_logistic, data_test)
class_pred_logistic <- as.factor(ifelse(
  1 / (1 + exp(-unname(pred_logistic))) <= 0.5, "A", "D"))

# Fit kNN with k = 10 model
class_pred_knn <- FNN::knn(
  data[,2:3],   # training data (variables)
  data_test,          # test data (variables)
  data$Accept,   # training data (classes)
  k = 5)             # k


#predictions for plotting
data_pred <- tibble(
  Accept = as.factor(c(class_pred_knn, class_pred_logistic,class_pred_lda)),
  GPA = rep(data_test$GPA, times = 3),
  Apps = rep(data_test$Apps, times = 3),
  type = factor(
    rep(c("kNN (k = 5)", "Logistic", "LDA"), each = nrow(data_test)),
    levels = c("kNN (k = 5)", "Logistic", "LDA")))





# plotting predictions and decision boundary
ggplot() +
  geom_tile(
    aes(x = GPA, y = Apps, fill = Accept),
    data = data_pred, alpha = 0.5,
    width = h, height = h,
    colour = "transparent") +
  geom_point(
    aes(x = GPA, y = Apps, colour = Accept, fill = Accept),
    data = data, size = .8) +
  facet_wrap(~ type, ncol = 3) +
  scale_colour_manual("Accept", values = cbPalette[-1]) +
  scale_fill_manual("Accept", values = cbPalette[-1], guide = FALSE) +
  scale_x_continuous(
    "GPA",lim = boundary, expand = c(0, 0)) +
  scale_y_continuous("Apps",lim = boundary1, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-10,0,5,0),
    plot.margin = margin(0,0,0,0),
    panel.grid = element_blank(),
    axis.title = element_text(size = 9),
    legend.title = element_text(size = 9))





##### SECOND DATASET #####
data <- read.csv("Handwriting.csv")
data <- as_tibble(data)
data <- data[,-c(1,2,9)]
data <- na.omit(data)
data$Gender <- as.factor(data$Gender) 

##### PLOTTING SECOND DATASET #####
boundary <- c(10,105)
boundary1 <- c(0,105)

ggplot() +
  geom_point(
    aes(x = FemaleID, y = MaleID, colour = Gender, fill = Gender),
    data = data, size = 1.5) +
  scale_colour_manual("Gender", values = cbPalette[-1]) +
  scale_fill_manual("Gender", values = cbPalette[-1], guide = FALSE) +
  scale_x_continuous(
    "FemaleID",lim = boundary, expand = c(0, 0)) +
  scale_y_continuous("MaleID",lim = boundary1, expand = c(0, 0)) +
  #theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-10,0,5,0),
    plot.margin = margin(0,0,0,0),
    panel.grid = element_blank(),
    axis.title = element_text(size = 9),
    legend.title = element_text(size = 9))







set.seed(123)
k <- 5 #number of folds
n <- nrow(data)/k
data <- data[sample(nrow(data),nrow(data),replace=F),] #randomly shuffled dataset
logistic <- numeric(k)
lda <- numeric(k)
knn <- numeric(k)
count <- 0
for (i in seq(1,nrow(data),n)){
  count <- count + 1
  test_data <- data[i:(i+(n-1)),]
  train_data <- data[-(i:(i+(n-1))),]
  
  ### LOGISTIC REGRESSION ###
  glm_fit <- glm(Gender ~., data = train_data, family = binomial(link = "logit"))
  glm_probs <- predict(glm_fit,newdata = test_data)
  glm_pred <- as.factor(ifelse(1 / (1 + exp(-unname(glm_probs))) <= 0.5, 0, 1))
  logistic[count] <- mean(glm_pred != test_data$Gender)
  ### LDA ###
  lda_fit <- MASS::lda(Gender ~. ,data = train_data)
  lda_pred <- predict(lda_fit,newdata = test_data,type = 'response')$class
  lda[count] <- mean(lda_pred != test_data$Gender)
  ### KNN ###
  knn_pred <- FNN::knn(
    train_data[,-1],   # training data (variables)
    test_data[,-1],          # test data (variables)
    train_data$Gender,   # training data (classes)
    k = 10)             # k
  knn[count] <- mean(knn_pred != test_data$Gender)
}
std <- c(sd(logistic)/5,sd(knn)/5,sd(lda)/5) %>% round(.,3)
means <- c(mean(logistic),mean(knn),mean(lda)) %>% round(.,3)
cv_res_gender <- rbind(means,std)  
colnames(cv_res_gender) <- c("logistic","knn(k=10)","lda")

#### PLOTTING GENDER ####
h <- 0.2 #Step-width
x1_arr <- seq(10, 100, by = h)
x2_arr <- seq(0, 100, by = h)
data_test <- expand.grid(x1_arr, x2_arr)
colnames(data_test) <- c("FemaleID", "MaleID")
data_test <- as_tibble(data_test)

# Fit LDA model
fit_lda <- MASS::lda(Gender ~ FemaleID + MaleID, data)
class_pred_lda <- predict(fit_lda, data_test)$class

# Fit logistic model
fit_logistic <- glm(
  Gender ~ FemaleID + MaleID ,data = data, family = binomial(link = "logit"))
pred_logistic <- predict(fit_logistic, data_test)
class_pred_logistic <- as.factor(ifelse(
  1 / (1 + exp(-unname(pred_logistic))) <= 0.5, 0, 1))

# Fit kNN with k = 10 model
class_pred_knn <- FNN::knn(
  data[,4:5],   # training data (variables)
  data_test,          # test data (variables)
  data$Gender,   # training data (classes)
  k = 10)             # k


#predictions for plotting
data_pred <- tibble(
  Gender = as.factor(c(class_pred_knn, class_pred_logistic,class_pred_lda)),
  FemaleID = rep(data_test$FemaleID, times = 3),
  MaleID = rep(data_test$MaleID, times = 3),
  type = factor(
    rep(c("kNN (k = 10)", "Logistic", "LDA"), each = nrow(data_test)),
    levels = c("kNN (k = 10)", "Logistic", "LDA")))

levels(data$Gender) <-revalue(data$Gender,c("0"="2","1" = "1"))

boundary <- c(0,105)


# plotting predictions and decision boundary
ggplot() +
  geom_tile(
    aes(x = FemaleID, y = MaleID, fill = Gender),
    data = data_pred, alpha = 0.5,
    width = h, height = h,
    colour = "transparent") +
  geom_point(
    aes(x = FemaleID, y = MaleID, colour = Gender, fill = Gender),
    data = data, size = .8) +
  facet_wrap(~ type, ncol = 3) +
  scale_colour_manual("Gender", values = cbPalette[-1]) +
  scale_fill_manual("Gender", values = cbPalette[-1], guide = FALSE) +
  scale_x_continuous(
    "FemaleID",lim = boundary, expand = c(0, 0)) +
  scale_y_continuous("MaleID",lim = boundary, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-10,0,5,0),
    plot.margin = margin(0,0,0,0),
    panel.grid = element_blank(),
    axis.title = element_text(size = 9),
    legend.title = element_text(size = 9))







##### THIRD DATASET #####
data <- read.csv("vote92.csv")
data <- data[,-1]
data <- data[,-9]
data <- filter(data, vote != "Perot")
data %>% select(vote,dem,rep,female,persfinance,natlecon) %>% as.factor()


set.seed(123)
k <- 6 #number of folds
n <- nrow(data)/k
data <- data[sample(nrow(data),nrow(data),replace=F),] #randomly shuffled dataset
logistic <- numeric(k)
knn <- numeric(k)
lda <- numeric(k)
count <- 0
for (i in seq(1,nrow(data),n)){
  count <- count + 1
  test_data <- data[i:(i+(n-1)),]
  train_data <- data[-(i:(i+(n-1))),]
  
  ### LOGISTIC REGRESSION ###
  glm_fit <- glm(vote ~., data = train_data, family = binomial(link = "logit"))
  glm_probs <- predict(glm_fit,newdata = test_data)
  glm_pred <- as.factor(ifelse(1 / (1 + exp(-unname(glm_probs))) <= 0.5, "Bush", "Clinton"))
  logistic[count] <- mean(glm_pred != test_data$vote)
  ### LDA ###
  lda_fit <- MASS::lda(vote ~. ,data = train_data)
  lda_pred <- predict(lda_fit,newdata = test_data,type = 'response')$class
  lda[count] <- mean(lda_pred != test_data$vote)
  ### KNN ###
  knn_pred <- FNN::knn(
    train_data[,-1],   # training data (variables)
    test_data[,-1],          # test data (variables)
    train_data$vote,   # training data (classes)
    k = 10)             # k
  knn[count] <- mean(knn_pred != test_data$vote)
}
std <- c(sd(logistic),sd(knn),sd(lda)) %>% round(.,3)
means <- c(mean(logistic),mean(knn),mean(lda)) %>% round(.,3)
cv_res_vote <- rbind(means,std)  
colnames(cv_res_vote) <- c("logistic","knn(k=10)","lda")

  