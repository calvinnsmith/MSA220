library(tidyverse)
library(latex2exp)
library(MASS)
library(glmnet)
library(nnet)
library(FNN)

##### FIRST DATASET #### 

data <- read.csv("MedGPA.csv") # reading gpa data
data <- as_tibble(data)
data$Acceptance <- as.factor(data$Acceptance)
data$Sex <- as.factor(data$Sex)

#accepted <- gpa_data[which(gpa_data$Acceptance == 1),] #students accepted
#denied <- gpa_data[which(gpa_data$Acceptance == 0),] #student not accpeted

#mean_gpa_accepted <- mean(accepted$GPA)
#mean_gpa_denied <- mean(denied$GPA)

#plot(gpa_data$Acceptance,gpa_data$GPA)

#number of males and females almost equal, pretty balanced dataset
#males <- gpa_data[which(gpa_data$Sex == 'M'),]
#females <- gpa_data[which(gpa_data$Sex == 'F'),]

#Acceptance <- gpa_data$Acceptance
#x1 <- gpa_data$GPA
#x2 <- gpa_data$Apps
#model <- glm(Acceptance ~ x1 + x2, data = gpa_data, family = binomial)
#glm.probs <- predict(model, type = 'response')
#glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

#### k-fold cross validation ####
set.seed(123)
k <- 5 #number of folds
n <- nrow(data)/k
data <- data[sample(nrow(data),nrow(data),replace=F),] #randomly shuffled dataset
data$Sex <- ifelse(data$Sex == "M",1,0) #convert M/F to 1/0 for KNN to work
sensitivity_glm <- numeric(k)
sensitivity_lda <- numeric(k)
sensitivity_knn <- numeric(k)
count <- 0
for (i in seq(1,nrow(data),n)){
  count <- count + 1
  test_data <- data[i:(i+n-1),]
  train_data <- data[-(i:(i+n-1)),]
  
  ### LOGISTIC REGRESSION ###
  glm_fit <- glm(Acceptance ~ GPA + Sex -1, data = train_data, family = binomial(link = "logit"))
  glm_probs <- predict(glm_fit,newdata = test_data)
  glm_pred <- as.factor(ifelse(1 / (1 + exp(-unname(glm_probs))) <= 0.5, 0, 1))
  conf_matrix_glm <- table(test_data$Acceptance,glm_pred)
  ### LDA ###
  lda_fit <- MASS::lda(Acceptance ~ GPA + Sex, data = train_data)
  lda_pred <- predict(lda_fit,newdata = test_data,type = 'response')
  conf_matrix_lda <- table(test_data$Acceptance,lda_pred$class)
  ### KNN ###
  knn_pred <- FNN::knn(
    train_data[,c(4,6)],   # training data (variables)
    test_data[,c(4,6)],          # test data (variables)
    train_data$Acceptance,   # training data (classes)
    k = 10)             # k
    conf_matrix_knn <- table(test_data$Acceptance,knn_pred)

  sensitivity_glm[count] <- conf_matrix_glm[2,2]/(conf_matrix_glm[2,2] + conf_matrix_glm[2,1])
  sensitivity_lda[count] <- conf_matrix_lda[2,2]/(conf_matrix_lda[2,2] + conf_matrix_lda[2,1])
  sensitivity_knn[count] <- conf_matrix_knn[2,2]/(conf_matrix_knn[2,2] + conf_matrix_knn[2,1])
  
}
sensitivity_glm <- sum(sensitivity_glm)/k
sensitivity_lda <- sum(sensitivity_lda)/k
sensitivity_knn <- sum(sensitivity_knn)/k

##### SECOND DATASET #####
data <- read.csv("Handwriting.csv")
data <- as_tibble(data)
data <- data[,-c(1,2)]
data <- na.omit(data)
data$Gender <- as.factor(data$Gender) 


set.seed(123)
k <- 5 #number of folds
n <- nrow(data)/k
data <- data[sample(nrow(data),nrow(data),replace=F),] #randomly shuffled dataset
sensitivity_glm <- numeric(k)
sensitivity_lda <- numeric(k)
sensitivity_knn <- numeric(k)
count <- 0
for (i in seq(1,nrow(data),n)){
  count <- count + 1
  test_data <- data[i:(i+(n-1)),]
  train_data <- data[-(i:(i+(n-1))),]
  
  ### LOGISTIC REGRESSION ###
  glm_fit <- glm(Gender ~., data = train_data, family = binomial(link = "logit"))
  glm_probs <- predict(glm_fit,newdata = test_data)
  glm_pred <- as.factor(ifelse(1 / (1 + exp(-unname(glm_probs))) <= 0.5, 0, 1))
  conf_matrix_glm <- table(test_data$Gender,glm_pred)
  ### LDA ###
  lda_fit <- MASS::lda(Gender ~. ,data = train_data)
  lda_pred <- predict(lda_fit,newdata = test_data,type = 'response')
  conf_matrix_lda <- table(test_data$Gender,lda_pred$class)
  ### KNN ###
  knn_pred <- FNN::knn(
    train_data[,-1],   # training data (variables)
    test_data[,-1],          # test data (variables)
    train_data$Gender,   # training data (classes)
    k = 10)             # k
  conf_matrix_knn <- table(test_data$Gender,knn_pred)
  
  sensitivity_glm[count] <- conf_matrix_glm[2,2]/(conf_matrix_glm[2,2] + conf_matrix_glm[2,1])
  sensitivity_lda[count] <- conf_matrix_lda[2,2]/(conf_matrix_lda[2,2] + conf_matrix_lda[2,1])
  sensitivity_knn[count] <- conf_matrix_knn[2,2]/(conf_matrix_knn[2,2] + conf_matrix_knn[2,1])
  
}
sensitivity_glm <- sum(sensitivity_glm)/k
sensitivity_lda <- sum(sensitivity_lda)/k
sensitivity_knn <- sum(sensitivity_knn)/k
