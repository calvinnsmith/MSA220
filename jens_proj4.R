library(tidyverse)
library(NMF)
library(ggplot2)
library(tidyr)
library(dplyr)

#install.packages("devtools") # In case devtools isn't installed
#devtools::install_url("https://cran.r-project.org/src/contrib/Archive/ElemStatLearn/ElemStatLearn_2015.6.26.2.tar.gz (Links to an external site.)")

# Load data
data_train <- read.csv("/Users/Jensaeh/Downloads/fashion_train.csv")
data_test <- read.csv("/Users/Jensaeh/Downloads/fashion_test.csv")
set.seed(1234)
# Data prep
zerorows <- which(rowSums(t(data_train)) == 0)
X_train <- data_train[,-zerorows]
X_train <- X_train[,-1]
X_train <- t(X_train)
Y_train <- X_train[1,]
X_train_noY <- X_train[-1,]
nmf_fit_noY <- nmf(X_train_noY, rank = 10)
s <- summary(nmf_fit_noY, class=Y_train)

# NMF without response 

fits <- lapply(1:10, function(s){
  nmf(X_train_noY, rank = 10, seed = s)
})
purity_entropy_nmf <- lapply(1:5, function(x){
  summary(fits[[x]], class = Y_train)[c(4,5)]
})
 # k- means
purity_entropy_k <- tibble(
  purity = rep(0,5),
  entropy = rep(0,5)
)
for (i in 1:10){
  set.seed(i)
  fit_kmeans <- kmeans(t(X_train_noY), centers = 10)
  purity_entropy_k$purity[i] <- purity(as.factor(fit_kmeans$cluster),Y_train)
  purity_entropy_k$entropy[i] <- entropy(as.factor(fit_kmeans$cluster),Y_train)
}


