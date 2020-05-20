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
data_train <- group_by(data_train, label) 
data_train <- arrange(data_train, by = label)
# Data prep
zerorows <- which(rowSums(t(data_train)) == 0)
X_train <- data_train[,-zerorows]
X_train <- X_train[,-1]
X_train <- t(X_train)
Y_train <- X_train[1,]
X_train_noY <- X_train[-1,]
nmf_fit_noY <- nmf(X_train_noY, rank = 10)
s <- summary(nmf_fit_noY, class=Y_train)
# Function for plotting image
make_img <- function(x){
  rotate <- function(x) t(apply(x, 2, rev))
  matrix <- matrix(unlist(unname(x)),28,28)
  image(z = rotate(rotate(matrix)), x = seq(28), y = seq(28), col = gray.colors(255, start = 0, end = 1, rev = TRUE))
}
# Original averages
avg <- lapply(seq(0,9), function(i){
  round(apply(data_train[which(data_train$label==i),-c(1,2)], 2, mean))
})
# Plot averages of all labels
par(mfrow = c(2,5))
lapply(1:10, function(i){
  make_img(avg[i])
})

# NMF without response 
fits <- lapply(1:10, function(s){
  nmf(X_train_noY, rank = 10, seed = s)
})
purity_entropy_nmf <- lapply(1:10, function(x){
  summary(fits[[x]], class = Y_train)[c(4,5)]
})
# nr 1 best purity
# Add zerorows ( pixels )
zerorows <- as.numeric(zerorows)
W <- fits[[1]]@fit@W
W <- rbind(
  W[1:zerorows[1],],
  rep(0,dim(W)[2]),
  W[(zerorows[1]+1):zerorows[2],],
  rep(0,dim(W)[2]),
  W[(zerorows[2]+1):dim(W)[1],]
)
# Plot cluster centers NMF highest purity
par(mfrow = c(2,5))
lapply(1:10, function(i){
  make_img(W[,i])
})


# k- means
fit_kmeans <- lapply(1:10, function(s){
  set.seed(s)
  kmeans(t(X_train_noY), centers = 10)
})
purity_entropy_k <- lapply(1:10, function(x){
  cbind(purity(as.factor(fit_kmeans[[x]]$cluster),Y_train),entropy(as.factor(fit_kmeans[[x]]$cluster),Y_train))
})

