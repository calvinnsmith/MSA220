library(tidyverse)
library(NMF)
library(factoextra)
library(caret)

#### training data ####
train.data <- read_csv("fashion_train.csv",col_types = cols(.default = "i")) %>% group_by(label) %>%
  arrange(label)
train.data <- train.data[,-1]


# NMF training data
X_train <- select(train.data, starts_with("p")) %>% t() %>% as.tibble() %>% 
  filter_all(any_vars(. != 0))
Y_train <- train.data$label

# K-Means training data
X_kmeans <- t(X_train)

# calculating purity and entropy for NMF and K-means
purity_k <- numeric(10)
entropy_k <- numeric(10)
purity_nmf <- numeric(10)
entropy_nmf <- numeric(10)
for (i in 1:10){

  #NMF
  fit_nmf <- nmf(X_train, rank = 10, nrun =1)
  summary_nmf <- summary(fit_nmf, class = Y_train)
  purity_nmf[i] <- summary_nmf[4]
  entropy_nmf[i] <- summary_nmf[5]
  
  #K-Means
  fit_kmeans <- kmeans(X_kmeans,10)
  purity_k[i] <- purity(as.factor(fit_kmeans$cluster),Y_train)
  entropy_k [i]<- entropy(as.factor(fit_kmeans$cluster),Y_train)
}
purity_entropy <- data.frame(c(mean(purity_nmf),mean(entropy_nmf)),c(mean(purity_k),mean(entropy_k)))
colnames(purity_entropy) <- c("NMF","k-means")
row.names(purity_entropy) <- c("Purity","Entropy")

# function for plotting image
make_img <- function(x){
  rotate <- function(x) t(apply(x, 2, rev))
  matrix <- matrix(unlist(unname(x)),28,28)
  image(z = rotate(rotate(matrix)), x = seq(28), y = seq(28), col = gray.colors(255, start = 0, end = 1, rev = TRUE),
        axes = F, xlab = " ", ylab = " ")
}

# computing centroids for true label
X_train_full <- train.data
avg <- lapply(seq(0,9), function(i){
  round(apply(X_train_full[which(X_train_full$label==i),-1], 2, mean))
})

# and plotting
par(mfrow=c(2,5))
cat <- seq(0,9)
#cat <- c("T-shirt","Trouser", "Pullover", "Dress","Coat", "Sandal","Shirt","Sneaker","Bag","Ankle Boot")
for (i in seq(10)){
  make_img(avg[[i]])
  title(main = cat[i])
}
mtext("Centroids for true labels", side = 3, line = -1.5, outer = TRUE)

# computing centroids for NMF-clustering
fit_nmf <- nmf(X_train,rank = 10, nrun = 10)

W <- basis(fit_nmf) %>% t()
H <- coef(fit_nmf) %>% t()

#X_nmf <- train.data
X_nmf <- H%*%W
X_nmf$label <- fit_nmf@fit@H %>% t() %>% max.col()
avg_nmf <- lapply(seq(10), function(i){
  round(apply(X_nmf[which(X_nmf$label==i),-1], 2, mean))
})

# and plotting 
par(mfrow=c(2,5))
cat <- seq(10)
for (i in seq(10)){
  make_img(W[i,])
  title(main = cat[i])
}
mtext("Centroids for NMF clustering", side = 3, line = -1.5, outer = TRUE)

### Plotting KMEANS ####
fit_kmeans <- kmeans(X_kmeans,10,25)
k_centroids <- fit_kmeans$centers

X_km <- t(X_train)
X_km$label <- fit_kmeans$cluster
avg_k <- lapply(seq(10), function(i){
  round(apply(X_kmeans[which(X_km$label==i),-1], 2, mean))
})

# and plotting 
par(mfrow=c(2,5))
cat <- seq(10)
for (i in seq(10)){
  make_img(k_centroids[i,])
  title(main = cat[i])
}
mtext("Centroids for kmeans clustering", side = 3, line = -1.5, outer = TRUE)


# Confusion Matrices 
conf_kmean <- confusionMatrix(data = factor(fit_kmeans$cluster-1), reference = factor(train.data$label))
xtable(conf_kmean$table, caption = "ggg")

conf_nmf <- confusionMatrix(data = factor(X_nmf$label-1), reference = factor(Y_train))
xtable(conf_nmf$table, title ="NMF Clustering")
