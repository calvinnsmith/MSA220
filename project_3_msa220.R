library(tidyverse)
library(latex2exp)
library(gglasso)
library(MASS)
#### SIMULATING DATA #####
p <- 20 #number of parameters
n <- 500 #number of observations

I <- diag(p) # diagonal matrix I_p
 # R^p vector of coefficients
set.seed(123)
# simulating beta element-wise
beta <- mvrnorm(n = 1,mu = rep(0,p),Sigma = I)

#generating data with diagonal covariance matrix
X <- mvrnorm(n, matrix(0,1,p), diag(1/sqrt(p), nrow=p))

##
original_grouping <- rep(1:4,each = 5)

Beta <- data.frame(beta = beta,group = original_grouping)

org_zeros <- c(1,3)
ord_ind <- which(Beta[,2] %in% org_zeros)
Beta[ord_ind,1] <- 0

gam <- 20

sigma <- sqrt(norm(X%*%Beta[,1], type = "2")^2/(n-1))/(gam)

error <- rnorm(n,mean = 0,sd = sigma)

SNR <- norm(X%*%Beta[,1], type = "2")/norm(error, type = "2")

response <- X%*%Beta[,1] + error

### gglasso original groups ####
org_lasso <- gglasso(X,response,group = original_grouping,loss = "ls" ,intercept = FALSE)
org_lasso_cv <- cv.gglasso(X,response,group = original_grouping,intercept = FALSE)

org_est_coef <- coef(org_lasso_cv,s = "lambda.1se")

plot(org_lasso,group = TRUE, main = "Group lasso with correct grouping")
abline(v = log(org_lasso_cv$lambda.1se))


### gglasso random false groups ####
random_grouping <- sample(original_grouping,20)
random_lasso <- gglasso(X,response,group = random_grouping,loss = "ls" ,intercept = FALSE)
random_lasso_cv <- cv.gglasso(X,response,group = random_grouping,pred.loss = "L1",intercept = FALSE)

random_est_coef <- coef(random_lasso_cv)

plot(random_lasso, group = TRUE, main = "Group lasso (sampling groups without replacemen)")
abline(v = log(random_lasso_cv$lambda.1se))

### gglasso changing group structure ###
new_grouping <- sample(original_grouping,20, replace = TRUE )
new_lasso <- gglasso(X,response,group = new_grouping,loss = "ls" ,intercept = FALSE)
new_lasso_cv <- cv.gglasso(X,response,group = new_grouping,intercept = FALSE)

plot(new_lasso, group = TRUE, main = "Group lasso (sampling with replacement)")
abline(v = log(new_lasso_cv$lambda.1se))
### EXPLORING CORRELATION ####
####
cor_within <- 0.95
cov_within <- matrix(0,p,p)
cov_within[1:5,1:5] <- cor_within
cov_within[6:10,6:10] <- cor_within
cov_within[11:15,11:15] <- cor_within
cov_within[16:20,16:20] <- cor_within
diag(cov_within) <- 1

X_within <- mvrnorm(n, matrix(0,1,p), cov_within)
sigma_within <- sqrt(norm(X_within%*%Beta[,1], type = "2")^2/(n-1))/(gam)
error_within <- rnorm(n,mean = 0,sd = sigma_within)
response_within <- X_within%*%Beta[,1] + error_within

cor_between <- 0.6
cov_between <- matrix(0,p,p)
cov_between[which(cov_within == 0)] <- cor_between
diag(cov_between) <- 1
cov_between[which(cov_between == 0)] <- 0.5

X_between <- mvrnorm(n, matrix(0,1,p), cov_between)
sigma_between <- sqrt(norm(X_between%*%Beta[,1], type = "2")^2/(n-1))/(gam)
error_between <- rnorm(n,mean = 0,sd = sigma_between)
response_between <- X_between%*%Beta[,1] + error_between

### gglasso with within correlation high ###
corr_gglasso_cv <- cv.gglasso(X_within,response_within, group = original_grouping, loss = "ls", intercept = FALSE)
corr_gglasso <- gglasso(X_within,response_within,group = original_grouping, intercept = FALSE)
plot(corr_gglasso, group = TRUE,main = "Group lasso with high correlation within groups ")
abline(v = log(corr_gglasso_cv$lambda.1se))

### gglasso with between correlation high ###
corr_gglasso_cv_1 <- cv.gglasso(X_between,response_between, group = original_grouping, loss = "ls", intercept = FALSE)
corr_gglasso_1 <- gglasso(X_between,response_between,group = original_grouping, intercept = FALSE)
plot(corr_gglasso_1, group = TRUE, main = "Group lasso with correlation between groups ")
abline(v = log(corr_gglasso_cv_1$lambda.min))
