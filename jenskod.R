library(ggplot2)
library(tidyverse)
library(dendextend)
library(MASS)
library(RCurl)
library(cluster)
library(mclust)
library(latex2exp)
library(xtable)
library(factoextra)
cbPalette <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Load data
ogdata <- read.csv("StudentsPerformance.csv")
data <- read.csv("StudentsPerformance.csv") %>% select(gender, math.score, reading.score, writing.score, race.ethnicity)
comp_gender <- as.numeric(data[,1])
male_ind <- which(comp_gender == 2) 
comp_gender[male_ind] <- 1
comp_gender[-male_ind] <- 2

# Overview
ggplot(data=data, aes(x = math.score, y = reading.score, color = gender)) +
  geom_point()
ggsave("plotofdata.png", path = "~/MSA220/jensas")

#### Linkage testing ####

# function for plotting
plot_dendro <- function(hc, title) {
  dend <- as.dendrogram(hc)
  labels(dend) <- NULL
  dend <- dend %>%
    set("leaves_pch", 19) %>%
    set("leaves_cex", 0.5) %>%
    set("leaves_col", cbPalette[
      as.numeric(data[order.dendrogram(dend), 1]) + 3])
  par(mar=c(.5,4,1,.5), bg = "transparent",
      cex.lab = 0.8, cex.axis = 0.8, cex.main = 0.8)
  plot(dend, ylab = "Height", main = title)
  abline(h = 6, lty = 5)
  abline(h = 5, lty = 2)
}
# compute distance matrix
D <- dist(scale(data[,2:3]))

# Single Linkage
hc_single <- hclust(D, method = "single")
#plot_dendro(hc_single, "Single Linkage")
sub_single <- cutree(hc_single, k = 2)
fviz_cluster(list(data = data[,c(2,3)], cluster = sub_single), geom = "point", main = "Single Linkage")

# Average linkage
hc_avg <- hclust(D, method = "average")
#plot_dendro(hc_avg, "Average Linkage")
sub_avg <- cutree(hc_avg, k = 2)
fviz_cluster(list(data = data[,c(2,3)], cluster = sub_avg), geom = "point", main = "Average Linkage")

# Complete linkage
hc_complete <- hclust(D, method = "complete")
#plot_dendro(hc_complete, "Complete Linkage")
sub_complete <- cutree(hc_complete, k = 2)
fviz_cluster(list(data = data[,c(2,3)], cluster = sub_complete), geom = "point", main = "Average Linkage")

# Centroid linkage
hc_centroid <- hclust(D, method = "centroid")
#plot_dendro(hc_centroid, "Centroid Linkage")
sub_centroid <- cutree(hc_centroid, k = 2)
fviz_cluster(list(data = data[,c(2,3)], cluster = sub_centroid), geom = "point", main = "Centroid Linkage")

# Ward linkage
hc_ward <- hclust(D, method = "ward.D2")
#plot_dendro(hc_ward, "Ward linkage")
sub_ward <- cutree(hc_ward, k=2)
fviz_cluster(list(data = data[,c(2,3)], cluster = sub_ward), geom = "point", main = "Ward Linkage")

# Mod data
datanew <- data
datanew[which(data[,1] == "female"),3] <- datanew[which(data[,1] == "female"),3] * 1.8
Dnew <- dist(scale(datanew[,2:3]))
ggplot(data = data, aes(x = math.score, y = reading.score, color = gender)) + 
  geom_point()


#### Covariance testing ####

# Fit GMM
gmm_clust <- Mclust(data[,2:3], 1:10)

# BIC plot 
fviz_mclust(gmm_clust, "BIC", palette = "jco")
summary(gmm_clust)

# Fit GMM for 2 clusters
gmm_clust2 <- Mclust(data[,2:3], 2)
summary(gmm_clust2$BIC) # Best 3 cov in order: VEE , VEV, VVE

# Frame with all cov and corrsp. bic
BIC_cov <- tibble(
  bic = as.numeric(gmm_clust2$BIC[order(gmm_clust2$BIC)]) ,
  cov = as.factor(attr(gmm_clust$BIC,"dimnames")[[2]])[order(gmm_clust2$BIC)]
)
worst <- BIC_cov[1,] # EVI
best <- BIC_cov[14,] # VEE

# VEE BEST
gmm_VEE <- Mclust(data[,2:3], 2, modelNames = "VEE")
fviz_mclust(gmm_VEE, geom = "point", main = "VEE")

# VEV 2nd BEST
gmm_VEV <- Mclust(data[,2:3], 2, modelNames = "VEV")
fviz_mclust(gmm_VEV, geom = "point")

# VVE 3nd BEST
gmm_VVE <- Mclust(data[,2:3], 2, modelNames = "VVE")
fviz_mclust(gmm_VVE, geom = "point")

# EEV Intuition
gmm_EEV <- Mclust(data[,2:3], 2, modelNames = "EEV")
fviz_mclust(gmm_EEV, geom = "point")

# EEE test
gmm_EEE <- Mclust(data[,2:3], 2 ,modelNames = "EEE")
fviz_mclust(gmm_EEE, geom = "point")

# EEI
gmm_EEI <- Mclust(data[,2:3], 2 ,modelNames = "EEI")
fviz_mclust(gmm_EEI, geom = "point")

# EVI WORST
gmm_EVI <- Mclust(data[,2:3], 2, modelNames = "EVI")
fviz_mclust(gmm_EVI, geom = "point")
correct <- length(which(gmm_EVI$classification == comp_gender))/length(comp_gender)


       
       