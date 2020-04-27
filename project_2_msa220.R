library(tidyverse) #general purpose
library(latex2exp) #general purpose

#color friendly palette
cbPalette <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # colour-blind friendly palette

#packages for actual computations
library(MASS)    # Contains mvrnorm to simulate multivariate normal
#                  # Use as MASS::mvrnorm is recommended since MASS::select
#                  # clashes with dplyr::select
library(RCurl)   # Helps to retrieve online datasets
library(cluster) # Provides clustering algorithms and tools for analysis
#                  # (e.g silhouette width)
library(dendextend)  # Many more possibilities for the customization of
library(factoextra)

# loading data
data <- read.csv("StudentsPerformance.csv") %>% 
  select(gender,math.score,writing.score,reading.score) 

# scaled data  
datanew <- data
datanew[which(datanew == "female"),4] <- 1.8*datanew[which(datanew == "female"),4]


# plotting math score vs reading score for male and female
ggplot(data,aes(x=math.score, y = reading.score, shape = gender,color = gender)) +
  geom_point() +
    ggtitle("Math vs Reading")
  

# same plot but for datanew
ggplot(datanew,aes(x=math.score, y = reading.score, shape = gender,color = gender)) +
  geom_point() +
    ggtitle("Math vs Reading (female reading score*1.8) ")

### DENDRO FUNCTION
plot_dendro <- function(hc, title) {
  dend <- as.dendrogram(hc)
  labels(dend) <- NULL
  dend <- dend %>%
    set("leaves_pch", 19) %>%
    set("leaves_cex", 0.25) %>%
    set("leaves_col", cbPalette[
      as.numeric(data[order.dendrogram(dend), 1]) + 1])
  par(mar=c(.5,4,1,.5), bg = "transparent",
      cex.lab = 0.8, cex.axis = 0.8, cex.main = 0.8)
  plot(dend, ylab = "Height", main = title)
  #abline(h = 6, lty = 5)
  #abline(h = 5, lty = 2)
}


# WARD METHOD
D <- dist(scale(data[,c(2,4)]))
hc_ward <- hclust(D, method = "ward.D2")
plot_dendro(hc_ward, "Ward")

sub_ward <- cutree(hc_ward,k=2)
fviz_cluster(list(data = data[,c(2,4)], cluster = sub_ward), geom = "point", main = "Clustering using Ward")

Dnew <- dist(scale(datanew[,c(2,4)]))
hc_ward_new <- hclust(Dnew, method = "ward.D2")
plot_dendro(hc_ward_new, "Ward (female reading score*1.8)")

sub_ward_new <- cutree(hc_ward_new,k=2)
fviz_cluster(list(data = datanew[,c(2,4)], cluster = sub_ward_new),geom = "point",main = "Ward (female reading score*1.8)")

#MEDIAN METHOD
hc_median <- hclust(D, method = "median")
plot_dendro(hc_median, "Median Linkage")

sub_median <- cutree(hc_median,k=2)
fviz_cluster(list(data = data[,c(2,4)], cluster = sub_median), geom = "point",main = "Median")

hc_median_new <- hclust(Dnew, method = "median")
plot_dendro(hc_median_new, "Median (female reading score*1.8)")

sub_median_new <- cutree(hc_median_new,k=2)
fviz_cluster(list(data = datanew[,c(2,4)], cluster = sub_median_new),geom = "point",main = "Median (female reading score*1.8")

#CENTROID METHOD 
hc_centroid <- hclust(D, method = "centroid")
plot_dendro(hc_centroid, "Centroid")

sub_centroid <- cutree(hc_centroid,k=2)
fviz_cluster(list(data = data[,c(2,4)], cluster = sub_centroid), geom = "point",main = "Centroid")

hc_centroid_new <- hclust(Dnew, method = "centroid")
plot_dendro(hc_centroid_new, "Centroid (female reading score*1.8)")

sub_centroid_new <- cutree(hc_centroid_new,k=2)
fviz_cluster(list(data = datanew[,c(2,4)], cluster = sub_centroid_new), geom = "point",main = "Centroid (female reading score*1.8)")

###### GMM ANALYSIS
library(mclust)

BIC <- mclustBIC(data[,c(2,4)])
plot(BIC)

### EEV
gmm_clust <- Mclust(data[,c(2,4)], 2, model = "EEV")
fviz_mclust(gmm_clust,what = "classification", geom = "point",main = "GMM (EEV)")
### VEE
gmm_clust <- Mclust(data[,c(2,4)], 2, model = "VEE")
fviz_mclust(gmm_clust,what = "classification", geom = "point",main = "GMM (VEE)")


### scaled data EEV
gmm_clust_new <- Mclust(datanew[,c(2,4)], 2, model = "VVV")
fviz_mclust(gmm_clust_new,what = "classification", geom = "point",main ="GMM (EEV) (female reading score*1:8)")

###
gmm_clust_new <- Mclust(datanew[,c(2,4)], 2, model = "EEI")
fviz_mclust(gmm_clust_new,what = "classification", geom = "point",main ="GMM (EEI) (female reading score*1:8)")


##
BIC_new <- mclustBIC(datanew[,c(2,4)])
plot(BIC_new)


