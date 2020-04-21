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
# dendrograms

data <- read.csv("StudentsPerformance.csv")

data1 <- data %>% select(parental.level.of.education,math.score,writing.score,reading.score) 

#%>%
  filter(parental.level.of.education == c("master's degree","bachelor's degree","high school"))




ggplot(data,aes(x=math.score, y = reading.score, shape = parental.level.of.education,color = parental.level.of.education)) +
  geom_point()

D <- dist(scale(iris[,1:4]))
hc_complete <- hclust(D, method = "complete")
