# Import library
library(plyr)
library(tidyr)
library(ggplot2) 
library(cluster) 
library(graphics)

# Check what dataset available
data()
## we can see ruspini is in our dataset library

# Import dataset
data("ruspini")
attach(ruspini)
rus = ruspini
summary(rus)
plot(rus)
## Basic overview of the ruspini dataset and seems we need just a few centriods

# Determine the value of k, number of centriods/clusters
# A reasonable guess after looking at the plot, k should be less than 10
wss <- numeric(10)
for(k in 1:10) wss[k] <- sum(kmeans(rus, centers = k, nstart = 30)$withinss)
plot (1:10, wss, type = "b", xlab = "Number of cluster", ylab = "WSS")
## Look at the plot we can easily tell that we should choose 4 centriods, as the value of WSS do not decrease in any significant amount. (i.e. spotting the elbow)
## It is safe to justify that clustering the ruspini dataset in to 4 clusters would be an efficient and concise enough to explain the dataset insight

# Cluster the ruspini dataset into 4 cluster by kmeans algo, with iteration = 30
km <- kmeans(rus, centers = 4, nstart = 30)
rus$cluster<- factor(km$cluster) #merge the cluster no into the table
head(rus)

# Look at the four centriods location
centers <- data.frame(cluster =factor(1:4), km$centers)
centers

# Interpretation the clusters
km$size
## The cluster size is quite balanced, meaning extreme outlier doesn't exist
# Visualize the findings
Final <- ggplot(data = rus, aes(x=x, y=y, shape=cluster))+
  geom_point(alpha=.3)+
  geom_point(data=centers, aes(x=x, y=y), size=3,stroke=2)
Final
## A nice four clusters are shown on the graph
