install.packages("plyr")
library(plyr)
x <-  runif(50) # generating 50 random numbers
x
y <-  runif(50) # generating 50 random numbers 
y
data <- cbind(x,y) 
data
plot(data)

km <- kmeans(data,3) #kmeans clustering
str(km)

install.packages("animation")
library(animation)

km <- kmeans.ani(data, 3)
km$centers

library(readxl)
input <- read_excel("C:\\Users\\Zahin Nazhan\\Downloads\\ds_mod11\\University_Clustering.xlsx")
mydata <- input[,c(1,3:8)]
normalized_data <- scale(mydata[,2:7])

#elbow curve & k ~ sqrt(n/2) to decide the k value
twss <- NULL
for (i in 2:8) {twss <- c(twss,kmeans(normalized_data, centers=i)$tot.withinss)}
twss
plot(2:8, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- kmeans(normalized_data, 5) # 3 cluster solution
str(fit)
fit$cluster
final <- data.frame(fit$cluster, mydata) # append cluster membership

aggregate(mydata[,2:7], by=list(fit$cluster), FUN=mean)
