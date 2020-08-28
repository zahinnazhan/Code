library(plyr)
library(readxl)
airline <- read_excel('C:\\Users\\Zahin Nazhan\\Downloads\\ds_mod11\\assigment\\EastWestAirlines (4).xlsx', sheet = 'data')

colnames(airline)
str(airline)
summary(airline)

normalized_data <- scale(airline[,2:12])

twss <- NULL
for(i in 2:12) {twss <- c(twss, kmeans(normalized_data, centers = i)$tot.whitinss)}
twss
plot(2:12, twss, type = 'b', xlab = 'Number of Cluster', ylab = 'within group sum squares')
title(sub = 'K-Means Clustering Scree Plot')

fit <- kmeans(normalized_data, 3)
str(fit)
fit$cluster
final <- data.frame(fit$cluster, airline)

aggregate(airline[,2:12], by = list(fit$cluster), FUN = mean)
table(fit$cluster)
