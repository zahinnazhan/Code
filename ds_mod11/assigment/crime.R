library(plyr)

crime <- read.csv('C:\\Users\\Zahin Nazhan\\Downloads\\ds_mod11\\assigment\\crime_data (3).csv')
normalized_data <- scale(crime[,2:5])
summary(crime)

twss <- NULL
for (i in 2:8) {twss <- c(twss, kmeans(normalized_data, centers = i)$tot.withinss)}
twss
plot(2:8, twss, type = 'b', xlab = 'Number of Cluster', ylab = 'within group sum of squares')
title(sub = 'K-Means CLustering Scree-Plot')

fit <- kmeans(normalized_data, 5)
str(fit)
fit$cluster

final <- data.frame(fit$cluster, crime)
aggregate(crime[,2:5], by = list(fit$cluster), FUN = mean)
table(fit$cluster)