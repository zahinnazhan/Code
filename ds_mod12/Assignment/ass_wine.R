library(plyr)

wine <- read.csv('C:\\Users\\Zahin Nazhan\\Downloads\\ds_mod12\\Assignment\\wine.csv')
summary(wine)

data <- scale(wine[,2:14])

pco <- princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)

summary(pco)
loadings(pco)

plot(pco)
biplot(pco)

plot(cumsum(pco$sdev * pco$sdev)* 100/ (sum(pco$sdev * pco$sdev)), type = 'b')

pco$scores
pco$score[,1:3]

final <- cbind(wine$Type, pco$scores[,1:3])
final

# Hierarchial Clustering

d <- dist(data, method = 'euclidean')
fit <- hclust(d, method = 'complete')

plot(fit, hang = -1)

group <- cutree(fit, k =3)
rect.hclust(fit, k = 3, border = 'red')

member <- as.matrix(group)

final2 <- data.frame(wine, member)
final2

aggregate(final2[, -c(1,15)], by = list(member), FUN = mean)

## kmeans clustering

twss <- NULL
for (i in 1:13) {twss <- c(twss,kmeans(data, centers=i)$tot.withinss)}
twss
plot(1:13, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   
title(sub = "K-Means Clustering Scree-Plot")

fit2 <- kmeans(data, 3)
fit2$cluster

final3 <- data.frame(wine, fit2$cluster)

aggregate(wine[,-1], by = list(fit2$cluster), FUN = mean)

table(fit2$cluster)
