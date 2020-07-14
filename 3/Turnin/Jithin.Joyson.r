
library('dplyr')
setwd(getwd())
buddymove <- read.csv("buddymove_holidayiq.csv", sep = ",", header = T, row.names = 1)

head(buddymove)

summary(buddymove)

print('Feature Scaling: Z-score (to standardize Sports)')
buddymove.transformed <- scale(buddymove)
head(buddymove.transformed)

library(cluster)
library(factoextra)

fviz_cluster(kmeans(buddymove.transformed, centers=2, nstart=1000), data=buddymove.transformed)

fviz_cluster(kmeans(buddymove.transformed, centers=3, nstart=1000), data=buddymove.transformed)

fviz_cluster(kmeans(buddymove.transformed, centers=4, nstart=1000), data=buddymove.transformed)

fviz_cluster(kmeans(buddymove.transformed, centers=5, nstart=1000), data=buddymove.transformed)

fviz_cluster(kmeans(buddymove.transformed, centers=6, nstart=1000), data=buddymove.transformed)

fviz_cluster(kmeans(buddymove.transformed, centers=7, nstart=1000), data=buddymove.transformed)

fviz_cluster(kmeans(buddymove.transformed, centers=8, nstart=1000), data=buddymove.transformed)

fviz_cluster(kmeans(buddymove.transformed, centers=9, nstart=1000), data=buddymove.transformed)

fviz_nbclust(buddymove.transformed, kmeans, method="wss")

fviz_nbclust(buddymove.transformed, kmeans, method="silhouette")

k <- kmeans(buddymove.transformed, centers=3, nstart=1000)
fviz_cluster(k, data=buddymove.transformed)

k$size

k$tot.withinss

k$withinss

cluster1 <- buddymove[which(k$cluster == 1),]
cluster2 <- buddymove[which(k$cluster == 2),]
cluster3 <- buddymove[which(k$cluster == 3),]

summary(cluster1)

summary(cluster2)

summary(cluster3)

par(mfrow=c(3,2))
boxplot(cluster1$Sports,cluster2$Sports,cluster3$Sports, main = "Sports", names = c('1','2','3'), col = c('red','green','blue'), xlab = "Clusters")
boxplot(cluster1$Religious,cluster2$Religious,cluster3$Religious, main = "Religious", names = c('1','2','3'), col = c('red','green','blue'), xlab = "Clusters")
boxplot(cluster1$Nature,cluster2$Nature,cluster3$Nature, main = "Nature", names = c('1','2','3'), col = c('red','green','blue'), xlab = "Clusters")
boxplot(cluster1$Theatre,cluster2$Theatre,cluster3$Theatre, main = "Theatre", names = c('1','2','3'), col = c('red','green','blue'), xlab = "Clusters")
boxplot(cluster1$Shopping,cluster2$Shopping,cluster3$Shopping, main = "Shopping", names = c('1','2','3'), col = c('red','green','blue'), xlab = "Clusters")
boxplot(cluster1$Picnic,cluster2$Picnic,cluster3$Picnic, main = "Picnic", names = c('1','2','3'), col = c('red','green','blue'), xlab = "Clusters")

cluster1.trans <- buddymove.transformed[which(k$cluster == 1),]
cluster2.trans <- buddymove.transformed[which(k$cluster == 2),]
cluster3.trans <- buddymove.transformed[which(k$cluster == 3),]

par(mfrow=c(1,3))
boxplot(cluster1.trans, main = "Cluster 1", names = c('SP','R','N','T','SH','P'), col = rainbow(6), xlab = "Features")
boxplot(cluster2.trans, main = "Cluster 2", names = c('SP','R','N','T','SH','P'),col = rainbow(6), xlab = "Features")
boxplot(cluster3.trans, main = "Cluster 3", names = c('SP','R','N','T','SH','P'),col = rainbow(6), xlab = "Features")

head(buddymove.transformed)

set.seed(1122)
indx <- sample(1:nrow(buddymove.transformed), 50)
data <- buddymove.transformed[indx,]
head(data)

sing_hclust <- eclust(data, "hclust", k = 1, hc_method="single")
comp_hclust <- eclust(data, "hclust", k = 1, hc_method="complete")
aver_hclust <- eclust(data, "hclust", k = 1, hc_method="average")

fviz_dend(sing_hclust, palette="jco", as.ggplot=T, main = "Single")

fviz_dend(comp_hclust, palette="jco", as.ggplot=T, main = "Complete")

fviz_dend(aver_hclust, palette="jco", as.ggplot=T, main = "Average")

#x <- fviz_dend(sing_hclust, palette="jco", as.ggplot=T, main = "Single")
plot(sing_hclust, main = "Method = Single")
abline(h = 1.7, lty = 2)

sing_hclust.2 <- eclust(data, "hclust", k = 2, hc_method="single")
comp_hclust.2 <- eclust(data, "hclust", k = 2, hc_method="complete")
aver_hclust.2 <- eclust(data, "hclust", k = 2, hc_method="average")

fviz_dend(sing_hclust.2, palette="jco", as.ggplot=T, main = "Single")

sing_hclust.2$silinfo$avg.width   

fviz_dend(comp_hclust.2, palette="jco", as.ggplot=T, main = "Complete")

comp_hclust.2$silinfo$avg.width

fviz_dend(aver_hclust.2, palette="jco", as.ggplot=T, main = "Average")

aver_hclust.2$silinfo$avg.width

library('NbClust')

clust_sing <- NbClust(data, method='single')

clust_comp <- NbClust(data, method='complete')

clust_aver <- NbClust(data, method='average')

clust_sing$All.index["2","Silhouette"]

clust_comp$All.index["2","Silhouette"]

clust_aver$All.index["2","Silhouette"]

max(sing_hclust.2$silinfo$avg.width, comp_hclust.2$silinfo$avg.width, aver_hclust.2$silinfo$avg.width)

max(clust_sing$All.index["2","Silhouette"], clust_comp$All.index["2","Silhouette"],clust_aver$All.index["2","Silhouette"])

HTRU <- read.csv("HTRU_2-small.csv", sep = ",", header = T)
head(HTRU)

X <- HTRU[1:8]
head(X)

pca <- prcomp(scale(X))

var <- pca$sdev^2
var_tot <- sum(var)
varPC.1.2 <- var[1] + var[2]
varPC.1.2/var_tot

biplot(pca, scale=0, col = c("red","blue"))

plot(x=pca$x[,'PC1'], y=pca$x[,'PC2'], xlab='PC1', ylab='PC2', col = c('blue','red')[as.factor(HTRU$class)])
legend(x="bottomright", legend = levels(as.factor(HTRU$class)), col=c("red","blue"), pch=1)

k <- kmeans(scale(X), centers=2, nstart=25)
fviz_cluster(k, data = scale(X))

k$size

barplot(k$size, col = c("blue","red"), main = "Distribution", names.arg= c("1","2"),xlab="Cluster",ylab="Observations")

nrow(HTRU[HTRU$class > 0,])

nrow(HTRU[HTRU$class < 1,])

barplot(c(nrow(HTRU[HTRU$class < 1,]),nrow(HTRU[HTRU$class > 0,])), col = c("blue","red"), main = "HTRU", names.arg= c("0","1"),xlab="Class",ylab="Observations")

cluster1 <- HTRU[which(k$cluster == 1),]
nrow(cluster1[cluster1$class > 0,])
nrow(cluster1[cluster1$class < 1,])
barplot(c(nrow(cluster1[cluster1$class < 1,]),nrow(cluster1[cluster1$class > 0,])), col = c("blue","red"), main = "Majority Cluster", names.arg= c("0","1"),xlab="Class",ylab="Observations")

k$betweenss/k$totss

s <- silhouette(k$cluster, dist(scale(X)))
mean(s[,'sil_width'])

mean(s[,'sil_width'][which(k$cluster == 1)])

mean(s[,'sil_width'][which(k$cluster == 2)])

k <- kmeans(pca$x[, 1:2], centers=2, nstart=25)

fviz_cluster(k, data = pca$x[, 1:2])

s <- silhouette(k$cluster, dist(pca$x[, 1:2]))
mean(s[,'sil_width'])

mean(s[,'sil_width'][which(k$cluster == 1)])

mean(s[,'sil_width'][which(k$cluster == 2)])
