Bank_mktng_kmeans <- read.csv(file.choose(), header=TRUE)

colSums(is.na(Bank_mktng_kmeans))

summary(Bank_mktng_kmeans)

### As the Dataset Features have mix weight, we will scale the variables

Bank_mktng_kmeans_scale<- scale(Bank_mktng_kmeans[,1:7], center = TRUE, scale = TRUE)
summary(Bank_mktng_kmeans_scale)

Bank_mktng_kmeans_scale<- as.data.frame(Bank_mktng_kmeans_scale)
### Calculate optimum number of clusters using silhouette method

silhouette_score <- function(k){
  km <- kmeans(Bank_mktng_kmeans_scale, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(Bank_mktng_kmeans_scale))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

wss <-c()

## Identifying the optimal number of clusters form WSS

wssplot <- function(data, nc=15, seed=123){
  wss <- c()
  for (i in 1:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(Bank_mktng_kmeans_scale,nc=10)


### Start actual creation of clusters

kmeans.clus = kmeans(x=Bank_mktng_kmeans_scale, centers = 2)
kmeans.clus


# Another Cluster Plot
library(cluster)

clusplot(Bank_mktng_kmeans_scale[,1:7], kmeans.clus$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)


## profiling the clusters
####
#####
Bank_mktng_kmeans_scale$Clusters <- kmeans.clus$cluster
View(Bank_mktng_kmeans_scale)

aggr = aggregate(Bank_mktng_kmeans_scale[,-c(8)],list(Bank_mktng_kmeans_scale$Clusters),mean)

aggr
# Silhouette coefficient of observations

sil <- silhouette(kmeans.clus$cluster, dist(Bank_mktng_kmeans_scale))
head(sil[, 1:3], 10)


summary(sil)
##Final data
Bank_mktng_kmeans_scale_new<-cbind(Bank_mktng_kmeans[,1:7],kmeans.clus$cluster,sil[, 2:3])

write.csv(Bank_mktng_kmeans_scale_new,file="Bank_Marketing_kmeans.csv")
