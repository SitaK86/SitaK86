##Heirarchical Clustering on the given data
Bank_marketing <- read.csv(file.choose(), header=TRUE)

dim(Bank_marketing)

colSums(is.na(Bank_marketing))

summary(Bank_marketing)

cor(Bank_marketing)

library(DataExplorer)

plot_density(Bank_marketing)
plot_histogram(Bank_marketing)
### As the Dataset Features have mix weight, we will scale the variables

Bank_marketing_scale<- scale(Bank_marketing[,1:7], center = TRUE, scale = TRUE)
summary(Bank_marketing_scale)

#Heirarchical Clustering method
dd <- dist(Bank_marketing_scale[,1:7], method ="euclidean")
res.hclust <- hclust(dd, method = "complete")

plot(res.hclust)

cluster.height<-res.hclust$height
cluster.height<- sort(cluster.height, decreasing = TRUE)
plot(cluster.height)

plot(res.hclust)
rect.hclust(res.hclust, k=3, border="red")

Clusters<-cutree(res.hclust, k=3)
Bank_marketing_New<-cbind(Bank_marketing_scale[,1:7],Clusters)
Bank_marketing_New= as.data.frame(Bank_marketing_New)

names(Bank_marketing_New)

attach(Bank_marketing_New)

group1<- subset(Bank_marketing_New, Clusters==1)
group2<- subset(Bank_marketing_New, Clusters==2)
group3<- subset(Bank_marketing_New,Clusters==3)


group1
group2
group3

## plotting the clusters


library(cluster)


clusplot(Bank_marketing_New[,-8], Clusters, 
         color=TRUE, shade=TRUE, labels=2, lines=1)


#Profiling

aggr = aggregate(Bank_marketing_New[,-c(8)],list(Clusters),mean)

aggr


# Silhouette coefficient of observations

sil <- silhouette(Clusters ,dist(Bank_marketing_New))
head(sil[, 1:3], 10)

# Summary of silhouette analysis
summary(sil)

si.sum <- summary(sil)

# Average silhouette width of each cluster
si.sum$clus.avg.widths

# The total average (mean of all individual silhouette widths)
si.sum$avg.width

# The size of each clusters
si.sum$clus.sizes

Bank_marketing_New2<-cbind(Bank_marketing[,1:7],Clusters,sil[, 2:3])

write.csv(Bank_marketing_New2,file="Bank_Marketing.csv")
getwd()







