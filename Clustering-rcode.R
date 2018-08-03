rm(list = ls(all=TRUE))
setwd("G:/INSOFE Internship/ML algorithm/Data sets for clustering")
iris_data <- read.csv("Iris.csv")

install.packages("ggplot2")
library(ggplot2)

#ggplot(iris_data, aes(SepalLengthCm,SepalWidthCm, color = Species))+geom_point()

ggplot(iris_data, aes(PetalLengthCm,PetalWidthCm, color = Species))+geom_point()

  
## applying hierarchical clustering algorithm with its default complete linkage
hcluster_comp <- hclust(dist(iris_data[,2:5]))
plot(hcluster_comp)

# from the plot obtained wee can see that the best choice for total number
# of clusters is 3 or 4
# so we cut the dendrogram tree with required clusters using cutree()

cut_hcluster_comp <- cutree(hcluster_comp,3)
plot(cut_hcluster_comp)

## comparing the results with original species
table(cut_hcluster_comp,iris_data$Species)

#The algorithm is able to identify Iris-setosa, Iris-versicolor correctly but
#had trouble with Iris-virginica



## applying hierarchical clustering algorithm with single linkage
hcluster_single <- hclust(dist(iris_data[,2:5]), method = "single")
plot(hcluster_single)
cut_hcluster_single <- cutree(hcluster_single,3)
plot(cut_hcluster_single)
table(cut_hcluster_single, iris_data$Species)

# with single linkage the algorithm identified Iris-versicolor and Iris-virginica
# into a single cluster



## applying hierarchical clustering algorithm with average linkage
hcluster_average <- hclust(dist(iris_data[,2:5]), method = "average")
plot(hcluster_average)
cut_hcluster_average <- cutree(hcluster_average,3)
plot(cut_hcluster_average)
table(cut_hcluster_average, iris_data$Species)

# with average linkage the algorithm identifies only Iris-setosa and Iris-versicolor
# correctly

## applying hierarchical clustering algorithm with median linkage
hcluster_median <- hclust(dist(iris_data[,2:5]), method = "median")
plot(hcluster_median)

cut_hcluster_median <- cutree(hcluster_median,3)
plot(cut_hcluster_median)
table(cut_hcluster_median, iris_data$Species)

## applying hierarchical clustering algorithm with centroid linkage
hcluster_centroid <- hclust(dist(iris_data[,2:5]), method = "centroid")
plot(hcluster_centroid)
cut_hcluster_centroid <- cutree(hcluster_centroid,3)
plot(cut_hcluster_centroid)
table(cut_hcluster_centroid, iris_data$Species)

## applying hierarchical clustering algorithm with ward.D linkage
hcluster_wardD <- hclust(dist(iris_data[,2:5]), method = "ward.D")
plot(hcluster_wardD)
cut_hcluster_wardD <- cutree(hcluster_wardD,3)
#cut_hcluster_wardD
plot(cut_hcluster_wardD)
table(cut_hcluster_wardD, iris_data$Species)

# with ward.D linkage the algorithm identifies only Iris-setosa and Iris-versicolor
# correctly

# --------------------------------   k-means   ---------------------------------------  #

kmeans_default <- kmeans(iris_data[,2:5],3,nstart = 20)
library(cluster)
clusplot(iris_data, kmeans_default$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,labels=3, lines = 0)

table(kmeans_default$cluster, iris_data$Species)

# K-means:  Determine number of clusters by considering the withinness measure
wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(iris_data[,2:5],centers=i)$withinss)
}

plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


original <- iris_data$Species
original <- ifelse(iris_data$Species == "Iris-setosa",1,
                   ifelse(iris_data$Species == "Iris-versicolor",2,3))



install.packages("fossil")
library(fossil)
stabilitycheck <- adj.rand.index(original, kmeans_default$cluster)
stabilitycheck

install.packages("clusteval")
library(clusteval)
Stabindex <- cluster_similarity(original, kmeans_default$cluster, similarity = "jaccard", method="independence")
Stabindex


# ------------------------- k-medoids ----------------------------------------
library(cluster)
#Pam: partitioning around medoids
set.seed(12)
pamx <- pam(iris_data[,2:5], k = 3)
summary(pamx)
pamx$medoids
plot(pamx)

table(pamx$clustering,iris_data$Species)
# ------------------------ dbscan --------------------------------------------
install.packages("dbscan")
library(dbscan)
#dbscan_clust <- dbscan(iris_data[,4:5],eps = 0.8)
#hullplot(iris_data[,4:5], dbscan_clust$cluster)

dbscan_clust <- dbscan(iris_data[,2:5],eps = 0.6)
hullplot(iris_data[,2:5], dbscan_clust$cluster)
table(dbscan_clust$cluster,iris_data$Species)

#  ------------------------------ EM ------------------------------------
install.packages("EMCluster")
library(EMCluster)
em_clus <- init.EM(iris_data[,2:5], nclass = 3)
em_clus
str(em_clus)
hullplot(iris_data[,2:5], em_clus$class)
table(em_clus$class, iris_data$Species)

#plotem(em_clus,iris_data[,2:5])

#ggplot(iris_data, aes(PetalLengthCm, PetalWidthCm, color = Species)) +
#  geom_point(alpha =0.4, size =3.5) +geom_point(col = em_clus$class) +
#  scale_color_manual(values =c('black', 'red', 'green'))


################################################################################
#---------    Internal validation of clustering models  with DUNN index and Silhouette   ---------------------

#------ With DUNN index
install.packages("clValid")
library(clValid)
#Showing for hierarchical cluster with clusters = 3 and with single linkage
dunn(dist(iris_data[,2:5]), cut_hcluster_single)

#Showing for hierarchical cluster with clusters = 3 and with average linkage
dunn(dist(iris_data[,2:5]), cut_hcluster_average)

#Showing for hierarchical cluster with clusters = 3 and with median linkage
dunn(dist(iris_data[,2:5]), cut_hcluster_median)

#Showing for hierarchical cluster with clusters = 3 and with centroid linkage
dunn(dist(iris_data[,2:5]), cut_hcluster_centroid)

#Showing for hierarchical cluster with clusters = 3 and with  complete linkage
dunn(dist(iris_data[,2:5]), cut_hcluster_comp)

#Showing for hierarchical cluster with clusters = 3 and with median linkage
dunn(dist(iris_data[,2:5]), cut_hcluster_wardD)

#Showing for k-means and with clusters=3
dunn(dist(iris_data[,2:5]), kmeans_default$cluster)

#Showing for  EM
dunn(dist(iris_data[,2:5]), em_clus$class)

#Showing for dbscan
dunn(dist(iris_data[,2:5]), dbscan_clust$cluster)

#-------- with Silhouette

#Showing for hierarchical cluster with clusters = 3 and with single linkage
silhouette_hclust_single <- silhouette(cut_hcluster_single,dist(iris_data[,2:5]))
plot(silhouette_hclust_single)

#Showing for hierarchical cluster with clusters = 3 and with average linkage
silhouette_hclust_average <- silhouette(cut_hcluster_average,dist(iris_data[,2:5]))
plot(silhouette_hclust_average)

#Showing for hierarchical cluster with clusters = 3 and with median linkage
silhouette_hclust_median <- silhouette(cut_hcluster_median,dist(iris_data[,2:5]))
plot(silhouette_hclust_median)

#Showing for hierarchical cluster with clusters = 3 and with centroid linkage
silhouette_hclust_centroid <- silhouette(cut_hcluster_centroid,dist(iris_data[,2:5]))
plot(silhouette_hclust_centroid)

#Showing for hierarchical cluster with clusters = 3 and with complete  linkage
silhouette_hclust_complete <- silhouette(cut_hcluster_comp,dist(iris_data[,2:5]))
plot(silhouette_hclust_complete)

#Showing for hierarchical cluster with clusters = 3 and with median linkage
silhouette_hclust_wardD <- silhouette(cut_hcluster_wardD,dist(iris_data[,2:5]))
plot(silhouette_hclust_wardD)

#Showing for k-means and with clusters=3
silhouette_hclust_kmeans <- silhouette(kmeans_default$cluster,dist(iris_data[,2:5]))
plot(silhouette_hclust_kmeans)

#Showing for  EM
silhouette_hclust_EM <- silhouette(em_clus$class,dist(iris_data[,2:5]))
plot(silhouette_hclust_kmeans)

#Showing for dbscan
silhouette_hclust_dbscan <- silhouette(dbscan_clust$cluster,dist(iris_data[,2:5]))
plot(silhouette_hclust_dbscan)


################################################################################
#---------    External evaluation of clustering models  with RAND index and Jaccard index   ---------------------

#------ With RAND index

original <- iris_data$Species
original <- ifelse(iris_data$Species == "Iris-setosa",1,
                   ifelse(iris_data$Species == "Iris-versicolor",2,3))
original

#Showing for hierarchical cluster with clusters = 3 and with single linkage
RRand(original, cut_hcluster_single)

#Showing for hierarchical cluster with clusters = 3 and with average linkage
RRand(original, cut_hcluster_average)

#Showing for hierarchical cluster with clusters = 3 and with median linkage
RRand(original, cut_hcluster_median)

#Showing for hierarchical cluster with clusters = 3 and with centroid linkage
RRand(original, cut_hcluster_centroid)

#Showing for hierarchical cluster with clusters = 3 and with  complete linkage
RRand(original, cut_hcluster_comp)

#Showing for hierarchical cluster with clusters = 3 and with wardD linkage
RRand(original, cut_hcluster_wardD)


#Showing for k-means and with clusters=3
RRand(original, kmeans_default$cluster)

#Showing for  EM
RRand(original, em_clus$class)

#Showing for dbscan
RRand(original, dbscan_clust$cluster)


#------ With Jaccard index

#Showing for hierarchical cluster with clusters = 3 and with single linkage
Jaccard.Index(original, cut_hcluster_single)

#Showing for hierarchical cluster with clusters = 3 and with average linkage
Jaccard.Index(original, cut_hcluster_average)

#Showing for hierarchical cluster with clusters = 3 and with median linkage
Jaccard.Index(original, cut_hcluster_median)

#Showing for hierarchical cluster with clusters = 3 and with centroid linkage
Jaccard.Index(original, cut_hcluster_centroid)

#Showing for hierarchical cluster with clusters = 3 and with  complete linkage
Jaccard.Index(original, cut_hcluster_comp)

#Showing for hierarchical cluster with clusters = 3 and with median linkage
Jaccard.Index(original, cut_hcluster_wardD)

#Showing for k-means and with clusters=3
Jaccard.Index(original, kmeans_default$cluster)

#Showing for  EM
Jaccard.Index(original, em_clus$class)

#Showing for dbscan
Jaccard.Index(original, dbscan_clust$cluster)




