#######################################
#                                     #
#             Clustering              #
#                                     #
#              Class 22               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
# 1. What is unsupervised learning?
# 2. Examples of unsupervised learning
# 3. K-means clustering algorithm
# 4. hierarchical clustering algorithm
# 5. Constructing dissimilarity distance matrices
# 6. How to pick optimal clusters k
# 7. Estimating k-means clustering using k-means
# 8. Plotting clusters using clusplot()
# 9. Going from clusters assigmments to conceptual name 
#######################################

#------------------------------------------------
### Customer Segmentation
#------------------------------------------------
# load customer data
customers_df <- read.csv("/Users/jakeflath/Desktop/MGSC310/wholesale_customers.csv")
# https://archive.ics.uci.edu/ml/datasets/wholesale+customers
# Attribute Information:
#   
# 1) FRESH: annual spending (m.u.) on fresh products (Continuous);
# 2) MILK: annual spending (m.u.) on milk products (Continuous);
# 3) GROCERY: annual spending (m.u.)on grocery products (Continuous);
# 4) FROZEN: annual spending (m.u.)on frozen products (Continuous)
# 5) DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous)
# 6) DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous);
# 7) CHANNEL: customers' Channel - Horeca (Hotel/Restaurant/Cafe) or Retail channel (Nominal)
# 8) REGION: customers' Region Lisnon, Oporto or Other (Nominal)
#   
# REGION Frequency
# Lisbon 77
# Oporto 47
# Other Region 316
# Total 440
# 
# CHANNEL Frequency
# Horeca 298
# Retail 142
# Total 440

head(customers_df)

summary(customers_df)

str(customers_df)

customers_sub <- customers_df[,3:8]

#------------------------------------------------
### number of clusters?
#------------------------------------------------
install.packages("factoextra")
library(factoextra)
library(ggplot2)

#--------
# Explore these methods on your own
# elbow method
fviz_nbclust(Customers_DF,
             kmeans,
             method="wss") +
  geom_vline(xintercept=4, linetype =2) + 
  labs(subtitle ="Elbow Method")

# Silhouette method
fviz_nbclust(Customers_DF,
             kmeans,
             method="silhouette")

# Gap Statistic method
fviz_nbclust(Customers_DF,
             kmeans,
             method="gap_stat",
             nboot=100)
#--------

# use NbClust package to find the best # of clusters
install.packages('NbClust')
library(NbClust)
nc <- NbClust(customers_num,
              diss = NULL,
              distance = "euclidean",
              min.nc = 2,
              max.nc = 15,
              methods = "kmeans")

# summary of results
nc$Best.nc[1,]

barplot(table(nc$Best.nc[1,]), xlab = "Number of Cluster",
        main = "Number of Clusters Chosen by 26 Criteria")

#------------------------------------------------
### K-Means
#------------------------------------------------
# perform K-means clustering with 3 clusters
kmeans3 <- kmeans(customers_num,
                  centers = 3,
                  nstart = 25)

# print the average feature for each cluster
kmeans3$centers

# print the cluster each observation belongs to
kmeans3$cluster

# cluster plot
install.packages("cluster")
library(cluster)

clusplot(customers_num,
         kmeans3$cluster,
         color = TRUE,
         shade = TRUE)

fviz_cluster(kmeans3, data = customers_num)

#------------------------------------------------
### Hierarchical Clustering
#------------------------------------------------

# choose random rows from original datasets
set.seed(310)
idx <- sample(1:nrow(customers_num), size = 40)
customers_sample <- customers_num[idx,]

dim(customers_sample)

# define the dissimilarity matrix
diss<- dist(customers_sample)


# define the hierarchical clustering
clusters <- hclut(diss)

# show the dendogram for the hclust
plot(clusters)

# cutting the tree to find the clusters
rect.hclust(clusters, k = 5, border = "red")
plot(clusters)

# cluster assignment
cluster_cut <- cutree(clusters, 3)
cluster_cut

