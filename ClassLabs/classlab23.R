#######################################
#                                     #
#                PCA                  #
#                                     #
#              Class 23               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################
rm(list=ls())
#######################################
# Goals:
#   - Review on clustering
#   - Apply dimension reduction by PCA
#   - Understanding PCA factors
#   - Visualize PCA results
#######################################

library(ggplot2)

library(factoextra)

#------------------------------------------------
### Auto data
#------------------------------------------------
# using Auto data
library(ISLR)
data(auto)
# converting Origin to factor
Auto$origin <- factor(Auto$origin, lables = c("American", "European", "Japanese"))
# scaling
Auto_scaled <- scale(Auto[,1:7])

# scaling doesn't change the distribution of the variables
library(ggridges)

ggplot(Auto, aes(x = mpg, y = origin)) + geom_density_ridges(aes(fill = origin)) +
    theme_ridges()
ggplot(Auto, aes(x = scale(mpg) y = origin)) + geom_density_ridges(aes(fill = origin)) +
  theme_ridges()
#------------------------------------------------
### number of clusters?
#------------------------------------------------

library("NbClust")
Nb_cl <- NbClust(Auto_scaled,
                 diss=NULL,
                 distance="euclidean",
                 min.nc=2,
                 max.nc=10,
                 method="kmeans")
fviz_nbclust(Nb_cl)

# pick the best number of clusters
kmeans2 <- kmeans(Auto_scaled, 
                  centers = 2,
                  nstart = 30)

fviz_cluster(kmeans2, data= Auto_scaled)
# store the cluster information in the data frame
Auto$cluster <- as.factor(kmeans2$cluster)

# use ggpairs to see if the clusters are meaningful
library(GGally)


ggpairs(Auto, 1:5, mapping = aes(color = cluster, alpha = 0.5))
#------------------------------------------------
### PCA
#------------------------------------------------

# run pca model (scale=TRUE does the scaling automatically)
pca_auto <- prcomp(Auto [,1:7],
                   center = TRUE,
                   scale = TRUE)

# summary
summary(pca_auto)

# how much variation each dimension captures 
fviz_screeplot(pca_auto, addlables=TRUE)

# Extract the results for variables
var <- get_pca_var(pca_auto)
var
# Contributions of variables to PC1
fviz_contrib(pca_auto,
             choice = "var",
             axes = 1,
             top = 10)

# plot the first two principal factors (dimensions)
fviz_pca_biplot(pca_auto , label="var")

# remember the default plotting option is biplot; It is not very interesting though
#biplot(pca_auto, scale = 0)


