#------------------- Multivariate Analysis
# ------------------ Cluster Analysis

install.packages('fatoextra')

require(datasets)
require(dplyr)
require(ggplot2)
require(plotly)
require(factoextra)

data <- iris

head(data)
glimpse(data)

data%>%
  ggplot(aes(x=sepal_length, y=sepal_width))+
  geom_point(aes(color=species))+
  theme_minimal()


# k-means minimize the within group dispersion and maximize the between-group dispersion
# cluster the species on basis of sepal_length and sepal_width
# 3 number of clusters
# nstart: number of starting assignments, select the one with lowest within cluster variation
iris_cluster <- kmeans(data[, c('sepal_length', 'sepal_width')],
                       3,
                       nstart=15)
iris_cluster
#71% is a measure of the total variance in dataset that is explained by the clustering.

# compare cluster with species
table(iris_cluster$cluster, data$species)
# all setosa species assigned to cluster 1
# 38 versicolor assigned correctly to cluster 2 and 12 to cluster 3
# 15 virginica assigned incorrectly to cluster 2 and 35 assigned correctly to cluster 3

iris_cluster$cluster <- as.factor(iris_cluster$cluster)

data%>%
  ggplot(aes(x=sepal_length, y=sepal_width))+
  geom_point(aes(color=iris_cluster$cluster))+
  theme_minimal()

# Choosing the right number of K (reduce within cluster variation)

# Elbow method is one of methods to find the optimal number of clusters
set.seed(123)
# compute and plot wss for k=2 to k=15
max <- 15
data2 <- scale(data[, -5]) # remove species column
wss <- sapply(1:max,
              function(k){
                kmeans(data2, k, nstart=50, iter.max = 15)$tot.withinss
              }) # how many clusters will reduce within group variation

plot(1:max,
     wss,
     type='b',
     pch=19,
     frame=F,
     xlab='Number of Cluster K',
     ylab='Total within-clusters sum of squares')

head(data)

iris_cluster <- kmeans(data[, c('sepal_length','sepal_width')],
                       5,
                       nstart=15)# just test with k=5
iris_cluster
table(iris_cluster$cluster, data$species)

iris_cluster$cluster <- as.factor(iris_cluster$cluster)

data%>%
  ggplot(aes(x=sepal_length, y=sepal_width))+
  geom_point(aes(color=iris_cluster$cluster))+
  theme_minimal()


# visualize using factoextra

iris_cluster%>%
  fviz_cluster(data=data2,
               geom='point',
               stand=F,
               ellipse.type ='norm')

