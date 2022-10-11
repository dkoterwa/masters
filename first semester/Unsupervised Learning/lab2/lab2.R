# Clustering
# recommended packages for cluster analysis
# also other packages are available

library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(ClusterR)

# changing the path for accessing the Working Directory
# warning: change \ on /
getwd() # checking current WD
setwd("/Volumes/Macintosh HD – dane/GitHub/masters/first semester/Unsupervised Learning/datasets")
getwd()


#import of data
price_where<-read.csv("prices_regions.csv", sep=";", dec=".", header=TRUE) 
summary(price_where)
dim(price_where) # checking the dimensions of the dataset

price_when<-read.csv("prices_months.csv", sep=";", dec=".", header=TRUE) 
summary(price_when)
dim(price_when) # checking the dimensions of the dataset

price_what<-read.csv("prices_products.csv", sep=";", dec=".", header=TRUE) 
summary(price_what)
dim(price_what) # checking the dimensions of the dataset

# no labels at data – price_when
region_when<-price_when[,1] # first column
product_when<-price_when[,2] # second column
months_when<-colnames(price_when[3:14]) # first row
price_when<-as.matrix(price_when[,3:14]) # data only

# no labels at data – price_what
region_what<-price_what[,1] # first column
months_what<-price_what[,2] # second column
product_what<-colnames(price_what[3:68]) # first row
price_what<-as.matrix(price_what[,3:68]) # data only

## 01. Clustering / K-means
#Using factoextra:: package:
#  One can use the eclust() function from the factoextra :: package. It allows for clusters using k-means, PAM, CLARA etc. methods, using Euclidean, Manhattan, Canberra, Minkowski distance etc.

# clustering of dataset – by individual obs. (in rows)
km1<-eclust(price_when, "kmeans", hc_metric="euclidean",k=3)
fviz_cluster(km1, main="kmeans / Euclidean")


# clustering of dataset – by time (in rows after transposition)
km2<-eclust(t(price_when), "kmeans", hc_metric="euclidean",k=3)
x1<-apply(t(price_when), 2, sd) 
x2<-which(x1==0)
km2<-eclust(t(price_when)[,-x2], "kmeans", hc_metric="Manhattan",k=3)
fviz_cluster(km2, main="kmeans / Manhattan")

test = t(price_when)
# one variable extra, different number of clusters and distance metrics
km3<-eclust(t(price_when)[,-x2], "kmeans", hc_metric="euclidean",k=4)
km4<-eclust(t(price_when)[,-x2],"kmeans", hc_metric="manhattan", k=4)

attributes(km2)

km2$cluster # each time period was assigned to cluster

centers = km2$centers
View(test)

km2$silinfo

# clustering on other dataset
km1<-eclust(price_what, "kmeans", hc_metric="euclidean",k=3)
km1<-eclust(t(price_what), "kmeans", hc_metric="euclidean",k=3)

rownames(t(price_what))

# analisis without carp fish
km1<-eclust(t(price_what)[-14,], "kmeans", hc_metric="euclidean",k=3)

# Alternatives with stats:: package & factoextra:: package
km10<-kmeans(price_when, 2) # stats::
plot(price_when, col = km10$cluster, pch=".", cex=3) # figure has only 2D
points(km10$centers, col = 1:5, pch = 6, cex=3, lwd=2)

fviz_cluster(list(data=price_when, cluster=km10$cluster), ellipse.type="norm", geom="point", stand=FALSE, palette="jco", ggtheme=theme_classic()) #factoextra::

?fviz_cluster
# to get the silhouette plot for k-means one needs the dissimilarity matrix for given distance metric
D<-daisy(price_when) # calculates the dissimilarity matrix, cluster:: 
plot(silhouette(km10$cluster, D), col=1:2, border=NA)

# or apply dist() and fviz_silhouette()
sil<-silhouette(km10$cluster, dist(price_when))
fviz_silhouette(sil)

# Alternatives with flexclust:: package*
km11<-cclust(t(price_when)[,-x2], k=4, simple=FALSE, save.data=TRUE) #flexclust:: class kcca
km11
plot(km11)
summary(km11)
attributes(km11) # checking the slots of output

# flexclust::kcca() - performs k-centroids clustering on a data matrix
km13<-kcca(t(price_when)[,-x2], k=4, family=kccaFamily("kmedians"), control=list(initcent="kmeanspp"))  
km13
parameters(km13) # to get centroids from KCCA object
plot(km13)

image(km13)
points(t(price_when)[,-x2])


# clustering with triangle graphics
pam1<-eclust(price_when, "pam", k=3) # factoextra::
fviz_silhouette(pam1)
fviz_cluster(pam1) # 

# more clusters, other distance metric
pam2<-eclust(price_when, "pam", k=4, hc_metric="manhattan") # factoextra::
fviz_silhouette(pam2)
fviz_cluster(pam2) # 

attributes(pam1)

# Alternatives* - one can use other functions to get the same result

pam3<-pam(price_when,3) #cluster::pam(), works for n<65536
summary(pam3)

class(pam3)

# silhouette statistics can be extracted from pam objects as well as calculated with separate code

silhouette(pam3)

# clustering vector can be extracted from pam object
pam3$clustering

# to run those figures please click the active Graphics Device
plot(pam3) # in fact cluster::plot.partition() combined with cluster::clusplot()

# one can use ggplot() for pam object to get better graphics
fviz_cluster(pam3, geom="point", ellipse.type="norm") # factoextra:: 
fviz_cluster(pam3, geom="point", ellipse.type="convex") # factoextra:: 

