rm(list=ls())
library(fpc)
library(jpeg)
library(imager)
library(fields)
library(ggplot2)
library(raster)
library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(ClusterR)
library(ggplot2)

library(RColorBrewer)

pick_colours = function (n) {
  
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  list_of_colors = sample(col_vector, n, replace=FALSE)
  
  return (list_of_colors)
  
}

contrast_image = function (dataframe){
  for (j in 1:ncol(dataframe)){
    for (i in 1:nrow(dataframe)){
      if(dataframe[i, j] > 0.5){
        dataframe[i, j] = 1
      }
      else{
        dataframe[i, j] = 0
      }
    }
  }
  return(dataframe)
}



#Setting language
Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANGUAGE='en')

#Loading image
raw_image = readJPEG("bobcat_greyscale.jpeg")


dim(raw_image)

?image.smooth




#Plotting image
raw_image = t(apply(raw_image, 2, rev)) # otherwise the image will be rotated
image(raw_image, col  = gray((0:480)/480)) # plot in grayscale

#Smoothing image
smoothing = image.smooth(raw_image, theta=5)
image(smoothing,col = grey((0:480)/480))

#CLARA
clara = clara(raw_image, 6)


colours <- pick_colours(length(unique(clara$cluster)))
img = image(raw_image, col  = colours) # plot in grayscale

plot(silhouette(clara))



#K-MEANS
kmeans = kmeans(raw_image, centers = 2)

colours = pick_colours(length(unique(kmeans$cluster)))

#colours <- rgb(raw_image[kmeans$cluster, ])
image(raw_image, col  = colours) # plot in grayscale



#K-MEANS with smoothing
smooth_df = data.frame(smoothing[3])
kmeans_smooth = kmeans(smooth_df, centers = 2)

colours = pick_colours(length(unique(kmeans_smooth$cluster)))

#colours <- rgb(raw_image[kmeans$cluster, ])
image(raw_image, col  = colours) # plot in grayscale



#DB-SCAN
db_scan = dbscan(raw_image, eps = 0.25, MinPts=10)

test2 = data.frame(raw_image[db_scan$cluster, ])
colours = pick_colours(length(unique(db_scan$cluster)))
#colours <- rgb(test2)
image(raw_image, col  = colours) # plot in grayscale

fviz_cluster(db_scan, raw_image, geom = "point")

?dbscan

