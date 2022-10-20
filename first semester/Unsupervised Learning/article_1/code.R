rm(list=ls())

library(fpc)
library(jpeg)
library(imager)
library(fields)
library(ggplot2)
library(raster)
library(dplyr)
library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(ClusterR)
library(meanShiftR)
library(RColorBrewer)
library(Dict)

#FUNCTION TO PICK N CONTRAST COLORS
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

#FUNCTION TO ASSIGN COLOR TO A CLUSTER
draw_colours = function(dataframe, n_clusters){
  
  colours = pick_colours(n_clusters - 1)
  clusters = seq(from=1, to=n_clusters - 1, by=1)
  names(colours) = clusters
  
  print(colours)
  
  for (i in 1:nrow(dataframe)){
    
    if(dataframe[i, ]['cluster'] != 0){
      dataframe[i, ]['color'] = colours[as.numeric(dataframe[i, ]['cluster'])]
    } else{
      dataframe[i , ]['color'] = '#000000'
    }
    
  }
  
  return(dataframe)
}




#Setting language
Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
Sys.setenv(LANGUAGE='en')



#LOADING IMAGE

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

kColours <- rgb(kmeans$centers[kmeans$cluster,])
kColours[c(1:200)] = '#FFFF99'

#colours <- rgb(raw_image[kmeans$cluster, ])
image(raw_image, col  = kColours) # plot in grayscale



#K-MEANS with smoothing
smooth_df = data.frame(smoothing[3])
kmeans_smooth = kmeans(smooth_df, centers = 2)

kColours <- rgb(kmeans_smooth$centers[kmeans_smooth$cluster,])


image(raw_image, col  = kColours) # plot in grayscale


#DB-SCAN
db_scan = dbscan(raw_image, eps = 0.4, MinPts=10)
dim(raw_image)

df_image = data.frame(raw_image)
df_image$cluster = db_scan$cluster
df_image$color = ""

df_image = draw_colours(df_image, n_clusters = length(unique(db_scan$cluster)) - 1)
colours = df_image$color

image(raw_image, col = colours)



#MEAN SHIFT
mean_shift = meanShift(raw_image)
colours = pick_colours(length(unique(mean_shift$assignment)))

kColours <- rgb(mean_shift$value[mean_shift$assignment,])
image(raw_image, col  = kColours)
?dbscan

test = data.frame(mean_shift$value[mean_shift$assignment,])

