rm(list=ls())
install.packages('edge_detection')
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
library(cannyEdges)

#FUNCTION TO PICK N CONTRAST COLORS
pick_colours = function (n) {
  
  #load palette
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  
  #create colours vector
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  #sample n colours from the vector
  list_of_colors = sample(col_vector, n, replace=FALSE)
  
  return (list_of_colors)
  
}

#FUNCTION TO DISTINGUISH REALLY DARK PIXELS
contrast_image = function (dataframe){
  
  #if greyscale value is larger than 0.5 - set a pixel value to 1, else set it to 0
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
draw_colours_dbscan = function(dataframe, n_clusters){
  
  if (n_clusters < 2){
    print ("Error: insufficient number of clusters : check your parameters")
    break}
    
    
  #sample n colours
  colours = pick_colours(n_clusters - 1)
  
  #compute number of clusters
  clusters = seq(from=1, to=n_clusters - 1, by=1)
  names(colours) = clusters
  
  #this is specific for dbscan: we want to assign colour black to non-clustered pixels and contrasting colours for clusters
  for (i in 1:nrow(dataframe)){
    
    if(dataframe[i, ]['cluster'] != 0){
      dataframe[i, ]['color'] = colours[as.numeric(dataframe[i, ]['cluster'])]
    } else{
      dataframe[i , ]['color'] = '#000000'
    }
    
  }
  
  return(dataframe)
}


draw_colours = function(dataframe, n_clusters){
  
  if (n_clusters == 0){
    print ("Error: insufficient number of clusters : check your parameters")
    break}
  
  
  #sample n colours
  colours = pick_colours(n_clusters)
  
  #compute number of clusters
  clusters = seq(from=1, to=n_clusters, by=1)
  names(colours) = clusters
  
  #this is specific for dbscan: we want to assign colour black to non-clustered pixels and contrasting colours for clusters
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

#checking dimensions to be sure that it's not a RGB image
dim(raw_image)


#Plotting image
raw_image = t(apply(raw_image, 2, rev)) #otherwise the image will be rotated
image(raw_image, col  = gray((0:480)/480)) #plot in grayscale

#Smoothing image
smoothing = image.smooth(raw_image, theta=10)
smoothing_df = data.frame(smoothing[3])

image(smoothing,col = grey((0:480)/480))

#CLARA
clara = clara(raw_image, 6)
colours <- pick_colours(length(unique(clara$cluster)))
img = image(raw_image, col  = colours) # plot in grayscale

plot(silhouette(clara))



#K-MEANS
kmeans = kmeans(raw_image, centers = 3)

df_image = data.frame(raw_image)
df_image$cluster = kmeans$cluster #adding information about assigned cluster
df_image$color = "" #setting a column for a color

df_image = draw_colours(df_image, n_clusters = length(unique(kmeans$cluster))) #sampling colors for clusters
colours = df_image$color 

image(raw_image, col = colours) #drawing image




#K-MEANS with smoothing
kmeans_smooth = kmeans(smoothing_df, centers=3)

df_image = data.frame(raw_image)
df_image$cluster = kmeans_smooth$cluster #adding information about assigned cluster
df_image$color = "" #setting a column for a color

df_image = draw_colours(df_image, n_clusters = length(unique(kmeans_smooth$cluster))) #sampling colors for clusters
colours = df_image$color 

image(raw_image, col = colours) #drawing image




#DB-SCAN
db_scan = dbscan(raw_image, eps = 0.35, MinPts=25) #performing dbscan

df_image = data.frame(raw_image)
df_image$cluster = db_scan$cluster #adding information about assigned cluster
df_image$color = "" #setting a column for a color

df_image = draw_colours_dbscan(df_image, n_clusters = length(unique(db_scan$cluster))) #sampling colors for clusters
colours = df_image$color 

image(raw_image, col = colours) #drawing image


#DB-SCAN with smoothed image
db_scan = dbscan(smoothing_df, eps = 0.15, MinPts=25) #performing dbscan


df_image = data.frame(raw_image)
df_image$cluster = db_scan$cluster #adding information about assigned cluster
df_image$color = "" #setting a column for a color

df_image = draw_colours_dbscan(df_image, n_clusters = length(unique(db_scan$cluster))) #sampling colors for clusters
colours = df_image$color 

image(raw_image, col = colours) #drawing image



#MEAN SHIFT
mean_shift = meanShift(raw_image, epsilon = 1e-10, iterations = 10)
df_image = data.frame(raw_image)
df_image$cluster = mean_shift$assignment #adding information about assigned cluster
df_image$color = ""

df_image = draw_colours(df_image, n_clusters = length(unique(mean_shift$assignment))) #sampling colors for clusters
colours = df_image$color 

image(raw_image, col = colours) #drawing image
?meanShift
