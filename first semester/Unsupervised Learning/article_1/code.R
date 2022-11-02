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
library(fdm2id)
library(showtext)



pick_colours = function (n) {
  
  #DESCRIPTION : this function is picking n contrasting colours from the palette
  #INPUT: n: number of colours that we want to pick
  #OUTPUT: list_of_colors: list of n colours
  
  #load palette
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  
  #create colors vector
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  #sample n colors from the vector
  list_of_colors = sample(col_vector, n, replace=FALSE)
  
  return (list_of_colors)
  
}


contrast_image = function (dataframe, scalar_white, scalar_black){
  
  #DESCRIPTION : this function is contrasting pixels of the grayscale image
  #INPUT: dataframe: 2D dataframe of our image
          #scalar_white : value which corresponds to how much we want to brighten light pixels
          #scalar_black : value which corresponds to how much we want to darken dark pixels
  #OUTPUT: dataframe: converted dataframe with contrasted pixels
  
  #if grayscale value is larger than a threshold - set a pixel value to scalar_white * pixel, else set it to scalar_black * pixel
  for (j in 1:ncol(dataframe)){
    for (i in 1:nrow(dataframe)){
      if(dataframe[i, j] > 0.3){
        dataframe[i, j] = dataframe[i,j] * scalar_white
      }
      else{
        dataframe[i, j] = dataframe[i, j] * scalar_black
      }
    }
  }
  return(dataframe)
}


draw_colours_dbscan = function(dataframe, n_clusters){
  
  #DESCRIPTION : this function is assigning colours to specific clusters. It is made for DBSCAN algorithm, because this method can output cluster #0
  #INPUT: dataframe: 2D dataframe of our grayscale image with additional column 'cluster', which is the assignment of each pixel to specific cluster
          #n_clusters: number of clusters which our algorithm has outputted
  #OUTPUT: dataframe: inputted dataframe with additional column 'color'
  
  if (n_clusters < 2){
    print ("Error: insufficient number of clusters : check your parameters")
    break}
    
    
  #sample n colours
  colours = pick_colours(n_clusters - 1)
  
  #compute number of clusters
  clusters = seq(from=1, to=n_clusters - 1, by=1)
  names(colours) = clusters
  
  #this is specific for dbscan: we want to assign color black to non-clustered pixels and contrasting colors for clusters
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
  
  #DESCRIPTION : this function is assigning colours to specific clusters. 
  #INPUT: dataframe: 2D dataframe of our grayscale image with additional column 'cluster', which is the assignment of each pixel to specific cluster
  #n_clusters: number of clusters which our algorithm has outputted
  #OUTPUT: dataframe: inputted dataframe with additional column 'color'
  
  if (n_clusters < 2){
    print ("Error: insufficient number of clusters : check your parameters")
    break}
  
  
  #sample n colors
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


#loading image
raw_image = readJPEG("deer.jpeg")

#checking dimensions to be sure that it's not a RGB image
dim(raw_image)

#plotting image
raw_image = t(apply(raw_image, 2, rev)) #otherwise the image will be rotated
image(raw_image, col  = grey((0:dim(raw_image)[1])/dim(raw_image)[1]))


#Contrasting image
contrasted_image = contrast_image(raw_image, scalar_white = 1, scalar_black = 1.3)
image(contrasted_image, col = grey((0:480)/480))


#K-MEANS
kmeans = kmeans(raw_image, centers = 2)

df_image = data.frame(raw_image)
df_image$cluster = kmeans$cluster #adding information about assigned cluster
df_image$color = "" #setting a column for a color

df_image = draw_colours(df_image, n_clusters = length(unique(kmeans$cluster))) #sampling colors for clusters
colours = df_image$color 

image(raw_image, col = colours) #drawing image

#CLARA
clara = clara(raw_image, 2)
colours <- pick_colours(length(unique(clara$cluster)))
img = image(raw_image, col  = colours) # plot in colours

plot(silhouette(clara))

#CLARA with contrasted image
clara = clara(contrasted_image, 3)
colours <- pick_colours(length(unique(clara$cluster)))
img = image(raw_image, col  = colours) # plot in colours

plot(silhouette(clara))


#DB-SCAN
db_scan = dbscan(raw_image, eps = 0.27, MinPts=10) #performing dbscan

df_image = data.frame(raw_image)
df_image$cluster = db_scan$cluster #adding information about assigned cluster
df_image$color = "" #setting a column for a color

df_image = draw_colours_dbscan(df_image, n_clusters = length(unique(db_scan$cluster))) #sampling colors for clusters
colours = df_image$color 

image(raw_image, col = colours) #drawing image


#DB-SCAN with contrasted image
db_scan = dbscan(contrasted_image, eps=0.6, MinPts=3) #performing dbscan


df_image = data.frame(raw_image)
df_image$cluster = db_scan$cluster #adding information about assigned cluster
df_image$color = "" #setting a column for a color

df_image = draw_colours_dbscan(df_image, n_clusters = length(unique(db_scan$cluster))) #sampling colors for clusters
colours = df_image$color 

image(raw_image, col = colours) #drawing image


#MEAN SHIFT
mean_shift = meanShift(raw_image)
df_image = data.frame(raw_image)
df_image$cluster = mean_shift$assignment #adding information about assigned cluster
df_image$color = ""

df_image = draw_colours(df_image, n_clusters = length(unique(mean_shift$assignment))) #sampling colors for clusters
colours = df_image$color 

image(raw_image, col = colours) #drawing image

#MEAN SHIFT with contrasted image
mean_shift = meanShift(contrasted_image, epsilonCluster=0.01)
df_image = data.frame(raw_image)
df_image$cluster = mean_shift$assignment #adding information about assigned cluster
df_image$color = ""

mean_shift$assignment
df_image = draw_colours(df_image, n_clusters = length(unique(mean_shift$assignment))) #sampling colors for clusters
colours = df_image$color 

image(raw_image, col = colours) #drawing image


#SPECTRAL CLUSTERING
spectral = SPECTRAL(raw_image, k = 2, sigma = 0.15)

df_image = data.frame(raw_image)
df_image$cluster = spectral$cluster #adding information about assigned cluster
df_image$color = "" #setting a column for a color

df_image = draw_colours(df_image, n_clusters = length(unique(spectral$cluster))) #sampling colors for clusters
colours = df_image$color

image(raw_image, col = colours)

#SPECTRAL CLUSTERING with contrasted image
spectral = SPECTRAL(contrasted_image, k = 2, sigma = 2.5, graph = FALSE)

df_image = data.frame(raw_image)
df_image$cluster = spectral$cluster #adding information about assigned cluster
df_image$color = "" #setting a column for a color

df_image = draw_colours(df_image, n_clusters = length(unique(spectral$cluster))) #sampling colors for clusters
colours = df_image$color

image(raw_image, col = colours)

