rm(list=ls())
# Clustering
library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(hopkins)
library(ClusterR)
library(NbClust)

# changing the path for accessing the Working Directory
# warning: change \ on /
setwd("/Volumes/Macintosh HD – dane/GitHub/masters/first semester/Unsupervised Learning/datasets")
getwd()# checking current WD

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



?NbClust # comprehensive description on distances, methods, tests

c3<-NbClust(price_when, distance="euclidean", min.nc=2, max.nc=8, method="complete", index="ch")
c3 			# it chooses the best partition
c3$All.index
c3$Best.nc
c3$Best.partition


