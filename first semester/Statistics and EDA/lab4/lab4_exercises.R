library(ggplot2)
library(dplyr)

rm(list=ls())
#Set English in R
Sys.setenv(LANG = "en")

#Set working directory
setwd("/Volumes/Macintosh HD – dane/GitHub/masters/first semester/Statistics and EDA/lab4") #Desktop

data = read.csv('ESS_data.csv')
str(data)

data$cgtsmke[which(data$cgtsmke == 1)] = "I smoke daily"
data$cgtsmke[which(data$cgtsmke == 2)] = "I smoke but not every day"
data$cgtsmke[which(data$cgtsmke == 3)] = "I don't smoke now but I used to"
data$cgtsmke[which(data$cgtsmke == 4)] = 'I have only smoked a few times'
data$cgtsmke[which(data$cgtsmke == 5)] = 'I have never smoked'

ggplot(data, aes(x = cgtsmke, fill=cntry)) +
  geom_histogram(color = 'red', stat = 'count', position = 'dodge')

#c)
bars = data.frame(country = data$cntry, smoking = data$cgtsmke)
bars = bars %>% filter(country == 'PL' | country == 'DE')

ggplot(bars, aes(x = smoking, fill = country)) +
  geom_histogram(color = 'red', stat = 'count', position = 'dodge')

#d)
data$fclcntr[which(data$fclcntr == 1)] = "very close"
data$fclcntr[which(data$fclcntr == 2)] = "close"
data$fclcntr[which(data$fclcntr == 3)] = "not very close"
data$fclcntr[which(data$fclcntr == 4)] = "not close at all"

ggplot(data, aes(x = fclcntr, fill = cntry)) +
  geom_bar(stat = 'count', position = 'fill')

#f)
dens_data = data[complete.cases(data), ]
dens_pl = density(dens_data[which(dens_data$cntry == "PL" ), 'agea'])
dens_de = density(dens_data[which(dens_data$cntry == "DE"), 'agea'])

plot(dens_pl, col = 'red')
lines(dens_de, col = 'blue')

#g)
bars = data.frame(education = data$edulvlb, country = data$cntry)
bars$education = bars$education/12

ggplot(bars, aes(x = education, y = country)) +
  geom_point()

#EX2
data
data$height[which(data$height <= 160)] = '<=160'
data$height[which(data$height > 160)] = '>160'

ggplot(data, aes(x = height)) +
  geom_bar(aes(y = (..count..)/sum(..count..)))

data_pl = data[data$cntry == 'PL', ]

ggplot(data_pl, aes(x = fclcntr)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent)

#EX3
ggplot(data, aes(x = cntry, y = weight)) + 
  geom_boxplot()

quantile(data$weight, c(0.25, 0.5 ,0.75), na.rm = TRUE)
  
