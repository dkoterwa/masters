rm(list=ls())

library(MASS)


#EX 1


sample = rnorm(1000, mean=2, sd=0.5)

mean(sample)
median(sample)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(sample)

#EX 2
data("anorexia")

anorexia$change  = anorexia$Postwt - anorexia$Prewt
aggregate(anorexia[, 'change'], list(anorexia$Treat), mean)

#EX 3
setwd("/Volumes/Macintosh HD – dane/GitHub/masters/first semester/Statistics and EDA/lab2")

NLSY<-read.csv("NLSY_EDA_class.csv", 
               header=T, 
               sep=",",
               dec="." ) 

summary(NLSY$wage)

aggregate(NLSY[, 'hours'], list(NLSY$sex, NLSY$race), mean)
aggregate(NLSY[, 'hours'], list(NLSY$sex, NLSY$race), median)

aggregate(NLSY[, 'hours'], list(NLSY$sex, NLSY$race), quantile, probs=c(0.25, 0.5, 0.75))
aggregate(NLSY[, 'hours'], list(NLSY$sex, NLSY$race), quantile, probs=c(0.1, 0.9))
aggregate(NLSY[, 'hours'], list(NLSY$sex, NLSY$race), quantile, probs=c(0.05, 0.95))

aggregate(NLSY[, 'experience'], list(NLSY$sex, NLSY$race), quantile, probs=c(0.25, 0.5, 0.75))
aggregate(NLSY[, 'experience'], list(NLSY$sex, NLSY$race), quantile, probs=c(0.1, 0.9))
aggregate(NLSY[, 'experience'], list(NLSY$sex, NLSY$race), quantile, probs=c(0.05, 0.95))

#EX 4
data("Animals")

mean(Animals$body)
mean(Animals$brain)

median(Animals$body)
median(Animals$brain)

Animals$brain = Animals$brain/1000
Animals$share = (Animals$brain/Animals$body) * 100

quantile(Animals$body, probs = 0.27)
