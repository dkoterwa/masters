rm(list=ls())

#Set English in R
Sys.setenv(LANG = "en")

#Set working directory
setwd("/Volumes/Macintosh HD – dane/GitHub/masters/first semester/Statistics and EDA") #Desktop

#Exercise 1:
 # Suppose you roll an unfair die 50 times with the probability of obtaining “6” equal to 0.6 (i.e. we deal with binomial distribution with n=50 and p=0.6).
#a) Plot the probability density function for this distribution
#b) What is the probability of obtaining more than 40 times “6”?

p = pbinom(40, 50, 0.6)
1 - p  

probs = dbinom(0:50, 50, 0.6)
plot(probs)


#Exercise 2:
#  A study on birds collects information such as the length of their eggs (in mm). Assume that the length is normally distributed with μ = 42.1mm and σ2 = 20.82.
#a) b) c)
#What is the probability of finding an egg with a length greater than 50 mm?
#  What is the probability of finding an egg between 30 and 40 mm in length?
#  Generate probability distribution and cumulative distribution functions for this
#distribution and show graphically probabilities you calculated in points (a) and (b)
?dnorm
eggs = seq(20, 100, length=100)
probs = dnorm(eggs, 42.1, 20.82)
plot(eggs, probs, type = 'l')

#a)
cdf_50 = pnorm(50, 42.1, 20.82)
1 - cdf_50

#b)
cdf_30 = pnorm(30, 42.1, 20.82)
cdf_40 = pnorm(40, 42.1, 20.82)
cdf_40 - cdf_30

#Exercise 3:
#  Generate 1000 random numbers from a standard normal distribution. Plot the empirical cumulative distribution function of your sample (use ecdf fuction). What is the value of the ecdf for the random number of 0? What does it mean?

sample = rnorm(1000)
ecdf = ecdf(sample)
plot(ecdf)


#Exercise 4:
# Use the data on crime rates in the US that are available in R (USArrests).
#a) Plot the distribution of murders and rapes in the US
#b) Fit the distribution for these two variables. Try different distributions we covered during
#the class.

data = USArrests
?plotdist
plotdist(data$Murder, demp = TRUE)
plotdist(data$Rape, demp = TRUE)

murder = data$Murder
rape = data$Rape

#fit the distributions
fit_n_murder <- fitdist(murder, "norm")
fit_ln_murder <- fitdist(murder, "lnorm")
fit_g_murder  <- fitdist(murder, "gamma")
fit_x_murder  <- fitdist(murder, "exp")

fit_n_rape<- fitdist(rape, "norm")
fit_ln_rape <- fitdist(rape, "lnorm")
fit_g_rape  <- fitdist(rape, "gamma")
fit_x_rape  <- fitdist(rape, "exp")

plot.legend <- c("normal", "lognormal", "gamma", "exp")
denscomp(list(fit_n_murder, fit_ln_murder, fit_g_murder, fit_x_murder), legendtext = plot.legend)
cdfcomp (list(fit_n_murder, fit_ln_murder, fit_g_murder, fit_x_murder), legendtext = plot.legend)

plot.legend <- c("normal", "lognormal", "gamma", "exp")
denscomp(list(fit_n_rape, fit_ln_rape, fit_g_rape, fit_x_rape), legendtext = plot.legend)
cdfcomp (list(fit_n_rape, fit_ln_rape, fit_g_rape, fit_x_rape), legendtext = plot.legend)
