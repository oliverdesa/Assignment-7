# Name: Oliver De Sa
# SciNet username: tmp_odesa
# Description:
#   Functions to be used for Assignment 7


source('C:/Users/Oliver/MSC1090/assignment7/Circle.Utilities.R')
library(boot)
library(ggplot2)

# Function takes a vector of numeric values and returns the most probable values
most.probable.value <- function(vector){
  #analyze data in a histogram
  p.dist <- hist(vector, breaks = 41, plot = FALSE)
  # Retreive the midpoint of the bin containing the most probable value
  midPoint <- p.dist$mids[p.dist$counts == max(p.dist$counts)]
  return(midPoint[1])
}

# Function takes a vector of numeric values and an integer n, preforms 
# non-parametric bootstrapping on the data n time, calculating the most 
# probable value of the resulting dataset
nonpara.bs <- function(vector, n){
  # define a function that returns the most probable value
  my.mpv <- function(vector, i) return(most.probable.value(vector[i]))
  # bootstrap the data and return the most probable value from each result
  boot.data <- boot(data = vector, statistic = my.mpv,
                    R = n)
  return(boot.data)
  
}

# Function takes 3 integers as argument, k is number of birds in each sample
# m is number of artificial data generated, n is number of bootstreap 
# iterations to run
#for each value from 2:k return the most probable value and 95% CI
ang.diff.CI <- function(k, m, n){
  #Create empty vector of 2:k
  all.ks <- seq(2, k)
  # Create empty vectors to store test stat and lower + upper CI limits
  all.test.stats <- rep(0, length(all.ks))
  bca.lower <- rep(0, length(all.ks))
  bca.upper <- rep(0, length(all.ks))
  
  # for all ks simulate the null hypothesis of k birds m times and bootstrap each
  # distribution, returning the most probable value and the 95% CI
  for (i in all.ks) {
    null.dists <- sim.null.hypo(k, m)
    null.boot.data <- nonpara.bs(null.dists, n)
    all.test.stats[i] <- null.boot.data$t0
    conf.intervals <- boot.ci(null.boot.data)
    bca.lower[i] <- conf.intervals$bca[4]
    bca.upper[i] <- conf.intervals$bc[5]
  }
  # structure the data into a dataframe for easier plotting
  plotting.data <- data.frame(all.ks, all.test.stats[2:5], bca.lower[2:5],
                              bca.upper[2:5])
  return(plotting.data)
  
}

# plot the most probable value as a function of k, including the 95% CI for each
# point
plot.CIs <- function(dataframe){
  ggplot(dataframe, aes(all.ks, all.test.stats.2.5.)) +
    geom_point() +
    geom_errorbar(aes(ymin = bca.lower.2.5., ymax = bca.upper.2.5.))
  
}

