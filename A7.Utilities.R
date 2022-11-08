# Name: Oliver De Sa
# SciNet username: tmp_odesa
# Description:
#   Functions to be used for Assignment 7


source('C:/Users/Oliver/MSC1090/assignment7/Circle.Utilities.R')
library(boot)
library(ggplot2)

most.probable.value <- function(vector){
  p.dist <- hist(vector, breaks = 41, freqs = TRUE, plot = FALSE)
  midPoint <- p.dist$mids[p.dist$counts == max(p.dist$counts)]
  # midPoint <- p.dist$mids[abs((p.dist$density) - max(p.dist$density)) < 1e-10]
  return(midPoint[1])
}

nonpara.bs <- function(vector, n){
  my.mpv <- function(vector, i) return(most.probable.value(vector[i]))
  boot.data <- boot(data = vector, statistic = my.mpv,
                    R = n)
  return(boot.data)
  
 # my.mode <- function(vector, i) return(most.probable.value(vector[i]))
  
}

ang.diff.CI <- function(k, m, n){
  all.ks <- seq(2, k)
  all.test.stats <- rep(0, length(all.ks))
  bca.lower <- rep(0, length(all.ks))
  bca.upper <- rep(0, length(all.ks))
  for (i in all.ks) {
    null.dists <- sim.null.hypo(k, m)
    null.boot.data <- nonpara.bs(null.dists, n)
    all.test.stats[i] <- null.boot.data$t0
    conf.intervals <- boot.ci(null.boot.data)
    bca.lower[i] <- conf.intervals$bca[4]
    bca.upper[i] <- conf.intervals$bc[5]
  }
  plotting.data <- data.frame(all.ks, all.test.stats[2:5], bca.lower[2:5],
                              bca.upper[2:5])
  return(plotting.data)
  
}

plot.CIs <- function(dataframe){
  ggplot(dataframe, aes(all.ks, all.test.stats.2.5.)) +
    geom_point() +
    geom_errorbar(aes(ymin = bca.lower.2.5., ymax = bca.upper.2.5.))
  
}

