source('C:/Users/Oliver/MSC1090/assignment7/Circle.Utilities.R')
library(boot)

most.probable.value <- function(vector){
  p.dist <- hist(vector, breaks = 41, freq = TRUE)
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
  get.CI <- function(k, m, n){
    null.dists <- sim.null.hypo(k, m)
    null.boot.data <- nonpara.bs(null.dists, n)
    conf.intervals <- boot.ci(null.boot.data)
    return(conf.intervals)
  }
  all.CIs <- sapply(all.ks, getCI)
  
}
