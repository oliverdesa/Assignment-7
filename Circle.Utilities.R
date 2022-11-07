
# Name: Oliver De Sa
# SciNet username: tmp_odesa
# Description:
#   Functions to be used for assignment 5


# Function takes a vector of angles, sorts the vector into increasing order
# then takes the difference of (angle n+1 - angle n), returning these values as 
# a vector

max.angular.diff<- function(angles) {
  sorted.angles <- sort(angles, decreasing = FALSE)
  angle_diff <- rep(0, length(sorted.angles))
  for (i in 1:length(sorted.angles)) {
    if (i+1 > length(sorted.angles)){
      angle_diff[i] <- 2*pi - (sorted.angles[i] - sorted.angles[1])
    }else if (i+1 <= length(sorted.angles)) {
      angle_diff[i] <- (sorted.angles[i+1] - sorted.angles[i])
    }
  }
  return(max(angle_diff))
}

# Function takes a sample size (k) and number of samples (n) and returns a  
# distribution of max angles from those samples

sim.null.hypo <- function(k, n) {
  samples <- rep(0, n)
  for (sample in 1:length(samples)){
    samples[sample] <- list(runif(k, min=0, max=2*pi))
  }
  sample.angle.diffs <- sapply(samples, max.angular.diff)
  return(sample.angle.diffs)
}

# function takes a vector of max angle diffs and returns a dataframe including
# each break along with the respective cumulative distribution value

calc.cdf <- function(sample.max.diffs){
  p.dist <- hist(sample.max.diffs, breaks = 41, plot=FALSE)
  breaks <- p.dist$breaks
  density <- p.dist$density
  Cumulative.Data <- rep(0, length(breaks))
  for (rep in 1:length(Cumulative.Data)){
    if (rep > 1){
      Cumulative.Data[rep] <- density[rep]*(breaks[rep+1] - breaks[rep])
      
    } else if (rep == 1) {
      next
    }
  }
  Cumulative.Data <- cumsum(Cumulative.Data)
  cumulative.df <- data.frame(breaks, Cumulative.Data) 
  return(cumulative.df)
}

# Function takes a dataframe and a number (n) and returns the associated cumulative
# distribution value associated with the bin including n

calc.cumulative <- function(dataframe, n){
  greater.than.n <- dataframe[dataframe$breaks >= n, ]
  reverse.pvalue <- greater.than.n$Cumulative.Data[1]
  return(reverse.pvalue)
}







