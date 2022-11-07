source(Circle.Utilities.R)
library(boot)

most.probable.value <- function(vector){
  p.dist <- hist(vector, breaks = 41, freq = FALSE)
  midPoint <- p.dist$mids[p.dist$density == max(p.dist$density)]
  return(midPoint)
}

nonpara.bs <- function(vector, n){
  my.mode <- function(vector, i) return(most.probable.value(vector[i]))
  boot.data <- boot(data = vector, statistic = my.mode,
                    R = 2000)
  
 # my.mode <- function(vector, i) return(most.probable.value(vector[i]))
  
}
