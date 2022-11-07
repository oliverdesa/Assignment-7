source(Circle.Utilities.R)
library(boot)

most.probable.value <- function(vector){
  p.dist <- hist(vector, breaks = 41, freq = FALSE)
  midPoint <- p.dist$mids[p.dist$density == max(p.dist$density)]
  return(midPoint)
}

nonpara.bs <- function(vector, n){
  
}
