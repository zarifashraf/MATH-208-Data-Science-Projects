library(here)
library(tidyverse)

one_line <- function(stops) {
  
  total_time <- 0
  
  for (index in seq(1, stops, by = 1)) {
    
    stoppage_time = 2
    
    delay <- rbernoulli(1, 0.2)
    
    if (delay == 1) {
      
      delay_time = rgamma(1, shape = 5, scale = 0.5)
      stoppage_time = 2 + delay_time
    }
    
  total_time = total_time + stoppage_time
    
  }
  
  return(total_time)
}

vect <- c(2,4,5)


one_trip <- function(vec) {
  
  vec <- as.list(vec)
  
  final_time = 0
  
  for(v in vec) {
    
    for(e in v) {
      
      final_time = final_time + one_line(e)
    }
  }
  
  return(final_time)
}

lower_and_upper <- function(seed, vecto, trips) {
  set.seed(seed)
  samples <- vector()
  
  for (i in rep(1:trips)) {
    single = one_trip(vecto)
    samples = append(samples, single)
    
  }

    return(quantile(samples, c(.10, .90)))
  }

