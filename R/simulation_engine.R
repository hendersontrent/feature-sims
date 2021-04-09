#--------------------------------------
# This script sets out to produce a
# simulation function that can
# produce the values that are required
#
# NOTE: setup.R must have been run
# first
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 8 April 2021
#--------------------------------------

#' Function to simulate statistical processes needed to calculate time-series features on
#' 
#' @param min_length the minimum time-series length to produce
#' @param max_length the maximum time-series length to produce
#' @param num_ts the number of time series between min and max length to simulate at random
#' @param process the statistical process to simulate. Defaults to 'Gaussian'
#' @return a tidy dataframe with the simulated time-series process values
#' @author Trent Henderson
#' 

simulation_engine <- function(min_length = 1000, max_length = 10000, num_ts = 100,
                              process = c("Gaussian", "Sinusoidal", "ARIMA", "CumSum")){
  
  # Make Gaussian the default
  
  if(missing(process)){
    method <- "Gaussian"
  } else{
    process <- match.arg(process)
  }
  
  #---------- Argument checks ----------
  
  # Numeric parameters
  
  if(!is.numeric(min_length) | !is.numeric(max_length)){
    stop("min_length and max_length arguments should each be numerical scalar values.")
  }
  
  # Process parameter
  
  '%ni%' <- Negate('%in%')
  the_processes <- c("Gaussian", "Sinusoidal", "ARIMA", "CumSum")
  
  if(process %ni% the_processes){
    stop("process argument should be a single string specification of either 'Gaussian', 'Sinusoidal', 'ARIMA', or 'CumSum'.")
  }
  
  if(length(process) > 1){
    stop("process argument should be a single string specification of either 'Gaussian', 'Sinusoidal', 'ARIMA', or 'CumSum'.")
  }
  
  #---------- Main calcs ---------------
  
  message("Simulating time-series processes...")
  set.seed(123) # Fix random number generator for reproducibility
  
  # Sample random N lengths between min and max lengths with no replacement
  
  all_lengths <- seq(from = min_length, to = max_length, by = 1)
  use_lengths <- sample(all_lengths, size = num_ts, replace = FALSE) 
  
  # List to store outputs
  
  storage <- list()
  
  #---------
  # Gaussian
  #---------
  
  if(process == "Gaussian"){
    for(i in use_lengths){
      
      tmp <- data.frame(values = rnorm(i, mean = 0, sd = 1)) %>%
        mutate(timepoint = row_number()) %>%
        mutate(ts_length = i) %>%
        mutate(process = "Gaussian")
      
      storage[[i]] <- tmp
    }
    outData <- data.table::rbindlist(storage, use.names = TRUE)
  }
  
  #-----------
  # Sinusoidal
  #-----------
  
  if(process == "Sinusoidal"){
    
    t <- seq(from = 0, to = 4*pi, by = 100)
    a <- 3
    b <- 2
    amp <- 2
    
    for(i in use_lengths){
      
      n <- i
      noise <- runif(n)
      
      tmp <- data.frame(values = c(a*sin(b*t)+noise*amp)) %>%
        mutate(timepoint = row_number()) %>%
        mutate(ts_length = i) %>%
        mutate(process = "Sinusoidal")
      
      storage[[i]] <- tmp
    }
    outData <- data.table::rbindlist(storage, use.names = TRUE)
  }
  
  #------
  # ARIMA
  #------
  
  if(process == "ARIMA"){
    for(i in use_lengths){
      
      tmp <- data.frame(values = c(1 + 0.5 * 1:i + arima.sim(list(ma = 0.5), n = i))) %>%
        mutate(timepoint = row_number()) %>%
        mutate(ts_length = i) %>%
        mutate(process = "ARIMA")
      
      storage[[i]] <- tmp
    }
    outData <- data.table::rbindlist(storage, use.names = TRUE)
  }
  
  #---------
  # CumSum
  #---------
  
  if(process == "CumSum"){
    for(i in use_lengths){
      
      tmp <- data.frame(values = cumsum(rnorm(i, mean = 0, sd = 1))) %>%
        mutate(timepoint = row_number()) %>%
        mutate(ts_length = i) %>%
        mutate(process = "CumSum")
      
      storage[[i]] <- tmp
    }
    outData <- data.table::rbindlist(storage, use.names = TRUE)
  }
  
  return(outData)
}
