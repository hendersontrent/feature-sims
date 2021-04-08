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

#' Function to simulate processes needed to calculate time-series feature on
#' 
#' @param min_length the minimum time-series length to produce
#' @param max_length the maximum time-series length to produce
#' @param process the statistical process to simulate. Defaults to 'Gaussian'
#' @return a tidy dataframe with the simulated time-series process values
#' @author Trent Henderson
#' 

simulation_engine <- function(min_length = 100, max_length = 10000,
                              process = c("Gaussian", "Sinusoidal", "ARIMA", "RandomWalk")){
  
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
  the_processes <- c("Gaussian", "Sinusoidal", "ARIMA", "RandomWalk")
  
  if(process %ni% the_processes){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }
  
  if(length(process) > 1){
    stop("process argument should be a single string specification of either 'Gaussian', 'Sinusoidal', 'ARIMA', or 'RandomWalk'.")
  }
  
  #---------- Main calcs ---------------
  
  
  return(outs)
}
