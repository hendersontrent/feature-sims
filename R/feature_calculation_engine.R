#--------------------------------------
# This script sets out to produce a
# function that loops over simulated
# entries to calculate time-series
# features
#
# NOTE: setup.R must have been run
# first
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 8 April 2021
#--------------------------------------

#' Function to calculate time-series features in a loop
#' 
#' @param data the input dataframe of time-series values
#' @param feature_set the set of time-series features to calculate. Defaults to 'all'
#' @return a dataframe with data traits and computation time
#' @author Trent Henderson
#' 

feature_calculation_engine <- function(data, feature_set = c("catch22", "feasts", "tsfeatures")){
  
  # Make 'catch22' the default
  
  if(missing(feature_set)){
    feature_set <- "catch22"
  }
  if(is.null(feature_set)){
    feature_set <- "catch22"
  }
  
  the_feat <- feature_set
  
  #------- Argument checks -------
  
  expected_cols_1 <- "values"
  expected_cols_2 <- "timepoint"
  expected_cols_3 <- "ts_length"
  expected_cols_4 <- "process"
  the_cols <- colnames(data)
  '%ni%' <- Negate('%in%')
  
  if(expected_cols_1 %ni% the_cols){
    stop("data should contain four columns called 'values', 'timepoint', 'ts_length', and 'process'.")
  }
  
  if(expected_cols_2 %ni% the_cols){
    stop("data should contain four columns called 'values', 'timepoint', 'ts_length', and 'process'.")
  }
  
  if(expected_cols_3 %ni% the_cols){
    stop("data should contain four columns called 'values', 'timepoint', 'ts_length', and 'process'.")
  }
  
  if(expected_cols_4 %ni% the_cols){
    stop("data should contain four columns called 'values', 'timepoint', 'ts_length', and 'process'.")
  }
  
  #--------- Main calcs ----------
  
  lengths <- unique(data$ts_length)
  storage <- list()
  the_proc <- unique(data$process)
  
  for(i in lengths){
    
    tmp <- data %>%
      filter(ts_length == i)
    
    m <- summary(microbenchmark(theft::calculate_features(data = tmp, id_var = "ts_length", 
                                                          time_var = "timepoint", values_var = "values",
                                                          feature_set = the_feat), times = 1))
    
    m1 <- data.frame(m) %>%
      dplyr::select(c(mean)) %>%
      rename(comp_time = mean) %>%
      mutate(ts_length = i,
             feature_set = the_feat,
             process = the_proc)
      
    storage[[i]] <- m1
  }
  
  outs <- data.table::rbindlist(storage, use.names = TRUE)
  
  return(outs)
}
