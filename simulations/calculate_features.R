#--------------------------------------
# This script sets out to calculate
# time-series features for each set
#
# NOTE: setup.R must have been run
# first
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 8 April 2021
#--------------------------------------

source("setup.R") # This automatically loads the simulation function from the "/R" folder

#----------- Simulate the processes -------------

gauss_dat <- simulation_engine(min_length = 100, max_length = 10000, num_ts = 50, process = "Gaussian")
arima_dat <- simulation_engine(min_length = 100, max_length = 10000, num_ts = 50, process = "ARIMA")
sine_dat <- simulation_engine(min_length = 100, max_length = 10000, num_ts = 50, process = "Sinusoidal")

#sims <- bind_rows(gauss_dat, arima_dat, sine_dat)
#save(sims, file = "data/sims.Rda") # Store as .Rda in case of any crashes
#rm(gauss_dat, arima_dat, sine_dat) # Remove older objects from memory to save space

#----------- Calculate features -----------------

calculate_comptimes <- function(){
  
  gauss_22 <- feature_calculation_engine(data = gauss_dat, feature_set = "catch22")
  gauss_feasts <- feature_calculation_engine(data = gauss_dat, feature_set = "feasts")
  gauss_tsfeatures <- feature_calculation_engine(data = gauss_dat, feature_set = "tsfeatures")
  arima_22 <- feature_calculation_engine(data = arima_dat, feature_set = "catch22")
  arima_feasts <- feature_calculation_engine(data = arima_dat, feature_set = "feasts")
  arima_tsfeatures <- feature_calculation_engine(data = arima_dat, feature_set = "tsfeatures")
  sine_22 <- feature_calculation_engine(data = sine_dat, feature_set = "catch22")
  sine_feasts <- feature_calculation_engine(data = sine_dat, feature_set = "feasts")
  sine_tsfeatures <- feature_calculation_engine(data = sine_dat, feature_set = "tsfeatures")
  
  outs <- bind_rows(gauss_22, gauss_feasts, gauss_tsfeatures,
                    arima_22, arima_feasts, arima_tsfeatures,
                    sine_22, sine_feasts, sine_tsfeatures)
  
  return(outs)
}

comptimes <- calculate_comptimes()
save(comptimes, "data/comptimes.Rda") # Store as .Rda

#----------- Visualise performance --------------

plot_performance <- function(data, ratio = FALSE){
  
  if(ratio){
    x
  } else{
    x
  }
  
  p <- x %>%
    ggplot(aes(x = ts_length, y = calc_time, group = process)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    geom_point(aes(shape = process)) +
    labs(title = "Performance of each feature set",
         x = "Time-series Length") +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank())
  
  if(ratio){
    p <- p +
      labs(y = "Computational Time (s) per Feature")
  } else{
    p <- p +
      labs(y = "Computational Time (s)")
  }
  
  return(p)
}

overall <- plot_performance(data = outs, ratio = FALSE)
ratio <- plot_performance(data = outs, ratio = TRUE)
