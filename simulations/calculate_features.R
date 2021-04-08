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

source("setup.R") # This automatically loads the simulation functions from the "/R" folder

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
save(comptimes, file = "data/comptimes.Rda") # Store as .Rda

#----------- Visualise performance --------------

plot_performance <- function(data, ratio = FALSE, log = FALSE){
  
  tmp <- data
  
  if(ratio){
    
    # Extract number of features in each set
    
    total_feats <- theft::feature_list %>%
      group_by(feature_set) %>%
      summarise(num_feats = n()) %>%
      ungroup()
    
    # Calculate ratio
    
    tmp <- tmp %>%
      left_join(total_feats, by = c("feature_set" = "feature_set")) %>%
      mutate(comp_time_per_feature = comp_time / num_feats)
  }
  
  if(log){
    if(ratio){
      p <- tmp %>%
        ggplot(aes(x = log(ts_length), y = comp_time_per_feature, group = feature_set)) +
        labs(x = "log(Time-series Length)",
             y = "Computational Time per Feature")
    } else{
      p <- tmp %>%
        ggplot(aes(x = log(ts_length), y = comp_time, group = feature_set)) +
        labs(x = "log(Time-series Length)",
             y = "Computational Time")
    }
  } else{
    if(ratio){
      p <- tmp %>%
        ggplot(aes(x = ts_length, y = comp_time_per_feature, group = feature_set)) +
        labs(x = "Time-series Length",
             y = "Computational Time per Feature")
    } else{
      p <- tmp %>%
        ggplot(aes(x = ts_length, y = comp_time, group = feature_set)) +
        labs(x = "Time-series Length",
             y = "Computational Time")
    }
  }
  
  p <- p +
    geom_point(aes(shape = feature_set)) +
    labs(title = "Performance of each feature set",
         shape = "Feature Set") +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank()) +
    facet_wrap(~process, dir = "v")
  
  return(p)
}

p <- plot_performance(data = comptimes, ratio = FALSE, log = FALSE)
p1 <- plot_performance(data = comptimes, ratio = TRUE, log = FALSE)

p2 <- plot_performance(data = comptimes, ratio = FALSE, log = TRUE)
p3 <- plot_performance(data = comptimes, ratio = TRUE, log = TRUE)

# Save plots

ggsave(filename = "output/comptime.png", plot = p)
ggsave(filename = "output/comptime-ratio.png", plot = p1)
ggsave(filename = "output/comptime-log.png", plot = p2)
ggsave(filename = "output/comptime-ratio-log.png", plot = p3)
