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



#----------- Calculate features -----------------



#----------- Store as an .Rda -------------------

# This is just a safety against potential local machine crashes during calculations

save(outs, "output/calculated_features.Rda")

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
