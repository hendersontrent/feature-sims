#--------------------------------------
# This script sets out to load all the
# things necessary to run the project
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 8 April 2021
#--------------------------------------

library(data.table)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(scales)
library(catch22) # devtools::install_github("hendersontrent/catch22")
library(theft) # devtools::install_github("hendersontrent/theft")
library(microbenchmark)
library(Cairo)

# Create important folders if none exist:

if(!dir.exists('simulations')) dir.create('simulations')
if(!dir.exists('R')) dir.create('R')
if(!dir.exists('output')) dir.create('output')
if(!dir.exists('data')) dir.create('data')

# Load any functions written for this project

scripts <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)

for(s in scripts){
  source(s)
}
