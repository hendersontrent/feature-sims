#--------------------------------------
# This script sets out to visualise
# the low dimensional space of the
# Empirical 1000 features
#
# NOTE: This script requires setup.R to
# have been run first
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 9 April 2021
#--------------------------------------

# ----------------------- Webscrape the data --------------------

grab_emp1000 <- function(){
  
  # Data
  
  temp <- tempfile()
  download.file("https://ndownloader.figshare.com/files/24950795",temp)
  ts <- read.csv(temp, header = FALSE)
  
  ts <- ts %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::pivot_longer(!id, names_to = "timepoint", values_to = "value") %>%
    dplyr::mutate(timepoint = as.numeric(gsub("V", "\\1", timepoint)))
  
  # Information file
  
  temp1 <- tempfile()
  download.file("https://ndownloader.figshare.com/files/24950798",temp1)
  ts_info <- read.csv(temp1, header = TRUE)
  
  # Merge
  
  main <- ts %>%
    dplyr::left_join(ts_info, by = c("id" = "ID"))
  
  return(main)
}

empirical1000 <- grab_emp1000()

# ----------------------- Calculate features --------------------

outs <- calculate_features(data = empirical1000, id_var = "id", time_var = "timepoint", values_var = "value", 
                           feature_set = "catch22")

# Join group labels back in

group_labels <- empirical1000 %>%
  group_by(id, Keywords) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  dplyr::select(-c(counter))

removers <- c("pressure", "seismology", "chaos", "SantaFe", "space", "TSAR", "text", "medical", "Max")
'%ni%' <- Negate('%in%')

outs_label <- outs %>%
  left_join(group_labels, by = c("id" = "id")) %>%
  mutate(Keywords = sub(",.*", "\\1", Keywords)) %>%
  filter(Keywords %ni% removers)

# ----------------------- Plot low dimension---------------------

plot_low_dimension(outs_label, is_normalised = FALSE, id_var = "id", group_var = "Keywords", method = "RobustSigmoid", plot = TRUE)
