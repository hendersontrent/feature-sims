#--------------------------------------
# This script sets out to build a
# feature-based classifier on 
# WormsTwoClass data from:
# http://www.timeseriesclassification.com/description.php?Dataset=WormsTwoClass
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 9 April 2021
#--------------------------------------

# ----------------------- Read in data --------------------

# Load in .arff file and wrangle into tidy format
# NOTE: THIS SHOULD BE ADAPTED TO WEBSCRAPE RATHER THAN USE DOWNLOADED FILES

train <- foreign::read.arff("data/WormsTwoClass_TRAIN.arff") %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = att1:att900, names_to = "timepoint", values_to = "value") %>%
  mutate(timepoint = gsub("att", "\\1", timepoint)) %>%
  mutate(timepoint = as.numeric(timepoint)) %>%
  mutate(id = as.character(id)) %>%
  mutate(id = as.integer(id))

test <- foreign::read.arff("data/WormsTwoClass_TEST.arff") %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = att1:att900, names_to = "timepoint", values_to = "value") %>%
  mutate(timepoint = gsub("att", "\\1", timepoint)) %>%
  mutate(timepoint = as.numeric(timepoint)) %>%
  mutate(id = as.character(id)) %>%
  mutate(id = as.integer(id))

# ----------------------- Calculate features --------------

# Calculations

train_feats <- calculate_features(data = train, id_var = "id", time_var = "timepoint", values_var = "value",
                            feature_set = "catch22")

test_feats <- calculate_features(data = test, id_var = "id", time_var = "timepoint", values_var = "value",
                                  feature_set = "catch22")

# Normalisation

train_norm <- normalise_feature_frame(data = train_feats, names_var = "names", values_var = "values",
                                      method = "z-score")

test_norm <- normalise_feature_frame(data = test_feats, names_var = "names", values_var = "values",
                                     method = "z-score")

train_norm1 <- train_feats %>%
  dplyr::group_by(names) %>%
  dplyr::mutate(values = (values-mean(values, na.rm = TRUE))/sd(values, na.rm = TRUE)) %>%
  dplyr::ungroup()

test_norm1 <- test_feats %>%
  dplyr::group_by(names) %>%
  dplyr::mutate(values = (values-mean(values, na.rm = TRUE))/sd(values, na.rm = TRUE)) %>%
  dplyr::ungroup()

# Rejoin group labels

get_labs <- function(data1,data2){
  group_labels <- data1 %>%
    group_by(id, target) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    dplyr::select(-c(counter))
  
  tmp <- data2 %>%
    left_join(group_labels, by = c("id" = "id"))
}

train_norm1 <- get_labs(train,train_norm1)
test_norm1 <- get_labs(test,test_norm1)

# ----------------------- Data vis ------------------------

plot_feature_matrix(train_norm1, is_normalised = TRUE, id_var = "id")
plot_low_dimension(train_norm1, is_normalised = TRUE, id_var = "id", group_var = "target", plot = TRUE)

# ----------------------- Regression prep ----------------

# Create wide dataframe

train_wide <- train_norm1 %>%
  pivot_wider(id_cols = c("id","target"), names_from = "names", values_from = "values") %>%
  mutate(target = as.numeric(target)) %>%
  mutate(target = target-1) %>% # Recode into [0,1]
  mutate(target = as.factor(target)) %>%
  dplyr::select(-c(id))

test_wide <- test_norm1 %>%
  pivot_wider(id_cols = c("id","target"), names_from = "names", values_from = "values") %>%
  mutate(target = as.numeric(target)) %>%
  mutate(target = target-1) %>% # Recode into [0,1]
  mutate(target = as.factor(target)) %>%
  dplyr::select(-c(id))

# ----------------------- Classification -----------------

# Fit a random forest

set.seed(123)

m1 <- randomForest::randomForest(formula = target ~ .,
                 data = train_wide,
                 importance = TRUE)

# Variable importance plot

Cairo::CairoPNG("output/varimp.png", 800, 600)
randomForest::varImpPlot(m1)
dev.off()

# Classification accuracy

y_pred <- predict(m1, newdata = test_wide)
a <- table(test_wide$target, y_pred)
caret::confusionMatrix(a)
