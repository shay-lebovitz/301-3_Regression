# {MODEL TYPE] tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)

# load required objects ----
load('data/setup.rda')

# Define model ----
knn_model <-
  nearest_neighbor(mode = 'regression', neighbors = tune('neighbors')) %>% 
  set_engine('kknn')

# Parameters
knn_params <- parameters(knn_model)

# set-up tuning grid ----
knn_grid <- grid_regular(knn_params, levels = 5)

# workflow ----
knn_workflow <- workflow() %>% 
  add_model(knn_model) %>% 
  add_recipe(rec)

# Tuning/fitting ----

knn_tune <- knn_workflow %>% 
  tune_grid(resamples = train_folds, grid = knn_grid)


# Write out results & workflow
save(knn_tune, knn_workflow, file = 'data/knn_tune.rda')

