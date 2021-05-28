# Load package(s) ----
library(tidyverse)
library(tidymodels)

# load required objects ----
load('data/setup.rda')

# Define model ----
rf_model <-
  rand_forest(mode = 'regression', mtry = tune('mtry'), min_n = tune('min_n')) %>% 
  set_engine('ranger')

# Parameters
rf_params <- parameters(rf_model) %>% 
  update(mtry = mtry(range = c(2, 8)))

# set-up tuning grid ----
rf_grid <- grid_regular(rf_params, levels = 5)

# workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rec)

# Tuning/fitting ----
rf_tune <- rf_workflow %>% 
  tune_grid(resamples = train_folds, grid = rf_grid)

save(rf_tune, rf_workflow, file = 'data/rf_tune.rda')
