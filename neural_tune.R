# Load package(s) ----
library(tidyverse)
library(tidymodels)


# load required objects ----
load('data/setup.rda')

# Define model ----
slnn_model <-
  mlp(mode = 'regression', hidden_units = tune('hidden_units'),
      penalty = tune('penalty')) %>% 
  set_engine('nnet', MaxNWts = 1500)

# Parameters
slnn_params <- parameters(slnn_model)

# set-up tuning grid ----
slnn_grid <- grid_regular(slnn_params, levels = 5)


# workflow ----
slnn_workflow <- workflow() %>% 
  add_model(slnn_model) %>% 
  add_recipe(rec)

# Tuning/fitting ----

slnn_tune <- slnn_workflow %>% 
  tune_grid(resamples = train_folds, grid = slnn_grid)


# Write out results & workflow
save(slnn_tune, slnn_workflow, file = 'data/slnn_tune.rda')