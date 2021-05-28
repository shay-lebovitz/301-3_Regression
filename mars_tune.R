

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(earth)
library(stacks)

# load required objects ----
load('data/setup.rda')

# Define model ----
mars_model <-
  mars(mode = 'regression', num_terms = tune('num_terms'),
       prod_degree = tune('prod_degree')) %>% 
  set_engine('earth')

# Parameters
mars_params <- parameters(mars_model) %>% 
  update(num_terms = num_terms(range = c(2L, 100L)))

# set-up tuning grid ----
mars_grid <- grid_regular(mars_params, levels = 5)


# workflow ----
mars_workflow <- workflow() %>% 
  add_model(mars_model) %>% 
  add_recipe(rec)

ctrl_grid <- control_stack_grid()

# Tuning/fitting ----
mars_tune <- mars_workflow %>% 
  tune_grid(resamples = train_folds, grid = mars_grid, control = ctrl_grid)


# Write out results & workflow
save(mars_tune, mars_workflow, file = 'data/mars_tune.rda')