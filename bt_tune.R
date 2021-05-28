# load required packages ----
library(tidymodels)
library(tidyverse)
library(stacks)

# load required objects ----
load('data/setup.rda')

# Define model ----
bt_model <-
  boost_tree(mode = 'regression', mtry = tune('mtry'), trees = 500,
             min_n = tune('min_n'), learn_rate = tune('learn_rate')) %>% 
  set_engine('xgboost')


# Parameters -----
bt_params <- parameters(bt_model) %>% 
  update(mtry = mtry(range = c(2, 1000))) %>% # Note: the other BT model used `mtry(range = c(2, 300))`
  update(learn_rate = learn_rate(c(-5, -0.2)))

# set-up tuning grid ----
bt_grid <- grid_regular(bt_params, levels = 8) # Note: the other BT model used `levels = 8`

# workflow ----
bt_workflow <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(rec_no_date_3) # Note: the other BT model used `rec_no_date`

ctrl_grid <- control_stack_grid()

# tuning ----
bt_tune <- bt_workflow %>% 
  tune_grid(resamples = train_folds, grid = bt_grid, control = ctrl_grid)

# write out data ----
save(bt_tune, bt_workflow, file = 'data/bt_tune.rda')


