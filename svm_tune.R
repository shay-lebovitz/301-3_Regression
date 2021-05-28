# svm rbf tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(stacks)

# Handle common conflicts
tidymodels_prefer()

# load required objects ----
load('data/setup.rda')


# Define model ----
svm_model <- svm_rbf(
  mode = "regression",
  cost = tune(),
  rbf_sigma = tune()
) %>%
  set_engine("kernlab")

# # check tuning parameters
# parameters(svm_model)

# set-up tuning grid ----
svm_params <- parameters(svm_model)

# define grid
svm_grid <- grid_regular(svm_params, levels = 5)

# workflow ----
svm_workflow <- workflow() %>%
  add_model(svm_model) %>%
  add_recipe(rec)

ctrl_grid <- control_stack_grid()

# Tuning/fitting ----
svm_res <- svm_workflow %>%
  tune_grid(
    resamples = train_folds,
    grid = svm_grid,
    control = ctrl_grid
  )

# Write out results & workflow
save(svm_res, file = "data/svm_res.rda")
