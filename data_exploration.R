# load packages
library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)
library(lubridate)
library(stacks)

# read in dada
sample_sub <- read_csv('data/sampleSubmission.csv')
train <- read_csv('data/train.csv')
test <- read_csv('data/test.csv')

# skim data
skim_without_charts(train)
# No missing data
# money_made_inv is the target variable
# id is not useful. 

# addr_state, application_type, emp_title, grade, home_ownership, initial_list_status,
# purpose, sub_grade, term, verification_status should be factors. 

# earliest_cr_line, last_credit_pull_d should be dates

# create factors, dates, and get number from emp_length
train <- train %>% 
  mutate(addr_state = as.factor(addr_state),
         application_type = as.factor(application_type),
         emp_title = as.factor(emp_title),
         grade = as.factor(grade),
         home_ownership = as.factor(home_ownership),
         initial_list_status = as.factor(initial_list_status),
         purpose = as.factor(purpose),
         sub_grade = as.factor(sub_grade),
         term = as.factor(term),
         verification_status = as.factor(verification_status),
         emp_length = parse_number(emp_length),
         earliest_cr_line = my(earliest_cr_line),
         last_credit_pull_d = my(last_credit_pull_d),
         emp_length = ifelse(is.na(emp_length), 10, emp_length),
         delinq_amnt = factor(ifelse(delinq_amnt == 0, 0, '>0')))

test <- test %>% 
  mutate(addr_state = as.factor(addr_state),
         application_type = as.factor(application_type),
         emp_title = as.factor(emp_title),
         grade = as.factor(grade),
         home_ownership = as.factor(home_ownership),
         initial_list_status = as.factor(initial_list_status),
         purpose = as.factor(purpose),
         sub_grade = as.factor(sub_grade),
         term = as.factor(term),
         verification_status = as.factor(verification_status),
         emp_length = parse_number(emp_length),
         earliest_cr_line = my(earliest_cr_line),
         last_credit_pull_d = my(last_credit_pull_d),
         emp_length = ifelse(is.na(emp_length), 10, emp_length),
         delinq_amnt = factor(ifelse(delinq_amnt == 0, 0, '>0')))

############################# EXPLORING SINGLE VARIABLES #############################
# explore money_made_inv
train %>% 
  ggplot(aes(x = money_made_inv)) + 
  geom_density()
# very left skewed, need to log

# state
train %>% 
  ggplot(aes(x = addr_state)) +
  geom_bar() +
  coord_flip()
# gonna want to `step_other() state`

# application type
train %>% 
  group_by(application_type) %>% 
  summarize(n())
# almost all are individual. 

# emp_length
train %>% 
  ggplot(aes(x = emp_length)) +
  geom_histogram()

# emp_title
train %>% 
  group_by(emp_title) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>% 
  view()

# num_tl_120_dpd_2m
train %>% 
  group_by(num_tl_120dpd_2m) %>% 
  summarize(count = n()) %>% 
  view()
# all 0 except 4 1's - Should be dummy

# num_tl_90g_dpd_24m
train %>% 
  group_by(num_tl_90g_dpd_24m) %>% 
  summarize(count = n())
# vast majority are 0, but goes up to 15. 

# num_tl_30dpd
train %>% 
  group_by(num_tl_30dpd) %>% 
  summarize(count = n())
# almost all 0, 17 1's, and 1 2. Should be dummy

# purpose
train %>% 
  group_by(purpose) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  view()
test %>% 
  group_by(purpose) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  view()

# acc_now_delinq
train %>% 
  group_by(acc_now_delinq) %>% 
  summarize(count = n())
# almost entirely 0 - should be dummy

# acc_open_past_24mths
train %>% 
  group_by(acc_open_past_24mths) %>% 
  summarize(count = n())
# USEFUL

# delinq_2yrs
train %>% 
  group_by(delinq_2yrs) %>% 
  summarize(count = n())
# goes up to 18, most are 0, 1, or 2

# delinq_amnt
train %>% 
  group_by(delinq_amnt) %>% 
  summarize(count = n())
# useless

# grade
train %>% 
  group_by(grade) %>% 
  summarize(count = n())
# USEFUL

# home_ownership
train %>% 
  group_by(home_ownership) %>% 
  summarize(count = n())
# USEFUL

# mort_acc
train %>% 
  group_by(mort_acc) %>% 
  summarize(count = n())
# Useful

# initial_list_status
train %>% 
  group_by(initial_list_status) %>% 
  summarize(count = n())
# USEFUL as factor

# pub_rec
train %>% 
  group_by(pub_rec) %>% 
  summarize(count = n())
# USEFUL as numeric

# pub_rec_bankruptcies
train %>% 
  group_by(pub_rec_bankruptcies) %>% 
  summarize(count = n())

########################### EXPLORING INTERACTIONS ####################
# with money made investing
cor(train %>% select(money_made_inv, int_rate, annual_inc, avg_cur_bal,
                     bc_util, emp_length, loan_amnt, out_prncp_inv))

# money_made_inv is highly negatively correlated with out_prncp_inv
train %>% 
  ggplot(aes(x = out_prncp_inv, y = money_made_inv)) +
  geom_point(aes(color = verification_status))
#OOOOHHH
# color by:
  # application type - NO
  # emp_length - MAYBE
  # grade - YES
  # home_ownership - MAYBE
  # initial_list_status - NO
  # purpose - NO
  # subgrade - MAYBE
  # term - YES
  # verification_status - NO

# linear model with just out_prncp_inv
linear_model <- lm(money_made_inv ~ (out_prncp_inv*grade*loan_amnt*term), data = train)
summary(linear_model)
sqrt(mean(linear_model$residuals^2))

########################### SETTING UP MODELS #########################
# folds
train_folds <- vfold_cv(train, v = 5, repeats = 3, strata = money_made_inv)

# recipe
rec <- recipe(money_made_inv ~ ., data = train) %>% 
  update_role(id, new_role = 'ID') %>% 
  step_rm(num_tl_120dpd_2m, num_tl_30dpd, acc_now_delinq, purpose) %>% 
  step_date(earliest_cr_line, last_credit_pull_d) %>% 
  step_other(addr_state, threshold = 100) %>% 
  step_other(emp_title, threshold = 50) %>% 
  step_dummy(all_nominal()) %>% 
  step_impute_median(emp_length) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_interact(terms = ~ (all_predictors() + all_predictors())^2)

rec_no_date <- recipe(money_made_inv ~ ., data = train) %>% 
  update_role(id, new_role = 'ID') %>% 
  step_novel(purpose) %>% 
  step_rm(purpose, earliest_cr_line, last_credit_pull_d) %>% 
  step_other(addr_state, threshold = 100) %>% 
  step_other(emp_title, threshold = 50) %>% 
  step_dummy(all_nominal()) %>% 
  step_impute_median(emp_length) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_interact(terms = ~ (all_predictors() + all_predictors())^2)

rec_no_date_3 <- recipe(money_made_inv ~ ., data = train) %>% 
  update_role(id, new_role = 'ID') %>% 
  step_novel(purpose) %>% 
  step_rm(purpose, earliest_cr_line, last_credit_pull_d) %>% 
  step_other(addr_state, threshold = 100) %>% 
  step_other(emp_title, threshold = 50) %>% 
  step_dummy(all_nominal()) %>% 
  step_impute_median(emp_length) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_interact(terms = ~ (all_predictors() + all_predictors())^3)


rec %>% prep() %>% bake(new_data = NULL) %>% skim_without_charts()
rec_no_date %>% prep() %>% bake(new_data = NULL) %>% dim()
# Notes:
  # can't step_normalize() all_predictors

# save out data
save(train_folds, train, rec, rec_no_date_3, file = 'data/setup.rda')

# load data
load('data/rf_tune.rda')
load('data/mars_tune.rda')
load('data/slnn_tune.rda')
load('data/knn_tune.rda')
load('data/svm_res.rda')
load('data/bt_tune.rda')

# find best performing model
show_best(rf_tune, metric = 'rmse')
show_best(mars_tune, metric = 'rmse')
show_best(slnn_tune, metric = 'rmse')
show_best(knn_tune, metric = 'rmse')
show_best(svm_res, metric = 'rmse')
show_best(bt_tune, metric = 'rmse')

# finalize workflow
rf_workflow_tuned <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

mars_workflow_tuned <- mars_workflow %>% 
  finalize_workflow(select_best(mars_tune, metric = 'rmse'))

slnn_workflow_tuned <- slnn_workflow %>% 
  finalize_workflow(select_best(slnn_tune, metric = 'rmse'))

knn_workflow_tuned <- knn_workflow %>% 
  finalize_workflow(select_best(knn_tune, metric = 'rmse'))

svm_workflow_tuned <- svm_workflow %>% 
  finalize_workflow(select_best(svm_res, metric = 'rmse'))

bt_workflow_tuned <- bt_workflow %>% 
  finalize_workflow(select_best(bt_tune, metric = 'rmse'))

# fit to whole training data
rf_results <- fit(rf_workflow_tuned, train)
mars_results <- fit(mars_workflow_tuned, train)
slnn_results <- fit(slnn_workflow_tuned, train)
knn_results <- fit(knn_workflow_tuned, train)
svm_results <- fit(svm_workflow_tuned, train)
bt_results <- fit(bt_workflow_tuned, train)

# create submission
test_rf <- bind_cols(test %>% select(id),
                     predict(rf_results, new_data = test)) 
test_mars <- bind_cols(test %>% select(id), 
                       predict(mars_results, new_data = test))
test_slnn <- bind_cols(test %>% select(id), 
                       predict(slnn_results, new_data = test))
test_knn <- bind_cols(test %>% select(id),
                      predict(knn_results, new_data = test))
test_svm <- bind_cols(test %>% select(id),
                      predict(svm_results, new_data = test))
test_bt <- bind_cols(test %>% select(id),
                     predict(bt_results, new_data = test))


# write out for submission
test_rf %>% 
  rename(Id = id, Predicted = .pred) %>% 
  write_csv('submissions/rf_submission_1.csv')
test_mars %>% 
  rename(Id = id, Predicted = .pred) %>% 
  write_csv('submissions/mars_submission_3.csv')
test_slnn %>% 
  rename(Id = id, Predicted = .pred) %>% 
  write_csv('submissions/slnn_submission_1.csv')
test_svm %>% 
  rename(ID = id, Predicted = .pred) %>%
  write_csv('submissions/svm_submission_1.csv')  
test_bt %>% 
  rename(ID = id, Predicted = .pred) %>% 
  write_csv('submissions/bt_submission_2.csv')

train %>% 
  group_by(purpose) %>% 
  summarize(count = n())
