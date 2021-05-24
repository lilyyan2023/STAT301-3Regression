# Load Packages
library(tidyverse)
library(tidymodels)
library(corrr)
library(ranger)
set.seed(123)
# Load data
loan_train <- read_csv("train.csv")
loan_test <- read_csv("test.csv")

# Split data
loan_folds <- vfold_cv(data = loan_train, v = 5, repeats = 3)

# Build recipe
loan_recipe <-
  recipe(money_made_inv ~ loan_amnt + out_prncp_inv + application_type +
           initial_list_status + term + grade + verification_status,
         data = loan_train) %>%
  step_interact(loan_amnt ~ out_prncp_inv) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# loan_train_try <-
#   loan_train %>% 
#   select(-c(earliest_cr_line, emp_title, id))
# 
# loan_recipe2 <- 
#   recipe(money_made_inv ~ ., data = loan_train_try) %>% 
#   step_zv(all_predictors()) %>% 
#   step_other(addr_state, emp_length, last_credit_pull_d, purpose, sub_grade, 
#              threshold = 0.1, other = "other values") %>% 
#   step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
#   step_normalize(all_predictors())

tibble <- prep(loan_recipe) %>% 
  bake(new_data = NULL)

# random forest model
rf_model <- rand_forest(
  mode = "regression",
  mtry = tune(),
  min_n = tune()
) %>% 
  set_engine("ranger")

# set-up tuning grid ----
rf_params <- parameters(rf_model) %>% 
  update(mtry = mtry(range = c(1,20)),
         min_n = min_n(range = c(30L, 50L)))

# define tuning grid
rf_grid <- grid_regular(rf_params, levels = 5)

# workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(loan_recipe)

rf_tuned <- rf_workflow %>% 
  tune_grid(loan_folds, rf_grid)

write_rds(rf_tuned, "rf_results.rds")


