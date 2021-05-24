# Load Packages
library(tidyverse)
library(tidymodels)
library(corrr)
library(ranger)

set.seed(123)
# Load data
loan_train <- read_csv("train.csv")
loan_test <- read_csv("test.csv")

skimr::skim_without_charts(loan_train)

# EDA
loan_train %>% 
  select(money_made_inv, acc_now_delinq, acc_open_past_24mths, annual_inc,
         avg_cur_bal, bc_util, delinq_2yrs, delinq_amnt, dti) %>% 
  cor() 
#bc_util has a somewhat negative correlation with acc_open_past_24mths, dti 
# has a somewhat positive correlation with acc_open_past_24mths, bc_util, annual_inc
# has somewhat positive correlation with avg_cur_bal, dti has somewhat negative 
# correlation with annual_inc, avg_cur_bal.

loan_train %>% 
  select(money_made_inv, int_rate, loan_amnt, mort_acc, num_sats, num_tl_120dpd_2m,
         num_tl_90g_dpd_24m, num_tl_30dpd, out_prncp_inv, pub_rec, pub_rec_bankruptcies,
         tot_coll_amt, tot_cur_bal, total_rec_late_fee) %>% 
  cor()
# money_made_inv has negative correlation with loan_amnt, strong negative correlation
# with out_prncp_inv, loan_amnt has positive correlation with mort_acc, num_sats
# has positive correlation with loan_amnt, mort_acc, out_prncp_inv has positive correlation
# with loan_amnt, tot_cur_bal has positive correlation with loan_amnt, tot_cur_bal
# has strong positive correlation with mort_acc, pub_rec has a strong positive correlation
# with pub_rec_bankruptcies
loan_train_at <-
  loan_train %>% 
  group_by(application_type) %>% 
  summarise(sum = sum(money_made_inv)) 
loan_train_c <- loan_train %>% 
  group_by(application_type) %>% 
  count()
loan_train_atc <- merge(loan_train_at, loan_train_c, by= c("application_type"))
loan_train_atc <- loan_train_atc %>% 
  mutate(prop = sum/n)
loan_train_atc %>% 
  ggplot(aes(x = application_type, y = prop))+
  geom_col() # application_type yes

loan_train_e <-
  loan_train %>% 
  group_by(emp_length) %>% 
  summarise(sum = sum(money_made_inv)) 
loan_train_ec <- loan_train %>% 
  group_by(emp_length) %>% 
  count()
loan_train_ecf <- merge(loan_train_e, loan_train_ec, by= c("emp_length"))
loan_train_ecf <- loan_train_ecf %>% 
  mutate(prop = sum/n)
loan_train_ecf %>% 
  ggplot(aes(x = emp_length, y = prop)) +
  geom_col() # emp_length, no

loan_train_g <-
  loan_train %>% 
  group_by(grade) %>% 
  summarise(sum = sum(money_made_inv)) 
loan_train_gc <- loan_train %>% 
  group_by(grade) %>% 
  count()
loan_train_gcf <- merge(loan_train_g, loan_train_gc, by= c("grade"))
loan_train_gcf <- loan_train_gcf %>% 
  mutate(prop = sum/n)

loan_train_gcf %>% 
  ggplot(aes(x = grade, y = prop)) +
  geom_col() # grade can consider

loan_train_h <-
  loan_train %>% 
  group_by(home_ownership) %>% 
  summarise(sum = sum(money_made_inv)) 
loan_train_hc <- loan_train %>% 
  group_by(home_ownership) %>% 
  count()
loan_train_hcf <- merge(loan_train_h, loan_train_hc, by= c("home_ownership"))
loan_train_hcf <- loan_train_hcf %>% 
  mutate(prop = sum/n)
loan_train_hcf %>% 
  ggplot(aes(x = home_ownership, y = prop)) +
  geom_col() # home_ownership yes

loan_train_i <-
  loan_train %>% 
  group_by(initial_list_status) %>% 
  summarise(sum = sum(money_made_inv)) 
loan_train_ic <- loan_train %>% 
  group_by(initial_list_status) %>% 
  count()
loan_train_icf <- merge(loan_train_i, loan_train_ic, by= c("initial_list_status"))
loan_train_icf <- loan_train_icf %>% 
  mutate(prop = sum/n)
loan_train_icf %>% 
  ggplot(aes(x = initial_list_status, y = prop)) +
  geom_col() # initial_list_status yes

loan_train_p <-
  loan_train %>% 
  group_by(purpose) %>% 
  summarise(sum = sum(money_made_inv)) 
loan_train_pc <- loan_train %>% 
  group_by(purpose) %>% 
  count()
loan_train_pcf <- merge(loan_train_p, loan_train_pc, by= c("purpose"))
loan_train_pcf <- loan_train_pcf %>% 
  mutate(prop = sum/n)
loan_train_pcf %>% 
  ggplot(aes(x = purpose, y = prop)) +
  geom_col() # purpose can consider

loan_train_t <-
  loan_train %>% 
  group_by(term) %>% 
  summarise(sum = sum(money_made_inv)) 
loan_train_tc <- loan_train %>% 
  group_by(term) %>% 
  count()
loan_train_tcf <- merge(loan_train_t, loan_train_tc, by= c("term"))
loan_train_tcf <- loan_train_tcf %>% 
  mutate(prop = sum/n)
loan_train_tcf %>% 
  ggplot(aes(x = term, y = prop)) +
  geom_col() # term yes

loan_train_v <-
  loan_train %>% 
  group_by(verification_status) %>% 
  summarise(sum = sum(money_made_inv)) 
loan_train_vc <- loan_train %>% 
  group_by(verification_status) %>% 
  count()
loan_train_vcf <- merge(loan_train_v, loan_train_vc, by= c("verification_status"))
loan_train_vcf <- loan_train_vcf %>% 
  mutate(prop = sum/n)
loan_train_vcf %>% 
  ggplot(aes(x = verification_status, y = prop)) +
  geom_col() # verification_status can consider ~

# Split data
loan_folds <- vfold_cv(data = loan_train, v = 5, repeats = 3)

# Build recipe
loan_train_try <-
  loan_train %>% 
  select(-earliest_cr_line, emp_title)
loan_recipe2 <- 
  recipe(money_made_inv ~ ., data = loan_train_try) %>% 
  step_zv(all_predictors()) %>% 
  step_other(addr_state, emp_length, last_credit_pull_d, purpose, sub_grade, 
             threshold = 0.1, other = "other values") %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_normalize(all_predictors())
prep(loan_recipe2) %>% 
  bake(new_data = NULL)

loan_recipe <- 
  recipe(money_made_inv ~ loan_amnt + out_prncp_inv + application_type +
            initial_list_status + term + grade + verification_status, 
         data = loan_train) %>% 
  step_interact(loan_amnt ~ out_prncp_inv) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

prep(loan_recipe) %>% 
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
  update(mtry = mtry(range = c(1,20)))

# define tuning grid
rf_grid <- grid_regular(rf_params, levels = c(3,4))

# workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(loan_recipe)

rf_tuned <- rf_workflow %>% 
  tune_grid(loan_folds, rf_grid)

# performance metrics
autoplot(rf_tuned, metric = "rmse")
autoplot(rf_tuned, metric = "rsq")
select_best(rf_tuned, metric = "rmse")
select_best(rf_tuned, metric = "rsq")
show_best(rf_tuned, metric = "rmse")
show_best(rf_tuned, metric = "rsq")

rf_workflow_tuned <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tuned, metric = "rmse"))

rf_results <- fit(rf_workflow_tuned, loan_train)

rf_final_results <- rf_results %>% 
  predict(new_data = loan_test) %>% 
  bind_cols(loan_test %>% select(id)) %>% 
  select(id, .pred)

write_csv(rf_final_results, "rf_output.csv")


