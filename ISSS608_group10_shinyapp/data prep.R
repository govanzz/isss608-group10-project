library(tidyverse)
library(tidymodels)
library(vip)
library(lubridate)

# ==========================================
# 1. SETUP & CUSTOMER DATA CLEANING
# ==========================================
data_root <- if (dir.exists("data")) {
  "data"
} else {
  file.path("In_class_Ex", "Inclass_Ex_07", "data")
}

# Load the existing app data
customers <- readRDS(file.path(data_root, "customers_shiny.rds"))

# Apply all transformations (Demographics & Financial Bins)
customers <- customers %>%
  # Demographic Groupings
  mutate(age_group = case_when(
    age <= 35 ~ "20-35", 
    age >= 36 & age <= 50 ~ "36-50",
    age >= 51 & age <= 65 ~ "51-65"
  )) %>%
  mutate(age_group = factor(age_group, levels = c("20-35", "36-50", "51-65"))) %>%
  
  mutate(hh_group = case_when(
    household_size == 1 ~ "1",
    household_size >= 2 & household_size <= 3 ~ "2-3",
    household_size >= 4 & household_size <= 5 ~ "4-5",
    household_size >= 6 ~ "6+",
    TRUE ~ "Unknown"
  )) %>%
  mutate(hh_group = factor(hh_group, levels = c("1", "2-3", "4-5", "6+"))) %>%
  
  # Financial Binning
  mutate(Average_Transaction_bins = cut(
    avg_tx_value, 
    breaks = c(0, 1339108, 4658605, Inf),
    labels = c("Low Spend", "Medium Spend", "High Spend")
  )) %>%
  mutate(Transaction_Count_bins = cut(
    tx_count, 
    breaks = c(0, 12, 35, Inf),
    labels = c("Low Spend", "Medium Spend", "High Spend")
  )) %>%
  mutate(Total_Volume_bins = cut(
    total_tx_volume, 
    breaks = c(0, 20157600, 120461400, Inf),
    labels = c("Low Spend", "Medium Spend", "High Spend")
  ))

# ==========================================
# 2. PREDICTIVE MODELLING (TRANSACTIONS)
# ==========================================
predictive_selected_vars <- c(
  "age", "gender", "income_bracket", "occupation", "education_level",
  "marital_status", "household_size", "customer_segment", "acquisition_channel",
  "customer_tenure", "savings_account", "credit_card", "personal_loan",
  "investment_account", "insurance_product", "active_products", "app_logins_frequency",
  "feature_usage_diversity", "bill_payment_user", "auto_savings_enabled",
  "credit_utilization_ratio", "international_transactions", "failed_transactions",
  "base_satisfaction", "product_satisfaction", "satisfaction_score", "nps_score",
  "support_tickets_count", "resolved_tickets_ratio", "app_store_rating",
  "feedback_sentiment", "feature_requests"
)

predictive_customer_data <- readr::read_csv(
  file.path(data_root, "transaction", "customer_data.csv"),
  show_col_types = FALSE
)

predictive_customer_model_data <- predictive_customer_data %>%
  filter(!is.na(avg_daily_transactions)) %>%
  mutate(log_avg_daily_transactions = log1p(avg_daily_transactions))

predictive_model_data <- predictive_customer_model_data %>%
  select(all_of(predictive_selected_vars), log_avg_daily_transactions)

set.seed(1234)
predictive_split <- initial_split(predictive_model_data, prop = 0.70)
predictive_train_data <- training(predictive_split)
predictive_temp_data <- testing(predictive_split)

set.seed(1234)
predictive_validation_split <- initial_split(predictive_temp_data, prop = 0.50)
predictive_validation_data <- training(predictive_validation_split)

predictive_recipe <- recipe(log_avg_daily_transactions ~ ., data = predictive_train_data) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

predictive_lm_fit <- readRDS(file.path(data_root, "transaction", "models", "lm_fit.rds"))
predictive_rf_final <- readRDS(file.path(data_root, "transaction", "models", "rf_final.rds"))
predictive_tree_tuned <- readRDS(file.path(data_root, "transaction", "models", "tree_tuned.rds"))

predictive_best_tree <- select_best(predictive_tree_tuned, metric = "rmse")

predictive_tree_model <- decision_tree(
  tree_depth = predictive_best_tree$tree_depth,
  cost_complexity = predictive_best_tree$cost_complexity,
  min_n = predictive_best_tree$min_n
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

predictive_tree_final <- workflow() %>%
  add_recipe(predictive_recipe) %>%
  add_model(predictive_tree_model) %>%
  fit(data = predictive_train_data)

predictive_lm_preds <- suppressWarnings(
  predict(predictive_lm_fit, new_data = predictive_validation_data)
) %>%
  bind_cols(predictive_validation_data %>% select(log_avg_daily_transactions)) %>%
  mutate(model = "Linear Regression", residual = log_avg_daily_transactions - .pred)

predictive_tree_preds <- predict(predictive_tree_final, new_data = predictive_validation_data) %>%
  bind_cols(predictive_validation_data %>% select(log_avg_daily_transactions)) %>%
  mutate(model = "Decision Tree", residual = log_avg_daily_transactions - .pred)

predictive_rf_preds <- predict(predictive_rf_final, new_data = predictive_validation_data) %>%
  bind_cols(predictive_validation_data %>% select(log_avg_daily_transactions)) %>%
  mutate(model = "Random Forest", residual = log_avg_daily_transactions - .pred)

predictive_all_preds <- bind_rows(predictive_lm_preds, predictive_tree_preds, predictive_rf_preds)

predictive_model_metrics <- predictive_all_preds %>%
  group_by(model) %>%
  summarise(
    RMSE = rmse_vec(log_avg_daily_transactions, .pred),
    MAE = mae_vec(log_avg_daily_transactions, .pred),
    R2 = rsq_vec(log_avg_daily_transactions, .pred),
    .groups = "drop"
  )

predictive_metric_plot_data <- predictive_model_metrics %>%
  mutate(model = factor(model, levels = c("Decision Tree", "Linear Regression", "Random Forest"))) %>%
  pivot_longer(cols = c(RMSE, MAE, R2), names_to = "Metric", values_to = "Value")

predictive_rf_importance <- vi(extract_fit_parsnip(predictive_rf_final)$fit, num_features = 15)

predictive_lm_coef <- extract_fit_parsnip(predictive_lm_fit)$fit %>%
  broom::tidy() %>%
  filter(term != "(Intercept)", !is.na(estimate)) %>%
  mutate(abs_estimate = abs(estimate)) %>%
  arrange(desc(abs_estimate)) %>%
  slice_head(n = 15)


# ==========================================
# 3. PREDICTIVE MODELLING (SATISFACTION)
# ==========================================
satisfaction_customer_data <- readr::read_csv(
  file.path(data_root, "satisfaction", "customer_data.csv"),
  show_col_types = FALSE
)

satisfaction_transactions_data <- readr::read_csv(
  file.path(data_root, "satisfaction", "transactions_data.csv"),
  show_col_types = FALSE
) %>%
  mutate(date = lubridate::ymd(date))

satisfaction_reference_date <- max(satisfaction_transactions_data$date, na.rm = TRUE)

satisfaction_transaction_features <- satisfaction_transactions_data %>%
  group_by(customer_id) %>%
  summarise(
    trans_count_eng = n(),
    avg_amount_eng = mean(amount, na.rm = TRUE),
    sd_amount_eng = sd(amount, na.rm = TRUE),
    total_amount_eng = sum(amount, na.rm = TRUE),
    max_amount_eng = max(amount, na.rm = TRUE),
    min_amount_eng = min(amount, na.rm = TRUE),
    recency_days_eng = as.numeric(satisfaction_reference_date - max(date, na.rm = TRUE)),
    active_months_eng = n_distinct(floor_date(date, unit = "month")),
    unique_tx_types_eng = n_distinct(type),
    weekend_ratio_eng = mean(wday(date) %in% c(1, 7), na.rm = TRUE),
    .groups = "drop"
  )

satisfaction_model_data <- satisfaction_customer_data %>%
  filter(!is.na(satisfaction_score)) %>%
  left_join(satisfaction_transaction_features, by = "customer_id") %>%
  select(-any_of(c(
    "customer_id", "first_tx", "last_tx", "last_survey_date", "last_transaction_date",
    "first_transaction_date", "base_satisfaction", "tx_satisfaction", "product_satisfaction",
    "nps_score", "tx_count", "avg_tx_value", "total_tx_volume", "monthly_transaction_count",
    "average_transaction_value", "total_transaction_volume", "transaction_frequency",
    "churn_probability", "customer_lifetime_value", "location", "feature_requests",
    "complaint_topics", "occupation", "weekend_transaction_ratio", "total_amount_eng",
    "unique_tx_types_eng", "min_amount_eng", "sd_amount_eng"
  )))

set.seed(1234)
satisfaction_initial_split <- initial_split(satisfaction_model_data, prop = 0.70)
satisfaction_temp_data <- testing(satisfaction_initial_split)

set.seed(1234)
satisfaction_temp_split <- initial_split(satisfaction_temp_data, prop = 0.50)
satisfaction_valid_data <- training(satisfaction_temp_split)

satisfaction_lm_fit <- readRDS(file.path(data_root, "satisfaction", "models", "final_lm.rds"))
satisfaction_tree_fit <- readRDS(file.path(data_root, "satisfaction", "models", "final_tree.rds"))
satisfaction_rf_fit <- readRDS(file.path(data_root, "satisfaction", "models", "final_rf.rds"))

satisfaction_lm_preds <- predict(satisfaction_lm_fit, new_data = satisfaction_valid_data) %>%
  bind_cols(satisfaction_valid_data %>% select(satisfaction_score)) %>%
  mutate(model = "Linear Regression", residual = satisfaction_score - .pred)

satisfaction_tree_preds <- predict(satisfaction_tree_fit, new_data = satisfaction_valid_data) %>%
  bind_cols(satisfaction_valid_data %>% select(satisfaction_score)) %>%
  mutate(model = "Decision Tree", residual = satisfaction_score - .pred)

satisfaction_rf_preds <- predict(satisfaction_rf_fit, new_data = satisfaction_valid_data) %>%
  bind_cols(satisfaction_valid_data %>% select(satisfaction_score)) %>%
  mutate(model = "Random Forest", residual = satisfaction_score - .pred)

satisfaction_all_preds <- bind_rows(satisfaction_lm_preds, satisfaction_tree_preds, satisfaction_rf_preds)

satisfaction_model_metrics <- satisfaction_all_preds %>%
  group_by(model) %>%
  summarise(
    RMSE = rmse_vec(satisfaction_score, .pred),
    MAE = mae_vec(satisfaction_score, .pred),
    R2 = rsq_vec(satisfaction_score, .pred),
    .groups = "drop"
  )

satisfaction_metric_plot_data <- satisfaction_model_metrics %>%
  mutate(model = factor(model, levels = c("Decision Tree", "Linear Regression", "Random Forest"))) %>%
  pivot_longer(cols = c(RMSE, MAE, R2), names_to = "Metric", values_to = "Value")

satisfaction_rf_importance <- vi(extract_fit_parsnip(satisfaction_rf_fit)$fit, num_features = 15)

satisfaction_lm_coef <- extract_fit_parsnip(satisfaction_lm_fit)$fit %>%
  broom::tidy() %>%
  filter(term != "(Intercept)", !is.na(estimate)) %>%
  mutate(abs_estimate = abs(estimate)) %>%
  arrange(desc(abs_estimate)) %>%
  slice_head(n = 15)

# ==========================================
# 4. EXPORT ALL PRECOMPUTED FILES
# ==========================================

# Overwrite the old customers file with the newly binned data
saveRDS(customers, file.path(data_root, "customers_shiny.rds"))

# Bundle and save the Transaction Prediction data
predictive_precomputed <- list(
  metrics_plot = predictive_metric_plot_data,
  all_preds = predictive_all_preds,
  rf_vip = predictive_rf_importance,
  lm_coef = predictive_lm_coef,
  metrics_table = predictive_model_metrics
)
saveRDS(predictive_precomputed, file.path(data_root, "predictive_precomputed.rds"))

# Bundle and save the Satisfaction Prediction data
satisfaction_precomputed <- list(
  metrics_plot = satisfaction_metric_plot_data,
  all_preds = satisfaction_all_preds,
  rf_vip = satisfaction_rf_importance,
  lm_coef = satisfaction_lm_coef,
  metrics_table = satisfaction_model_metrics
)
saveRDS(satisfaction_precomputed, file.path(data_root, "satisfaction_precomputed.rds"))

# Save the final tuned tree for plotting in the UI
saveRDS(predictive_tree_final, file.path(data_root, "predictive_tree_final.rds"))

print("All data prepped and saved successfully! You are ready to deploy.")