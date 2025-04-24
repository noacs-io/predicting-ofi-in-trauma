set.seed(2022)

library(tidyverse)
library(tidymodels)
library(vip)
library(probably)

source("functions/create_dataset.R")
source("functions/models.R")
source("functions/ici.R")
source("functions/threshold.R")


# Options ---------------------------------------------------------------------------------------------------------

MODELS_TO_RUN <- c(
  "cat" = cat_hyperopt,    # CatBoost model
  "dt" = dt_hyperopt,      # Decision tree
  #"knn" = knn_hyperopt,   # K-nearest neighbors
  "lgb" = lgb_hyperopt,    # LightGBM
  "lr" = lr_hyperopt,      # Logistic regression
  "rf" = rf_hyperopt,      # Random forest
  #"svm" = svm_hyperopt,   # Support vector machine
  "xgb" = xgb_hyperopt     # XGBoost
)

COLUMNS_TO_KEEP = c(
  "pt_age_yrs",
  "pt_Gender",
  
  "ISS",
  "ed_rr_value",
  "ed_gcs_sum",
  "ed_sbp_value",
  
  "ed_emerg_proc",
  "ed_emerg_proc_other",
  "dt_ed_emerg_proc",
  "TraumaAlarmAtHospital",
  "AlarmRePrioritised",
  "FirstTraumaDT_NotDone",
  "dt_ed_first_ct",
  "intub",
  
  "host_care_level",
  "hosp_dischg_dest",
  "hosp_los_days",
  "res_survival",
  "res_gos_dischg",
  
  "ofi"
)

HIGH_SENSITIVITY_THRESHOLD <- 0.95

# Load data -------------------------------------------------------------------------------------------------------

dataset <- create_dataset() %>% 
  filter(year(DateTime_Case) < 2023) %>%
  arrange(DateTime_Case)

# Create time series splits for temporal validation
# Start training on 2013-2016 and validating on 2017, then train on 2013-2017 and validate on 2018, etc.
time_series_splits <- sliding_period(
  dataset,
  DateTime_Case,
  "year",
  lookback = Inf,    # Use all available historical data for training
  assess_stop = 1,   # Use 1 year for validation
  skip = 3           # Start with 4 years of training data (skip first 3 years)
)

# Run analysis ----------------------------------------------------------------------------------------------------

# Run the analysis for each split.
# 1. Use the audit filters to predict on the test data
# 2. For each model:
# - Hyperparameter tune on 5-fold CV on training data
# - Calibrate the ML model using held out validation predictions on 5-fold CV on training data
# - Find the high sensitivity and optimal thresholds for outcome classification based on the training data.
# - Train the model on all training data
# - Predict on validation data for that split (i.e. the year after the training data)
# - Calculate the variable importance based on permutation
# 3. Save all the predictions and variable importance values for each model and year
results <- map(time_series_splits$splits, function(split){
  train_data <- training(split)
  test_data <- testing(split)
  
  year <- test_data$DateTime_Case %>% year() %>% unique()
  print(sprintf("[%s]: Starting year <%s>", strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), year))
  
  # Get audit filter prediction for the year
  af_predictions_test_data <- audit_filters_predict(test_data)
  
  # Save audit filter predictions in a tidymodels friendly format
  af_predictions_test_data <- tibble(
    af.pred_class = if_else(af_predictions_test_data == 1, "Yes", "No") %>% factor(c("No", "Yes")),
    af.pred_Yes = af_predictions_test_data,
    af.pred_No = 1 - af_predictions_test_data
  )
  
  # Create dataframe for storing predictions for each model. Populate it with the audit filter predictions.
  predictions_year <- af_predictions_test_data %>% mutate(tra_id = test_data$tra_id, ofi = test_data$ofi)
  
  # Create dataframe for storing variable importance for each model
  var_imp_year <- tibble()
  
  # Select only the columns needed for modeling
  train_data <- train_data %>% select(all_of(COLUMNS_TO_KEEP))
  test_data <- test_data %>% select(all_of(COLUMNS_TO_KEEP))
  
  # Create cross-validation folds for hyperparameter tuning
  hyperopt_folds <- vfold_cv(train_data, v = 5)
  
  # Create preprocessing recipe
  recipe <- create_recipe(train_data)
  
  # Loop over models to run and save predictions and variable importance
  for (model_name in names(MODELS_TO_RUN)) {
    print(sprintf("[%s]: Starting model <%s> for year <%s>", strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), model_name, year))
    
    # Hyperparameter tuning for current model
    model <- MODELS_TO_RUN[[model_name]](hyperopt_folds, recipe)
    
    # Train the model on 5 folds, collect the predictions and find the highsensitivity and optimal thresholds
    cal_preds <- model %>%
      fit_resamples(resamples = vfold_cv(train_data, v = 5), control = control_resamples(save_pred = TRUE)) %>%
      collect_predictions()
    
    cal_model <- cal_preds %>% cal_estimate_logistic(truth = ofi)
    cal_preds <- cal_preds %>% cal_apply(cal_model)
    
    threshold_high_sensitivity <- calculate_threshold(cal_preds$.pred_Yes, cal_preds$ofi, HIGH_SENSITIVITY_THRESHOLD)
    threshold_optimal <- calculate_optimal_threshold(cal_preds$.pred_Yes, cal_preds$ofi)
    
    # Re-train the model on all the training data
    model <- fit(model, data = train_data)
    
    # Predict on the test data, use the high sensitivity and optimal thresholds for determining outcome
    preds <- augment(model, test_data) %>% 
      cal_apply(cal_model) %>%
      mutate(
        .pred_class_highsensitivity = if_else(.pred_Yes >= threshold_high_sensitivity, "Yes", "No") %>% factor(c("No", "Yes")),
        .pred_class_optimal = if_else(.pred_Yes >= threshold_optimal, "Yes", "No") %>% factor(c("No", "Yes"))
      )
    
    # Add the predictions to the year dataframe
    preds <- preds %>% 
      select(.pred_No, .pred_Yes, .pred_class_highsensitivity, .pred_class_optimal) %>%
      rename_with(~ paste0(model_name, .), everything())
    
    predictions_year <- bind_cols(predictions_year, preds)
    
    
    # This is a wrapper for the vip package so it knows how to predict on data
    pred_wrapper <- function(object, newdata) {
      tmp <- object %>% predict(newdata, type = "prob")
      
      return(tmp$.pred_Yes)
    }
    
    # Calculate variable importance for model and year and add it to the year dataframe
    var_imp_data <- model %>%
      vi(
        method = "permute",
        train = test_data,
        target = "ofi",
        metric = "roc_auc",
        nsim = 10,
        pred_wrapper = pred_wrapper,
        event_level = "second"
      ) %>%
      mutate(model_name = model_name)
    
    var_imp_year <- rbind(var_imp_year, var_imp_data)
  }
  
  return(list(
    var_imp = var_imp_year,
    predictions = predictions_year,
    year = year
  ))
})

saveRDS(results, "results.Rds")

# Bootstrap results and calculate metrics -------------------------------------------------------------------------
print(sprintf("[%s]: Starting to calculate metrics", strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")))

classification_metrics <- yardstick::metric_set(yardstick::roc_auc, yardstick::f_meas, yardstick::sens, yardstick::spec, yardstick::mcc, yardstick::ppv, yardstick::npv)

calculate_metrics <- function(data, model_name, calibration) {
  if(model_name != "af"){
    outcome_column <- sprintf("%s.pred_class_%s", model_name, calibration) %>% as.symbol()
  }
  else {
    outcome_column <- as.symbol("af.pred_class")
  }
  probability_column <- sprintf("%s.pred_Yes", model_name) %>% as.symbol()
  
  model_metrics <- data %>% classification_metrics(truth = ofi, estimate = outcome_column, probability_column, event_level = "second")
  
  # Add number of true positive, false positive, false negative, and true negative via a confusion matrix
  conf_mat <- data %>% conf_mat(truth = ofi, estimate = !!outcome_column)
  conf_metrics <- tibble(
    .metric = c("TP", "FP", "FN", "TN"),
    .estimate = c(conf_mat$table[2,2], conf_mat$table[2,1], conf_mat$table[1,2], conf_mat$table[1,1])
  )
  
  model_metrics <- bind_rows(model_metrics, conf_metrics)
  
  if(model_name != "af") {
    # Add ICI
    ici_value <- ici(data[[probability_column]], as.numeric(data$ofi) - 1)
    model_metrics <- model_metrics %>% bind_rows(tibble(.metric = "ici", .estimate = ici_value))
  }
  
  model_metrics <- model_metrics %>% select(.metric, .estimate)
  
  return(model_metrics)
}

boot_func <- function(split, ...) {
  data <- analysis(split)
  
  model_names <- data %>% colnames() %>% sub("\\..*", "", .) %>% unique() %>% setdiff(c("af", "tra_id", "ofi"))
  
  af_metrics <- data %>% calculate_metrics(data = data, model_name = "af") %>% mutate(model_name = "af", calibration = "")
  
  # Calculate metrics for the high sensitivity calibration
  model_metrics_highsensitivity <- model_names %>% map_dfr(function(model_name){
    # for each model calculate the metrics and add the model name to the metrics
    calculate_metrics(data, model_name, "highsensitivity") %>% mutate(model_name = model_name)
  }) %>% mutate(calibration = "highsensitivity")
  
  # Calculate metrics for the optimal calibration
  model_metrics_optimal <- model_names %>% map_dfr(function(model_name){
    # for each model calculate the metrics and add the model name to the metrics
    calculate_metrics(data, model_name, "optimal") %>% mutate(model_name = model_name)
  }) %>% mutate(calibration = "optimal")
  
  # Calculate differences between all models
  all_metrics <- bind_rows(model_metrics_highsensitivity, model_metrics_optimal, af_metrics)
  
  # For each calibration type, calculate differences between all model pairs (avoiding duplicates)
  diff_metrics <- bind_rows(
    # Differences within high sensitivity models
    cross_join(
      filter(all_metrics, calibration %in% c("highsensitivity", "")),
      filter(all_metrics, calibration %in% c("highsensitivity", ""))
    ) %>%
      # Only keep pairs where model_name.x comes before model_name.y alphabetically
      # So that only model1_vs_model2 and not model2_vs_model1
      filter(model_name.x < model_name.y, .metric.x == .metric.y) %>%
      mutate(
        .estimate = .estimate.x - .estimate.y,
        .metric = paste0(.metric.x, "_diff"),
        model_name = paste0(model_name.x, "_vs_", model_name.y),
        calibration = "highsensitivity"
      ) %>%
      select(model_name, calibration, .metric, .estimate),
    
    # Differences within optimal models
    cross_join(
      filter(all_metrics, calibration %in% c("optimal", "")),
      filter(all_metrics, calibration %in% c("optimal", ""))
    ) %>%
      # Only keep pairs where model_name.x comes before model_name.y alphabetically
      # So that only model1_vs_model2 and not model2_vs_model1
      filter(model_name.x < model_name.y, .metric.x == .metric.y) %>%
      mutate(
        .estimate = .estimate.x - .estimate.y,
        .metric = paste0(.metric.x, "_diff"),
        model_name = paste0(model_name.x, "_vs_", model_name.y),
        calibration = "optimal"
      ) %>%
      select(model_name, calibration, .metric, .estimate)
  )
  
  # Combine original metrics with difference metrics
  metrics <- bind_rows(all_metrics, diff_metrics) %>%
    mutate(term = sprintf("%s.%s.%s", model_name, calibration, .metric), estimate = .estimate) %>%
    select(term, estimate)
  
  return(metrics)
}


metrics_year <- results %>% map_dfr(function(x){
  year <- x$year
  predictions <- x$predictions
  
  print(sprintf("[%s]: Starting to calculate metrics for year <%s>", strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), year))
  
  metrics <- bootstraps(predictions, times = 1000, apparent = TRUE, strata = ofi) %>%
    mutate(results = map(splits, boot_func)) %>%
    int_pctl(results, alpha = 0.05) %>%
    select(-.alpha, -.method) %>%
    separate(term, into = c("model_name", "calibration", "metric"), sep = "\\.") %>%
    rename(lower = .lower, estimate = .estimate, upper = .upper) %>%
    mutate(year = year)
  
  return(metrics)
})

print(sprintf("[%s]: Starting to calculate metrics for all years", strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")))

predictions <- results %>% map_dfr(function(x){
  return(x$predictions)
})

metrics_pooled <- bootstraps(predictions, times = 1000, apparent = TRUE, strata = ofi) %>%
  mutate(results = map(splits, boot_func)) %>%
  int_pctl(results, alpha = 0.05) %>%
  select(-.alpha, -.method) %>%
  separate(term, into = c("model_name", "calibration", "metric"), sep = "\\.") %>%
  rename(lower = .lower, estimate = .estimate, upper = .upper) %>%
  mutate(year = "all")

metrics_year$year <- as.character(metrics_year$year)

metrics <- bind_rows(metrics_year, metrics_pooled)

# Add the FPR metric
metrics <- metrics %>%
  filter(metric == "spec") %>%
  rename(
    lower_old = lower,
    estimate_old = estimate,
    upper_old = upper
  ) %>%
  mutate(
    metric = "fpr",
    lower = 1 - upper_old,
    estimate = 1 - estimate_old,
    upper = 1 - lower_old
  ) %>%
  select(-upper_old, -estimate_old, -lower_old) %>%
  bind_rows(metrics)

# Add the FPR diff metric
metrics <- metrics %>%
  filter(metric == "spec_diff") %>%
  rename(
    lower_old = lower,
    estimate_old = estimate,
    upper_old = upper
  ) %>%
  mutate(
    metric = "fpr_diff",
    lower = -upper_old,
    estimate = -estimate_old,
    upper = -lower_old
  ) %>%
  select(-upper_old, -estimate_old, -lower_old) %>%
  bind_rows(metrics)

saveRDS(metrics, "metrics.Rds")
