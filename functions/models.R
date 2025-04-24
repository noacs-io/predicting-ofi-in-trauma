library(tidymodels)
library(tidypredict)
library(catboost) # devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.2.8/catboost-R-Linux-1.2.8.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
library(lightgbm)
library(bonsai) # LightGBM
library(treesnip, include.only = "add_boost_tree_catboost") # catboost remotes::install_github("curso-r/treesnip@catboost")
library(baguette) # DT
library(ranger)

create_recipe <- function(train_data) {
  model_recipe <- recipe(ofi ~ ., data = train_data) %>%
    # step_indicate_na(all_predictors()) %>%
    # step_impute_median(all_numeric_predictors()) %>%
    # step_unknown(all_nominal_predictors()) %>%
    step_impute_knn(all_predictors()) %>%
    step_YeoJohnson(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    # step_nzv(all_predictors()) %>%
    step_adasyn(ofi)
  
  return(model_recipe)
}

model_hyperopt <- function(model, recipe, folds) {
    model_workflow <- workflow() %>%
      add_model(model) %>%
      add_recipe(recipe)
    
    param_set <- extract_parameter_set_dials(model_workflow)
    
    if(model$engine == "lightgbm"){
      param_set <- param_set %>%
        update(trees = trees(range = c(1, 500)))
    } else if (model$engine == "ranger"){
      param_set <- param_set %>%
        update(trees = trees(range = c(1, 500)))
    }
    
    initial <- model_workflow %>%
      tune_grid(
        resamples = folds,
        grid = grid_space_filling(param_set, size = 30),
        metrics = metric_set(roc_auc),
        control = control_grid(verbose = FALSE)
      )
    
    tuner <- model_workflow %>%
      tune_bayes(
        resamples = folds,
        param_info = param_set,
        initial = initial,
        iter = 50,
        metrics = metric_set(roc_auc),
        control = control_bayes(no_improve = 15, verbose = FALSE, time_limit = 60)
      )
    
    model_workflow_tuned <- finalize_workflow(model_workflow, select_best(tuner, metric = "roc_auc"))
    
    return(model_workflow_tuned)
  }

cat_hyperopt <- function(folds, recipe) {
  cat_model <- boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    learn_rate = tune(),
  ) %>%
    set_engine("catboost") %>%
    set_mode("classification")
  
  
  return(model_hyperopt(cat_model, recipe, folds))
}

dt_hyperopt <- function(folds, recipe) {
  dt_model <- bag_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune(),
    class_cost = 0.03
  ) %>%
    set_engine("rpart") %>%
    set_mode("classification")
  
  
  return(model_hyperopt(dt_model, recipe, folds))
}

knn_hyperopt <- function(folds, recipe) {
  knn_model <- nearest_neighbor(
    neighbors = tune(),
    weight_func = tune(),
    dist_power = tune()
  ) %>%
    set_engine("kknn") %>%
    set_mode("classification")
  
  return(model_hyperopt(knn_model, recipe, folds))
}

lgb_hyperopt <- function(folds, recipe) {
  lgb_model <- boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    learn_rate = tune(),
  ) %>%
    set_engine("lightgbm", verbose = -1, nthread = 1) %>% #
    set_mode("classification")
  
  return(model_hyperopt(lgb_model, recipe, folds))
}

lr_hyperopt <- function(folds, recipe) {
  lr_model <- logistic_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet")
  
  return(model_hyperopt(lr_model, recipe, folds))
}

rf_hyperopt <- function(folds, recipe) {
  rf_model <- rand_forest(trees = tune(), min_n = tune()) %>%
    set_mode("classification") %>%
    set_engine("ranger", importance = "impurity")
  
  
  return(model_hyperopt(rf_model, recipe, folds))
}

svm_hyperopt <- function(folds, recipe) {
  svm_model <- svm_poly(cost = tune(), degree = tune(), scale_factor = tune()) %>%
    set_mode("classification")

  return(model_hyperopt(svm_model, recipe, folds))
}

xgb_hyperopt <- function(folds, recipe) {
  xgb_model <- boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    learn_rate = tune(),
  ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")

  return(model_hyperopt(xgb_model, recipe, folds))
}

manual_audit_filters_create <- function(data){
  audit_filters <- data.frame(matrix(nrow = nrow(data), ncol = 0)) 
  
  # VK_hlr_thorak
  audit_filters$hlr_thorak <- ifelse(data$VK_hlr_thorak == "Yes", TRUE, FALSE)
  audit_filters$hlr_thorak[is.na(audit_filters$hlr_thorak)] <- FALSE
  
  # VK_sap_less90
  audit_filters$sap_less90 <- ifelse(data$VK_sap_less90 == "Yes", TRUE, FALSE)
  audit_filters$sap_less90[is.na(audit_filters$sap_less90)] <- FALSE
  
  # VK_leverskada
  # ais codes?
  audit_filters$leverskada <- ifelse(data$VK_leverskada == "Yes", TRUE, FALSE)
  audit_filters$leverskada[is.na(audit_filters$leverskada)] <- FALSE
  
  # VK_gcs_less9_ej_intubTE
  # Include ed_intub_type?
  audit_filters$gcs_less9_ej_intubTE <- ifelse(data$VK_gcs_less9_ej_intubTE == "Yes", TRUE, FALSE)
  audit_filters$gcs_less9_ej_intubTE[is.na(audit_filters$gcs_less9_ej_intubTE)] <- FALSE
  
  # VK_mjaltskada
  # ais codes?
  audit_filters$mjaltskada <- ifelse(data$VK_mjaltskada == "Yes", TRUE, FALSE)
  audit_filters$mjaltskada[is.na(audit_filters$mjaltskada)] <- FALSE
  
  
  # VK_mer_30min_DT
  audit_filters$mer_30min_DT <- ifelse(data$VK_mer_30min_DT == "Yes", TRUE, FALSE)
  audit_filters$mer_30min_DT[is.na(audit_filters$mer_30min_DT)] <- FALSE
  
  
  # VK_mass_transf
  audit_filters$mass_transf <- ifelse(data$VK_mass_transf == "Yes", TRUE, FALSE)
  audit_filters$mass_transf[is.na(audit_filters$mass_transf)] <- FALSE
  
  
  # VK_mer_60_min_interv
  audit_filters$mer_60_min_interv <- ifelse(data$VK_mer_60min_interv == "Yes", TRUE, FALSE)
  audit_filters$mer_60_min_interv[is.na(audit_filters$mer_60_min_interv)] <- FALSE
  
  # VK_iss_15_ej_iva
  # Check if vent days done?
  audit_filters$iss_15_ej_iva <- ifelse(data$VK_iss_15_ej_iva == "Yes", TRUE, FALSE)
  audit_filters$iss_15_ej_iva[is.na(audit_filters$iss_15_ej_iva)] <- FALSE
  
  # VK_ej_trombrof_TBI_72h
  audit_filters$ej_trombrof_TBI_72h <- ifelse(data$VK_ej_trombrof_TBI_72h == "Yes", TRUE, FALSE)
  audit_filters$ej_trombrof_TBI_72h[is.na(audit_filters$ej_trombrof_TBI_72h)] <- FALSE
  
  
  # VK_iss_15_ej_TE
  # Include reprioritised alarms?
  audit_filters$iss_15_ej_TE <- ifelse(data$VK_iss_15_ej_TE == "Yes", TRUE, FALSE)
  audit_filters$iss_15_ej_TE[is.na(audit_filters$iss_15_ej_TE)] <- FALSE
  
  return(audit_filters)
}

audit_filters_create <- function(data){
  audit_filters <- data.frame(matrix(nrow = nrow(data), ncol = 0)) 
  
  # Death within 30-days
  audit_filters$death_30d <- data$res_survival == 1
  audit_filters$death_30d[is.na(audit_filters$death_30d)] <- FALSE
  
  # VK_hlr_thorak
  audit_filters$hlr_thorak <- ifelse(data$VK_hlr_thorak == "Yes", TRUE, FALSE)
  audit_filters$hlr_thorak[is.na(audit_filters$hlr_thorak)] <- FALSE
  
  # VK_sap_less90
  audit_filters$sap_less90 <- data$ed_sbp_value < 90 #| data$ed_sbp_rtscat <= 3
  audit_filters$sap_less90[is.na(audit_filters$sap_less90)] <- FALSE
  
  # VK_leverskada
  # ais codes?
  audit_filters$leverskada <- ifelse(data$VK_leverskada == "Yes", TRUE, FALSE)
  audit_filters$leverskada[is.na(audit_filters$leverskada)] <- FALSE
  
  
  # VK_gcs_less9_ej_intubTE
  # Include ed_intub_type?
  audit_filters$gcs_less9_ej_intubTE <- data$ed_gcs_sum < 9 & data$ed_intubated != 1
  audit_filters$gcs_less9_ej_intubTE[is.na(audit_filters$gcs_less9_ej_intubTE)] <- FALSE
  
  # VK_mjaltskada
  # ais codes?
  audit_filters$mjaltskada <- ifelse(data$VK_mjaltskada == "Yes", TRUE, FALSE)
  audit_filters$mjaltskada[is.na(audit_filters$mjaltskada)] <- FALSE
  
  
  # VK_mer_30min_DT
  audit_filters$mer_30min_DT <- data$dt_ed_first_ct > 30
  audit_filters$mer_30min_DT[is.na(audit_filters$mer_30min_DT)] <- FALSE
  
  
  # VK_mass_transf
  audit_filters$mass_transf <- ifelse(data$VK_mass_transf == "Yes", TRUE, FALSE)
  audit_filters$mass_transf[is.na(audit_filters$mass_transf)] <- FALSE
  
  
  # VK_mer_60_min_interv
  audit_filters$mer_60_min_interv <- data$dt_ed_emerg_proc > 60
  audit_filters$mer_60_min_interv[is.na(audit_filters$mer_60_min_interv)] <- FALSE
  
  # VK_iss_15_ej_iva
  # Check if vent days done?
  audit_filters$iss_15_ej_iva <- data$ISS >= 15 & data$host_care_level != 5
  audit_filters$iss_15_ej_iva[is.na(audit_filters$iss_15_ej_iva)] <- FALSE
  
  # VK_ej_trombrof_TBI_72h
  audit_filters$ej_trombrof_TBI_72h <- ifelse(data$VK_ej_trombrof_TBI_72h == "Yes", TRUE, FALSE)
  audit_filters$ej_trombrof_TBI_72h[is.na(audit_filters$ej_trombrof_TBI_72h)] <- FALSE
  
  
  # VK_iss_15_ej_TE
  # Include reprioritised alarms?
  audit_filters$iss_15_ej_TE <- data$ISS >= 15 & (data$Tr_Nivå == 2 | data$Tr_Nivå == 4)
  audit_filters$iss_15_ej_TE[is.na(audit_filters$iss_15_ej_TE)] <- FALSE
  
  return(audit_filters)
}

manual_audit_filters_predict <- function(data){
  audit_filters <- manual_audit_filters_create(data)
  
  preds <- as.integer(rowSums(audit_filters, na.rm = TRUE) >= 1)
  
  return(preds)
}

audit_filters_predict <- function(data){
  audit_filters <- audit_filters_create(data)
  
  preds <- as.integer(rowSums(audit_filters, na.rm = TRUE) >= 1)
  
  return(preds)
}

