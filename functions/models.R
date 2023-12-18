library(tidymodels)
library(tidypredict)
library(treesnip) # LGB, CAT remotes::install_github("curso-r/treesnip@catboost")
library(catboost) # devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.2/catboost-R-Linux-1.2.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
library(baguette) # DT
library(ranger)

model.hyperopt <-
  function(model, recipe, folds, model.name = "", cache = FALSE) {
    
    if (model.name != "" && cache) {
      cache.file <- sprintf("out/%s.rds", model.name)
      if (file.exists(cache.file)) {
        return(readRDS(cache.file))
      }
    }
    
    model.workflow <- workflow() %>%
      add_model(model) %>%
      add_recipe(recipe)
    
    param.set <- extract_parameter_set_dials(model.workflow)
    
    cls.met <- metric_set(roc_auc)
    
    grid <- grid_max_entropy(param.set, size = 30)
    initial <- model.workflow %>%
      tune_grid(
        resamples = folds,
        grid = grid,
        metrics = cls.met,
        control = control_grid(verbose = FALSE)
      )
    
    tuner <- model.workflow %>%
      tune_bayes(
        resamples = folds,
        param_info = param.set,
        initial = initial,
        iter = 50,
        metrics = cls.met,
        control = control_bayes(no_improve = 30, verbose = FALSE, time_limit = 60)
      )
    
    model.workflow.tuned <- finalize_workflow(model.workflow, select_best(tuner, metric = "roc_auc"))
    
    if (model.name != "" && cache) {
      saveRDS(model.workflow.tuned, cache.file)
    }
    
    return(model.workflow.tuned)
  }

cat.hyperopt <- function(folds, recipe, cache) {
  cat.model <-
    boost_tree(
      trees = tune(),
      tree_depth = tune(),
      min_n = tune(),
      learn_rate = tune(),
    ) %>%
    set_engine("catboost") %>%
    set_mode("classification")
  
  
  return(model.hyperopt(cat.model, recipe, folds, model.name = "cat", cache))
}

dt.hyperopt <- function(folds, recipe, cache) {
  dt.model <-
    bag_tree(
      cost_complexity = tune(),
      tree_depth = tune(),
      min_n = tune(),
      class_cost = 0.03
    ) %>%
    set_engine("rpart") %>%
    set_mode("classification")
  
  
  return(model.hyperopt(dt.model, recipe, folds, model.name = "dt", cache))
}

knn.hyperopt <- function(folds, recipe, cache) {
  knn.model <-
    bag_tree(
      cost_complexity = tune(),
      tree_depth = tune(),
      min_n = tune(),
      class_cost = 0.03
    ) %>%
    set_engine("rpart") %>%
    set_mode("classification")
  
  return(model.hyperopt(knn.model, recipe, folds, model.name = "knn", cache))
}

lgb.hyperopt <- function(folds, recipe, cache) {
  lgb.model <-
    boost_tree(
      trees = tune(),
      tree_depth = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      learn_rate = tune(),
    ) %>%
    set_engine("lightgbm", verbose = -1, nthread = 1) %>% #
    set_mode("classification")
  
  return(model.hyperopt(lgb.model, recipe, folds, model.name = "lgb", cache))
}

lr.hyperopt <- function(folds, recipe, cache) {
  lr.model <-
    logistic_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet")
  
  return(model.hyperopt(lr.model, recipe, folds, model.name = "lr", cache))
}

rf.hyperopt <- function(folds, recipe, cache) {
  rf.model <-
    rand_forest(trees = tune(),
                min_n = tune()) %>%
    set_mode("classification") %>%
    set_engine("ranger", importance = "impurity")
  
  
  return(model.hyperopt(rf.model, recipe, folds, model.name = "rf", cache))
}

svm.hyperopt <- function(folds, recipe, cache) {
  svm.model <-
    svm_poly(cost = tune(),
             degree = tune(),
             scale_factor = tune()) %>%
    set_mode("classification")

  return(model.hyperopt(svm.model, recipe, folds, model.name = "svm", cache))
}

xgb.hyperopt <- function(folds, recipe, cache) {
  xgb.model <-
    boost_tree(
      trees = tune(),
      tree_depth = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      learn_rate = tune(),
    ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")

  return(model.hyperopt(xgb.model, recipe, folds, model.name = "xgboost", cache))
}

manual.audit.filters.create <- function(data){
  audit.filters <- data.frame(matrix(nrow = nrow(data), ncol = 0)) 
  
  # VK_hlr_thorak
  audit.filters$hlr_thorak <- ifelse(data$VK_hlr_thorak == "Yes", TRUE, FALSE)
  audit.filters$hlr_thorak[is.na(audit.filters$hlr_thorak)] <- FALSE
  
  # VK_sap_less90
  audit.filters$sap_less90 <- ifelse(data$VK_sap_less90 == "Yes", TRUE, FALSE)
  audit.filters$sap_less90[is.na(audit.filters$sap_less90)] <- FALSE
  
  # VK_leverskada
  # ais codes?
  audit.filters$leverskada <- ifelse(data$VK_leverskada == "Yes", TRUE, FALSE)
  audit.filters$leverskada[is.na(audit.filters$leverskada)] <- FALSE
  
  # VK_gcs_less9_ej_intubTE
  # Include ed_intub_type?
  audit.filters$gcs_less9_ej_intubTE <- ifelse(data$VK_gcs_less9_ej_intubTE == "Yes", TRUE, FALSE)
  audit.filters$gcs_less9_ej_intubTE[is.na(audit.filters$gcs_less9_ej_intubTE)] <- FALSE
  
  # VK_mjaltskada
  # ais codes?
  audit.filters$mjaltskada <- ifelse(data$VK_mjaltskada == "Yes", TRUE, FALSE)
  audit.filters$mjaltskada[is.na(audit.filters$mjaltskada)] <- FALSE
  
  
  # VK_mer_30min_DT
  audit.filters$mer_30min_DT <- ifelse(data$VK_mer_30min_DT == "Yes", TRUE, FALSE)
  audit.filters$mer_30min_DT[is.na(audit.filters$mer_30min_DT)] <- FALSE
  
  
  # VK_mass_transf
  audit.filters$mass_transf <- ifelse(data$VK_mass_transf == "Yes", TRUE, FALSE)
  audit.filters$mass_transf[is.na(audit.filters$mass_transf)] <- FALSE
  
  
  # VK_mer_60_min_interv
  audit.filters$mer_60_min_interv <- ifelse(data$VK_mer_60min_interv == "Yes", TRUE, FALSE)
  audit.filters$mer_60_min_interv[is.na(audit.filters$mer_60_min_interv)] <- FALSE
  
  # VK_iss_15_ej_iva
  # Check if vent days done?
  audit.filters$iss_15_ej_iva <- ifelse(data$VK_iss_15_ej_iva == "Yes", TRUE, FALSE)
  audit.filters$iss_15_ej_iva[is.na(audit.filters$iss_15_ej_iva)] <- FALSE
  
  # VK_ej_trombrof_TBI_72h
  audit.filters$ej_trombrof_TBI_72h <- ifelse(data$VK_ej_trombrof_TBI_72h == "Yes", TRUE, FALSE)
  audit.filters$ej_trombrof_TBI_72h[is.na(audit.filters$ej_trombrof_TBI_72h)] <- FALSE
  
  
  # VK_iss_15_ej_TE
  # Include reprioritised alarms?
  audit.filters$iss_15_ej_TE <- ifelse(data$VK_iss_15_ej_TE == "Yes", TRUE, FALSE)
  audit.filters$iss_15_ej_TE[is.na(audit.filters$iss_15_ej_TE)] <- FALSE
  
  return(audit.filters)
}

audit.filters.create <- function(data){
  audit.filters <- data.frame(matrix(nrow = nrow(data), ncol = 0)) 
  
  # Death within 30-days
  audit.filters$death_30d <- data$res_survival == 1
  audit.filters$death_30d[is.na(audit.filters$death_30d)] <- FALSE
  
  # VK_hlr_thorak
  audit.filters$hlr_thorak <- ifelse(data$VK_hlr_thorak == "Yes", TRUE, FALSE)
  audit.filters$hlr_thorak[is.na(audit.filters$hlr_thorak)] <- FALSE
  
  # VK_sap_less90
  audit.filters$sap_less90 <- data$ed_sbp_value < 90 #| data$ed_sbp_rtscat <= 3
  audit.filters$sap_less90[is.na(audit.filters$sap_less90)] <- FALSE
  
  # VK_leverskada
  # ais codes?
  audit.filters$leverskada <- ifelse(data$VK_leverskada == "Yes", TRUE, FALSE)
  audit.filters$leverskada[is.na(audit.filters$leverskada)] <- FALSE
  
  
  # VK_gcs_less9_ej_intubTE
  # Include ed_intub_type?
  audit.filters$gcs_less9_ej_intubTE <- data$ed_gcs_sum < 9 & data$ed_intubated != 1
  audit.filters$gcs_less9_ej_intubTE[is.na(audit.filters$gcs_less9_ej_intubTE)] <- FALSE
  
  # VK_mjaltskada
  # ais codes?
  audit.filters$mjaltskada <- ifelse(data$VK_mjaltskada == "Yes", TRUE, FALSE)
  audit.filters$mjaltskada[is.na(audit.filters$mjaltskada)] <- FALSE
  
  
  # VK_mer_30min_DT
  audit.filters$mer_30min_DT <- data$dt_ed_first_ct > 30
  audit.filters$mer_30min_DT[is.na(audit.filters$mer_30min_DT)] <- FALSE
  
  
  # VK_mass_transf
  audit.filters$mass_transf <- ifelse(data$VK_mass_transf == "Yes", TRUE, FALSE)
  audit.filters$mass_transf[is.na(audit.filters$mass_transf)] <- FALSE
  
  
  # VK_mer_60_min_interv
  audit.filters$mer_60_min_interv <- data$dt_ed_emerg_proc > 60
  audit.filters$mer_60_min_interv[is.na(audit.filters$mer_60_min_interv)] <- FALSE
  
  # VK_iss_15_ej_iva
  # Check if vent days done?
  audit.filters$iss_15_ej_iva <- data$ISS >= 15 & data$host_care_level != 5
  audit.filters$iss_15_ej_iva[is.na(audit.filters$iss_15_ej_iva)] <- FALSE
  
  # VK_ej_trombrof_TBI_72h
  audit.filters$ej_trombrof_TBI_72h <- ifelse(data$VK_ej_trombrof_TBI_72h == "Yes", TRUE, FALSE)
  audit.filters$ej_trombrof_TBI_72h[is.na(audit.filters$ej_trombrof_TBI_72h)] <- FALSE
  
  
  # VK_iss_15_ej_TE
  # Include reprioritised alarms?
  audit.filters$iss_15_ej_TE <- data$ISS >= 15 & (data$Tr_Nivå == 2 | data$Tr_Nivå == 4)
  audit.filters$iss_15_ej_TE[is.na(audit.filters$iss_15_ej_TE)] <- FALSE
  
  return(audit.filters)
}

manual.audit.filters.predict <- function(data){
  audit.filters <- manual.audit.filters.create(data)
  
  preds <- as.integer(rowSums(audit.filters, na.rm = TRUE) >= 1)
  
  return(preds)
}

audit.filters.predict <- function(data){
  audit.filters <- audit.filters.create(data)
  
  preds <- as.integer(rowSums(audit.filters, na.rm = TRUE) >= 1)
  
  return(preds)
}

