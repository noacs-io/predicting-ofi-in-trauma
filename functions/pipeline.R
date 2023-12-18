library(tidymodels)
library(caret)

source("functions/data.R")
source("functions/models.R")
source("functions/metrics.R")

pipeline <-
  function(train.dataset,
           validation.dataset,
           hyperopt.model.funcs,
           hyperopt.n.folds,
           n.resamples,
           calibrate = TRUE,
           calculate.variable.importance = FALSE,
           bootstrap.validation = TRUE,
           cache = FALSE
  ) {
  calibration.models <- NULL
    
  if(calibrate){
    calibration.set.predictions <- list()
    calibration.models <- list()
  
    # create calibration set
    split.idx <- floor(0.8 * nrow(train.dataset))
    sample.idx <- sample(seq_len(nrow(train.dataset)), size = split.idx)
    
    calibration.dataset <- train.dataset[-sample.idx, ]
    train.dataset <- train.dataset[sample.idx, ]
    
    calibration.set.predictions[["target"]] <- calibration.dataset$ofi
    calibration.set.predictions[["auditfilter"]] <- audit.filters.predict(calibration.dataset)
  } else {
    # BUG: Used for the fallback code below
    calibration.dataset <- train.dataset
  }
  
  recipe  <- train.dataset %>% preprocess.data()
  
  hyperopt.folds <- vfold_cv(train.dataset, v = hyperopt.n.folds, strata = ofi)
  
  models.fitted <- c()
  for (model.name in names(hyperopt.model.funcs)) {
    print(sprintf("%s | HYPEROPTING AND FITTING %s.", Sys.time(), model.name))
    
    # BUG: SVM fails to fit randomly. Try to fit 32 times. Changing the seed can fix this...
    for (i in 1:32) {
      temp.fitted.model <- hyperopt.model.funcs[model.name][[1]](hyperopt.folds, recipe, cache) %>% fit(data = train.dataset)
      temp.calibration.results <- try(temp.fitted.model %>% predict(calibration.dataset, type = "prob") %>% pull(2))
      
      if (!inherits(temp.calibration.results, "try-error")) {
        models.fitted[[model.name]] <- temp.fitted.model
        
        if(calibrate){
          calibration.set.predictions[[model.name]] <- temp.calibration.results
        }
        break
      }
    }
    
    if(calibrate){
      cal.df <- tibble(probs = calibration.set.predictions[[model.name]], ofi = calibration.dataset$ofi)
      calibration.models[[model.name]] <- glm(ofi ~ probs, cal.df, family = "binomial")

      calibration.set.predictions[[model.name]] <- calibration.models[[model.name]] %>% predict(cal.df, type = "response")
    }
  }
  
  print(sprintf("%s | STARTING RESAMPLE.", Sys.time()))
  
  pred.models <- function(data, models.fitted, calibration.models = NULL) {
      res <- list()
      
      for (model.name in names(models.fitted)) {
        res[[model.name]] <- predict(models.fitted[[model.name]], data, type = "prob")[[2]]
        
        if (!is.null(calibration.models)) {
          cal.df <- tibble(probs = res[[model.name]])
          res[[model.name]] <- predict(calibration.models[[model.name]], cal.df, type = "response")
        }
      }
      
      res[["auditfilter"]] <- audit.filters.predict(data)
      
      return(res)
    }
  
  if(bootstrap.validation){
    resamples <- bootstraps(validation.dataset, times = n.resamples, strata = ofi)
    predictions <- resamples %>%
      mutate(
        pred = map(splits, ~ pred.models(assessment(.x), models.fitted, calibration.models)),
        target = map(splits, ~ assessment(.x)$ofi)
      )
    
    predictions <- predictions %>% select(-splits, -id)
  } else {
    predictions <- list()
    for (model.name in names(models.fitted)) {
      predictions[[model.name]] <- predict(models.fitted[[model.name]], validation.dataset, type = "prob")[[2]]
      
      if(calibrate){
        cal.df <- tibble(probs = predictions[[model.name]])
        predictions[[model.name]] <- predict(calibration.models[[model.name]], cal.df, type = "response")
      }
    }
    
    predictions[["auditfilter"]] <- audit.filters.predict(validation.dataset)
    predictions[["target"]] <- validation.dataset$ofi
  }
  
  res <- list("predictions" = predictions)
  
  if(calibrate){
    res[["calibration_set_predictions"]] <- calibration.set.predictions
  }
  
  if(calculate.variable.importance){
    for (model.name in names(models.fitted)) {
      res[["variable_importance"]][[model.name]] <- permutation.importance(models.fitted[[model.name]], validation.dataset, 5)
    }
  }
  
  return(res)
}