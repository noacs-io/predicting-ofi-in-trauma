#!/usr/bin/env Rscript

## Lock seed
set.seed(2022)
# setwd("")

## Activate multithreading (do not use when hyperopting lgb due to memory usage. May need to use nthread = 1 in lgb or lower trees)
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

library(progress)
library(dplyr)
library(tidymodels)
library(caret)

source("functions/functions.R")

tidymodels_prefer()

# Make sure out dir exists
dir.create("out", showWarnings = FALSE)

# Select which models to run
hyperopt.model.funcs <- c(
  "cat" = cat.hyperopt,
  "dt" = dt.hyperopt,
  "knn" = knn.hyperopt,
  "lgb" = lgb.hyperopt,
  "lr" = lr.hyperopt,
  "rf" = rf.hyperopt,
  "svm" = svm.hyperopt,
  "xgb" = xgb.hyperopt
)

# Settings
data.fraction <- 1 # Used for debugging
hyperopt.n.folds <- 5
n.resamples <- 1000 # per year
save.results <- TRUE
exclude.after <- 2023

# Create run out dir
if (save.results) {
  run.time <- format(Sys.time(), "%y-%m-%d-%H-%M")
  run.out.dir <- sprintf("out/%s_loyo_20_cal", run.time)
  results.out.path <-  sprintf("%s/results.rds", run.out.dir)
  dir.create(run.out.dir)
}

dataset <- create.dataset(data.fraction)

dataset <- dataset %>% filter(as.numeric(DateTime_Case_Year) < exclude.after)

unique.years <- sort(unique(dataset$DateTime_Case_Year))

pb <-
  progress::progress_bar$new(
    format = "RUNNING :spin [:bar] :current/:total (:tick_rate/s) | :elapsedfull (:eta)",
    total = length(unique.years),
    show_after = 0
  )
pb$tick(0)

for (year in unique.years) {
  print(sprintf("%s | STARTING YEAR %s.", Sys.time(), year))
  
  validation.dataset <- dataset[dataset$DateTime_Case_Year == year, ]
  train.dataset <- dataset[dataset$DateTime_Case_Year != year, ]
  
  res <- pipeline(
    train.dataset,
    validation.dataset,
    hyperopt.model.funcs,
    hyperopt.n.folds,
    n.resamples,
    calibrate = TRUE,
    calculate.variable.importance = TRUE,
    bootstrap.validation = TRUE,
    cache = FALSE
  )
  
  if (save.results) {
    if(!file.exists(results.out.path)) {
      results <- list("predictions" = list(), "calibration_set_predictions" = list())
    } else {
      results <- readRDS(results.out.path)
    }
    
    results[["predictions"]][[as.character(year)]] <- res[["predictions"]]
    results[["calibration_set_predictions"]][[as.character(year)]] <- res[["calibration_set_predictions"]]
    results[["variable_importance"]][[as.character(year)]] <- res[["variable_importance"]]
    
    saveRDS(results, results.out.path)
    
    rm(results)
  }
  
  rm(res)
  gc()
  
  print(sprintf("%s | ENDING YEAR %s.", Sys.time(), year))
  
  pb$tick()
}
