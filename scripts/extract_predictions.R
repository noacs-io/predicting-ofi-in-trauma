#!/usr/bin/env Rscript
# setwd("")
set.seed(2023)

library(dplyr)
library(tidymodels)

source("functions/functions.R")

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
n.folds <- 5
n.repeats <- 1
save.results <- TRUE
data.fraction <- 1 # Used for debugging
hyperopt.n.folds <- 10
exclude.after <- 2023

# Create run out dir
if (save.results) {
  run.time <- format(Sys.time(), "%y-%m-%d-%H-%M")
  run.out.dir <- sprintf("out/%s_extract", run.time)
  results.out.path <-  sprintf("%s/results.rds", run.out.dir)
  dir.create(run.out.dir)
}

dataset <- create.dataset(data.fraction)

dataset <- dataset %>% filter(as.numeric(DateTime_Case_Year) < exclude.after)

dataset <- dataset[!duplicated(dataset$did), ]


results <- c()

for (model.name in names(hyperopt.model.funcs)){
 results[[model.name]] <- data.frame(did = dataset$did)
}

results[["auditfilter"]] <- data.frame(did = dataset$did)


folds <- vfold_cv(dataset, v = n.folds, repeats = n.repeats)

for(split in folds$splits){
  repeat.column.name <- split$id$id
  
  if(startsWith(repeat.column.name, "Fold")) {
    repeat.column.name <- "Repeat1"
  }
  
  train.dataset <- training(split)
  validation.dataset <- testing(split)
  
  res <- pipeline(
    train.dataset,
    validation.dataset,
    hyperopt.model.funcs,
    hyperopt.n.folds,
    n.resamples,
    calibrate = TRUE,
    bootstrap.validation = FALSE,
    cache = TRUE
  )
  
  
  model.names <- names(res$predictions)

  for(model.name in model.names){
    results[[model.name]][which(results[[model.name]]$did %in% validation.dataset$did), repeat.column.name] <- res$predictions[[model.name]]
  }
}

if(save.results){
  saveRDS(results, file = results.out.path)
}
