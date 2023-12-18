#!/usr/bin/env Rscript
# setwd("")
set.seed(2022)

library(gmish)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gmodels)
library(knitr)
library(kableExtra)
library(tinytex)
library(tidymodels)
library(probably)

source("functions/functions.R")

dir <- "out/23-11-30-18-24_aoyi"
save.metrics <- TRUE

results <- readRDS(sprintf("%s/results.rds", dir))
calibration.set.predictions <- results[["calibration_set_predictions"]]
predictions <- results[["predictions"]]

model.names <- names(predictions[[1]]$pred[[1]])

n.years <- length(predictions)
n.resamples <- length(predictions[[1]]$pred)
n.models <- length(model.names)

pb <-
  progress::progress_bar$new(
    format = "CALCULATING METRICS :spin [:bar] :current/:total (:tick_rate/s) | :elapsedfull (:eta)",
    total = n.years * n.resamples * n.models,
    show_after = 0
  )
pb$tick(0)

metrics <- list()

for (year in names(predictions)) {
  year <- as.character(year)
  
  metrics[[year]] <- list()
  
  for (resample.idx in 1:n.resamples) {
    resample <- predictions[[year]]$pred[[resample.idx]]
    target <- predictions[[year]]$target[[resample.idx]]

    if(length(unique(target)) == 1){
      print(sprintf("skipping resample %s year %s", resample.idx, year))
      next
    }
    
    for(model.name in model.names){
      probs <- resample[[model.name]]
      
      if(model.name != "auditfilter"){
        calibration.probs <- calibration.set.predictions[[year]][[model.name]]
        calibration.target <- as.numeric(calibration.set.predictions[[year]][["target"]]) - 1
        
        # cutoff <- calculate.optimal.threshold(calibration.probs, calibration.target)
        cutoff <- calculate.threshold(calibration.probs, calibration.target, 0.95)

        pred.classes <- probs >= cutoff
      } else {
        pred.classes <- probs
        
      }
      
      pred.classes <- factor(ifelse(pred.classes, "Yes", "No"), levels = c("No", "Yes"))
      
      auc <- roc_auc_vec(target, probs, event_level = "second")
      sens <- sens_vec(target, pred.classes, event_level = "second")
      spec <- spec_vec(target, pred.classes, event_level = "second")
      
      metrics[[year]][[model.name]][["auc"]] <- metrics[[year]][[model.name]][["auc"]] %>%
        append(auc)
      
      metrics[[year]][[model.name]][["spec"]] <- metrics[[year]][[model.name]][["spec"]] %>%
        append(spec)
      
      metrics[[year]][[model.name]][["sens"]] <-
        metrics[[year]][[model.name]][["sens"]] %>%
        append(sens)
      
      if(model.name != "auditfilter"){
        ici <- suppressMessages(gmish::ici(probs, as.numeric(target) - 1))

        metrics[[year]][[model.name]][["ici"]] <- metrics[[year]][[model.name]][["ici"]] %>%
          append(ici)
      }

      pb$tick()
    }
  }
}

if (save.metrics) {
  saveRDS(metrics, file = sprintf("%s/metrics.rds", dir))
}

table <- ""

for (year in names(metrics)) {
  year <- as.character(year)

  table <- paste(table, year, "\n")

  for (model.name in names(metrics[[year]])) {
    table <- paste(table, model.name, "\n")
    metrics.for.model <- metrics[[year]][[model.name]]

    for (metric.name in names(metrics.for.model)) {
      metric.results <- metrics.for.model[[metric.name]]
      if (any(is.na(metric.results))) {
        next
      }

      ci <- round(gmodels::ci(metric.results, ci = 0.95), digits = 3)

      table <-
        paste(table,
              sprintf("%s: %f (%f, %f)\n", metric.name, ci[["Estimate"]], ci[["CI lower"]], ci[["CI upper"]]))
    }

    table <- paste(table, "\n")
  }
}

cat(table)
