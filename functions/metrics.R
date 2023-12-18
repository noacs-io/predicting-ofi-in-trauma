library(pROC)
library(ggplot2)
library(scales)
library(Metrics)

plot.calibration.curve <- function(probs, targets) {
  # Load necessary libraries
  library(ggplot2)
  library(scales)
  library(Metrics)
  
  # Create a data frame with probabilities and targets
  validation_data <- data.frame(probabilities = probs, targets = targets)
  
  # Calculate the bins based on probabilities
  validation_data$prob_bins <- cut(validation_data$probabilities, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
  
  # Calculate the mean predicted probability and actual probability per bin
  calibration_data <- aggregate(cbind(mean_predicted = probabilities, mean_actual = targets) ~ prob_bins, data = validation_data, mean)
  
  # Plot the calibration curve
  ggplot(calibration_data, aes(x = mean_predicted, y = mean_actual)) +
    geom_line() +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    scale_x_continuous("Mean Predicted Probability", labels = percent_format()) +
    scale_y_continuous("Mean Actual Probability", labels = percent_format()) +
    ggtitle("Calibration Curve") +
    theme_minimal()
}

calculate.threshold <- function(probabilities, targets, desired.sensitivity) {
  roc.obj <- roc(targets, probabilities, quiet = TRUE)
  
  # Find the threshold
  threshold.index <- which.min(abs(roc.obj$sensitivities - desired.sensitivity))
  optimal.threshold <- roc.obj$thresholds[threshold.index]
  
  return(optimal.threshold)
}

calculate.optimal.threshold <- function(probabilities, targets) {
  roc.obj <- roc(targets, probabilities, quiet = TRUE)
  
  # Calculate the Euclidean distances for each threshold
  distances <- sqrt((1 - roc.obj$sensitivities)^2 + (roc.obj$specificities - 1)^2)
  
  # Get the index of the optimal threshold
  optimal.idx <- which.min(distances)
  
  # Return the optimal threshold
  optimal.threshold <- roc.obj$thresholds[optimal.idx]
  return(optimal.threshold)
}

connected.features <- list(
  "hosp_vent_days" = "host_vent_days_NotDone",
  "ed_inr" = "ed_inr_NotDone",
  "dt_ed_first_ct" = "FirstTraumaDT_NotDone",
  "ed_be_art" = "dt_ed_norm_be"
)

ignored.features <- c("ofi")

for(feature in connected.features){
  ignored.features <- append(ignored.features, feature)
}

permutation.importance <- function(model, data, n.permutations, features = c(
             "ed_emerg_proc_other",
             "AlarmRePrioritised",
             "dt_ed_emerg_proc",
             "hosp_dischg_dest",
             "dt_ed_first_ct",
             "TraumaAlarmAtHospital",
             "pt_Gender",
             "ISS",
             "ed_rr_value",
             "ed_gcs_sum",
             "hosp_los_days",
             "ed_emerg_proc",
             "ed_sbp_value",
             "res_survival",
             "res_gos_dischg",
             "pt_age_yrs",
             "host_care_level"
           )) {
    
  importance.results <- c()
  
  # calculate auc metric
  probs <- predict(model, data, type = "prob")[[2]]
  original.auc <- roc_auc_vec(data$ofi, probs, event_level = "second")
  
  for(feature in features){
    if(feature %in% ignored.features){
      next
    }
    
    # Accumulate to total auc score for each permutation to later calculate average
    total.feature.auc <- 0
    
    for(idx.permutation in 1:n.permutations){
      data.permuted <- data.frame(data) # copy df
      idx.permute.order <- sample(1:nrow(data.permuted)) # shuffle order of columns
      
      # shuffle column
      data.permuted[[feature]] <- data.permuted[[feature]][idx.permute.order] 
      
      # If feature is connected to another shuffle that feature as well
      if(!is.null(connected.features[[feature]])){
        connected.feature <- connected.features[[feature]]
        
        data.permuted[[connected.feature]] <- data.permuted[[connected.feature]][idx.permute.order]
      }
      
      probs <- predict(model, data.permuted, type = "prob")[[2]]
      permuted.auc <- roc_auc_vec(data.permuted$ofi, probs, event_level = "second")
      
      total.feature.auc <- total.feature.auc + permuted.auc
    }
    
    # calculate average auc score across permutations
    average.permutation.auc <- total.feature.auc / n.permutations
    
    # calculate variable importance
    importance.results[[feature]] <- original.auc - average.permutation.auc
  }
  
  return(importance.results)
}
