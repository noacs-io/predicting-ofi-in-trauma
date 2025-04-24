library(pROC, include.only = "roc")

calculate_threshold <- function(probabilities, targets, desired.sensitivity) {
  roc.obj <- roc(targets, probabilities, quiet = TRUE)
  
  # Find the threshold
  threshold.index <- which.min(abs(roc.obj$sensitivities - desired.sensitivity))
  optimal.threshold <- roc.obj$thresholds[threshold.index]
  
  if(threshold.index == 1 || threshold.index == 2){
    threshold.index <- 3
    optimal.threshold <- roc.obj$thresholds[threshold.index]
  } else if(threshold.index == length(roc.obj$thresholds) || threshold.index == (length(roc.obj$thresholds) - 1)){
    threshold.index <- length(roc.obj$thresholds) - 2
    
    optimal.threshold <- roc.obj$thresholds[threshold.index]
  }
  
  return(optimal.threshold)
}

calculate_optimal_threshold <- function(probabilities, targets) {
  roc.obj <- roc(targets, probabilities, quiet = TRUE)
  
  # Calculate the Euclidean distances for each threshold
  distances <- sqrt((1 - roc.obj$sensitivities)^2 + (roc.obj$specificities - 1)^2)
  
  # Get the index of the optimal threshold
  optimal.idx <- which.min(distances)
  
  # Return the optimal threshold
  optimal.threshold <- roc.obj$thresholds[optimal.idx]
  return(optimal.threshold)
}
