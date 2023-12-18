#' Model Lookup
#'
#' This function takes a model abbreviation and returns the corresponding full model name.
#'
#' @param model.abbrev A character string representing the model abbreviation.
#' @return The full model name corresponding to the given abbreviation.
#' @examples
#' model_lookup("lr")
#' # Output: "Logistic Regression"
#' @export
model_lookup <- function(model.abbrev) {
    ## Check arguments
    assertthat::assert_that(is.character(model.abbrev) & length(model.abbrev) == 1)

    ## Lookup table
    lookup.table <- list(
        lr = "Logistic Regression",
        rf = "Random Forest",
        svm = "Support Vector Machine",
        xgb = "XGBoost",
        nn = "Neural Network",
        knn = "K-Nearest Neighbors",
        nb = "Naive Bayes",
        lda = "Linear Discriminant Analysis",
        qda = "Quadratic Discriminant Analysis",
        gbm = "Gradient Boosting Machine",
        glm = "Generalized Linear Model",
        glmnet = "Elastic Net",
        gam = "Generalized Additive Model",
        auditfilter = "audit filters",
        cat = "CatBoost"
    )

    ## Lookup
    model.name <- lookup.table[[model.abbrev]]

    ## Return
    return(model.name)
}

model_lookup_vector <- function(model.abbreviations) {
  ## Check arguments
  assertthat::assert_that(is.character(model.abbreviations))
  
  ## Lookup table
  lookup.table <- list(
    lr = "Logistic Regression",
    rf = "Random Forest",
    svm = "Support Vector Machine",
    xgb = "XGBoost",
    nn = "Neural Network",
    knn = "K-Nearest Neighbors",
    nb = "Naive Bayes",
    lda = "Linear Discriminant Analysis",
    qda = "Quadratic Discriminant Analysis",
    gbm = "Gradient Boosting Machine",
    glm = "Generalized Linear Model",
    glmnet = "Elastic Net",
    gam = "Generalized Additive Model",
    auditfilter = "audit filters",
    cat = "CatBoost"
    
  )
  
  ## Lookup using sapply and preserve order
  corrected_names <- sapply(model.abbreviations, function(abbrev) {
    if (abbrev %in% names(lookup.table)) {
      return(lookup.table[[abbrev]])
    } else {
      return(NA)  # Return NA for unmatched abbreviations
    }
  })
  
  ## Return corrected names with preserved order
  return(corrected_names)
}
