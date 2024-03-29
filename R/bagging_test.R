
#' Title
#'
#' @param formula
#' @param data
#' @param indices
#' @param p
#' @param nbagg
#' @param n_folds
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
bagging_test <- function(formula = NULL,
                         data = NULL,
                         indices,
                         p = NULL,
                         nbagg = 50,
                         n_folds = 10,
                         ...) {

  if (is.null(indices)) {
    resample <- data
  } else {
    resample <- data[indices, ]
  }

  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]]
  # Split the data
  inTraining <- sample(1:nrow(resample), size = floor(p * nrow(resample)))
  training <- resample[inTraining, ]
  testing <- resample[-inTraining, ]
  # Model1
  model1 <- ipred::bagging(formula = formula, data = training, coob = TRUE, nbagg = nbagg)
  # Check if outcome is a factor variable (binary variables must be factor var.))
  if (inherits(data[[dependent]], "factor")) { #inherit
    # Predict on test set using model 1
    predictions <- predict(model1, newdata=testing)
    # Confusion Matrix
    conf_matrix <- caret::confusionMatrix(predictions, testing[[dependent]])
    # Extract accuracy
    mod1_metric1 <- conf_matrix$overall[1]
    # Extract Kappa score
    mod1_metric2 <- conf_matrix$overall[2]
  } else {
    # Predict on test set using model 1
    predictions <- predict(model1, newdata=testing)
    # Calculate RMSE
    mod1_metric1 <- Metrics::rmse(testing[[dependent]], predictions)
    # Calculate R2
    if (sd(predictions)==0) {
      mod1_metric2 <- 0
    } else {
      mod1_metric2 <- cor(predictions, testing[[dependent]])^2
    }
  }
  # Replacing the variable with the reshuffled variable
  training[independent[[1]]] <- sample(training[[independent[1]]])
  # model2
  model2 <- ipred::bagging(formula = formula, data = training, coob = TRUE)
  if (inherits(data[[dependent]], "factor")) {
    # Predict on test set using model 1
    predictions <- predict(model2, newdata=testing)
    # Confusion Matrix
    conf_matrix <- caret::confusionMatrix(predictions, testing[[dependent]])
    # Extract accuracy
    mod2_metric1 <- conf_matrix$overall[1]
    # Extract Kappa score
    mod2_metric2 <- conf_matrix$overall[2]
  } else {
    # Predict on test set using model 2
    predictions <- predict(model2, newdata=testing)
    # Calculate RMSE
    mod2_metric1 <- Metrics::rmse(testing[[dependent]], predictions)
    # Calculate R2
    if (sd(predictions)==0) {
      mod2_metric2 <- 0
    } else {
      mod2_metric2 <- cor(predictions, testing[[dependent]])^2
    }
  }

  if (inherits(data[[dependent]], "factor")) {
    result <- c(mod1_metric1 - mod2_metric1, mod1_metric2 - mod2_metric2)
    names(result) <- c("Difference Accuracy", "Difference Kappa score")
  } else {
    result <- c(mod1_metric1 - mod2_metric1, mod1_metric2 - mod2_metric2)
    names(result) <- c("Difference RMSE", "Difference R-squared")
  }
  return(result)
}

