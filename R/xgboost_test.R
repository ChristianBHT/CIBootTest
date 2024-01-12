#' Title
#'
#' @param formula
#' @param data
#' @param indices
#' @param p
#' @param objective
#' @param early_stopping
#' @param nrounds
#' @param eta
#' @param max_depth
#' @param n_folds
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
xgboost_test <- function(formula = NULL,
                         data = NULL,
                         indices,
                         p = NULL,
                         objective = "reg:squarederror",
                         early_stopping = 10,
                         nrounds = 60,
                         eta = 0.1,
                         max_depth = c(3,4,5),
                         n_folds = 5,
                         ...) {
  if (is.null(indices)) {
    resample <- data
  } else {
    resample <- data[indices, ]
  }

  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]]
  # Check if there are factor variables in the feature set
  if (any(sapply(resample, is.factor))) {
    features <- resample[independent]
    label <- resample[[dependent]]
    # create data matrix
    features <- model.matrix(~ . - 1, data = features)
    # Create the data matrix for the XGBoost function
    data_matrix <- xgboost::xgb.DMatrix(data = as.matrix(features), label = as.matrix(label))
  } else {
    # Create the data matrix for the XGBoost function without factor variables in the feature set
    features <- resample[independent]
    label <- resample[[dependent]]
    data_matrix <- xgboost::xgb.DMatrix(data = as.matrix(features), label = as.matrix(label))
  }
  # Hyperparameters search
  nrounds_values <- nrounds
  best_max_depth <- NULL
  best_nrounds <- NULL
  best_test <- Inf
  best_iteration <- NULL
  # Perform grid search over hyperparameters
  for (depth in max_depth) {
    params <- list(
      eta = eta,
      max_depth = depth
    )
    for (fold in 1:n_folds) {
      fold_size <- floor(nrow(data_matrix) / n_folds)
      test_indices <- ((fold - 1) * fold_size + 1):(fold * fold_size)
      train_indices <- setdiff(1:nrow(data_matrix), test_indices)
      train_data <- data_matrix[train_indices, ]
      test_data <- data_matrix[test_indices, ]
      watchlist <- list(train = train_data, test = test_data)
      model <- xgboost::xgb.train(
        data = watchlist$train,
        objective = objective,
        max_depth = depth,
        eta =eta,
        nrounds = nrounds,
        early_stopping_rounds = early_stopping,
        nthread = 1,
        watchlist = watchlist,
        verbose = FALSE
      )
      if (inherits(objective, c("binary:logistic", "multi:softmax"))) {
        best_iteration <-  which.min(model$evaluation_log$test_logloss)
        min_cv <- model$evaluation_log$test_logloss[best_iteration]
      } else {
        best_iteration <-  which.min(model$evaluation_log$test_rmse)
        min_cv <- model$evaluation_log$test_rmse[best_iteration]
      }
      # Check if this combination has a lower RMSE than the previous best
      if (min_cv < best_test) {
        best_test <- min_cv
        best_max_depth <- depth
        best_nrounds <- best_iteration
      }
    }
  }

  # Split the data
  inTraining <- sample(1:nrow(resample), size = floor(p * nrow(resample)))
  training <- resample[inTraining, ]
  testing <- resample[-inTraining, ]

  # Check if there are factor variables in the feature set
  if (any(sapply(training, is.factor))) {
    train_features <- training[independent]
    train_label <- training[[dependent]]
    test_features <- testing[independent]
    test_label <- testing[[dependent]]
    # create data matrix
    train_features <- model.matrix(~ . - 1, data = train_features)
    test_features <- model.matrix(~ . - 1, data = test_features)
    # Create the data matrix for the XGBoost function
    train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgboost::xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
  } else {
    # Create the data matrix for the XGBoost function without factor variables in the feature set
    train_features <- training[independent]
    train_label <- training[[dependent]]
    test_features <- testing[independent]
    test_label <- testing[[dependent]]
    train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(train_features), label = as.matrix(train_label))
    test_matrix <- xgboost::xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_label))
  }

  # Model1
  # Set best max depth
  if (inherits(objective, "multi:softmax")) {
    params <- list(
      eta = eta,
      max_depth = best_max_depth,
      num_class = num_class
    )
  } else {
    params <- list(
      eta = eta,
      max_depth = best_max_depth
    )
  }

  # Train the XGBoost models
  model1 <- xgboost::xgboost(data = train_matrix,
                    objective = objective,
                    params = params,
                    nrounds = best_nrounds,
                    verbose=0,
                    nthread = 1)
  # Get performance score model 1 if statement for separating classification and regression
  if (inherits(objective, c("binary:logistic", "multi:softmax"))) {
    # Predict on test set using model 1
    predictions <- predict(model1, test_matrix)
    # Predictions of binary outcome are probabilities
    if (inherits(objective, "binary:logistic")) {
      predictions <- ifelse(predictions > 0.5, 1, 0)
    }
    # Confusion Matrix
    conf_matrix <- caret::confusionMatrix(as.factor(predictions), as.factor(test_label))
    # Extract accuracy
    mod1_metric1 <- conf_matrix$overall[1]
    # Extract Kappa score
    mod1_metric2 <- conf_matrix$overall[2]
  } else {
    # Predict on test set using model 1
    predictions <- predict(model1, newdata = test_matrix)
    # Calculate RMSE
    mod1_metric1 <- Metrics::rmse(test_label, predictions)
    # Calculate R2
    mod1_metric2 <- cor(predictions, test_label)^2
  }
  # Replacing the variable with the reshuffled variable
  training[independent[[1]]] <- sample(training[[independent[1]]]) # Se på R()
  # Creating new feature set, same steps as above
  if (any(sapply(training, is.factor))) {
    model2_train_features <- training[independent]
    model2_train_features <- model.matrix(~ . - 1, data = model2_train_features)
    model2_train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(model2_train_features), label = as.matrix(train_label))
  } else {
    # Create the data matrix for the XGBoost function without factor variables in the feature set
    model2_train_features <- training[independent]
    model2_train_matrix <- xgboost::xgb.DMatrix(data = as.matrix(model2_train_features), label = as.matrix(train_label))
  }

  model2 <- xgboost::xgboost(data = model2_train_matrix,
                    objective = objective,
                    params = params,
                    nrounds = best_nrounds,
                    verbose=0,
                    nthread = 1)

  if (inherits(objective, c("binary:logistic", "multi:softmax"))) {
    predictions <- predict(model2, test_matrix)
    if (inherits(objective, "binary:logistic")) {
      predictions <- ifelse(predictions > 0.5, 1, 0)
    }
    conf_matrix <- caret::confusionMatrix(as.factor(predictions), as.factor(test_label))
    mod2_metric1 <- conf_matrix$overall[1]
    mod2_metric2 <- conf_matrix$overall[2]
  } else {
    predictions <- predict(model2, newdata = test_matrix)
    mod2_metric1 <- Metrics::rmse(test_label, predictions)
    mod2_metric2 <- cor(predictions, test_label)^2
  }

  if (inherits(objective, c("binary:logistic", "multi:softmax"))) {
    result <- c(mod1_metric1 - mod2_metric1, mod1_metric2 - mod2_metric2)
    names(result) <- c("Difference Accuracy", "Difference Kappa score")
  } else {
    result <- c(mod1_metric1 - mod2_metric1, mod1_metric2 - mod2_metric2)
    names(result) <- c("Difference RMSE", "Difference R-squared")
  }
  return(result)
}















