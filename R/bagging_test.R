bagging_test <- function(data = NULL, formula = NULL, p = NULL, nbagg = 50, n_folds = 10, bootstrap_sample = FALSE, weights = NULL) {

  if (is.null(data)) {

    stop("Please provide some data")
  }

  if (is.null(p)) {

    stop("Please provide the parameter p (size of training set)")
  }

  if (!is.null(weights)) {
    # Check length
    if (length(weights) != nrow(data)) {
      stop("Error: 'weights' should have the same number of entries as the rows in 'data'")
    }
  }

  if (!(class(formula) %in% "formula")) {
    formula <- as.formula(formula)
  }

  if (bootstrap_sample) {
    if (!is.null(weights)) {
      index <- sample.int(nrow(data), replace = TRUE, prob = weights)
      resample <- data[index, ]
    } else {
      index <- sample.int(nrow(data), replace = TRUE)
      resample <- data[index, ]
    }
  } else {
    # Do not resample
    resample <- data
  }

  independent <- all.vars(formula)[-1]
  dependent <- update(formula, . ~ .)[[2]]

  # Split the data
  inTraining <- sample(1:nrow(resample), size = floor(p * nrow(resample)))
  training <- resample[inTraining, ]
  testing <- resample[-inTraining, ]

  # Model1
  model1 <- bagging(formula = formula, data = training, coob = TRUE, nbagg = nbagg)

  # Check if outcome is a factor variable (binary variables must be factor var.))
  if (class(data[[dependent]]) %in% "factor") {
    # Predict on test set using model 1
    predictions <- predict(model1, newdata=testing)

    # Confusion Matrix
    conf_matrix <- confusionMatrix(predictions, testing[[dependent]])

    # Extract accuracy
    mod1_metric1 <- conf_matrix$overall[1]

    # Extract Kappa score
    mod1_metric2 <- conf_matrix$overall[2]

  } else {

    # Predict on test set using model 1
    predictions <- predict(model1, newdata=testing)

    # Calculate RMSE
    mod1_metric1 <- rmse(testing[[dependent]], predictions)

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

  model2 <- bagging(
    formula = formula,
    data = training,
    coob = TRUE
  )


  if (class(data[[dependent]]) %in% "factor") {
    # Predict on test set using model 1
    predictions <- predict(model2, newdata=testing)

    # Confusion Matrix
    conf_matrix <- confusionMatrix(predictions, testing[[dependent]])

    # Extract accuracy
    mod2_metric1 <- conf_matrix$overall[1]

    # Extract Kappa score
    mod2_metric2 <- conf_matrix$overall[2]

  } else {

    # Predict on test set using model 2
    predictions <- predict(model2, newdata=testing)

    # Calculate RMSE
    mod2_metric1 <- rmse(testing[[dependent]], predictions)

    # Calculate R2
    if (sd(predictions)==0) {
      mod2_metric2 <- 0
    } else {
      mod2_metric2 <- cor(predictions, testing[[dependent]])^2
    }
  }

  result <- list()
  result$mod1_metric1 <- mod1_metric1
  result$mod1_metric2 <- mod1_metric2
  result$mod2_metric1 <- mod2_metric1
  result$mod2_metric2 <- mod2_metric2
  result$diff_met1 <- mod1_metric1 - mod2_metric1
  result$diff_met2 <-mod1_metric2 - mod2_metric2

  return(result)
}

