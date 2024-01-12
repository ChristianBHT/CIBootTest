
#' Boocoin Test
#' @description
#' Assessing the implied conditional independence assumptions in causal directed acyclic graphs, by creating a posterior distribution
#' of the difference in predictive performance between two models.
#'
#' @param data
#' @param formula
#' @param statistic
#' @param nboot
#' @param bootstrap_sample
#' @param bayes
#' @param p
#' @param alpha
#' @param dag
#' @param dag_cond
#' @param parallel
#' @param ncores
#' @param metric
#' @param ...
#'
#' @importFrom caret bagging confusionMatrix
#' @return
#' @export
#'
#' @examples
#' set.seed(1984)
#' # Generate data, Z is standard normal and causes both Y and X, plus stochastic noise
#' N <- 1000
#' Z <- rnorm(N)
#' Y <- Z + rnorm(N,0,0.5)
#' X <- Z + rnorm(N,0,0.5)
#' data <- data.frame(Z,X,Y)
#' # The checking the condition Y_||_ X | Z which correspond to the formula = Y~X + Z, with 250 bootstrap samples
#' Test <- BootyTest(formula = Y~X + Z, data = data, nboot = 250)
#' # Summarise the results
#'
#' # Plot the posterior distribution
#' plot(Test)
#' # The function can also be used with a Dagitty object, the name of the variables in the dag must exactly match the names in the 'data'
#' library(dagitty)
#' dag <- dagitty('')
#' @details
#' The test estimates a posterior distribution for the difference in predictive performance
#' between two prediction models, f()
#'
#'
BootyTest <- function(formula = NULL, data = data, statistic, nboot = 25,
                       bootstrap_sample = TRUE, bayes = TRUE, p = NULL,
                       alpha = c(rep(1,nrow(data))), dag = NULL, dag_cond = NULL,
                       parallel = NULL, metric  = c('RMSE', 'R2', 'Accuracy', 'Kappa score'), ...){
  if (is.null(data)) {
    stop("No data, no testing")
  }
  if (is.null(p)) {
    stop("Please provide the parameter p (size of training set)")
  }
  if (p <= 0 || p >= 1) {
    stop("You silly you, p must be between 0 and 1")
  }
  if (is.null(formula) & is.null(dag)) {
    stop("Please provide a R-formula for the condition to be tested, i.e. Y _||_ X | Z -> Y ~ X + Z.
         Or provide a dagitty DAG with the option: dag = 'your DAG' and dag_cond = '# of the condition in need of testing'")
  } else if (inherits(!class(formula), "formula") & !is.null(formula)) {
    formula <- as.formula(formula)
  } else if (is.null(formula) & !is.null(dag)) {
    # Use provided DAG and extract the testable condition
    if (inherits(!class(dag), 'dagitty')) {
      stop('The provided DAG needs to be a dagitty object')
    }
    characters <- unlist(dagitty::impliedConditionalIndependencies(dag)[dag_cond])
    formula_str <- paste(characters[2:length(characters)], collapse = " + ")
    formula_dep <- characters[1]
    formula <- as.formula(paste(formula_dep ,"~", formula_str))
  }

  if (is.null(parallel)){
    if (bootstrap_sample) {
      if (bayes) {
        if (!is.null(alpha)) {
          # Check length of alpha
          if (length(alpha) != nrow(data)) {
            stop("Error: the prior alpha should have the same number of entries as the rows in data")
          }
        }
        weights <- DirichletReg::rdirichlet(nboot, alpha)
        output <- boot::boot(data = data,
                             statistic = function(data, indices, formula,...) {statistic(formula, data, indices, p, ...)},
                             R = rep(1,nboot),
                             formula = formula,
                             weights = weights)
      } else {
        output <- boot::boot(data = data,
                             statistic = function(data, indices, formula, ...) statistic(formula, data, indices, p, ...),
                             R = nboot,
                             formula = formula)
      }
    } else {
      if (identical(statistic, bagging_test)){
        output <- bagging_test(formula, data, indices = NULL, p, nboot)
      } else if (identical(statistic, xgboost_test)){
        output <- xgboost_test(formula, data, indices = NULL,  p)
      }
    }
  } else if (!is.null(parallel)){
    if (bootstrap_sample) {
      if (bayes) {
        if (!is.null(alpha)) {
          # Check length of alpha
          if (length(alpha) != nrow(data)) {
            stop("Error: the prior alpha should have the same number of entries as the rows in data")
          }
        }
        weights <- DirichletReg::rdirichlet(nboot, alpha)
        # Applying the parallel = "snow" option, which is only valid for windows computers
        output <- boot::boot(data = data,
                             statistic = function(data, indices, formula, ...) {statistic(formula, data, indices, p, ...)},
                             R = rep(1,nboot),
                             weights = weights,
                             formula = formula,
                             parallel = 'snow')
      } else {
        output <- boot::boot(data = data, statistic = function(data, indices, formula, ...) statistic(formula, data, indices, p, ...),
                             R = nboot,
                             formula = formula,
                             parallel = 'snow')
      }
    } else { # Case with no bootstrapping
      if (identical(statistic, bagging_test)){
        output <- bagging_test(data, indices = NULL, formula, p, nboot,...)
      } else if (identical(statistic, xgboost_test)){
        output <- xgboost_test(data, indices = NULL, formula, p,...)
      }
    }
  }

  return(output)
}
set.seed(9)
data <- data_gen(2000)
test <- BootyTest(data = data, formula = NULL, dag = DAG, dag_cond = 2, statistic = bagging_test, p = 0.9, nboot = 100)
baggingtest <- BootyTest(formula = X3 ~ X4 + X2 + X1, data = data, statistic = bagging_test, p = 0.9, nboot = 100)
# Regular bootstrap
NoBayes_test <- BootyTest(formula = X3 ~ X4 + X2 + X1, data = data, statistic = bagging_test, p = 0.9, nboot = 100, bayes = FALSE)
hist(NoBayes_test$t[,1])
# With parallel computing
parallel_test <- BootyTest(formula = X3 ~ X4 + X2 + X1, data = data, statistic = bagging_test, p = 0.9, nboot = 200, parallel = T)
hist(parallel_test$t[,1])
parallel_xgboosttest <- BootyTest(formula = X3 ~ X4 + X2 + X1, data = data, statistic = xgboost_test, p = 0.9, nboot = 200, parallel = T)
hist(parallel_xgboosttest$t[,1])

